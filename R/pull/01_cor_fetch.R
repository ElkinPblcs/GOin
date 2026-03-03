get_token <- function() {
  basic <- openssl::base64_encode(charToRaw(paste0(COR_API_KEY, ":", COR_CLIENT_SECRET)))
  r <- httr::POST(
    url = paste0(API_BASE, "/oauth/token"),
    query = list(grant_type = "client_credentials"),
    httr::add_headers(Authorization = paste("Basic", basic), Accept = "application/json")
  )
  httr::stop_for_status(r)
  httr::content(r, as = "parsed", type = "application/json")$access_token
}

get_tasks_by_status_paged <- function(token, status_value, per_page = 200) {
  filters_json <- jsonlite::toJSON(list(status = status_value), auto_unbox = TRUE)
  
  page <- 1
  out <- list()
  
  repeat {
    r <- httr::GET(
      url = paste0(API_BASE, "/tasks"),
      query = list(page = page, perPage = per_page, filters = filters_json),
      httr::add_headers(Authorization = paste("Bearer", token), Accept = "application/json")
    )
    httr::stop_for_status(r)
    
    payload <- httr::content(r, as = "parsed", type = "application/json")
    rows <- payload$data
    if (is.null(rows) || length(rows) == 0) break
    
    out[[length(out) + 1]] <- jsonlite::fromJSON(jsonlite::toJSON(rows), flatten = TRUE)
    if (!is.null(payload$lastPage) && !is.null(payload$page) && payload$page >= payload$lastPage) break
    page <- page + 1
  }
  
  if (length(out) == 0) return(data.frame())
  dplyr::bind_rows(out)
}

get_task_by_id <- function(token, task_id) {
  r <- httr::GET(
    url = paste0(API_BASE, "/tasks/", task_id),
    httr::add_headers(Authorization = paste("Bearer", token), Accept = "application/json")
  )
  httr::stop_for_status(r)
  httr::content(r, as = "parsed", type = "application/json")
}

norm_name <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  x <- stringr::str_to_lower(stringr::str_squish(x))
  iconv(x, from = "UTF-8", to = "ASCII//TRANSLIT")
}

collab_to_list <- function(cobj) {
  if (is.null(cobj)) return(NULL)
  if (is.data.frame(cobj)) {
    if (nrow(cobj) == 0) return(NULL)
    return(as.list(cobj[1, , drop = FALSE]))
  }
  if (is.list(cobj)) return(cobj)
  NULL
}

collab_full_name <- function(cobj) {
  fn <- if (!is.null(cobj$first_name)) as.character(cobj$first_name)[1] else ""
  ln <- if (!is.null(cobj$last_name)) as.character(cobj$last_name)[1] else ""
  stringr::str_squish(paste(fn, ln))
}

build_worker_lookup <- function(workers_df,
                                allowed_roles = c("Designer 2", "Designer 1", "Multimedia producer")) {
  if (is.null(workers_df) || !is.data.frame(workers_df) || nrow(workers_df) == 0) return(character(0))
  need <- c("worker_name", "first_name", "last_name", "puesto")
  if (!all(need %in% names(workers_df))) return(character(0))

  wk <- workers_df %>%
    dplyr::mutate(
      .puesto = norm_name(puesto),
      .full_1 = norm_name(worker_name),
      .full_2 = norm_name(paste(first_name, last_name))
    ) %>%
    dplyr::filter(.puesto %in% norm_name(allowed_roles))

  unique(c(wk$.full_1, wk$.full_2))
}

collab_is_allowed_worker <- function(cobj, worker_lookup) {
  if (length(worker_lookup) == 0) return(FALSE)

  c_full <- norm_name(collab_full_name(cobj))
  c_pair <- norm_name(paste(
    if (!is.null(cobj$first_name)) as.character(cobj$first_name)[1] else "",
    if (!is.null(cobj$last_name)) as.character(cobj$last_name)[1] else ""
  ))

  (c_full != "" && c_full %in% worker_lookup) ||
    (c_pair != "" && c_pair %in% worker_lookup)
}

get_first_collab <- function(x, worker_lookup = character(0)) {
  if (is.null(x) || length(x) == 0) return(NULL)

  candidates <- list()
  if (is.data.frame(x)) {
    candidates <- lapply(seq_len(nrow(x)), function(i) as.list(x[i, , drop = FALSE]))
  } else if (is.list(x)) {
    candidates <- lapply(x, collab_to_list)
    candidates <- Filter(Negate(is.null), candidates)
  }

  if (length(candidates) == 0) return(NULL)
  if (length(worker_lookup) == 0) return(candidates[[1]])

  for (cnd in candidates) {
    if (collab_is_allowed_worker(cnd, worker_lookup)) return(cnd)
  }

  candidates[[1]]
}

extract_skills_one_lang <- function(skill_list, preferred_lang = "es") {
  if (is.null(skill_list) || length(skill_list) == 0) return(NA_character_)
  df <- tryCatch(jsonlite::fromJSON(jsonlite::toJSON(skill_list), flatten = TRUE), error = function(e) NULL)
  if (is.null(df) || nrow(df) == 0) return(NA_character_)
  
  df$lang <- tolower(as.character(df$lang))
  df$label_id <- as.character(df$label_id)
  df$name <- trimws(as.character(df$name))
  
  picked <- df %>%
    dplyr::group_by(label_id) %>%
    dplyr::arrange(dplyr::desc(lang == tolower(preferred_lang))) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup()
  
  out <- unique(picked$name)
  out <- out[!is.na(out) & out != ""]
  if (length(out) == 0) return(NA_character_)
  paste(out, collapse = " | ")
}

extract_typeTask_name <- function(typeTask_obj) {
  if (is.null(typeTask_obj) || is.null(typeTask_obj$names)) return(NA_character_)
  nm <- typeTask_obj$names
  if (is.list(nm) && length(nm) > 0 && is.list(nm[[1]]) && !is.null(nm[[1]]$name)) return(as.character(nm[[1]]$name)[1])
  NA_character_
}

enrich_tasks_with_details <- function(token, task_ids, preferred_lang = "es", pause_sec = 0.00, cores = 2L) {
  task_ids <- unique(as.character(task_ids))
  if (length(task_ids) == 0) return(tibble::tibble(id = character(), skill_names = character(), typeTask_name = character()))

  fetch_one <- function(tid) {
    if (!is.null(pause_sec) && pause_sec > 0) Sys.sleep(pause_sec)
    payload <- tryCatch(get_task_by_id(token, tid), error = function(e) NULL)
    if (is.null(payload)) {
      return(tibble::tibble(id = as.character(tid), skill_names = NA_character_, typeTask_name = NA_character_))
    }
    tibble::tibble(
      id = as.character(payload$id),
      skill_names = extract_skills_one_lang(payload$skill, preferred_lang = preferred_lang),
      typeTask_name = extract_typeTask_name(payload$typeTask)
    )
  }

  n_cores <- max(1L, min(as.integer(cores), 2L, length(task_ids)))
  if (length(task_ids) < 20L || n_cores <= 1L) {
    rows <- lapply(task_ids, fetch_one)
    return(dplyr::bind_rows(rows))
  }

  rows <- if (.Platform$OS.type != "windows") {
    parallel::mclapply(task_ids, fetch_one, mc.cores = n_cores)
  } else {
    cl <- parallel::makeCluster(n_cores)
    on.exit(parallel::stopCluster(cl), add = TRUE)
    parallel::clusterExport(cl, varlist = c("token", "pause_sec", "preferred_lang",
                                            "get_task_by_id", "extract_skills_one_lang", "extract_typeTask_name"),
                            envir = environment())
    parallel::parLapply(cl, task_ids, fetch_one)
  }

  dplyr::bind_rows(rows)
}

build_a_from_cor <- function(preferred_lang = "es", pause_sec = 0.00, workers_df = NULL) {
  token <- get_token()
  
  statuses_no_final <- c("en_revision", "nueva", "en_proceso", "en_diseno")
  tasks <- dplyr::bind_rows(lapply(statuses_no_final, function(s) get_tasks_by_status_paged(token, s)))

  if ("archived" %in% names(tasks)) {
    tasks <- tasks %>%
      dplyr::mutate(.archived_flag = tolower(trimws(as.character(archived))) %in% c("true", "1", "t")) %>%
      dplyr::filter(!.archived_flag) %>%
      dplyr::select(-.archived_flag)
  }
  
  if (is.null(workers_df) && exists("df_workers", inherits = TRUE)) workers_df <- get("df_workers", inherits = TRUE)
  worker_lookup <- build_worker_lookup(workers_df)
  first_collabs <- lapply(tasks$collaborators, function(z) get_first_collab(z, worker_lookup = worker_lookup))
  
  tasks_flat <- tasks %>%
    dplyr::mutate(
      collab_id = vapply(first_collabs, function(c) if (!is.null(c$id)) as.character(c$id) else NA_character_, character(1)),
      collab_first_name = vapply(first_collabs, function(c) if (!is.null(c$first_name)) as.character(c$first_name) else NA_character_, character(1)),
      collab_last_name  = vapply(first_collabs, function(c) if (!is.null(c$last_name)) as.character(c$last_name) else NA_character_, character(1)),
      collab_email      = vapply(first_collabs, function(c) if (!is.null(c$email)) as.character(c$email) else NA_character_, character(1)),
      collab_userPosition_name = vapply(first_collabs, function(c) {
        if (is.null(c)) return(NA_character_)
        if (!is.null(c[["userPosition.name"]])) return(as.character(c[["userPosition.name"]])[1])
        if (!is.null(c$userPosition) && is.list(c$userPosition) && !is.null(c$userPosition$name)) return(as.character(c$userPosition$name)[1])
        NA_character_
      }, character(1))
    ) %>%
    dplyr::select(-collaborators)
  
  details <- enrich_tasks_with_details(token, tasks_flat$id, preferred_lang = preferred_lang, pause_sec = pause_sec)
  
  project_name_col <- if ("project_name" %in% names(tasks_flat)) {
    "project_name"
  } else if ("project.name" %in% names(tasks_flat)) {
    "project.name"
  } else {
    NULL
  }

  a <- tasks_flat %>%
    dplyr::mutate(
      id = as.character(id),
      project_name = if (is.null(project_name_col)) NA_character_ else as.character(.data[[project_name_col]])
    ) %>%
    dplyr::left_join(details, by = "id") %>%
    dplyr::transmute(
      id, title, description, project_id, project_name, deadline, status, priority,
      archived, user_id, task_father, hour_charged, order_tasks, datetime, deliverable,
      collab_id, collab_first_name, collab_last_name, collab_email, collab_userPosition_name,
      skill_names, typeTask_name,
      tag = dplyr::if_else(grepl("\\[(.*?)\\]", title), sub(".*\\[(.*?)\\].*", "\\1", title), NA_character_)
    )
  
  a
}
