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

get_first_collab <- function(x) {
  if (is.null(x) || length(x) == 0) return(NULL)
  if (is.data.frame(x)) return(if (nrow(x) == 0) NULL else as.list(x[1, , drop = FALSE]))
  if (is.list(x)) {
    first <- x[[1]]
    if (is.null(first)) return(NULL)
    if (is.data.frame(first)) return(if (nrow(first) == 0) NULL else as.list(first[1, , drop = FALSE]))
    if (is.list(first)) return(first)
  }
  NULL
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

enrich_tasks_with_details <- function(token, task_ids, preferred_lang = "es", pause_sec = 0.05) {
  task_ids <- unique(as.character(task_ids))
  
  rows <- lapply(task_ids, function(tid) {
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
  })
  
  dplyr::bind_rows(rows)
}

build_a_from_cor <- function(preferred_lang = "es", pause_sec = 0.05) {
  token <- get_token()
  
  statuses_no_final <- c("en_revision", "nueva", "en_proceso", "en_diseno")
  tasks <- dplyr::bind_rows(lapply(statuses_no_final, function(s) get_tasks_by_status_paged(token, s)))
  
  first_collabs <- lapply(tasks$collaborators, get_first_collab)
  
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
  
  a <- tasks_flat %>%
    dplyr::mutate(id = as.character(id)) %>%
    dplyr::left_join(details, by = "id") %>%
    dplyr::transmute(
      id, title, description, project_id, deadline, status, priority,
      archived, user_id, task_father, hour_charged, order_tasks, datetime, deliverable,
      collab_id, collab_first_name, collab_last_name, collab_email, collab_userPosition_name,
      skill_names, typeTask_name,
      tag = dplyr::if_else(grepl("\\[(.*?)\\]", title), sub(".*\\[(.*?)\\].*", "\\1", title), NA_character_)
    )
  
  a
}
