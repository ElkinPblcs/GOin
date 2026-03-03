infer_skill_main <- function(skill_names) {
  if (is.na(skill_names) || stringr::str_squish(skill_names) == "") return(NA_character_)
  x <- toupper(skill_names)
  if (stringr::str_detect(x, "ADAPTACIONES|ADAP")) return("ADAPTACIONES")
  if (stringr::str_detect(x, "\\bPRO\\b")) return("PRO")
  if (stringr::str_detect(x, "\\bPLUS\\b")) return("PLUS")
  if (stringr::str_detect(x, "EST[ÁA]NDAR|\\bESTANDAR\\b")) return("ESTÁNDAR")
  NA_character_
}

normalize_typeTask_for_sla <- function(typeTask_name, skill_main) {
  if (!is.na(skill_main) && skill_main == "ADAPTACIONES") return("ADAP")
  if (is.na(typeTask_name) || stringr::str_squish(typeTask_name) == "") return(NA_character_)
  x <- tolower(stringr::str_squish(typeTask_name))
  if (stringr::str_detect(x, "est[aá]tic")) return("Key Visual Estático")
  if (stringr::str_detect(x, "animad"))     return("Key Visual Animado")
  stringr::str_squish(typeTask_name)
}

parse_dt <- function(x) suppressWarnings(lubridate::ymd_hms(x, tz = API_TZ))

tag_to_country <- function(tag) {
  if (is.na(tag)) return(NA_character_)
  t <- toupper(stringr::str_squish(tag))
  if (t %in% COUNTRY_CODES) return(t)
  NA_character_
}

is_micro <- function(x) !is.na(x) && (abs(x - 1/30) < 1e-9 || abs(x - 1/15) < 1e-9)
micro_kind <- function(x) {
  if (is.na(x)) return(NA_character_)
  if (abs(x - 1/30) < 1e-9) return("micro_30")
  if (abs(x - 1/15) < 1e-9) return("micro_15")
  NA_character_
}
micro_cap <- function(kind) ifelse(kind == "micro_30", 30L, 15L)

day_start <- function(d) as.POSIXct(paste0(as.Date(d), sprintf(" %02d:00:00", DAY_START_HOUR)), tz = API_TZ)









add_sla_to_a <- function(a, df_sla, debug = TRUE) {
  
  # --- validaciones mínimas (para que no sea un join silencioso a nada) ---
  needed_sla_cols <- c("collab_userPosition_name","typeTask_name","skill_main","tiempo_dias","cantidad_por_dia")
  miss <- setdiff(needed_sla_cols, names(df_sla))
  if (length(miss) > 0) stop(paste0("df_sla NO tiene columnas: ", paste(miss, collapse=", ")))
  
  if (!"skill_main" %in% names(a)) a$skill_main <- NA_character_
  if (!"typeTask_sla" %in% names(a)) a$typeTask_sla <- NA_character_
  if (!"tiempo_estimado_dias" %in% names(a)) a$tiempo_estimado_dias <- NA_real_
  
  # 1) calcular llaves SLA en a (igual que antes)
  a2 <- a %>%
    dplyr::mutate(
      skill_main = dplyr::if_else(
        is.na(skill_main),
        vapply(skill_names, infer_skill_main, character(1)),
        as.character(skill_main)
      ),
      typeTask_sla = dplyr::if_else(
        is.na(typeTask_sla),
        vapply(seq_along(typeTask_name), function(i) {
          normalize_typeTask_for_sla(typeTask_name[i], skill_main[i])
        }, character(1)),
        as.character(typeTask_sla)
      )
    )
  
  # 2) llaves normalizadas SOLO para el join (NO toca tus columnas originales)
  a_keys <- a2 %>%
    dplyr::mutate(
      k_role  = stringr::str_squish(tolower(as.character(collab_userPosition_name))),
      k_type  = stringr::str_squish(tolower(as.character(typeTask_sla))),
      k_skill = stringr::str_squish(tolower(as.character(skill_main)))
    )
  
  sla_keys <- df_sla %>%
    dplyr::mutate(
      k_role  = stringr::str_squish(tolower(as.character(collab_userPosition_name))),
      k_type  = stringr::str_squish(tolower(as.character(typeTask_name))),
      k_skill = stringr::str_squish(tolower(as.character(skill_main)))
    ) %>%
    dplyr::select(k_role, k_type, k_skill, tiempo_dias, cantidad_por_dia)
  
  # 3) join robusto (por k_*)
  out <- a_keys %>%
    dplyr::left_join(sla_keys, by = c("k_role","k_type","k_skill")) %>%
    dplyr::mutate(
      tiempo_estimado_dias = dplyr::if_else(
        is.na(tiempo_estimado_dias),
        dplyr::case_when(
          !is.na(cantidad_por_dia) & cantidad_por_dia == 30 ~ 1/30*as.numeric(deliverable),
          !is.na(cantidad_por_dia) & cantidad_por_dia == 15 ~ 1/15*as.numeric(deliverable),
          !is.na(cantidad_por_dia) & cantidad_por_dia == 1  ~ 1,
          TRUE ~ tiempo_dias
        ),
        tiempo_estimado_dias
      )
    ) %>%
    dplyr::select(-k_role, -k_type, -k_skill)
  
  # 4) DEBUG que prueba si matcheó algo de verdad
  if (isTRUE(debug)) {
    cat("\n================ DEBUG add_sla_to_a (ROBUST JOIN) ================\n")
    cat("[NA counts en a (llaves)]\n")
    cat("skill_main NA:", sum(is.na(a2$skill_main)), "/", nrow(a2), "\n")
    cat("typeTask_sla NA:", sum(is.na(a2$typeTask_sla)), "/", nrow(a2), "\n")
    
    cat("\n[Post-join: cuántas filas SI trajeron SLA]\n")
    cat("tiempo_dias NO-NA:", sum(!is.na(out$tiempo_dias)), "/", nrow(out), "\n")
    cat("cantidad_por_dia NO-NA:", sum(!is.na(out$cantidad_por_dia)), "/", nrow(out), "\n")
    cat("tiempo_estimado_dias NO-NA:", sum(!is.na(out$tiempo_estimado_dias)), "/", nrow(out), "\n")
    
    cat("\n[Ejemplos matcheados (deberías ver Designer 1/2 ADAP etc.)]\n")
    print(
      out %>%
        dplyr::filter(!is.na(tiempo_dias) | !is.na(cantidad_por_dia)) %>%
        dplyr::select(id, collab_userPosition_name, typeTask_name, typeTask_sla, skill_names, skill_main,
                      tiempo_dias, cantidad_por_dia, tiempo_estimado_dias) %>%
        head(20)
    )
    
    cat("\n[Ejemplos NO matcheados]\n")
    print(
      out %>%
        dplyr::filter(is.na(tiempo_dias) & is.na(cantidad_por_dia)) %>%
        dplyr::select(id, collab_userPosition_name, typeTask_name, typeTask_sla, skill_names, skill_main) %>%
        head(20)
    )
    cat("==================================================================\n\n")
  }
  
  out
}











init_schedule <- function(a, workers_allowed, anchor_date = Sys.Date()) {
  workers_from_a <- a %>%
    dplyr::filter(!is.na(collab_email), collab_email != "",
                  collab_userPosition_name %in% WORK_ROLES) %>%
    dplyr::distinct(collab_email, collab_userPosition_name)
  
  workers_all <- workers_allowed %>% dplyr::distinct(collab_email)
  
  dplyr::bind_rows(
    workers_from_a %>% dplyr::transmute(collab_email, collab_userPosition_name),
    workers_all %>% dplyr::transmute(collab_email, collab_userPosition_name = NA_character_)
  ) %>%
    dplyr::distinct(collab_email, .keep_all = TRUE) %>%
    dplyr::mutate(
      next_free = day_start(anchor_date),
      cnt_GT=0L, cnt_CR=0L, cnt_NI=0L, cnt_PA=0L, cnt_SV=0L, cnt_HN=0L,
      cnt_total = 0L
    )
}

pick_worker <- function(sched, workers_allowed, cc, preferred_email = NA_character_) {
  cand <- sched
  
  if (!is.na(cc)) {
    allowed_emails <- workers_allowed %>% dplyr::filter(allowed_code == cc) %>% dplyr::distinct(collab_email)
    cand <- dplyr::inner_join(cand, allowed_emails, by = "collab_email")
    if (nrow(cand) == 0) return(NULL)
  }
  
  if (!is.na(preferred_email) && preferred_email != "" && preferred_email %in% cand$collab_email) {
    return(cand %>% dplyr::filter(collab_email == preferred_email) %>% dplyr::slice(1))
  }
  
  pen_country <- rep(0, nrow(cand))
  if (!is.na(cc)) {
    coln <- paste0("cnt_", cc)
    if (coln %in% names(cand)) pen_country <- cand[[coln]]
  }
  
  pen_total <- cand$cnt_total
  
  cand %>%
    dplyr::mutate(score = as.numeric(next_free) + pen_country * 1e6 + pen_total * 1e4) %>%
    dplyr::arrange(score, collab_email) %>%
    dplyr::slice(1)
}

bump_counts <- function(sched, email, cc, add_total = 1L, add_cc = 1L) {
  sched <- sched %>% dplyr::mutate(cnt_total = dplyr::if_else(collab_email == email, cnt_total + add_total, cnt_total))
  if (!is.na(cc)) {
    coln <- paste0("cnt_", cc)
    if (coln %in% names(sched)) {
      sched <- sched %>%
        dplyr::mutate(!!coln := dplyr::if_else(collab_email == email, .data[[coln]] + add_cc, .data[[coln]]))
    }
  }
  sched
}

schedule_tasks_from_today <- function(a, workers_allowed, anchor_date = Sys.Date()) {
  sched <- init_schedule(a, workers_allowed, anchor_date = anchor_date)
  assigned <- list()
  
  todo <- a %>%
    dplyr::mutate(
      arrival = parse_dt(datetime),
      arrival = dplyr::if_else(is.na(arrival), as.POSIXct("2100-01-01 00:00:00", tz = API_TZ), arrival),
      cc = vapply(tag, tag_to_country, character(1)),
      micro = vapply(tiempo_estimado_dias, is_micro, logical(1)),
      mkind = vapply(tiempo_estimado_dias, micro_kind, character(1)),
      preferred_email = dplyr::if_else(!is.na(collab_email) & collab_email != "", as.character(collab_email), NA_character_)
    ) %>%
    dplyr::arrange(arrival, id)
  
  normal_tasks <- todo %>% dplyr::filter(!micro)
  micro_tasks  <- todo %>% dplyr::filter(micro)
  
  for (i in seq_len(nrow(normal_tasks))) {
    trow <- normal_tasks[i, ]
    
    if (is.na(trow$tiempo_estimado_dias)) {
      assigned[[length(assigned)+1]] <- tibble::tibble(
        id = trow$id, collab_email_new = NA_character_,
        planned_start = as.POSIXct(NA, tz=API_TZ), planned_end = as.POSIXct(NA, tz=API_TZ),
        note = "NO_DURATION"
      )
      next
    }
    
    w <- pick_worker(sched, workers_allowed, cc = trow$cc, preferred_email = trow$preferred_email)
    if (is.null(w)) {
      assigned[[length(assigned)+1]] <- tibble::tibble(
        id = trow$id, collab_email_new = NA_character_,
        planned_start = as.POSIXct(NA, tz=API_TZ), planned_end = as.POSIXct(NA, tz=API_TZ),
        note = "NO_WORKER_FOR_COUNTRY"
      )
      next
    }
    
    start <- w$next_free
    end <- start + as.difftime(trow$tiempo_estimado_dias * WORKDAY_HOURS, units="hours")
    
    assigned[[length(assigned)+1]] <- tibble::tibble(
      id = trow$id, collab_email_new = w$collab_email,
      planned_start = start, planned_end = end,
      note = ifelse(!is.na(trow$preferred_email) && trow$preferred_email == w$collab_email, "OK_PREFERRED", "OK")
    )
    
    sched <- sched %>% dplyr::mutate(next_free = dplyr::if_else(collab_email == w$collab_email, end, next_free))
    sched <- bump_counts(sched, w$collab_email, trow$cc, add_total = 1L, add_cc = 1L)
  }
  
  micro_groups <- micro_tasks %>%
    dplyr::filter(!is.na(mkind)) %>%
    dplyr::mutate(cc_grp = dplyr::if_else(is.na(cc), "NO_COUNTRY", cc)) %>%
    dplyr::group_by(cc_grp, mkind) %>%
    dplyr::group_split()
  
  for (g in micro_groups) {
    g <- g %>% dplyr::arrange(arrival, id)
    
    ccg_raw <- unique(g$cc_grp)[1]
    ccg <- if (ccg_raw == "NO_COUNTRY") NA_character_ else ccg_raw
    
    kind <- unique(g$mkind)[1]
    cap <- micro_cap(kind)
    each_dur <- g$tiempo_estimado_dias[1]
    
    idx <- 1
    while (idx <= nrow(g)) {
      pref_block <- g$preferred_email[idx]
      if (is.na(pref_block) || pref_block == "") pref_block <- NA_character_
      
      w <- pick_worker(sched, workers_allowed, cc = ccg, preferred_email = pref_block)
      if (is.null(w)) {
        rest <- g[idx:nrow(g), ]
        assigned[[length(assigned)+1]] <- tibble::tibble(
          id = rest$id, collab_email_new = NA_character_,
          planned_start = as.POSIXct(NA, tz=API_TZ), planned_end = as.POSIXct(NA, tz=API_TZ),
          note = "NO_WORKER_FOR_COUNTRY"
        )
        break
      }
      
      base_day <- as.Date(w$next_free)
      block_start <- day_start(base_day)
      if (w$next_free > block_start) block_start <- w$next_free
      
      take_n <- min(cap, nrow(g) - idx + 1)
      chunk <- g[idx:(idx + take_n - 1), ]
      
      starts <- block_start + as.difftime((seq_len(take_n)-1) * each_dur * WORKDAY_HOURS, units="hours")
      ends   <- block_start + as.difftime((seq_len(take_n))   * each_dur * WORKDAY_HOURS, units="hours")
      block_end <- max(ends, na.rm = TRUE)
      
      assigned[[length(assigned)+1]] <- tibble::tibble(
        id = chunk$id, collab_email_new = w$collab_email,
        planned_start = starts, planned_end = ends,
        note = ifelse(!is.na(pref_block) && pref_block == w$collab_email,
                      paste0("OK_MICRO_PACK_", cap, "_PREFERRED"),
                      paste0("OK_MICRO_PACK_", cap))
      )
      
      sched <- sched %>% dplyr::mutate(next_free = dplyr::if_else(collab_email == w$collab_email, block_end, next_free))
      sched <- bump_counts(sched, w$collab_email, ccg, add_total = take_n, add_cc = take_n)
      idx <- idx + take_n
    }
  }
  
  assigned_df <- dplyr::bind_rows(assigned) %>% dplyr::mutate(id = as.character(id))
  
  a_plan <- a %>%
    dplyr::mutate(id = as.character(id)) %>%
    dplyr::select(-dplyr::any_of(c("planned_start","planned_end","note","collab_email_plan","collab_email_new"))) %>%
    dplyr::left_join(assigned_df, by = "id") %>%
    dplyr::mutate(collab_email_plan = collab_email_new) %>%
    dplyr::select(-collab_email_new)
  
  list(a_plan = a_plan, schedule = sched, assignments = assigned_df)
}
