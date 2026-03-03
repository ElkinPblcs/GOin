# R/build_payload.R
library(dplyr)
library(stringr)
library(tibble)
library(lubridate)


detect_business_unit <- function(skill_names_chr) {
  units <- c(
    "Migra Pre2Pos",
    "Base Development",
    "Refresh Key Visual",
    "Network / Coverage",
    "FMC Gross",
    "Gross",
    "Reloads",
    "Gross (low penetration)",
    "Devices"
  )

  x <- safe_chr(skill_names_chr)

  vapply(x, function(v) {
    if (is.na(v) || trimws(v) == "") return("")
    tags <- trimws(unlist(strsplit(v, "\\|", fixed = FALSE)))
    hit <- units[units %in% tags]
    if (length(hit) == 0) "" else hit[1]
  }, character(1))
}

build_planned_from_a_plan <- function(a_plan) {
  if (is.null(a_plan) || nrow(a_plan) == 0) {
    return(list(tasks = tibble(), resources = tibble()))
  }
  
  need <- c("id","title","status","priority","description",
            "planned_start","planned_end",
            "collab_email_plan","skill_main","typeTask_name","tag")
  miss <- setdiff(need, colnames(a_plan))
  if (length(miss) > 0) {
    stop("a_plan no tiene estas columnas: ", paste(miss, collapse = ", "))
  }
  if (!"skill_names" %in% colnames(a_plan)) a_plan$skill_names <- ""
  
  resources <- a_plan %>%
    mutate(
      resource_id = if_else(is.na(collab_email_plan) | collab_email_plan == "",
                            "SIN_ASIGNAR", trimws(as.character(collab_email_plan))),
      resource_name = resource_id
    ) %>%
    distinct(resource_id, resource_name) %>%   # <-- SOLO estas dos
    arrange(resource_name)
  
  
  tasks <- a_plan %>%
    mutate(
      id = as.character(.data$id),
      text = safe_chr(title),
      resource_id = if_else(is.na(collab_email_plan) | collab_email_plan == "",
                            "SIN_ASIGNAR", as.character(collab_email_plan)),
      start_date = fmt_gantt(planned_start),
      duration = if_else(
        !is.na(planned_start) & !is.na(planned_end),
        as.numeric(difftime(as.POSIXct(planned_end, tz = TZ_LOCAL),
                            as.POSIXct(planned_start, tz = TZ_LOCAL),
                            units = "hours")),
        NA_real_
      ),
      duration = if_else(is.na(duration) | duration <= 0, TASK_FALLBACK_HOURS, duration),
      status = safe_chr(status),
      priority = safe_chr(priority),
      description = safe_chr(description),
      skill_main = safe_chr(skill_main),
      typeTask_name = safe_chr(typeTask_name),
      tag = safe_chr(tag),
      skill_names = safe_chr(skill_names),
      business_unit = detect_business_unit(skill_names),
      pais = extract_country_from_tag(tag)
    ) %>%
    left_join(resources, by = "resource_id") %>%
    mutate(resource_name = coalesce(resource_name, resource_id)) %>%
    filter(!is.na(start_date) & start_date != "") %>%
    arrange(resource_id, start_date, id) %>%
    select(id, text, start_date, duration, resource_id, resource_name,
           skill_main, typeTask_name, tag, skill_names, business_unit, pais, status, priority, description)
  
  list(tasks = tasks, resources = resources)
}
