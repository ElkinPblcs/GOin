library(dplyr)
library(lubridate)

pack_all_resources_no_gaps <- function(tasks,
                                       tz = TZ_LOCAL,
                                       start_col = "start_date",
                                       dur_col = "duration",
                                       rid_col = "resource_id",
                                       id_col = "id",
                                       default_start_hour = "09:00") {
  if (is.null(tasks) || nrow(tasks) == 0) return(tasks)
  
  tasks <- tasks %>%
    mutate(
      .rid = trimws(as.character(.data[[rid_col]])),
      .id  = as.character(.data[[id_col]]),
      .st  = suppressWarnings(as.POSIXct(.data[[start_col]], format="%Y-%m-%d %H:%M", tz=tz)),
      .dur = suppressWarnings(as.numeric(.data[[dur_col]]))
    )
  
  tasks$.dur[is.na(tasks$.dur) | tasks$.dur <= 0] <- TASK_FALLBACK_HOURS
  
  rids <- unique(tasks$.rid)
  rids <- rids[!is.na(rids) & rids != ""]
  
  for (rid in rids) {
    idx <- which(tasks$.rid == rid)
    if (length(idx) <= 1) next
    
    sub <- tasks[idx, , drop = FALSE]
    
    # Orden actual: por start_date (NA al final) + id para estabilidad
    st_ord <- sub$.st
    st_ord[is.na(st_ord)] <- as.POSIXct("2999-12-31 00:00:00", tz=tz)
    sub <- sub[order(st_ord, sub$.id), , drop = FALSE]
    
    # Anchor: el mínimo start real; si no hay, hoy 09:00
    anchor <- suppressWarnings(min(sub$.st, na.rm = TRUE))
    if (!is.finite(as.numeric(anchor))) {
      anchor <- as.POSIXct(paste(Sys.Date(), paste0(default_start_hour, ":00")), tz = tz)
    }
    
    cur <- anchor
    for (k in seq_len(nrow(sub))) {
      tasks[[start_col]][ idx[ match(sub$.id[k], tasks$.id[idx]) ][1] ] <- format(cur, "%Y-%m-%d %H:%M")
      cur <- cur + hours(sub$.dur[k])
    }
    
    # refrescar .st del bloque (opcional)
    tasks$.st[idx] <- suppressWarnings(as.POSIXct(tasks[[start_col]][idx], format="%Y-%m-%d %H:%M", tz=tz))
  }
  
  tasks %>% select(-.rid, -.id, -.st, -.dur)
}
