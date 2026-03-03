# R/reschedule.R
library(lubridate)

reschedule_resource_chain <- function(gantt_df, resource_id, moved_task_id, anchor_start) {
  g <- gantt_df
  idx <- which(g$resource_id == resource_id)
  if (length(idx) == 0) return(g)
  
  st <- suppressWarnings(as.POSIXct(g$start_date[idx], format = "%Y-%m-%d %H:%M", tz = TZ_LOCAL))
  ord <- order(st, g$id[idx])
  
  moved_row <- which(g$id[idx] == as.character(moved_task_id))
  if (length(moved_row) == 1) ord <- c(moved_row, setdiff(ord, moved_row))
  
  cur <- suppressWarnings(as.POSIXct(anchor_start, tz = TZ_LOCAL))
  if (is.na(cur)) {
    cur <- suppressWarnings(min(st, na.rm = TRUE))
    if (!is.finite(as.numeric(cur))) cur <- as.POSIXct(paste(Sys.Date(), "09:00:00"), tz = TZ_LOCAL)
  }
  
  target_rows <- idx[ord]
  for (k in seq_along(target_rows)) {
    r <- target_rows[k]
    g$start_date[r] <- format(cur, "%Y-%m-%d %H:%M")
    dur <- suppressWarnings(as.numeric(g$duration[r]))
    if (is.na(dur) || dur <= 0) dur <- TASK_FALLBACK_HOURS
    cur <- cur + as.difftime(dur, units = "hours")
  }
  
  g
}
