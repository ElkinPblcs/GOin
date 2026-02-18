# R/helpers.R
library(dplyr)
library(lubridate)
library(stringr)
library(tibble)

df_to_rows <- function(df) {
  if (is.null(df) || nrow(df) == 0) return(list())
  lapply(seq_len(nrow(df)), function(i) as.list(df[i, , drop = FALSE]))
}

fmt_gantt <- function(x) {
  x <- suppressWarnings(as.POSIXct(x, tz = TZ_LOCAL))
  ifelse(is.na(x), NA_character_, format(x, "%Y-%m-%d %H:%M"))
}

safe_chr <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  x
}

extract_country_from_tag <- function(tag_chr) {
  t <- safe_chr(tag_chr)
  t <- str_to_upper(str_trim(t))
  
  iso2_list <- c("CO","PA","CR","HN","NI","PY","BO","SV","GT","EC","PE","CL","AR","UY","BR","MX","DO","CA")
  
  out <- ifelse(t %in% iso2_list, t, NA_character_)
  miss <- is.na(out) | out == ""
  
  if (any(miss)) {
    pat <- paste0("(^|[^A-Z])(", paste(iso2_list, collapse="|"), ")([^A-Z]|$)")
    m <- str_match(t[miss], pat)
    out[miss] <- ifelse(!is.na(m[,3]), m[,3], NA_character_)
  }
  
  out[is.na(out)] <- ""
  out
}
