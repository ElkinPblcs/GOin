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
  x <- parse_datetime_safe(x, tz = TZ_LOCAL)
  ifelse(is.na(x), NA_character_, format(x, "%Y-%m-%d %H:%M"))
}

parse_datetime_safe <- function(x, tz = TZ_LOCAL) {
  parse_one <- function(v) {
    if (is.null(v) || length(v) == 0) return(as.POSIXct(NA, tz = tz))
    if (inherits(v, "POSIXt")) return(as.POSIXct(v, tz = tz))

    if (is.list(v)) {
      if (length(v) == 0) return(as.POSIXct(NA, tz = tz))
      v <- v[[1]]
      if (is.null(v) || length(v) == 0) return(as.POSIXct(NA, tz = tz))
    }

    if (is.numeric(v)) {
      return(suppressWarnings(as.POSIXct(v, origin = "1970-01-01", tz = tz)))
    }

    vv <- trimws(as.character(v)[1])
    if (is.na(vv) || vv == "") return(as.POSIXct(NA, tz = tz))

    parsed <- suppressWarnings(lubridate::ymd_hms(vv, tz = tz, quiet = TRUE))
    if (!is.na(parsed)) return(parsed)

    parsed <- suppressWarnings(lubridate::parse_date_time(
      vv,
      orders = c(
        "Y-m-d H:M:S", "Y-m-d H:M",
        "Y/m/d H:M:S", "Y/m/d H:M",
        "Ymd HMS", "Ymd HM",
        "d/m/Y H:M:S", "d/m/Y H:M"
      ),
      tz = tz,
      quiet = TRUE
    ))

    suppressWarnings(as.POSIXct(parsed, tz = tz))
  }

  if (inherits(x, "POSIXt")) return(as.POSIXct(x, tz = tz))

  out <- lapply(seq_along(x), function(i) parse_one(x[[i]]))
  do.call(c, out)
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
