library(dplyr)
library(readr)
library(httr)
library(jsonlite)
library(openssl)
library(stringr)
library(lubridate)
library(tidyr)
library(tibble)

source("R/pull/00_config.R")
source("R/pull/01_cor_fetch.R")
source("R/pull/02_scheduler.R")

df_sla <- readr::read_csv(CSV_SLA, show_col_types = FALSE)
workers_allowed <- readr::read_csv(CSV_WORKERS_ALLOWED, show_col_types = FALSE)
df_workers <- readr::read_csv(CSV_WORKERS_PODS, show_col_types = FALSE)

a <- build_a_from_cor(preferred_lang = "es", pause_sec = 0.05)
a <- add_sla_to_a(a, df_sla)

res <- schedule_tasks_from_today(a, workers_allowed, anchor_date = Sys.Date())
a_plan <- res$a_plan

head(a_plan %>% select(id, tag, collab_email, collab_email_plan, planned_start, planned_end, note), 30)
