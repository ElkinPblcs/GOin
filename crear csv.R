# ============================================================
# CREATE READY-TO-USE CSVs (NO parsing later)
#   1) sla.csv            (incluye skill_main)
#   2) workers_allowed.csv (formato largo: email + allowed_code)
#   3) workers_pods.csv
# ============================================================

library(dplyr)
library(tidyr)
library(stringr)
library(tibble)
library(readr)

# ----------------------------
# 1) sla.csv (READY)
# ----------------------------
df_sla <- tibble::tribble(
  ~collab_userPosition_name, ~typeTask_name,        ~skill_main,     ~tiempo_dias, ~cantidad_por_dia,
  "Designer 1",              "Key Visual Estático", "PRO",           4,            NA,
  "Designer 1",              "Key Visual Estático", "PLUS",          2,            NA,
  "Designer 1",              "Key Visual Estático", "ESTÁNDAR",      1,            1,
  "Designer 1",              "ADAP",                "ADAPTACIONES",  1,            30,
  
  "Designer 2",              "Key Visual Animado",  "PRO",           3,            NA,
  "Designer 2",              "Key Visual Animado",  "PLUS",          2,            NA,
  "Designer 2",              "Key Visual Animado",  "ESTÁNDAR",      1,            1,
  "Designer 2",              "ADAP",                "ADAPTACIONES",  1,            15,
  
  "Multimedia Producer",     "Key Visual Animado",  "PRO",           3,            NA,
  "Multimedia Producer",     "Key Visual Animado",  "PLUS",          2,            NA,
  "Multimedia Producer",     "Key Visual Animado",  "ESTÁNDAR",      1,            1,
  "Multimedia Producer",     "ADAP",                "ADAPTACIONES",  1,            15
) %>%
  mutate(
    collab_userPosition_name = str_squish(collab_userPosition_name),
    typeTask_name = str_squish(typeTask_name),
    skill_main = str_squish(skill_main)
  )

write_csv(df_sla, "sla.csv", na = "")

# ----------------------------
# 2) workers_allowed.csv (READY, LONG FORMAT)
# ----------------------------
workers_allowed_long <- tibble::tribble(
  ~collab_email,                         ~allowed_code,
  "oscar.juarez@millicom.com",           "GT",
  "oscar.juarez@millicom.com",           "CR",
  "marylyn.gomez@millicom.com",          "GT",
  "marylyn.gomez@millicom.com",          "CR",
  "javier.molina@millicom.com",          "GT",
  "javier.molina@millicom.com",          "CR",
  
  "lars.aguirre@millicom.com",           "NI",
  "lars.aguirre@millicom.com",           "PA",
  "walter.ruiz@millicom.com",            "NI",
  "walter.ruiz@millicom.com",            "PA",
  "juandiego.porras@millicom.com",       "NI",
  "juandiego.porras@millicom.com",       "PA",
  "jonathanarmando.pixtun@millicom.com", "NI",
  "jonathanarmando.pixtun@millicom.com", "PA",
  
  "karlaveronica.ramos@millicom.com",    "SV",
  "karlaveronica.ramos@millicom.com",    "HN",
  "jonathan.perez@millicom.com",         "SV",
  "jonathan.perez@millicom.com",         "HN",
  "jorge.anleu@millicom.com",            "SV",
  "jorge.anleu@millicom.com",            "HN",
  "shirley.perez@millicom.com",          "SV",
  "shirley.perez@millicom.com",          "HN",
  "kristel.cuevas@millicom.com",         "SV",
  "kristel.cuevas@millicom.com",         "HN"
) %>%
  mutate(
    collab_email = str_squish(tolower(collab_email)),
    allowed_code = str_squish(toupper(allowed_code))
  ) %>%
  distinct()

write_csv(workers_allowed_long, "workers_allowed.csv", na = "")

# ----------------------------
# 3) workers_pods.csv (READY)
# ----------------------------
pod_map <- tibble::tribble(
  ~pod,   ~cluster,   ~countries,                       ~country_codes,
  "Pod 1","Cluster 1","Guatemala, Costa Rica",          "GT,CR",
  "Pod 2","Cluster 2","Nicaragua, Panamá",              "NI,PA",
  "Pod 3","Cluster 3","El Salvador, Honduras",          "SV,HN"
)

pods_wide <- tibble::tribble(
  ~puesto,                ~`Pod 1`,            ~`Pod 2`,             ~`Pod 3`,
  "Key Account Manager",  "Andrea Girón",      "Marcela Gonzalez",   "Fabiola Gonzalez",
  "Designer 1",           "Santiago Juárez",   "Lars Aguirre",       "Karla Ramos",
  "Designer 1",           "Marylyn Gómez",     "Ivan Gonzalez",      "Jonathan Perez",
  "Designer 2",           "David Sil",         "Alberto Ruiz",       "Jorge Anleu",
  "Designer 2",           "Zayori Pérez",      "Juan Diego Porras",  "Gabriela Caballeros",
  "Multimedia producer",  "Javier Molina",     "Jonathan Pixtún",    "Kristel Cuevas"
)

df_workers <- pods_wide %>%
  pivot_longer(cols = starts_with("Pod "), names_to = "pod", values_to = "worker_name") %>%
  left_join(pod_map, by = "pod") %>%
  mutate(
    worker_name = str_squish(worker_name),
    first_name = str_squish(str_remove(worker_name, "\\s+[^\\s]+$")),
    last_name  = str_squish(str_extract(worker_name, "[^\\s]+$")),
    puesto = str_squish(puesto),
    pod = str_squish(pod),
    cluster = str_squish(cluster),
    countries = str_squish(countries),
    country_codes = gsub("\\s+", "", country_codes)
  ) %>%
  select(worker_name, first_name, last_name, puesto, pod, cluster, countries, country_codes) %>%
  arrange(cluster, pod, puesto, worker_name)

write_csv(df_workers, "workers_pods.csv", na = "")

cat("✅ Listo. Se crearon:\n- sla.csv\n- workers_allowed.csv\n- workers_pods.csv\n")
