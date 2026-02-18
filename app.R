# app.R

# 0) TEMP (para que no se rompa)
dir.create(tempdir(), recursive = TRUE, showWarnings = FALSE)

# 1) Registra la carpeta www/ como estáticos aunque uses shinyApp()
shiny::addResourcePath("assets", "www")

# 2) Tu código
source("R/config.R")
source("R/helpers.R")
source("R/build_payload.R")
source("R/reschedule.R")

source("ui.R")
source("server.R")

# 3) Arranca
shinyApp(ui, server)
