# server.R
library(shiny)
library(dplyr)
library(lubridate)

server <- function(input, output, session) {
  planned_rv <- reactiveVal(NULL)
  a_plan_rv <- reactiveVal(NULL)
  selected_id_rv <- reactiveVal(NULL)
  log_rv <- reactiveVal("Listo. Presiona 'Pintar/Refrescar'.")
  
  output$log <- renderText(log_rv())
  
  send_gantt <- function(planned) {
    tasks <- planned$tasks %>%
      mutate(
        resource_id = trimws(as.character(resource_id)),
        pais = trimws(toupper(as.character(pais)))
      )

    # 1) filtro colaborador
    sel <- input$filter_resource
    if (!is.null(sel) && sel != "__ALL__") {
      sel <- trimws(as.character(sel))
      tasks <- tasks %>% dplyr::filter(resource_id == sel)
    }

    # 2) filtro país (multi)
    csel <- input$filter_country
    if (!is.null(csel) && length(csel) > 0 && !("__ALL__" %in% csel)) {
      csel <- trimws(toupper(as.character(csel)))
      tasks <- tasks %>% dplyr::filter(pais %in% csel)
    }

    tasks_original <- tasks %>%
      mutate(
        start_date = dplyr::coalesce(original_start_date, start_date),
        duration = dplyr::coalesce(original_duration, duration)
      )

    session$sendCustomMessage("gantt_data", list(tasks = df_to_rows(tasks)))
    session$sendCustomMessage("gantt_original_data", list(tasks = df_to_rows(tasks_original)))
  }

  

  # ----------------------------
  # Tareas filtradas (mismos filtros que el Gantt)
  # ----------------------------
  get_tasks_filtered <- reactive({
    planned <- planned_rv()
    if (is.null(planned) || is.null(planned$tasks) || nrow(planned$tasks) == 0) {
      return(NULL)
    }
    
    tasks <- planned$tasks %>%
      mutate(
        resource_id = trimws(as.character(resource_id)),
        resource_name = trimws(as.character(resource_name)),
        pais = trimws(toupper(as.character(pais))),
        start_dt = suppressWarnings(as.POSIXct(start_date, format="%Y-%m-%d %H:%M", tz = TZ_LOCAL)),
        dur_h = suppressWarnings(as.numeric(duration))
      ) %>%
      mutate(
        dur_h = dplyr::if_else(is.na(dur_h) | dur_h <= 0, TASK_FALLBACK_HOURS, dur_h),
        end_dt = start_dt + lubridate::hours(dur_h)
      )
    
    # 1) filtro colaborador
    sel <- input$filter_resource
    if (!is.null(sel) && sel != "__ALL__") {
      sel <- trimws(as.character(sel))
      tasks <- tasks %>% dplyr::filter(resource_id == sel)
    }
    
    # 2) filtro país (multi)
    csel <- input$filter_country
    if (!is.null(csel) && length(csel) > 0 && !("__ALL__" %in% csel)) {
      csel <- trimws(toupper(as.character(csel)))
      tasks <- tasks %>% dplyr::filter(pais %in% csel)
    }
    
    tasks
  })
  
  
  
  

  
  observeEvent(input$btn_run, {
    e <- new.env(parent = globalenv()); source("R/pull/run.R", local = e)
    a_plan_rv(e$a_plan)
  }, ignoreInit = TRUE)
  
  
  
  
  observeEvent(input$task_selected_id, {
    selected_id_rv(as.character(input$task_selected_id$id))
    log_rv(paste0("CLICK id=", input$task_selected_id$id, " | nonce=", input$task_selected_id$nonce))
  }, ignoreInit = TRUE)
  
  output$task_details <- renderUI({
    planned <- planned_rv()
    sel_id <- selected_id_rv()
    
    if (is.null(planned) || is.null(sel_id)) {
      return(tags$div(class="detail-box",
                      tags$div(class="detail-title", "Detalle de tarea"),
                      tags$div(style="font-size:12px;opacity:.7;", "Aún no has seleccionado una cápsula.")))
    }
    
    i <- which(planned$tasks$id == sel_id)
    if (length(i) != 1) {
      return(tags$div(class="detail-box",
                      tags$div(class="detail-title", "Detalle de tarea"),
                      tags$div(style="font-size:12px;opacity:.7;", paste0("No encontré el id: ", sel_id))))
    }
    
    t <- planned$tasks[i, , drop = FALSE]
    
    tags$div(class="detail-box",
             tags$div(class="detail-title", paste0("Tarea #", t$id)),
             tags$div(class="detail-row", tags$span(class="detail-k", "Título:"), tags$span(class="detail-v", t$text)),
             tags$div(class="detail-row", tags$span(class="detail-k", "Colab:"),  tags$span(class="detail-v", t$resource_name)),
             tags$div(class="detail-row", tags$span(class="detail-k", "Inicio:"), tags$span(class="detail-v", t$start_date)),
             tags$div(class="detail-row", tags$span(class="detail-k", "Dur(h):"), tags$span(class="detail-v", as.character(t$duration))),
             tags$div(class="detail-row", tags$span(class="detail-k", "Skill:"),  tags$span(class="detail-v", t$skill_main)),
             tags$div(class="detail-row", tags$span(class="detail-k", "Type:"),   tags$span(class="detail-v", t$typeTask_name)),
             tags$div(class="detail-row", tags$span(class="detail-k", "Tag:"),    tags$span(class="detail-v", t$tag)),
             tags$div(class="detail-row", tags$span(class="detail-k", "País:"),   tags$span(class="detail-v", t$pais)),
             tags$div(class="detail-row", tags$span(class="detail-k", "Status:"), tags$span(class="detail-v", t$status)),
             tags$div(class="detail-row", tags$span(class="detail-k", "Prior:"),  tags$span(class="detail-v", t$priority)),
             tags$div(style="margin-top:8px;font-size:12px;opacity:.75;font-weight:800;", "Descripción:"),
             tags$div(
               style="font-size:12px;",
               htmltools::HTML(ifelse(is.na(t$description), "", as.character(t$description)))
             )
             
    )
  })
  
  observeEvent(input$btn_draw, {
    tryCatch({
      ap <- a_plan_rv()
      if (is.null(ap)) stop("Primero presiona 'Actualizar a_plan (run.R)'.")
      
      
      planned <- build_planned_from_a_plan(ap)
      if (nrow(planned$tasks) == 0) {
        log_rv("No hay tareas con planned_start/planned_end para pintar.")
        showNotification("No hay tareas planeadas para pintar.", type = "warning", duration = 8)
        return()
      }
      
      planned_rv(planned)
      
      # ======================
      # choices país (multi)
      # ======================
      cc <- planned$tasks %>%
        mutate(pais = trimws(toupper(as.character(pais)))) %>%
        filter(!is.na(pais), pais != "") %>%
        distinct(pais) %>%
        arrange(pais) %>%
        pull(pais)
      
      country_choices <- c("Todos"="__ALL__", setNames(cc, cc))
      
      updateSelectizeInput(
        session, "filter_country",
        choices = country_choices,
        selected = "__ALL__",
        server = TRUE
      )
      
      
      res <- planned$resources
      res <- planned$resources %>%
        mutate(
          resource_id   = trimws(as.character(resource_id)),
          resource_name = trimws(as.character(resource_name))
        ) %>%
        distinct(resource_id, .keep_all = TRUE)
      
      # label desambiguado si hay nombres repetidos
      res <- res %>%
        group_by(resource_name) %>%
        mutate(nm_n = n()) %>%
        ungroup() %>%
        mutate(resource_label = if_else(
          nm_n > 1,
          paste0(resource_name, " [", resource_id, "]"),
          resource_name
        ))
      
      choices <- c("Todos"="__ALL__", setNames(res$resource_id, res$resource_label))
      
      updateSelectInput(session, "filter_resource",
                        choices = choices,
                        selected = "__ALL__")
      send_gantt(planned)

      originals_ready <- sum(!is.na(planned$tasks$original_start_date) & planned$tasks$original_start_date != "")
      log_rv(paste0("OK. tasks=", nrow(planned$tasks), " | resources=", nrow(planned$resources),
                    " | originales=", originals_ready,
                    "\nAhora: click en una cápsula y mira el panel izquierdo."))
    }, error = function(e) {
      msg <- paste("ERROR:", conditionMessage(e))
      log_rv(msg)
      showNotification(msg, type = "error", duration = 10)
    })
  }, ignoreInit = TRUE)
  
  observeEvent(input$gantt_update, {
    planned <- planned_rv()
    if (is.null(planned)) return()
    
    upd <- input$gantt_update
    moved_id <- as.character(upd$id)
    
    i <- which(planned$tasks$id == moved_id)
    if (length(i) != 1) return()
    
    old_rid <- as.character(planned$tasks$resource_id[i])
    new_rid <- as.character(upd$resource_id)
    
    sd <- suppressWarnings(as.POSIXct(upd$start_date, tz = TZ_LOCAL))
    if (!is.na(sd)) planned$tasks$start_date[i] <- format(sd, "%Y-%m-%d %H:%M")
    
    dur <- suppressWarnings(as.numeric(upd$duration))
    if (is.na(dur) || dur <= 0) dur <- TASK_FALLBACK_HOURS
    planned$tasks$duration[i] <- dur
    planned$tasks$resource_id[i] <- new_rid
    
    anchor <- suppressWarnings(as.POSIXct(planned$tasks$start_date[i], format="%Y-%m-%d %H:%M", tz=TZ_LOCAL))
    planned$tasks <- reschedule_resource_chain(planned$tasks, new_rid, moved_id, anchor)
    
    if (!is.na(old_rid) && old_rid != new_rid) {
      idx_old <- which(planned$tasks$resource_id == old_rid)
      if (length(idx_old) > 0) {
        st_old <- suppressWarnings(as.POSIXct(planned$tasks$start_date[idx_old], format="%Y-%m-%d %H:%M", tz=TZ_LOCAL))
        st_min <- suppressWarnings(min(st_old, na.rm=TRUE))
        if (!is.finite(as.numeric(st_min))) st_min <- as.POSIXct(paste(Sys.Date(), "09:00:00"), tz = TZ_LOCAL)
        planned$tasks <- reschedule_resource_chain(planned$tasks, old_rid, planned$tasks$id[idx_old][1], st_min)
      }
    }
    
    planned$tasks <- planned$tasks %>%
      left_join(planned$resources %>% select(resource_id, resource_name), by = "resource_id") %>%
      mutate(resource_name = coalesce(resource_name.y, resource_name.x)) %>%
      select(-resource_name.x, -resource_name.y)
    
    planned_rv(planned)
    send_gantt(planned)
  }, ignoreInit = TRUE)
  
  
  
  
  
  observeEvent(input$gantt_snapshot, {
    planned <- planned_rv()
    if (is.null(planned)) {
      showNotification("Primero pinta/refresca para tener planned_rv listo.", type="warning", duration=6)
      return()
    }
    
    snap <- input$gantt_snapshot
    if (is.null(snap$tasks) || length(snap$tasks) == 0) return()
    
    # Convertir lista JS -> data.frame
    snap_df <- dplyr::bind_rows(lapply(snap$tasks, function(x) {
      tibble::tibble(
        id         = as.character(x$id),
        start_date = as.character(x$start_date),
        duration   = suppressWarnings(as.numeric(x$duration)),
        resource_id = if (!is.null(x$resource_id)) as.character(x$resource_id) else NA_character_
      )
    }))
    
    # Actualizar planned$tasks con lo que está en el navegador
    planned$tasks <- planned$tasks %>%
      mutate(id = as.character(id)) %>%
      left_join(snap_df %>% select(id, start_date, duration, resource_id),
                by = "id", suffix = c("", ".snap")) %>%
      mutate(
        start_date  = dplyr::coalesce(start_date.snap, start_date),
        duration    = dplyr::coalesce(duration.snap, duration),
        resource_id = dplyr::coalesce(resource_id.snap, resource_id)
      ) %>%
      select(-start_date.snap, -duration.snap, -resource_id.snap)
    
    # Ahora sí: reorganizar sin huecos por trabajador
    planned$tasks <- pack_all_resources_no_gaps(planned$tasks)
    
    planned_rv(planned)
    send_gantt(planned)
    log_rv(paste0("Reorganizado sin huecos. nonce=", snap$nonce))
  }, ignoreInit = TRUE)
  
  
  
  
  
  
  
  observeEvent(input$filter_resource, {
    planned <- planned_rv()
    if (is.null(planned)) return()
    send_gantt(planned)
  }, ignoreInit = TRUE)
  
  
  observeEvent(input$filter_country, {
    planned <- planned_rv()
    if (is.null(planned)) return()
    send_gantt(planned)
  }, ignoreInit = TRUE)
  
  
  
  
  # ==========================
  # TAB: Disponibilidad (libre desde)
  # ==========================
  output$tbl_free <- renderTable({
    planned <- planned_rv()
    if (is.null(planned) || is.null(planned$resources) || nrow(planned$resources) == 0) {
      return(data.frame(Mensaje = "Primero presiona 'Pintar / Refrescar'."))
    }
    
    tasks <- get_tasks_filtered()
    
    # recursos (para mostrar también los que no tienen tareas)
    res <- planned$resources %>%
      mutate(
        resource_id   = trimws(as.character(resource_id)),
        resource_name = trimws(as.character(resource_name))
      ) %>%
      distinct(resource_id, .keep_all = TRUE)
    
    # si no hay tareas con filtros actuales
    if (is.null(tasks) || nrow(tasks) == 0) {
      return(
        res %>%
          transmute(
            resource_name,
            resource_id,
            libre_desde = NA_character_,
            horas_asignadas = 0,
            tareas = 0
          ) %>%
          arrange(resource_name)
      )
    }
    
    sum_by_worker <- tasks %>%
      filter(!is.na(resource_id), resource_id != "") %>%
      group_by(resource_id) %>%
      summarise(
        resource_name = dplyr::first(na.omit(resource_name)),
        libre_desde_dt = suppressWarnings(max(end_dt, na.rm = TRUE)),
        horas_asignadas = sum(dur_h, na.rm = TRUE),
        tareas = dplyr::n(),
        .groups = "drop"
      ) %>%
      mutate(
        libre_desde = dplyr::if_else(
          is.finite(as.numeric(libre_desde_dt)),
          gsub("\\.", "", format(libre_desde_dt, "%b-%d %H:%M")),
          NA_character_
        )
      ) %>%
      select(resource_id, resource_name, libre_desde, horas_asignadas, tareas)
    
    out <- res %>%
      select(resource_id, resource_name) %>%
      left_join(sum_by_worker, by = c("resource_id", "resource_name")) %>%
      mutate(
        libre_desde = dplyr::coalesce(libre_desde, NA_character_),
        horas_asignadas = dplyr::coalesce(horas_asignadas, 0),
        tareas = dplyr::coalesce(tareas, 0)
      ) %>%
      arrange(resource_name)
    
    out
  }, striped = TRUE, bordered = TRUE, spacing = "s")
  
  
  
  
  
  
  # ==========================
  # Descargar Excel (tabla del Gantt con filtros)
  # ==========================
  output$btn_dl_xlsx <- downloadHandler(
    filename = function() {
      paste0("gantt_tasks_", format(Sys.Date(), "%Y%m%d"), ".xlsx")
    },
    content = function(file) {
      planned <- planned_rv()
      if (is.null(planned) || is.null(planned$tasks) || nrow(planned$tasks) == 0) {
        writexl::write_xlsx(data.frame(Mensaje="No hay tareas para exportar."), path = file)
        return()
      }
      
      # MISMA TABLA que estás pintando en el Gantt (con filtros)
      tasks <- planned$tasks %>%
        mutate(
          resource_id = trimws(as.character(resource_id)),
          pais = trimws(toupper(as.character(pais)))
        )
      
      sel <- input$filter_resource
      if (!is.null(sel) && sel != "__ALL__") {
        sel <- trimws(as.character(sel))
        tasks <- tasks %>% dplyr::filter(resource_id == sel)
      }
      
      csel <- input$filter_country
      if (!is.null(csel) && length(csel) > 0 && !("__ALL__" %in% csel)) {
        csel <- trimws(toupper(as.character(csel)))
        tasks <- tasks %>% dplyr::filter(pais %in% csel)
      }
      
      # (opcional) ordena para que quede “bonito”
      if ("start_date" %in% names(tasks)) {
        tasks <- tasks %>% arrange(resource_name, start_date)
      }
      
      writexl::write_xlsx(list(tasks = tasks), path = file)
    }
  )
  
  
  
  
}
