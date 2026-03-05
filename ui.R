# ui.R
library(shiny)
library(htmltools)

ui <- fluidPage(
  tags$head(
    # DHTMLX
    tags$link(rel = "stylesheet",
              href = "https://cdn.jsdelivr.net/npm/dhtmlx-gantt@9.1.1/codebase/dhtmlxgantt.css"),
    tags$script(src = "https://cdn.jsdelivr.net/npm/dhtmlx-gantt@9.1.1/codebase/dhtmlxgantt.js"),
    
    # ✅ CSS local (va en /www/tigo.css)
    tags$link(rel = "stylesheet", type = "text/css", href = "tigo.css"),
    
    # ✅ TU JS (igual que lo tenías)
    tags$script(HTML("
      function normText(s){
        if(!s) return '';
        s = String(s).toLowerCase();
        if (s.normalize) s = s.normalize('NFD').replace(/[\\u0300-\\u036f]/g,'');
        return s;
      }
      function statusKey(s){
        return normText(s).replace(/\\s+/g, '_');
      }
      function statusClass(s){
        s = statusKey(s);
        if (s === 'nueva') return 'st_nueva';
        if (s === 'en_proceso') return 'st_en_proceso';
        if (s === 'en_revision') return 'st_en_revision';
        if (s === 'en_diseno') return 'st_en_diseno';
        if (s === 'suspendida') return 'st_suspendida';
        if (s === 'finalizada') return 'st_finalizada';
        return 'st_default';
      }
      function statusColor(s){
        s = statusKey(s);
        if (s === 'nueva') return '#239CC6';
        if (s === 'en_proceso') return '#FBB346';
        if (s === 'en_revision') return '#A378BF';
        if (s === 'en_diseno') return '#E84362';
        if (s === 'suspendida') return '#6B6B6B';
        if (s === 'finalizada') return '#1FCB7F';
        return '#64748b';
      }
      function countryClass(iso2){
        if(!iso2) return 'cty_default';
        iso2 = String(iso2).toLowerCase().trim();
        if (iso2.length >= 2) iso2 = iso2.slice(0,2);
        return 'cty_' + iso2;
      }

      window.__gantt_inited = false;

      function sendSelectedId(id){
        if (!window.Shiny) return;
        Shiny.setInputValue('task_selected_id', { id: String(id), nonce: Date.now() }, {priority:'event'});
      }

      function configureGanttOnce(){
        gantt.config.date_format = \"%Y-%m-%d %H:%i\";
        gantt.config.duration_unit = \"hour\";

        gantt.config.drag_move = true;
        gantt.config.drag_resize = true;
        gantt.config.drag_progress = false;

        gantt.config.scale_unit = \"day\";
        gantt.config.date_scale = \"%d %b\";
        gantt.config.subscales = [{ unit: \"hour\", step: 3, date: \"%H\" }];
        gantt.config.min_column_width = 105; // ~1.5x ancho diario

        gantt.config.grid_width = 520;
        gantt.config.columns = [
          {name:'text', label:'Tarea', tree:true, width:270},
          {name:'resource_name', label:'Colaborador', width:250}
        ];

        gantt.templates.task_class = function(start, end, task){
          return statusClass(task.status) + ' ' + countryClass(task.pais);
        };
        gantt.templates.task_style = function(start, end, task){
          var c = statusColor(task.status);
          var txt = (statusKey(task.status) === 'en_proceso') ? 'color:#000;' : '';
          return 'background:' + c + ';border-color:' + c + ';--task-status-color:' + c + ';' + txt;
        };

        gantt.attachEvent('onTaskClick', function(id, e){
          sendSelectedId(id);
          return true;
        });
        gantt.attachEvent('onTaskRowClick', function(id){
          sendSelectedId(id);
          return true;
        });

        gantt.attachEvent('onBeforeTaskChanged', function(id, mode, task){
          task.$old_resource_id = task.resource_id;
          return true;
        });

        gantt.attachEvent('onAfterTaskDrag', function(id, mode, e) {
          var item = gantt.getTask(id);
          if (mode !== 'move' && mode !== 'resize') return true;

          if (window.Shiny) {
            Shiny.setInputValue('gantt_update', {
              id: String(id),
              mode: mode,
              start_date: gantt.templates.xml_format(item.start_date),
              duration: item.duration,
              resource_id: item.resource_id,
              old_resource_id: item.$old_resource_id || item.resource_id,
              nonce: Date.now()
            }, {priority: 'event'});
          }
          return true;
        });
      }

      function initOrUpdateGantt(tasks){
        tasks = (tasks || []).map(function(t){
          var c = statusColor(t.status);
          t.color = c;
          t.progressColor = c;
          if (statusKey(t.status) === 'en_proceso') t.textColor = '#000';
          return t;
        });

        if (!window.__gantt_inited) {
          configureGanttOnce();
          gantt.init('gantt_here');
          window.__gantt_inited = true;
        }
        gantt.clearAll();
        gantt.parse({ data: tasks, links: [] });
        gantt.render();
      }

      Shiny.addCustomMessageHandler('gantt_data', function(payload){
        initOrUpdateGantt(payload.tasks || []);
      });
      
      // ✅ Botón: Reorganizar (snapshot -> Shiny)
      document.addEventListener('click', function(ev){
        if (ev.target && ev.target.id === 'btn_pack') {
          if (!window.Shiny || !window.__gantt_inited) return;
          var snap = gantt.serialize(); // {data:[...], links:[...]}
          Shiny.setInputValue('gantt_snapshot', {
            nonce: Date.now(),
            tasks: snap.data
          }, {priority: 'event'});
        }
      });
    "))
  ),
  
  # ✅ Topbar (Tigo blue)
  div(class="tigo-topbar",
      div(class="tigo-topbar__inner",
          div(class="tigo-brand",
              div(class="tigo-brand__dot"),
              div(class="tigo-brand__title", "Planner Gantt"),
              div(class="tigo-brand__subtitle", "Portal UI · DHTMLX")
          ),
          div(class="tigo-topbar__right",
              span(class="tigo-badge", "INTERNAL")
          )
      )
  ),
  
  div(class="tigo-shell",
      fluidRow(
        column(
          2,
          div(class="portal",
              div(class="portal__header",
                  div(class="portal__title", "Controles"),
                  div(class="portal__hint", "Actualiza, pinta y revisa detalle")
              ),
              div(class="portal__body",
                  selectInput("filter_resource", "Colaborador",
                              choices = c("Todos"="__ALL__"), selected="__ALL__", width="100%"),
                  
                  selectizeInput(
                    "filter_country", "País",
                    choices = c("Todos" = "__ALL__"),
                    selected = "__ALL__",
                    multiple = TRUE,
                    options = list(placeholder = "Selecciona uno o varios países…"),
                    width = "100%"
                  ),

                  selectInput("filter_objective", "Objetivo",
                              choices = c("Todas" = "__ALL__"), selected = "__ALL__", width = "100%"),

                  selectInput("filter_business_unit", "Unidad de negocio",
                              choices = c("Todas" = "__ALL__"), selected = "__ALL__", width = "100%"),
                  
                  
                  div(class="btn-stack",
                      actionButton("btn_run",  "Actualizar a_plan (run.R)", class="btn-tigo btn-tigo--primary"),
                      actionButton("btn_draw", "Pintar / Refrescar",        class="btn-tigo btn-tigo--ghost"),
                      actionButton("btn_pack", "Reorganizar (sin huecos)",  class="btn-tigo btn-tigo--ghost"),
                      downloadButton("btn_dl_xlsx", "Descargar Excel (Gantt)", class="btn-tigo btn-tigo--ghost")
                  ),
                  div(class="portal__divider"),
                  uiOutput("task_details"),
                  div(class="portal__divider"),
                  div(class="microcopy", "Click en cápsula o fila → detalle aquí.")
              )
          )
        ),
        column(
          10,
          div(class="portal",
              div(class="portal__header",
                  div(class="portal__title", "Timeline"),
                  div(class="portal__hint", "Arrastra tareas para reencadenar visualmente")
              ),
              div(class="portal__body",
                  tabsetPanel(
                    id = "tabs_main",
                    type = "tabs",
                    selected = "Gantt",
                    tabPanel("Gantt",
                             div(id="gantt_here", class="gantt-portal")
                    ),
                    tabPanel("Disponibilidad",
                             div(class="microcopy",
                                 "Queda libre desde el fin de su última tarea (según filtros actuales)."),
                             div(class = "availability-grid",
                                 div(class = "availability-card",
                                     div(class = "availability-card__title", "Carga pendiente por recurso"),
                                     plotly::plotlyOutput("hours_pie", height = "500px")
                                 ),
                                 div(class = "availability-card",
                                     div(class = "availability-card__title", "Detalle de disponibilidad"),
                                     DT::DTOutput("tbl_free")
                                 )
                             )
                    ),
                    tabPanel("Comentarios",
                             div(class = "comments-wrap",
                                 div(class = "comments-form",
                                     div(class = "comments-form__title", "Registrar comentario"),
                                     div(class = "comments-form__row",
                                         dateInput("comment_date", "Fecha", value = Sys.Date(), width = "100%")
                                     ),
                                     div(class = "comments-form__row",
                                         textInput("comment_country", "País", placeholder = "Ej: CO, CR, PA", width = "100%")
                                     ),
                                     div(class = "comments-form__row",
                                         textAreaInput("comment_text", "Comentario", rows = 6,
                                                       placeholder = "Escribe aquí el comentario...", width = "100%")
                                     ),
                                     actionButton("btn_save_comment", "Guardar comentario", class = "btn-tigo btn-tigo--primary")
                                 ),
                                 div(class = "comments-list",
                                     div(class = "comments-form__title", "Historial guardado en VM"),
                                     div(class = "microcopy", "Tip: haz click en una fila para ver el comentario completo."),
                                     DT::DTOutput("tbl_comments"),
                                     div(class = "portal__divider"),
                                     uiOutput("comment_details")
                                 )
                             )
                    )
                  ),
                  div(class="portal__divider"),
                  div(class="logbox", verbatimTextOutput("log"))
              )
          )
        )
      )
  )
)
