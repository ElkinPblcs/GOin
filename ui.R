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
      function statusClass(s){
        s = normText(s).replace(/\s+/g, '_');
        if (s === 'nueva') return 'st_nueva';
        if (s === 'en_proceso') return 'st_en_proceso';
        if (s === 'en_revision') return 'st_en_revision';
        if (s === 'en_diseno') return 'st_en_diseno';
        if (s === 'suspendida') return 'st_suspendida';
        if (s === 'finalizada') return 'st_finalizada';
        return 'st_default';
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

        gantt.config.grid_width = 520;
        gantt.config.columns = [
          {name:'text', label:'Tarea', tree:true, width:270},
          {name:'resource_name', label:'Colaborador', width:250}
        ];

        gantt.templates.task_class = function(start, end, task){
          return statusClass(task.status) + ' ' + countryClass(task.pais);
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
                    tabPanel("Gantt",
                             div(id="gantt_here", class="gantt-portal")
                    ),
                    tabPanel("Disponibilidad",
                             div(class="microcopy",
                                 "Queda libre desde el fin de su última tarea (según filtros actuales)."),
                             tableOutput("tbl_free")
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
