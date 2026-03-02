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
      function normSkill(s){
        if(!s) return '';
        s = String(s).toLowerCase();
        if (s.normalize) s = s.normalize('NFD').replace(/[\\u0300-\\u036f]/g,'');
        return s;
      }
      function skillClass(s){
        s = normSkill(s);
        if (s.includes('adapt')) return 'sk_adaptaciones';
        if (s.includes('pro')) return 'sk_pro';
        if (s.includes('plus')) return 'sk_plus';
        if (s.includes('estandar')) return 'sk_estandar';
        return 'sk_default';
      }
      function countryClass(iso2){
        if(!iso2) return 'cty_default';
        iso2 = String(iso2).toLowerCase().trim();
        if (iso2.length >= 2) iso2 = iso2.slice(0,2);
        return 'cty_' + iso2;
      }


      window.__gantt_single = window.gantt;
      window.__gantt_single_inited = false;
      window.__tasks_current = [];
      window.__tasks_original = [];

      function sendSelectedId(id){
        if (!window.Shiny) return;
        Shiny.setInputValue('task_selected_id', { id: String(id), nonce: Date.now() }, {priority:'event'});
      }

      function configureGantt(){
        var g = window.__gantt_single;
        g.config.date_format = '%Y-%m-%d %H:%i';
        g.config.duration_unit = 'hour';
        g.config.drag_progress = false;

        g.config.scale_unit = 'day';
        g.config.date_scale = '%d %b';
        g.config.subscales = [{ unit: 'hour', step: 3, date: '%H' }];

        g.config.grid_width = 520;
        g.config.columns = [
          {name:'text', label:'Tarea', tree:true, width:270},
          {name:'resource_name', label:'Colaborador', width:250}
        ];

        g.templates.task_class = function(start, end, task){
          return skillClass(task.skill_main) + ' ' + countryClass(task.pais);
        };

        g.attachEvent('onTaskClick', function(id){
          sendSelectedId(id);
          return true;
        });
        g.attachEvent('onTaskRowClick', function(id){
          sendSelectedId(id);
          return true;
        });

        g.attachEvent('onBeforeTaskChanged', function(id, mode, task){
          if (g.config.readonly) return true;
          task.$old_resource_id = task.resource_id;
          return true;
        });

        g.attachEvent('onAfterTaskDrag', function(id, mode) {
          if (g.config.readonly) return true;
          var item = g.getTask(id);
          if (mode !== 'move' && mode !== 'resize') return true;
          if (window.Shiny) {
            Shiny.setInputValue('gantt_update', {
              id: String(id),
              mode: mode,
              start_date: g.templates.xml_format(item.start_date),
              duration: item.duration,
              resource_id: item.resource_id,
              old_resource_id: item.$old_resource_id || item.resource_id,
              nonce: Date.now()
            }, {priority: 'event'});
          }
          return true;
        });
      }

      function initGanttIfNeeded(containerId){
        var g = window.__gantt_single;
        if (!window.__gantt_single_inited) {
          configureGantt();
          g.init(containerId);
          window.__gantt_single_inited = true;
          return;
        }

        var node = document.getElementById(containerId);
        if (!node) return;
        if (g.$container !== node) {
          g.init(containerId);
        }
      }

      function renderActiveGantt(){
        var activeTab = document.querySelector('#tabs_main li.active a');
        var tabName = activeTab ? (activeTab.getAttribute('data-value') || activeTab.textContent || '') : 'Gantt';
        var isOriginal = tabName.trim() === 'Gantt original';
        var g = window.__gantt_single;

        initGanttIfNeeded(isOriginal ? 'gantt_original_here' : 'gantt_here');

        g.config.readonly = isOriginal;
        g.config.drag_move = !isOriginal;
        g.config.drag_resize = !isOriginal;

        var data = isOriginal ? window.__tasks_original : window.__tasks_current;
        g.clearAll();
        g.parse({ data: data || [], links: [] });
        g.render();
        if (typeof g.setSizes === 'function') g.setSizes();
      }

      Shiny.addCustomMessageHandler('gantt_data', function(payload){
        window.__tasks_current = (payload && payload.tasks) ? payload.tasks : [];
        renderActiveGantt();
      });

      Shiny.addCustomMessageHandler('gantt_original_data', function(payload){
        window.__tasks_original = (payload && payload.tasks) ? payload.tasks : [];
        renderActiveGantt();
      });

      document.addEventListener('shown.bs.tab', function(){
        setTimeout(renderActiveGantt, 0);
      });

      window.addEventListener('resize', function(){
        setTimeout(renderActiveGantt, 0);
      });

      // ✅ Botón: Reorganizar (snapshot -> Shiny)
      document.addEventListener('click', function(ev){
        if (ev.target && ev.target.id === 'btn_pack') {
          var activeTab = document.querySelector('#tabs_main li.active a');
          var tabName = activeTab ? (activeTab.getAttribute('data-value') || activeTab.textContent || '') : 'Gantt';
          if (tabName.trim() === 'Gantt original') {
            if (window.Shiny) {
              Shiny.setInputValue('gantt_snapshot', { nonce: Date.now(), tasks: window.__tasks_current || [] }, {priority: 'event'});
            }
            return;
          }

          var g = window.__gantt_single;
          if (!window.Shiny || !g) return;
          var snap = g.serialize();
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
                    tabPanel("Gantt original",
                             div(class="microcopy", "Vista de fechas originales de las tareas."),
                             div(id="gantt_original_here", class="gantt-portal")
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
