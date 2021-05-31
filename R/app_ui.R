#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny bs4Dash
#' @noRd
app_ui <- function(request) {
  shiny::tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    bs4Dash::dashboardPage(
      title = "Ant Colony Optimization",
      fullscreen = FALSE,
      help = FALSE,
      header = bs4Dash::dashboardHeader(
        title = bs4Dash::dashboardBrand(
          title = "ACO Algorithm",
          href = "https://adminlte.io/themes/v3",
          image = "https://fbassets.blob.core.windows.net/images/ant-2.png"
        )
      ),
      sidebar = bs4Dash::dashboardSidebar(
        id = "sidebar",
        skin = "light",
        status = "primary",
        brandColor = "primary",
        collapsed = TRUE,
        
        bs4Dash::sidebarMenu(
          id = "welcome",
          bs4Dash::sidebarHeader("Welcome!"),
          bs4Dash::menuItem(
            text = "Welcome",
            tabName = "welcome",
            icon = icon("star")
          )
        ),
  
        br(),
        bs4Dash::sidebarMenu(
          id = "general",
          bs4Dash::sidebarHeader("Theoretical Background"),
          bs4Dash::menuItem(
            text = "Classification and Origin",
            tabName = "classsification_and_origin",
            icon = icon("edit")
          ),
          bs4Dash::menuItem(
            text = "Ant Foraging",
            tabName = "ant_foraging",
            icon = icon("object-ungroup")
          ),
          bs4Dash::menuItem(
            text = "Algorithm",
            tabName = "algorithm",
            icon = icon("file-code")
          )
        ),
        br(),
        bs4Dash::sidebarMenu(
          id = "visualisations",
          bs4Dash::sidebarHeader("Visualisations"),
          bs4Dash::menuItem(
            text = "Ant Generations Plot",
            tabName = "ant_generations_plot",
            icon = icon("compass")
          ),
          bs4Dash::menuItem(
            text = "Rosenbrock Plot",
            tabName = "rosenbrock_plot",
            icon = icon("gem")
          ),
          bs4Dash::menuItem(
            text = "Himmelblau Plot",
            tabName = "himmelblau_plot",
            icon = icon("paper-plane")
          )
        ),
        br(),
        bs4Dash::sidebarMenu(
          id = "application",
          bs4Dash::sidebarHeader("Applying the algorithm"),
          bs4Dash::menuItem(
            text = "Traveling Salesman Problem",
            tabName = "tsp_plot",
            icon = icon("map")
          ),
          bs4Dash::menuItem(
            text = "Performance",
            tabName = "performance",
            icon = icon("chart-bar")
          )
        )
      ),
      controlbar = bs4Dash::dashboardControlbar(
        id = "controlbar",
        skin = "light",
        pinned = NULL,
        collapsed = TRUE,
        overlay = FALSE,
        shiny::uiOutput("controlbar")
      ),
      body = bs4Dash::dashboardBody(
        tabItems(
          tabItem(
            tabName = "classsification_and_origin",
            mod_introduction_tab_ui("introduction_tab1")
          ),
          tabItem(
            tabName = "ant_foraging",
            mod_ant_foraging_tab_ui("mod_ant_foraging_tab_ui_1")
          ),
          tabItem(
            tabName = "algorithm",
            mod_algorithm_tab_ui("mod_algorithm_tab_ui_1")
          ),
          tabItem(
            tabName = "rosenbrock_plot",
            mod_rosenbrock_tab_ui("mod_rosenbrock_tab_ui_1")
          ),
          tabItem(
            tabName = "himmelblau_plot",
            mod_himmelblau_tab_ui("himmelblau_tab_ui_1")
          ),
          tabItem(
            tabName = "ant_generations_plot",
            mod_ant_generations_tab_ui("ant_generations_tab_ui_1")
          ),
          tabItem(
            tabName = "tsp_plot",
            mod_tsp_tab_ui("tsp_tab_ui_1")
          ),
          tabItem(
            tabName = "performance",
            mod_performance_tab_ui("performance_tab_ui_1")
          )
        )
      ),
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www", app_sys("app/www")
  )
  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "Ant Colony Optimization"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
