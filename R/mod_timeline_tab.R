#' introduction_tab UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @import shiny bs4Dash htmltools
#'
#' @noRd
mod_timeline_tab_ui <- function(id) {
  ns <- shiny::NS(id) # nolint
  shiny::tagList(
    shiny::titlePanel("History of the ACO Algorithm"),
    shiny::fluidRow(
      bs4Dash::box(
        id = "introduction",
        title = "Timeline",
        maximizable = TRUE,
        collapsible = TRUE,
        closable = TRUE,
        width = 12,
        height = "100%",
        # htmltools::includeMarkdown(app_sys("app/www/rmd/history.Rmd"))
        bs4Dash::timelineBlock(
          width = 12,
          reversed = TRUE,
          bs4Dash::timelineEnd(color = "danger"),
          bs4Dash::timelineLabel("2012", color = "lightblue"),
          bs4Dash::timelineItem(
            title = "Recursive ant colony optimization",
            elevation = 2,
            icon = icon("info"),
            status = "orange",
            time = "Now",
            "It is a recursive form of ant system which divides the whole search domain into several sub-domains
            and solves the objective on these subdomains. The results from all the subdomains are compared and the
            best few of them are promoted for the next level. The subdomains corresponding to the selected results are
            further subdivided and the process is repeated until an output of desired precision is obtained. This method has been tested on
            ill-posed geophysical inversion problems and works well"
          ),
          bs4Dash::timelineLabel("1999", color = "lightblue"),
          bs4Dash::timelineItem(
            title = "Max Min Ant System (MMAS)",
            elevation = 2,
            icon = icon("info"),
            status = "orange",
            time = "1999",
            "This algorithm controls the maximum and minimum pheromone amounts on each trail.
            Only the global best tour or the iteration best tour are allowed to add pheromone to its trail.
            To avoid stagnation of the search algorithm, the range of possible pheromone amounts on each trail is limited to an interval [τmax,τmin].
            All edges are initialized to τmax to force a higher exploration of solutions.
            The trails are reinitialized to τmax when nearing stagnation."
          ),
          bs4Dash::timelineLabel("1997", color = "lightblue"),
          bs4Dash::timelineItem(
            title = "Elitist Strategy for Ant System",
            elevation = 2,
            icon = icon("info"),
            status = "orange",
            time = "1992",
            "In this algorithm, the global best solution deposits pheromone on its trail after every iteration
            (even if this trail has not been revisited), along with all the other ants."
          ),
          bs4Dash::timelineItem(
            title = "Ant Colony System",
            elevation = 2,
            icon = icon("info"),
            status = "orange",
            time = "1997",
            # nolint start
            "In the ant colony system algorithm, the original ant system was modified in three aspects:
            (i) The edge selection is biased towards exploitation
            (ii) While building a solution, ants change the pheromone level of the edges they are selecting by applying a local pheromone updating rule
            (iii) At the end of each iteration, only the best ant is allowed to update the trails by applying a modified global pheromone updating rule"
            # nolint end
          ),
         
          bs4Dash::timelineLabel("1991", color = "lightblue"),
          bs4Dash::timelineItem(
            title = "Ant Sytem (AS)",
            elevation = 2,
            icon = icon("info"),
            status = "orange",
            time = "1991",
            "The ant system is the first ACO algorithm. This algorithm corresponds to the one presented above. It was developed by Dorigo.",
          ),
          bs4Dash::timelineLabel("1940", color = "lightblue"),
          bs4Dash::timelineItem(
            title = "Richard P. Feynman",
            elevation = 2,
            icon = icon("info"),
            status = "orange",
            time = "1991",
            "Richard P. Feynman wrote about the behavior from ants",
          ),
          bs4Dash::timelineStart(color = "primary")
        )
      )
    )
  )
}
