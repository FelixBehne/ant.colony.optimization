# No Remotes ----
# Attachments ----
to_install <- c(
  "bs4Dash",
  "caret",
  "config",
  "dplyr",
  "evoper",
  "foreach",
  "fresh",
  "golem",
  "hash",
  "htmltools",
  "metaheuristicOpt",
  "plotly",
  "shiny",
  "shinyalert",
  "shinycssloaders",
  "shinyWidgets",
  "slickR",
  "thematic",
  "waiter"
)
for (i in to_install) {
  message(paste("looking for ", i))
  if (!requireNamespace(i)) {
    message(paste("     installing", i))
    install.packages(i)
  }
}
