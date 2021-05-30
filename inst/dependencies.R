# No Remotes ----
# Attachments ----
to_install <- c("attempt", "bs4Dash", "config", "DT", "glue", "golem", "htmltools", "shiny")
for (i in to_install) {
  message(paste("looking for ", i))
  if (!requireNamespace(i)) {
    message(paste("     installing", i))
    install.packages(i)
  }
}
