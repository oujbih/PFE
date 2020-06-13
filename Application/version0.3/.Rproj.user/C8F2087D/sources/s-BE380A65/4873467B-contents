# Add an html dependency, without overwriting existing ones
appendDependencies <- function(x, value) {
  if (inherits(value, "html_dependency"))
    value <- list(value)

  old <- attr(x, "html_dependencies", TRUE)

  htmltools::htmlDependencies(x) <- c(old, value)
  x
}

# Add dashboard dependencies to a tag object
addDeps <- function(x) {

  # put all necessary ressources here

  tablerDash_js <- "dashboard.js"
  tablerDash_css <- "dashboard.css"
  init_js <- "init.js"

  require_js <- "require.min.js"
  core_js <- "core.js"

  # fontawesome_css <- "https://maxcdn.bootstrapcdn.com/font-awesome/4.7.0/css/"
    
  dashboardDeps <- list(
    # fontawesome
   
    #tablerDash
    htmltools::htmlDependency(
      name = "tablerDash",
      version = "0.1.0",
      src =  c(file = "www/js"),
      script =c(init_js, tablerDash_js, core_js),
      stylesheet = tablerDash_css
    )
  )
  appendDependencies(x, dashboardDeps)
}

