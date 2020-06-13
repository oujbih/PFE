


tablerCard <- function(..., title = NULL, options = NULL, footer = NULL,
                       status = NULL, statusSide = c("top", "left"),
                       collapsible = TRUE, collapsed = F, closable = TRUE,
                       zoomable = TRUE, width = 6, overflow = FALSE) {

  statusSide <- match.arg(statusSide)

  statusCl <- if (!is.null(status)) paste0("card-status bg-", status)
  if (!is.null(status)) {
    if (statusSide == "left") {
      statusCl <- paste0(statusCl , " card-status-left")
    }
  }
  
  cardCl <- "card "
  if (collapsed) cardCl <- paste0(cardCl, " card-collapsed")

  cardTag <- shiny::tags$div(
    class = cardCl,
    style =  if (overflow) "max-height: 1000px; overflow-y: auto;" else NULL,
    if (!is.null(status)) shiny::tags$div(class = statusCl),
    # header
    if (!is.null(title)) {
      shiny::tags$div(
        class = "card-header",
        shiny::tags$h3(class = "card-title", title),

        # card toolbox and other elements such as buttons, ...
        shiny::tags$div(
          class = "card-options",
          if (!is.null(options)) {
            lapply(options, shiny::tagAppendAttributes, class = "mx-1")
          },
          if (collapsible) {
            shiny::tags$a(
              href = "#",
              class = "card-options-collapse",
              `data-toggle` = "card-collapse",
              icon('chevron-up')
            )
          },
          if (zoomable) {
            if (closable) {
              shiny::tags$a(
                href = "#",
                  class = "card-options-fullscreen",
                `data-toggle` = "card-fullscreen",
                icon("expand")
              )
            }
          },
          if (closable) {
            shiny::tags$a(
              href = "#",
              class = "card-options-remove",
              `data-toggle` = "card-remove",
              icon("times")
            )
          }
        )
      )
    },
    # body
    shiny::tags$div(class = "card-body ", ...),
    # footer
    if (!is.null(footer)) shiny::tags$div(class = "card-footer", footer)
  )

  if (!is.null(width)) shiny::column(width = width, cardTag) else cardTag

}



tablerStatCard <- function(value, title, trend = NULL, width = 3) {

  color <- ifelse(trend > 0, "green", "red")

  shiny::column(
    width = width,
    shiny::tags$div(
      class = "card",
      shiny::tags$div(
        class = "card-body p-3 text-center",
        if (!is.null(trend)) {
          shiny::tags$div(
            class = paste0("text-right text-", color),
            paste0(trend, "%"),
            if (trend > 0) {
              tablerIcon(name = "chevron-up", lib = "feather")
            } else {
              tablerIcon(name = "chevron-down", lib = "feather")
            }
          )
        },
        shiny::tags$div(class = "h1 m-0", value),
        shiny::tags$div(class = "text-muted mb-4", title)
      )
    )
  )
}




tablerInfoCard <- function(value, description = NULL, status,
                           icon, href = NULL, width = 4) {

  shiny::column(
    width = width,
    shiny::tags$div(
      class = "card p-3",
      # stamp
      shiny::tags$div(
        class = "d-flex align-items-center",
        shiny::tags$span(
          class = paste0("stamp stamp-md mr-3 bg-", status),
          tablerIcon(name = icon, lib = "feather")
        ),
        # body
        shiny::tags$div(
          shiny::tags$h4(
            class = "m-0",
            shiny::tags$a(href = if (!is.null(href)) href else "#", value)
          ),
          htmltools::tags$small(class = "text-muted", description)
        )
      )
    )
  )
}



tablerMediaCard <- function(..., title = NULL, date = NULL, href = NULL,
                            src = NULL, avatarUrl = NULL, width = 4) {

  shiny::column(
    width = width,
    shiny::tags$div(
      class = "card p-3",
      # image
      shiny::tags$a(
        href = if (!is.null(href)) href else "javascript:void(0)",
        class = "mb-3",
        shiny::img(src = src, class = "rounded")
      ),
      # content
      shiny::tags$div(
        class = "d-flex align-items-center px-2",
        shiny::tagAppendAttributes(
          tablerAvatar(
            url = avatarUrl,
            size = "md"
          ),
          class = "mr-3"
        ),
        shiny::tags$div(
          shiny::tags$div(title),
          htmltools::tags$small(class = "d-block text-muted", date)
        ),
        # other elements
        shiny::tags$div(class = "ml-auto text-muted", ...)
      )
    )
  )

}


tablerBlogCard <- function(..., title = NULL, author = NULL, date = NULL, href = NULL,
                           src = NULL, avatarUrl = NULL, width = 4, horizontal = FALSE) {

  cardCl <- "card"
  if (horizontal) cardCl <- paste0(cardCl, " card-aside")

  shiny::column(
    width = width,
    shiny::tags$div(
      class = cardCl,
      if (!is.null(src)) {
        if (!horizontal) {
          shiny::tags$a(
            href = href,
            target = "_blank",
            shiny::tags$img(
              class = "card-img-top",
              src = src
            )
          )
        } else {
          shiny::tags$a(
            href = href,
            target = "_blank",
            class = "card-aside-column",
            style = paste0("background-image: url(\"", src, "\")")
          )
        }
      },
      # body
      shiny::tags$div(
        class = "card-body d-flex flex-column",
        shiny::h4(shiny::a(href = "#", title)),
        shiny::tags$div(class = "text-muted", ...),
        shiny::tags$div(
          class = "d-flex align-items-center pt-5 mt-auto",
          shiny::tags$div(
            class = "d-flex align-items-center px-2",
            if (!is.null(avatarUrl)) {
              shiny::tagAppendAttributes(
                tablerAvatar(
                  url = avatarUrl,
                  size = "md"
                ),
                class = "mr-3"
              )
            },
            shiny::tags$div(
              shiny::tags$div(author),
              htmltools::tags$small(class = "d-block text-muted", date)
            )
          )
        )
      )
    )
  )
}


tablerProfileCard <- function(title = NULL, subtitle = NULL, background = NULL,
                              src = NULL, socials = NULL, width = 4) {

  shiny::column(
    width = width,
    shiny::tags$div(
      class = "card card-profile",
      # header
      shiny::tags$div(
        class = "card-header",
        style = paste0("background-image: url(\"", background, "\")")
      ),
      # body
      shiny::tags$div(
        class = "card-body text-center",
        shiny::img(src = src, class = "card-profile-img"),
        shiny::h3(title),
        shiny::p(class = "mb-4", subtitle),
        socials
      )
    )
  )
}


icon("expand")
icon('chevron-up')
icon('chevron-down')
icon("times")
header <- dashboardHeader(
  
  title = span(tagList(icon("poll"), "OCP"))
)
sidebar <- dashboardSidebar(
  # sidebarMenuOutput("Side_dash")
  sidebarMenu(
    menuItem(tabName = "Home", text = "Home", icon = icon("home"),
             menuSubItem("Modele", tabName = "Home", href = NULL, newtab = TRUE,
                         icon = shiny::icon("angle-double-right"), selected = F),
             menuSubItem("Parametres de marche", tabName = "pmarche",selected = F)
           
             ),
    menuItem(tabName = "Development", text = "Development", icon = icon("poll"),
             menuSubItem("Regression", tabName = "D_Regression", href = NULL, newtab = TRUE,
                         icon = shiny::icon("angle-double-right"), selected = F),
             menuSubItem("Classification", tabName = "D_Classification",selected = F)
             
    ),
    menuItem(tabName = "Models", text = "Models", icon = icon("expand"),
             menuSubItem("Regression", tabName = "M_Regression", href = NULL, newtab = TRUE,
                         icon = shiny::icon("angle-double-right"), selected = T),
             menuSubItem("Classification", tabName = "M_Classification",selected = F)
             
    )
  )
)
