#' Create a timelinevis widget
#'
#' Send a dataframe containing timeline entries to the javascript widget
#'
#' @param items a dataframe that will be converted into a timelinejs DataSet.
#'   Each row will be one timeline entry.
#'   See vis.js documentation for valid column names
#' @param groups groups
#' @param editable editable
#' @param zoomMin minimum timeframe allowed on zooming x axis. Defaults to 1 day.
#'
#' @examples
#' items=data.frame(content=c("item1","item2"), start=c("2013-04-02","2013-04-14"))
#' timelinevis(items)
#'
#' @import htmlwidgets
#'
#' @export
timelinevis <- function(items, groups=NULL, id=NULL,
                        editable=F,
                        zoomMin = 24*60*60*1000, # milliseconds
                        snap=NULL,
                        width = NULL, height = NULL) {

  # forward options using x
  x = list(id=id, items=items, groups=groups, options=list(editable=editable, zoomMin=zoomMin, snap=snap))
  attr(x, 'TOJSON_ARGS') <- list(dataframe = "rows")

  # create widget
  htmlwidgets::createWidget(
    name = 'timelinevis',
    x,
    width = width,
    height = height,
    package = 'timelinevis'
  )
}

#' Shiny bindings for timelinevis
#'
#' Output and render functions for using timelinevis within Shiny
#' applications and interactive Rmd documents.
#'
#' @param outputId output variable to read from
#' @param width,height Must be a valid CSS unit (like \code{'100\%'},
#'   \code{'400px'}, \code{'auto'}) or a number, which will be coerced to a
#'   string and have \code{'px'} appended.
#' @param expr An expression that generates a timelinevis
#' @param env The environment in which to evaluate \code{expr}.
#' @param quoted Is \code{expr} a quoted expression (with \code{quote()})? This
#'   is useful if you want to save an expression in a variable.
#'
#' @name timelinevis-shiny
#'
#' @export
timelinevisOutput <- function(outputId, width = '100%', height = 'auto'){
  htmlwidgets::shinyWidgetOutput(outputId, 'timelinevis', width, height, package = 'timelinevis')
}

#' @rdname timelinevis-shiny
#' @export
renderTimelinevis <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) { expr <- substitute(expr) } # force quoted
  htmlwidgets::shinyRenderWidget(expr, timelinevisOutput, env, quoted = TRUE)
}
