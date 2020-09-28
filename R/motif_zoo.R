#' Lists all supported signatures
#'
#' Returns a data frame with three columns: signature, a Boolean value
#' indicating whether the motifs are directed, the number of levels which the
#' motif spans across
#'
#' @return data frame with all supported signatures
#' @examples
#' supported_signatures()
#' @seealso \code{supported_classes()}
#' @export
#'
supported_signatures <- function() {
  result <- data.frame()
  iter <- sma$supportedSignatures()
  while (TRUE) {
    item <- reticulate::iter_next(iter)
    if (is.null(item)) {
      break
    }
    result <- rbind(
      result,
      data.frame(
        signature = c(paste(item[[1]], collapse = ",")),
        directed = c(item[[2]]),
        n_levels = c(length(item[[1]]))
      )
    )
  }
  return(result)
}

#' Lists all supported motif classes for a given signature
#'
#' Returns a list with all supported motif classes for the given signature.
#' Raises an error if the given signature is not supported.
#'
#' @return list of supported motif classes
#' @param signature head of a motif identifier string, i.e. string with
#'   comma-separated list specifying the signature of the motif
#' @param directed whether the motifs are directed.
#' @export
#' @seealso \code{supported_signatures()}
#' @examples
#' supported_classes("1,2", FALSE)
#' supported_classes("1,1", TRUE)
supported_classes <- function(signature, directed) {
  ls <- strsplit(signature, ",")
  lsi <- as.list(lapply(ls, as.integer)[[1]])
  motif_info <- sma$motifInfo(lsi, directed)
  return(motif_info$classes)
}

#' Explore the motif zoo interactively in a shiny app
#'
#' Without any arguments, this launches a shiny app, where all available motifs
#' in motifr can be graphically displayed by selecting signature-class combinations
#' from a dropdown list.
#'
#' If arguments net and lvl_attr are provided, you can load you own network into
#' the shiny app to explore what a given motif classifier looks like for your
#' network. Be aware that if your network does not contain a specific motif, an
#' example of the motif can also not be shown, because motifr illustrates motifs
#' by actually finding an example within a given network.
#'
#' @param net optional: you may supply your own network object here
#' (must be loaded as an R object in the global environment)
#' @param lvl_attr if you supply your own network object, indicate the name of
#' the network attribute where level information is stored for each node
#'
#' @return Launches a shiny app where all available motifs can be displayed or, alternatively,
#' all available motifs for a user-supplied network
#' @export
explore_motifs <- function(net = NULL,
                           lvl_attr = c("sesType")) {
  file_path <- system.file("shiny_examples", "explore_zoo", "app.R", package = "motifr")
  if (!nzchar(file_path)) {
    stop("Could not find example directory. Try re-installing `motifr`.", call. = FALSE)
  }

  ui <- server <- NULL # avoid NOTE about undefined globals
  source(file_path, local = TRUE)
  server_env <- environment(server)

  # add user supplied net variables for shiny server to find
  server_env$net <- net
  server_env$lvl_attr <- lvl_attr

  app <- shiny::shinyApp(ui, server)
  shiny::runApp(app, display.mode = "normal")
}
