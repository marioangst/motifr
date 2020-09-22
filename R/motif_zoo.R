#' Lists all supported signatures
#'
#' Returns a data frame with two columns: signature and a logical value
#' indicating whether the motifs are directed
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
        directed = c(item[[2]])
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
#' @return Launches a shiny app where all available motifs can be displayed
#' @export
#'
explore_motifs <- function() {
  appDir <- system.file("shiny_examples", "explore_zoo", package = "motifr")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `motifr`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")
}
