#' Lists all supported signatures
#'
#' Returns a data frame with two columns: signature and a logical value
#' indicating whether the motifs are directed
#'
#' @return data frame with all supported signatures
#' @examples supported_signatures()
#' @export
#'
supported_signatures <- function() {
  result <- data.frame()
  iter <- sma$supportedSignatures()
  while (TRUE) {
    item <- reticulate::iter_next(iter)
    if (is.null(item))
      break
    result <- rbind(result,
                    data.frame(signature = c(paste(item[[1]], collapse = ",")),
                               directed = c(item[[2]])))
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
#' @examples
#' supported_classes("1,2")
#' supported_classes("1,1", directed = TRUE)
supported_classes <- function(signature, directed = FALSE) {
  ls <- strsplit(signature, ",")
  lsi <- as.list(lapply(ls, as.integer)[[1]])
  motif_info <- sma$motifInfo(lsi, directed)
  return(motif_info$classes)
}

#' Display all supported motifs with signature (1,2).
#'
#' @return Opens image
#' @export
#'
#' @examples
#' show_3_motifs()
show_3_motifs <- function() {
  magick::image_read(path = system.file("motif_reference",
                                        "motif_reference_3motifs.png",
                                        package = utils::packageName()
  ))
}

#' Display all supported motifs with signature (2,2).
#'
#' @return Opens figure from Ö. Bodin, M. Tengö: Disentangling intangible
#'   social–ecological systems in Global Environmental Change 22 (2012) 430–439
#'   <10.1016/j.gloenvcha.2012.01.005>
#' @export
#'
#' @examples
#' show_4_motifs()
show_4_motifs <- function() {
  magick::image_read(path = system.file("motif_reference",
                                        "motif_reference_4motifs.jpg",
                                        package = utils::packageName()
  ))
}
