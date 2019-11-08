#' Rename a vector based on a codebook
#'
#' @param input Specify the vector to be recoded
#' @param codebook The data frame uses as a codebook for recoding
#' @param rawvar Character string giving the name of the column in the codebook containing references to the raw data
#' @param codevar Character string giving the name of the column in the codebook containing references to the coded data
#'
#' @return A named character vector of coded entries
#' @export
#'
#' @examples
#' animal_codebook <- data.frame(
#' animal = c("elephant", "pig", "lizard"),
#' family = c("mammal","mammal","reptile"))
#' vec <- sample(c("elephant", "pig", "lizard"), 50, TRUE)
#' rename_based_on_codebook(input = vec,
#' codebook = animal_codebook,
#' rawvar = "animal",
#' codevar = "family")
rename_based_on_codebook <- Vectorize(function(input,codebook,rawvar,codevar){
  #make sure there is only one coded entry in rawvar for input
  z <- codebook[[as.character(rawvar)]] %in% as.character(input)
  numberofentries <- sum(z, na.rm=TRUE)
  if (numberofentries > 1){
    replacement <- paste("Warning: More than one entry for","",as.character(gsub(input,pattern = ",",replacement = "")),"","in codebook")
  }
  if (numberofentries == 0){
    replacement <- paste("No entry for", as.character(input), "in codebook")
  }
  if (numberofentries == 1){
    replacement <- as.character(codebook[[as.character(codevar)]][codebook[[as.character(rawvar)]] %in% as.character(input)])
  }
  return(replacement)
},vectorize.args = c("input"))
