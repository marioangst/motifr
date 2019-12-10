#' Actor-concept edgelist
#'
#' A dataset, specifying actor-activity ties. Thus,
#' every row specifies that an actor (sender) partakes in a specific activity (receiver)
#'
#' @format A data frame with 140 rows and 2 variables
#' \describe{
#'   \item{sender}{Actors}
#'   \item{receiver}{Activities}
#'   ...
#' }
#' @source Reussebene survey
"actor_concept_el"

#' Actor dataframe
#'
#' A dataset specifying the list of unique actors
#'
#' @format A data frame with 80 rows and 1 variable:
#' \describe{
#'   \item{actor}{Actor names}
#'   ...
#' }
#' @source Reussebene survey
"actors"

#' Actor edgelist
#'
#' A dataset specifying collaboration links between actors. Every record indicates a tie between actors.
#'
#' @format A data frame with n rows and n variables:
#' \describe{
#'   \item{sender}{Actors who indicated collaboration}
#'   \item{receiver}{Target actor of collaboration indicated by sender}
#'   ...
#' }
#' @source survey
"actor_el"

#' CLD concepts dataframe
#'
#' A dataset providing information on all concepts (nodes) in a causal loop diagram elaborated for the Reussebene
#'
#' @format A data frame with 52 rows and 3 variables:
#' \describe{
#'   \item{concept}{Short name of the concept}
#'   \item{type}{Type of concept based on Open Standards methodology}
#'   \item{english}{Full english name of concept, used for labeling}
#'   ...
#' }
#' @source survey
"cld_concepts"

#' CLD edgelist dataframe
#'
#' A dataset of links between causal loop diagraming concepts. Every record indicates a link
#'
#' @format A data frame with n rows and n variables:
#' \describe{
#'   \item{sender}{Sender concept}
#'   \item{receiver}{Target concept}
#'   \item{sign}{Type of relation (negative/ positive)}
#'   ...
#' }
#' @source survey
"cld_el"
