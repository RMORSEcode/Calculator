#' Gets information about the latest release of a repo
#'
#' Connects to the GitHub API and pulls all release info and selects the most recent
#'
#' @return list
#' \item{tag}{the tag label assigned to the release}
#' \item{name}{the name assigned to the release}
#' \item{creationDate}{the date the release was created}
#' \item{Description}{the text found in the Description field of the release}
#'


get_release <- function() {
  # Define repo information, pull release names from GH API
  repo <- 'https://api.github.com/repos/RMORSECode/Calculator/releases'
  repo <- 'https://github.com/RMORSEcode/Calculator/releases/'
  # repo <- 'https://api.github.com/repos/NOAA-EDAB/stocksmart/releases'
  # pull all releases and picks out a few of the components
  releaseNames <- jsonlite::fromJSON(repo)$name
  releaseTags <- jsonlite::fromJSON(repo)$tag_name
  releaseDescriptions <- jsonlite::fromJSON(repo)$body
  releaseCreationDate <- jsonlite::fromJSON(repo)$created_at

  # the most recent release should be the first object in all variables
  latest <- list()
  latest$tag <- releaseTags[1]
  latest$name <- releaseNames[1]
  latest$creationDate <- releaseDescriptions[1]
  latest$description <- releaseCreationDate[1]

  return(latest)

}
