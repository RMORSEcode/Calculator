get_release <- function() {
  # Define repo information, pull release names from GH API
  repo <- 'https://api.github.com/repos/RMORSECode/calculator/releases'
  repo <- 'https://api.github.com/repos/NOAA-EDAB/stocksmart/releases'
  # pull all issues and select all submission issues
  # pulls 100 issues
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
