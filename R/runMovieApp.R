
#' @export
#'
runMovieApp <- function() {
  appDir <- system.file("movieapp-shiny", "MovieApp", package = "ptds2019FinalG4")
  if (appDir == "") {
    stop(
      # REPLACE N BY YOUR GROUP NUMBER AND DELETE THIS COMMENT
      "Could not find example directory. Try re-installing ptds2019hw4gN.",
      call. = FALSE
    )
  }

  shiny::runApp(appDir, display.mode = "normal")

}

