
#' @title App runner file
#'
#' @return  merged standardized dataset
#'
#' @export
#'
runMerger <- function(){

  appdir <- system.file("shiny", "mergerApp", package = "specleanr")

  #check if package is installed and the app is in
  if(appdir=="")stop("The app directory is not found and specleanr is possibly not installed succesfuly.", call. = FALSE)

  # lstfiles <- list.files(pattern = ".csv&")
  #
  # if(length(lstfiles)<=1) stop("the csv files dont exist in the working directory and merging cannot continue..")
  #
  shiny::runApp(appDir = appdir)


}
