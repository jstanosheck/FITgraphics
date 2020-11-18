#File for each of the functions
#' Import FIT File
#'
#' @description Checks for the existence of a file in the given location and
#' if it exist, collects data and outputs a Data Frame containing all data in
#' the file.
#'
#' @param fileName - String of file path corresponding to the FIT file. File must be .fit extension.
#'
#' @export
#'
#' @return Returns a data.frame ordered based on the timestamp of each instance.
#' Each column is named and represents the different variables e.g. heart_rate.

getFit <- function(fileName){
#Compatability Checks
###################################################
  #check that fileName is of type string
  if (is.character(fileName) == FALSE){
    stop("Argument fileName must be a character string.")
  }
  #Check if the file actually exists in the path from parameter 1
  if (file.exists(fileName) == FALSE){
    stop("File does not exist at given file path.")
  }
  #check if file is of proper type
  if (grepl("\\.fit$", fileName) == FALSE){
    stop("File must have the extension .fit")
  }

##################################################
  #Start of function file loading
##################################################
  #load the file using the readFitFile function
  out_file <- readFitFile(fileName = fileName)

  #use the dplyr library to bind the records together by the timestamp
  out_records <- records(out_file) %>%
    bind_rows() %>%
    arrange(timestamp)

  #extract the individual record names
  #these are the available datafields for user interaction
  records <- names(out_records)

  return(list(data = out_records, record_names = records))
}


#' plotFit
#'
#' @description
#'
#' @param fitFile -
#' @param varName -
#'
#' @export
#'
#' @return

plotFit <- function(fitFile, varName){
  #makes a vector out of the data from specified data field
  dataVector <- as.vector(unlist(fitFile[varName]))

  #output average speed
  average <- mean(dataVector)

  #This will plot a graphic of the varName vs time
  ggplot(fitFile, aes(x = timestamp, y = dataVector)) +
    geom_line(size = 0.5, color="orange") +
    geom_hline(yintercept = average, color = "blue")
}
