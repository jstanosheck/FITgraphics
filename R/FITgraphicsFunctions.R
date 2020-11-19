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
#Compatibility Checks
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

  #extract session data from out_file
  session <- getMessagesByType(out_file, "session")

  return(list(data = out_records, records = records, session = session))
}


#' plotFit
#'
#' @description
#'
#' @param fitFile - {Dataframe} Required parameters: data, records
#' @param varName - {Character} Name of requested variable to be graphed
#' @param showAverage {Logical} Show average line and value, default = False
#'
#' @export
#' @import ggplot2
#'
#' @return

plotFit <- function(fitFile, varName, showAverage = FALSE){
  #compatibility checks
########################################
  #check if varName is a character string
  if (is.character(varName) == FALSE){
    stop("Check type for varName; must be a character")
  }
  #check data class of fitFile
  if (data.class(fitFile) != "list"){
    stop("Argument fitFile must be of class list")
  }
  #check to see if varName is in the list of names from fitFile
  if ((varName %in% fitFile$records) == FALSE){
    stop("The varName provided is not a variable in fitFile.")
  }

#plotting of data
###########################################################
  #makes a vector out of the data from specified data field
  dataVector <- as.vector(unlist(fitFile$data[varName]))

  #output average speed
  average <- mean(dataVector)

  #set the title as a string
  #title <- sprintf('%s vs. Time', varName)

  #Set up the initial structure of the plot w/ line xlab and title
  g <- ggplot(fitFile$data, aes(x = timestamp, y = dataVector)) +
    geom_line(size = 0.5, color="orange") +
    xlab("Time of Day (HH:MM)") +
    ggtitle(toupper(sprintf('%s vs. Time', varName)))

  if (showAverage){
    #add the line for the average value to the plot
    g <- g + geom_hline(yintercept = average, color = "black")
    #add annotation for the average value label
    g <- g + geom_label(aes(x = as.POSIXct(timestamp[floor(length(timestamp) * 0.25)]),
                            y = max(dataVector) * 0.9,
                            label = sprintf("%.2f \n Average %s", average, varName)))

  }

  return(g)
}



#' mapFit
#'
#' @description
#'
#' @param fitFile - {Dataframe} Required parameters: data, records
#'
#' @export
#' @import leaflet
#' @importFrom dplyr select
#'
#' @return

mapFit <- function(fitFile){
  #get the coordinates long, lat
  coordinates <- fitFile$data %>%
    select(position_long, position_lat)

  #add the coordinates to the map file
  map <- coordinates %>%
    as.matrix() %>%
    leaflet() %>%
    addTiles() %>%
    addPolylines()

  return(map)
}

