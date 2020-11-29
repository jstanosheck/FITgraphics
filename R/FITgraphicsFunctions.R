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
#' @return Returns a \code{data.frame} ordered based on the timestamp of each instance.
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

  #Set up the initial structure of the plot w/ line xlab and title
  g <- ggplot(fitFile$data, aes(x = timestamp, y = dataVector)) +
    geom_line(size = 0.5, color="orange") +
    xlab("Time of Day (HH:MM)") +
    ggtitle(toupper(sprintf('%s vs. Time', varName)))

  if (showAverage){
    #add the line for the average value to the plot
    g <- g + geom_hline(yintercept = average, color = "black")
    #add annotation for the average value label
    g <- g + geom_label(aes(x = timestamp[floor(length(timestamp) * 0.25)],
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


#' showTrainingEffect
#'
#' @description
#'
#' @param fitFile - Must be the output from the getFit() function in this
#'    package.
#' @param AnaerobicTE - (Logical) Default = FALSE. If true, the function will
#'    return the Anaerobic Training Effect rather than the Aerobic Training
#'    Effect.
#'
#' @return A plot in the viewer window showing a gauge of range between 0 and 5.
#'    The gauge displays the training effect in as a number and in the gauge.
#' @export
#'
#' @examples
#' #load the necessary file using the getFIT function
#' NULL
showTrainingEffect <- function(fitFile, AnaerobicTE = FALSE){

  #check if the AnaerobicTE is True and assign the title and value for the function
  if (AnaerobicTE){
    title = "Anaerobic Training Effect"
    value = fitFile$session$total_anaerobic_training_effect
  } else {
    title = "Aerobic Training Effect"
    value = fitFile$session$total_training_effect
  }

  #check the value of the training effect to determine the color of the line
  if (value <= 1){
    colorvalue = "#ffffbf"
  }else if (value <= 2){
    colorvalue = "#2b83ba"
  }else if (value <= 3){
    colorvalue = "#abdda4"
  }else if (value <= 4){
    colorvalue = "#fdae61"
  }else {
    colorvalue = "#d7191c"
  }


  #plots the requested training effect Default is Aerobic TE
  fig <- plotly::plot_ly(
    domain = list(x = c(0, 1), y = c(0, 1)),
    value = value,
    title = list(text = title,
                 font = list(
                   color = "#414a4c",
                   size = 24,
                   family = "arial")),
    type = "indicator",
    number = list(
      font = list(
        color = "#414a4c"
      )),
    mode = "gauge+number",
    gauge = list(
      bar = list(color = colorvalue,
                 line = list(
                   width = 1
                 )),
      axis = list(range = list(NULL, 5),
                  tickfont = list(
                    family = "arial",
                    size = 17,
                    color = "#414a4c"
                  )),
      steps = list(
        list(range = c(0, 1), color = "#fffff0"),
        list(range = c(1, 2), color = "#c9e3f2"),
        list(range = c(2, 3), color = "#d2edcf"),
        list(range = c(3, 4), color = "#fedebd"),
        list(range = c(4, 5), color = "#f8c4c5")
      )
    ))
  fig <- fig %>%
    plotly::layout(
      margin = list(l=20,r=30),
      paper_bgcolor = "#cccccc")


  return(fig)
}
