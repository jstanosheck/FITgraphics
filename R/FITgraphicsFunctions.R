#File for each of the functions
#' Import FIT File
#'
#' @description Checks for the existence of a file in the given location and
#' if it exist, collects data and outputs a \code{data.frame} containing all data in
#' the file.
#'
#' @param fileName - {Character} (Required) File path corresponding to the FIT file. File must be .fit extension.
#'
#' @export
#'
#' @return Returns a \code{data.frame} ordered based on the timestamp of each instance.
#' Each column is named and represents the different variables e.g. heart_rate.
#'
#' @examples
#'
#'#file_path <- system.file("extdata", "TestRun.fit", package = "FITgraphics")
#'
#' #loaded_File <- getFit(file_path)
#'
#' #loaded_File

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
  out_file <- FITfileR::readFitFile(fileName = fileName)

  #use the dplyr library to bind the records together by the timestamp
  out_records <- FITfileR::records(out_file) %>%
    dplyr::bind_rows() %>%
    dplyr::arrange(timestamp)

  #extract the individual record names
  #these are the available datafields for user interaction
  records <- names(out_records)

  #extract session data from out_file
  session <- FITfileR::getMessagesByType(out_file, "session")

  return(list(data = out_records, records = records, session = session))
}


#' plotFit
#'
#' @description Creates a line plot of a varName vs time.
#'
#' @param fitFile - {Dataframe} (Required) Must be the output of \code{gitFit()}
#' @param varName - {Character} (Required) Name of requested variable to be graphed
#' @param showAverage - {Logical} (Optional) Show average line and value, \code{default = FALSE}
#' @param showMax - {Logical} (Optional) Show the maximum value in plot, \code{default = FALSE}
#'
#' @export
#' @import ggplot2
#'
#' @return Returns a plot of the \code{varName} vs time. This The average and
#' maximum values will be shown based on the input parameters. Each graph will
#' have a different graph color based on what \code{varName} is chosen.

plotFit <- function(fitFile, varName, showAverage = FALSE, showMax = FALSE){
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
  #check that showAverage is bool
  if (!is.logical(showAverage)){
    stop("showAverage must be logical")
  }
  #check that showMax is bool
  if (!is.logical(showMax)){
    stop("showMax must be logical")
  }

###########################################################
#plotting of data
###########################################################
  #Set the color of the graph and the Y label axis name.
  if (tolower(varName) == "speed"){
    #set graph color
    graphColor <- "#52bfeb"
    #set Y axis name
    yaxis <- "Speed (m/s)"
  }else if (tolower(varName) == "altitude"){
    #set graph color
    graphColor <- "#f09c00"
    #set Y axis name
    yaxis <- "Altitude (m)"
  } else if (tolower(varName) == "power"){
    #set graph color
    graphColor <- "#631f9e"
    #set Y axis name
    yaxis <- "Power (W)"
  }  else if (tolower(varName) == "heart_rate") {
    #set graph color
    graphColor <- "#ff1414"
    #set Y axis name
    yaxis <- "Heart Rate (bpm)"
  } else if (tolower(varName) == "temperature"){
    #set graph color
    graphColor <- "darkgrey"
    #set Y axis name
    yaxis <- "Temperature (*C)"
  } else if (tolower(varName) == "cadence"){
    #set graph color
    graphColor <- "#17c417"
    #set Y axis name
    yaxis <- "Cadence (SPM/RPM)"
  } else{
    #set standard graph color
    graphColor <- "#454545"
    #set standard Y axis name
    yaxis <- toupper(sprintf('%s (Unknown Units)', varName))
  }


  #makes a vector out of the data from specified data field
  #if the cadence is the varName then determine the cadence by multiplying by 2
  if (tolower(varName) == "cadence"){
    #this accounts for the cadence only being counted in halves.
    #Therefore shows the true spm or rpm
    dataVector <- as.vector(unlist(fitFile$data[varName])) * 2
  } else{
    dataVector <- as.vector(unlist(fitFile$data[varName]))
  }



  #Set up the initial structure of the plot w/ line xlab and title
  g <- ggplot(fitFile$data, aes(x = timestamp, y = dataVector)) +
    geom_line(size = 0.5, color = graphColor) +
    xlab("Time of Day (HH:MM)") +
    ylab(yaxis) +
    ggtitle(toupper(sprintf('%s vs. Time', varName)))

  if (showAverage){
    #output average of dataVector remove all NA values
    average <- mean(dataVector, na.rm = TRUE)
    #add the line for the average value to the plot
    g <- g + geom_hline(yintercept = average, color = "black")
    #add annotation for the average value label
    g <- g + geom_label(aes(x = timestamp[floor(length(timestamp) * 0.25)],
                            y = max(dataVector, na.rm = TRUE) * 0.9,
                            label = sprintf("%.2f \n Average %s", average, varName)))
  }
  if (showMax){
    #find the max value of dataVector
    max_output <- max(dataVector, na.rm = TRUE)
    #add annotation for the max value label
    g <- g + geom_label(aes(x = timestamp[floor(length(timestamp) * 0.75)],
                            y = max(dataVector, na.rm = TRUE) * 0.9,
                            label = sprintf("%.2f \n Max %s",
                                            max_output, varName)))
  }


  return(g)
}



#' mapFit
#'
#' @description Plots the map and the path of the activity.
#'
#' @param fitFile - {Dataframe} (Required) Must be the output of \code{gitFit()}
#'
#' @export
#' @import leaflet
#' @importFrom dplyr select
#'
#' @return A map showing the GPS route that was taken during the activity.

mapFit <- function(fitFile){
  #get the coordinates long, lat
  coordinates <- fitFile$data %>%
    select(position_long, position_lat) %>%
    as.matrix()

  #add the coordinates to the map file
  map <- coordinates %>%
    leaflet::leaflet() %>%
    leaflet::addTiles() %>%
    leaflet::addPolylines(color = "#0E4AC2", opacity = 0.75) %>%
    #add point to the starting postion that is green and ending position is red
    leaflet::addCircleMarkers(lng = c(utils::head(coordinates, n=1)[1, 1],
                                      utils::tail(coordinates, n=1)[1, 1]),
                              lat = c(utils::head(coordinates, n=1)[1, 2],
                                      utils::tail(coordinates, n=1)[1, 2]),
                              color = c("#04db62", "#f1070b"),
                              opacity = .75,
                              fillOpacity = .75,
                              radius = 3)

  return(map)
}


#' showTrainingEffect
#'
#'
#' @param fitFile - {Dataframe} (Required) Must be the output of \code{gitFit()}
#' @param AnaerobicTE - {Logical} (Optional) \code{Default = FALSE}. If true, the function will
#'    return the Anaerobic Training Effect rather than the Aerobic Training
#'    Effect.
#'
#' @return A plot in the viewer window showing a gauge of range between 0 and 5.
#'    The gauge displays the training effect in as a number and in the gauge.
#' @export
#'
#' @examples
#' #load the necessary file using the getFIT function
#' #fitFile <- getFit("Data/TestRun.fit")
#'
#' #plot aerobic training effect
#' #aerobic <- showTrainingEffect(fitFile, AnaerobicTE = F)
#' #aerobic
#'
#' #plot anaerobic training effect
#' #anaerobic <- showTrainingEffect(fitFile, AnaerobicTE = T)
showTrainingEffect <- function(fitFile, AnaerobicTE = FALSE){
  #check that fitFile Aerobic and Anaerobic Training Effect exist
  if (!exists("total_anaerobic_training_effect", fitFile$session) ||
      !exists("total_training_effect", fitFile$session)){
    stop("Training Effect does not exist in this file")
  }
  #check that AnaerobicTE is bool
  if (!is.logical(AnaerobicTE)){
    stop("AnaerobicTE must be logical")
  }

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
