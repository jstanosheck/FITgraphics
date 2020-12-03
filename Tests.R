#Testing for the fit graphics functions

library(FITfileR)
library(dplyr)
library(ggplot2)

#check if the file exists
file.exists("Data/TestRun.fit")

#check if file extension if of .fit
grepl("\\.fit$", "Data/TestRun.fit")

#load the file using the readFitFile function
test_file <- readFitFile("/Users/jacobstanosheck/Desktop/TestRun.fit")

#use the dplyr library to bind the records together by the timestamp
all_test_records <- records(test_file) %>%
  bind_rows() %>%
  arrange(timestamp)


#plot speed vs time
ave_speed <- mean(all_test_records$speed)


ggplot(all_test_records, aes(x=timestamp, y=altitude)) +
  geom_line(size = 1, color="orange") +
  geom_point(aes(alpha = ifelse(altitude == max(altitude), 1, 0), label = "max"))


#make diff_time
time_diff <- difftime(max(all_test_records$timestamp), min(all_test_records$timestamp), units = "min")


#convert column of df to vector
newV <- fitFile[varName]

newV <- as.vector(unlist(newVector))




test_func <- function(fitFile, varName){
  #set varName as string
  #varName <- as.character(varName)
  newVector <- fitFile[varName]

  newVector <- as.vector(unlist(newVector))

  #output average speed
  aveSpeed <- mean(newVector)

  return(aveSpeed)
}

#test the test_func
test_func(all_test_records, varName = "speed")





##############################
#test getFit() output
test_getfit <- getFit("/Users/jacobstanosheck/Desktop/TestRun.fit")
data.class(test_getfit)


#test plotFit() output
plotFit(test_getfit, "speed")


#testing for coordinate selection
coordinates <- test_getfit$data %>%
  select(position_long, position_lat)

#generate the map
map <- coordinates %>%
  as.matrix() %>%
  leaflet() %>%
  addTiles() %>%
  addPolylines()


#Testing for the addition of min points



me.2 <- me %>%
  group_by(variable) %>%
  mutate(color = (min(value) == value | max(value) == value))

ggplot(data=me.2, aes(x = date, y = value)) +
  geom_line() +
  geom_point(aes(color = color)) +
  facet_wrap(~variable, ncol=1, scales="free_y") +
  scale_color_manual(values = c(NA, "red"))





#test the gauge on a toy example
library(plotly)

fig <- plot_ly(
  domain = list(x = c(0, 1), y = c(0, 1)),
  value = session$total_anaerobic_training_effect,
  title = list(text = "Speed", font = list(color = "red")),
  type = "indicator",
  mode = "gauge+number",
  gauge = list(
    bar = list(color = "orange",
               line = list(color = "red", width = 10))
  ))
fig <- fig %>%
  layout(margin = list(l=20,r=30))

fig






#start of testing individual functions
##########################################################
#source functions
source("R/FITgraphicsFunctions.R")

#load FIT file into variavle test_fit
test_fit <- getFit("Data/TestRun.fit")

#graph the speed graph with showAverage = FALSE
plotFit(test_fit, "distance")

#test the ride file
ride_test <- getFit("Data/TestRide.fit")

plotFit(ride_test, "power", showMax = T)


#test mapFit
mapFit(ride_test)

mapFit(test_fit)
