#Testing for the fit graphics functions

library(FITfileR)
library(dplyr)
library(ggplot2)

#check if the file exists
file.exists("/Users/jacobstanosheck/Desktop/TestRun.fit")

#check if file extension if of .fit
grepl("\\.fit$", "/Users/jacobstanosheck/Desktop/TestRun.fit")

#load the file using the readFitFile function
test_file <- readFitFile("/Users/jacobstanosheck/Desktop/TestRun.fit")

#use the dplyr library to bind the records together by the timestamp
all_test_records <- records(test_file) %>%
  bind_rows() %>%
  arrange(timestamp)


#plot speed vs time
ave_speed <- mean(all_test_records$speed)


ggplot(all_test_records, aes(x=timestamp, y=altitude)) +
  geom_line(size = 0.5, color="orange")  +
  geom_hline(yintercept = ave_speed, color="blue") +
  scale_y_continuous(name = "Speed (m/s)") + xlab("Time of Day (HH:MM)")


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






