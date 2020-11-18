#Testing for the fit graphics functions

library(FITfileR)
library(dplyr)

#check if the file exists
file.exists("/Users/jacobstanosheck/Desktop/TestRun.fit")

#load the file using the readFitFile function
test_file <- readFitFile("/Users/jacobstanosheck/Desktop/TestRun.fit")

#use the dplyr library to bind the records together by the timestamp
all_test_records <- records(test_file) %>%
  bind_rows() %>%
  arrange(timestamp)


#plot speed vs time
ave_speed <- mean(all_test_records$speed * 2.23694)


ggplot(all_test_records, aes(x=timestamp, y=((speed)^-1 * 26.822))) +
  geom_line(size = 0.5, color="orange")  +
  geom_hline(yintercept = ave_speed, color="blue") +
  scale_y_continuous(name = "Speed (miles per hour)",
   trans = 'reverse')


