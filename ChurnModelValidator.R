setwd("/Users/OMITOBISAM/Desktop/Thesis Data Import/Progress_6_Dec")

library('dplyr')
library('ggplot2')

initial_path <- "/Users/omitobisam/Desktop/Thesis Data Import/New Projects/New Project 4 Cost Churn"
pids_ <- c(4) #c(1,4,6,9);

for (pid_ in pids_) {
  my.data <- read.csv2(paste(initial_path, "/Project",pid_,"Data.csv", sep = ''), sep=";")
}

plot(my.data$Actual, my.data$Estimation)

ggplot(my.data, aes(x=Actual, y=Estimation))+geom_point()
