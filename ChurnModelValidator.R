setwd("/Users/OMITOBISAM/Desktop/Thesis Data Import/Progress_6_Dec")

library('dplyr')
library('ggplot2')

initial_path <- "/Users/omitobisam/Desktop/Thesis Data Import/New Projects/New Project 4 Cost Churn"
pids_ <- c(4) #c(1,4,6,9);

for (pid_ in pids_) {
  my.data <- read.csv2(paste(initial_path, "/Project",pid_,"Data.csv", sep = ''), sep=";")
}

View(my.data)
head(my.data[,c('Estimation', 'Actual', 'Difference')])

my.data.summary <- summary.data.frame(my.data[,c('Estimation', 'Actual', 'Difference')])
my.data.summary

summary(my.data$Estimation)

plot(my.data$Estimation, type="l")
lines(my.data$Actual, col="red")

View(my.data.summary)
my.data$Difference <- abs(my.data$Actual - my.data$Estimation)

my.data$Compare <- cbind(ifelse(my.data$Actual==my.data$Estimation,0,ifelse(my.data$Actual>my.data$Estimation,1,-1)))



ggplot(my.data, aes(x=Actual, y=Estimation)) + geom_density()
# geom_point()+geom_text(aes(label=paste0('Actual', 'Estimation')), size=3, show.legend=FALSE)+
# theme_classic()