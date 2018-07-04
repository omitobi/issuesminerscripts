setwd("/Users/OMITOBISAM/Desktop/Thesis Data Import/Progress_6_Dec")

library('dplyr')
library('ggplot2')
library(reshape2)

pids_ <- c(4) #c(1,4,6,9);

for (pid_ in pids_) {
  initial_path <- paste("/Users/omitobisam/Desktop/Thesis Data Import/New Projects/New Project ", pid_, " Cost Churn", sep = '')
  my.data <- read.csv2(paste(initial_path, "/Project",pid_,"Data.csv", sep = ''), sep=";")
}

my.data_ <- arrange(my.data, Date)

head(my.data_)
# View(my.data)
# head(my.data[,c('Estimation', 'Actual')])

# my.data.summary <- summary.data.frame(my.data[,c('Estimation', 'Actual', 'Difference')])
# my.data.summary

# summary(my.data$Estimation)
# matplot(my.data[,c('Estimation', 'Actual')], type="l")
# plot(my.data$Estimation, type="l")
# lines(my.data$Actual, col="red")

# View(my.data.summary)
# my.data$Difference <- abs(my.data$Actual - my.data$Estimation)

# my.data$Compare <- cbind(ifelse(my.data$Actual==my.data$Estimation,0,ifelse(my.data$Actual>my.data$Estimation,1,-1)))

# d <- melt(my.data, id.vars=c("Actual", "Estimation"))

# ggplot(my.data, aes(x=Actual, y=Estimation, color=ModuleLevel)) + geom_point() + 
#   stat_smooth() +
#   facet_wrap(~ModuleDateRevisionId)

ggplot(my.data_, aes(x=X, y = value, color = variable)) + 
  geom_line(aes(y = Estimation, col = "Estimation")) + 
  geom_line(aes(y = Actual, col = "Actual"))


plot(my.data_$Date, my.data_$Actual, type='l', xlab='t /s', ylab='s1')
par(new=T)

plot(my.data_$Date, my.data_$Estimation, type='l', xlab='', ylab='', axes=F)
par(new=F)

# geom_point()+geom_text(aes(label=paste0('Actual', 'Estimation')), size=3, show.legend=FALSE)+
# theme_classic()