setwd("/Users/omitobisam/Desktop/Thesis Data Import/New Projects/New Project 4 Cost Churn/090718")

library("ggplot2")
library("dplyr")

my.data._<- read.csv2(paste("Project", 4, "ChurnEstimations09072018.csv", sep=""), sep=";")

pid_ = 4

{
  initial_path <- paste("/Users/omitobisam/Desktop/Thesis Data Import/New Projects/New Project ", pid_, " Cost Churn", sep = '')
  my.data_._ <- read.csv2(paste(initial_path, "/Project",pid_,"ChurnEstimationsg.csv", sep = ''), sep=";")
}

# my.data._$X <- seq.int(nrow(my.data._))
# my.data._$ModuleLevel <- lengths(regmatches(my.data._$ModulePath, gregexpr("/", my.data._$ModulePath)))

# my.data_ <- sub.my_data[order(sub.my_data$Estimation),]
sub_my_data <- subset(my.data._, ModuleLevel == 3)
my_data <- sub_my_data[order(sub_my_data$Date),]

head(my_data)

unique(my_data$ModulePath)

NROW(unique(my_data$ModulePath))
NROW(my.data_._)
NROW(my_data)

# my_data <- my.data_._ %>% filter(ModuleLevel==2)

ggplot(my_data, aes(y=Estimation,
     x=ModuleDateRevisionId))+geom_line() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
# par(las=2)


plot(density(my_data$Estimation))

# for(coln in data.col) {
#   print(coln)
#   plot(y=my.data._[,coln], x=my.data._$ModuleDateRevisionId, main=paste(coln, " DateRevisionId plot", sep = " "))
# }
# 
# 
# head(my_data)
# 
# plot()


# my.data._ <- arrange(subset(my.data._, ModuleLevel > 1), Date)
# 
# ggplot(my.data._, aes(x=Date, y=OODevelopersOnProjectToDate))+geom_point()

ggplot(my.data_._, aes(Estimation, group=ModuleLevel))+geom_histogram(binwidth = 0.3)
# 
# plot(y=my.data._$Imperative.Files, x=my.data._$Date)
# ggplot(my.data._, aes(y=Committer.Previous.OO.Commits, x=ModuleDateRevisionId))+geom_smooth(method = 'gam')
