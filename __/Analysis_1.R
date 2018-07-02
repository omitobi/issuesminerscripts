setwd("/Users/OMITOBISAM/Desktop/Thesis Data Import/Progress_6_Dec/Project1")

library("ggplot2")

my.bfreq <- read.csv(paste("Project1BugFreqLevel2.csv", sep=""))
my.machistory <- read.csv2(paste("Project1ModuleAChurnLevel2History.csv", sep=""))

# my.bfuniq <- unique(my.bfreq[my.bfreq['ProjectId'] == 1, 'Date'])
# my.bfuniq

# my.macunq <- unique(my.machistory[, 'Date'])
# my.macunq
# 
# intersect(my.bfuniq, my.macunq)

my.machistory.subs <- my.machistory[
  (my.machistory['ModulePath'] == 'jquery/src/') | (my.machistory['ModulePath'] == 'jquery/test/') 
  | (my.machistory['ModulePath'] == 'jquery/dist/') | (my.machistory['ModulePath'] == 'jquery/build/'), ]

my.bfreq.mduniq <- unique(my.bfreq[, 'module'])
my.bfreq.mduniq #The unique modules in the project

# my.bfixes <- data.frame(Date=as.Date(character()),
#                               File=character(), 
#                               User=character(), 
#                               stringsAsFactors=FALSE)
my.machistory.subs.mdtest <- subset(my.machistory.subs, ModulePath == 'jquery/test/') #picking a particular module in costanalysis history
my.machistory.subs.mdtest

my.bfreq.mdtest <- subset(my.bfreq, module == 'jquery/test/') #picking a particular module
my.bfreq.mdtest

my.bfreq.mdtest <- subset(my.)

for(i in 1:nrow(my.machistory.subs)){
  for(j in 1:nrow(my.bfreq)) {
    if(my.bfreq$Date[j]) {
      index <- my.bfreq[j]
    }
  }
  my.machistory.subs[index,]
}
for(my.machistory.sub in my.machistory.subs) {
  my.bfreq.history <- my.machistory.sub['Date'];
  View(my.machistory.sub)
  break;
}

View(my.machistory.sub)

my.bfd <- my.bfixes[(my.bfixes['Date'] == '2017-03-06') & my.bfixes['ProjectId'] == 1,]
my.bfd

my.macd <- my.machistory[my.machistory['Date'] == '2017-03-06',]
my.macd
View(my.bfreq)
View(my.machistory)

