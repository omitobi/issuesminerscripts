setwd("/Users/OMITOBISAM/Desktop/Thesis Data Import/Progress_6_Dec")

# if(!require(devtools)) install.packages("devtools")
# devtools::install_github("kassambara/ggpubr")

library("ggplot2")
library("ggpubr")
library('dplyr')
library("BSDA")

imgunit <- "mm"
imgres <- 600
# ?dplyr.select

my.bfreqchurn <- read.csv2(paste("issuesminer_projectmoduleachurnhistory.csv", sep=";"))
View(my.bfreqchurn)
# my.uniq.bfreq <- unique(subset(my.bfreqchurn, ProjectId==6 & ModuleLevel == 2))
# View(my.uniq.bfreq$ModulePath)
# View(data.frame(unique(my.uniq.bfreq[order(my.uniq.bfreq$ModulePath),]$ModulePath)))
# my_data <- mtcars
# head(my_data, 6)

pids <- c(1) #project ids
mlevs <- c(2,3,4) #module levels
my.cor.result <- data.frame() #initial datafrome to keep the data
my.cor.result.loc <- data.frame() #initial datafrome to keep the data
for(pid in pids) {
  
  
  for(mlev in mlevs) {
    my.bfreqchurn.bylevnpid <- filter(my.bfreqchurn, ProjectId == pid & ModuleLevel == mlev)
    cdates_ <- my.bfreqchurn.bylevnpid[order(my.bfreqchurn.bylevnpid$Date),]
    cdates <- unique(cdates_$Date)
    for(cnt in 1:length(cdates)) {
      my.bfreqchurn.sub.perdate <- filter(my.bfreqchurn.bylevnpid, Date == cdates[cnt])
      
      # correlation of AlternativeCost and count of fixes
      if(length(unique(my.bfreqchurn.sub.perdate$AlternativeCost)) != 1 & length(unique(my.bfreqchurn.sub.perdate$fixes)) != 1) {
      
        my.date.cor <- cor.test(
          x=my.bfreqchurn.sub.perdate$AlternativeCost,
          y=my.bfreqchurn.sub.perdate$fixes, 
          method = "kendall", 
          exact = FALSE
          )
        
        my.cor.result[nrow(my.cor.result)+1, 'ProjectId'] <- pid
        my.cor.result[nrow(my.cor.result), 'Date'] <- cdates[cnt]
        my.cor.result[nrow(my.cor.result), 'ModuleLevel'] <- mlev
        my.cor.result[nrow(my.cor.result), 'Correlation'] <- my.date.cor$estimate
      
      }
      
      # correlation of AlternativeCost and LOC
      if(length(unique(my.bfreqchurn.sub.perdate$fixes)) != 1 & length(unique(my.bfreqchurn.sub.perdate$loc)) != 1) {
        
        my.date.cor.loc <- cor.test(
          x=my.bfreqchurn.sub.perdate$fixes,
          y=my.bfreqchurn.sub.perdate$loc, 
          method = "kendall", 
          exact = FALSE
        )
        
        my.cor.result.loc[nrow(my.cor.result.loc)+1, 'ProjectId'] <- pid
        my.cor.result.loc[nrow(my.cor.result.loc), 'Date'] <- cdates[cnt]
        my.cor.result.loc[nrow(my.cor.result.loc), 'ModuleLevel'] <- mlev
        my.cor.result.loc[nrow(my.cor.result.loc), 'Correlation'] <- my.date.cor.loc$estimate
        
      }
     
    }
    
  }
  
  
}

# write.csv2(x=my.cor.result, file = 'CorResultProjectCostFixesCorr.csv')
# write.csv2(x=my.cor.result.loc, file = 'CorResultProjectFixesLocCorr.csv')


my.cor.result <- read.csv2(file = 'CorResultProjectCostFixesCorr.csv', sep = ';')
my.cor.result.loc <- read.csv2(file = 'CorResultProjectFixesLocCorr.csv', sep = ';')

View(my.cor.result)
View(my.cor.result.loc)

count(my.cor.result)
count(my.cor.result.loc)

for(pid in pids) {
  
  for(mlev in mlevs) {
    #Cost and Fixes(Issues)
    my.data <- filter(my.cor.result, ProjectId==pid & ModuleLevel==mlev)
    my.data.order <- my.data[order(my.data$Date),]
    
    sign.test1 <- SIGN.test(my.data.order$Correlation, conf.level=0.99)
    
    print(paste('Project ', pid, 'Level', mlev))
    sign.test1
    # capture.output(print(sign.test1), file=paste("Project", pid, "Level", mlev, "ModuleCorrelationSignTest.txt", sep=""))
    #<--/>
    
    #Fixes and Size(LOC)
    my.data.loc <- filter(my.cor.result.loc, ProjectId==pid & ModuleLevel==mlev)
    my.data.order.loc <- my.data.loc[order(my.data.loc$Date),]
    
    sign.test1.loc <- SIGN.test(my.data.order.loc$Correlation, conf.level=0.99)
    
    print(paste('Project ', pid, 'Level', mlev, 'FixesLocCorr'))
    sign.test1.loc
    # capture.output(print(sign.test1.loc), file=paste("Project", pid, "Level", mlev, "ModuleFixesLocSignTest.txt", sep=""))
    #<--/>
    
    cost_fixes_corr <- my.data.order
    fixes_size_corr <- my.data.order.loc
    
    df_corr.cf <- data.frame(Date=cost_fixes_corr$Date, Correlation=cost_fixes_corr$Correlation)
    df_corr.fs <- data.frame(Date=fixes_size_corr$Date, Correlation=fixes_size_corr$Correlation)
    
    df_corr <- make.groups(df_corr.cf, df_corr.fs)
    
    # png(paste("Project", pid, "Level", mlev, "Cost-Fixes-ModuleCorrelation.png", sep=""), unit=imgunit, width=300, height=200, res=imgres)
    my.p <- ggplot(df_corr, aes(x=Date, y=Correlation, color=which))+theme_bw()+geom_point()+ylim(-1, 1) +
      scale_color_manual(name="Metric", labels = c("Cost-Fixes","Fixes-LOC"), values=c("red", "blue")) +
      theme(axis.text.x=element_text(size=14), axis.title.x=element_text(size=16),
            axis.text.y=element_text(size=14), axis.title.y=element_text(size=16),
            plot.title=element_text(size=20, face="bold", color="darkgreen"))
    my.p
    # try(print(my.p + geom_step() + theme(axis.text.x=element_text(angle=90))))
    # dev.off()
  }
  
}

  # scale_size_manual(name="Dataset", labels = c("Fixes-LOC","Cost-Fixes"), values=c(10, 5))
#plot a graph x=Date y=Correlation z=ModuleLevel?

# ggplot(filter(my.cor.result, ProjectId==1), x = "Date", y = "Correlation", xlab = "Miles/(US) gallon", ylab = "Weight (1000 lbs)")
# p1.cor.res <- filter(my.cor.result, ProjectId==6 & ModuleLevel==3)
# p1.cor.res2 <- p1.cor.res[order(p1.cor.res$Date),]
# SIGN.test(p1.cor.res2$Correlation, conf.level=0.99)

# View(p1.cor.res2)
# 
# ggplot(p1.cor.res2, aes(x=Date, y=Correlation))+geom_point(color='firebrick')

# View(filter(my.cor.result, ProjectId==1))
# 
# # subset(my.machistory.subs, ModulePath == 'jquery/test/')
# 
# ggscatter(my.bfreqchurn.sub, x = "AlternativeCost", y = "fixes", 
#           add = "reg.line", conf.int = TRUE, 
#           cor.coef = TRUE, cor.method = "kendall",
#           xlab = "Miles/(US) gallon", ylab = "Weight (1000 lbs)")
# 
# my.bfreqchurn.sub <- filter(my.bfreqchurn.bylevnpid, Date == cdate)
# 
# my.date.cor = cor.test(x=my.bfreqchurn.sub$AlternativeCost, y=my.bfreqchurn.sub$fixes, method = "kendall", exact = FALSE)
# data.frame(my.date.cor$estimate)$my.date.cor.estimate
# my.date.cor$estimate['tau',]
# 
# str(my.date.cor) #Gets the structure of a data
# View(my.bfreqchurn)



# Date = c(2, 3, 5) 
# ModuleLevel = c("aa", "bb", "cc") 
# ProjectId = c(TRUE, FALSE, TRUE) 
# df = data.frame(ProjectId, Date, ModuleLevel)  
# 
# 
# for(i in 1:iterations){
#   output[i,] <- runif(2)
#   
# }




# for(pid in pids) {
#   
#   for(mlev in mlevs) {
#     my.data <- filter(my.cor.result, ProjectId==pid & ModuleLevel==mlev)
#     my.data.order <- my.data[order(my.data$Date),]
#     
#     sign.test1 <- SIGN.test(my.data.order$Correlation, conf.level=0.99)
#     
#     print(paste('Project ', pid, 'Level', mlev))
#     sign.test1
#     # Capture.output - print what's the output of the command run in the console ref: https://www.r-bloggers.com/export-r-output-to-a-file/
#     capture.output(print(sign.test1), file=paste("Project", pid, "Level", mlev, "ModuleCorrelationSignTest.txt", sep=""))
#     
#     png(paste("Project", pid, "Level", mlev, "ModuleFixesCorrelation.png", sep=""), unit=imgunit, width=300, height=200, res=imgres)
#     my.p <- ggplot(my.data.order, aes(x=Date, y=Correlation)) + theme_bw()+geom_point(color='firebrick') + theme(title=element_text(size=9), text=element_text(size=8))+ylim(-1,1)
#     try(print(my.p + geom_step() + theme(axis.text.x=element_text(angle=90))))
#     dev.off()
#   }
#   
# }



# for(pid in pids) {
#   
#   for(mlev in mlevs) {
#     my.data.loc <- filter(my.cor.result.loc, ProjectId==pid & ModuleLevel==mlev)
#     my.data.order.loc <- my.data.loc[order(my.data.loc$Date),]
#     
#     sign.test1.loc <- SIGN.test(my.data.order.loc$Correlation, conf.level=0.99)
#     
#     print(paste('Project ', pid, 'Level', mlev, 'FixesLocCorr'))
#     sign.test1.loc
#     capture.output(print(sign.test1.loc), file=paste("Project", pid, "Level", mlev, "ModuleFixesLocSignTest.txt", sep=""))
#     
#     png(paste("Project", pid, "Level", mlev, "ModuleFixesLocCorrelationLevel.png", sep=""), unit=imgunit, width=300, height=200, res=imgres)
#     my.p.loc <- ggplot(my.data.order.loc, aes(x=Date, y=Correlation)) + theme_bw()+geom_point(color='firebrick') + theme(title=element_text(size=9), text=element_text(size=8))+ylim(-1,1)
#     try(print(my.p.loc + geom_step() + theme(axis.text.x=element_text(angle=90))))
#     dev.off()
#   }
#   
# }

