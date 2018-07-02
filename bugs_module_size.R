setwd("/Users/OMITOBISAM/Desktop/Thesis Data Import/Progress_6_Dec")

library('dplyr')
library('ggplot2')
library(RMySQL)
# -------------------------
mydb = dbConnect(MySQL(), user='root', password='Sch..l1234', dbname='issuesminer', host='localhost')
# -------------------------
Sys.setenv(TZ='Europe/Tallinn')
pids_ <- c(1,4,6,9) #project ids
mlevs_ <- c(2,3,4) #module levels
my.cor.res_ <- data.frame() #initial datafrome to keep the data
# -------------------------

for(pid_ in pids_) {
  
  for(mlev_ in mlevs_) {
    start.time <- Sys.time()
    
    print(paste('Project ', pid_,' Level ', mlev_,' started at: ', start.time))
    
    rs = dbSendQuery(mydb, paste(
      'select ProjectId, ModuleLevel, ModulePath, fixesCompare, locCompare, Date from projectcostdifference where ProjectId =',pid_,' and ModuleLevel =', mlev_))
    print(paste('ran query : ', mlev_))
    ata = fetch(rs, n=-1)
    print(paste('fetched data: ', mlev_))
    my.compare.byPnL <- filter(ata, ProjectId == pid_ & ModuleLevel == mlev_)
    print(paste('filtered by projectid and modulelevel data: ', mlev_))
    
    cdates_ <- my.compare.byPnL[order(my.compare.byPnL$Date),]
    print(paste('sorted by date: ', mlev_))
    cdates <- unique(cdates_$Date)
    print(paste('found unique date by date: ', mlev_))
    for(dt in cdates) {
      
      print(paste('date: ', dt, ' project ', pid_, 'level: ', mlev_))
      my.compare.perdate <- filter(my.compare.byPnL, Date == dt)
      
      if(length(unique(my.compare.perdate$locCompare)) != 1 & length(unique(my.compare.perdate$fixesCompare)) != 1) {
        
        my.date.cor <- cor.test(
          x=my.compare.perdate$locCompare,
          y=my.compare.perdate$fixesCompare,
          method = "kendall",
          exact = FALSE
        )
        
        # nrow_cor_res <- nrow(my.cor.res_)
        my.cor.res_[nrow(my.cor.res_)+1, 'ProjectId'] <- pid_
        my.cor.res_[nrow(my.cor.res_), 'Date'] <- dt
        my.cor.res_[nrow(my.cor.res_), 'ModuleLevel'] <- mlev_
        # my.cor.res_[nrow_cor_res, 'ModulePath'] <- my.compare.perdate$ModulePath
        my.cor.res_[nrow(my.cor.res_), 'Correlation'] <- my.date.cor$estimate
      }
    }
    
    end.time <- Sys.time()
    time.taken <- end.time - start.time
    
    print(paste('Project ', pid_,' Level ', mlev_,' ended Taken: ', time.taken, " at: ", end.time))
  }
  
}

# View(my.cor.res_)

# write.csv2(x=my.cor.res_, file = 'ProjectSizeFixesCompareCorrC.csv')

my.cor.res_1 <- read.csv2(paste("ProjectSizeFixesComparreCorr.csv", sep=";"))

p_pid <- 9
{
  png(paste("Project",p_pid , "Size-Fixes-CompareCorrC.png", sep=""), unit=imgunit, width=300, height=200, res=600)
  my.p <- ggplot(filter(my.cor.res_1, ProjectId==p_pid), mapping=aes(y=Correlation,
                                                                     x=Date,
                                                                     # color=ModuleLevel,
                                                                     size=ModuleLevel
  ))+theme_bw()+ylim(-1,1)+geom_point(aes(colour=ModuleLevel),               # colour depends on cond2
                                      size=3) + 
    scale_colour_gradientn(colours=rainbow(3)) +
    theme(axis.text.x=element_text(size=8, angle = 90, margin= margin(t = 0, r = 20, b = 0, l = 0)), axis.title.x=element_text(size=16),
          axis.text.y=element_text(size=14), axis.title.y=element_text(size=16),
          plot.title=element_text(size=14, face="bold", color="darkgreen"))+
    labs(title = "JQuery's Module size and bugs correlation plot")
  
my.p
  
  try(print(my.p))
  dev.off()
}
