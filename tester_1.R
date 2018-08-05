setwd("/Users/OMITOBISAM/Desktop/Thesis Data Import/Progress_6_Dec")


summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
  library(plyr)
  
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col) {
                   c(N    = length2(xx[[col]], na.rm=na.rm),
                     mean = mean   (xx[[col]], na.rm=na.rm),
                     sd   = sd     (xx[[col]], na.rm=na.rm)
                   )
                 },
                 measurevar
  )
  
  # Rename the "mean" column    
  datac <- rename(datac, c("mean" = measurevar))
  
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}

# install.packages("RMySQL")
# dbListTables(mydb)

# by fixesDifference ASC limit 1 offset 24354918

# rs = dbSendQuery(mydb, "select count(*) from projectcostdifference where fixesDifference <= 2")

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
              'select ProjectId, ModuleLevel, ModulePath, fixesCompare, costCompare, Date from projectcostdifference where ProjectId =',pid_,' and ModuleLevel =', mlev_))
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

              if(length(unique(my.compare.perdate$costCompare)) != 1 & length(unique(my.compare.perdate$fixesCompare)) != 1) {
              
                  my.date.cor <- cor.test(
                    x=my.compare.perdate$costCompare,
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


# write.csv2(x=my.cor.res_, file = 'ProjectCostFixesComparreCorr.csv')

my.cor.res_1 <- read.csv2(
  "ProjectSizeFixesBeforeCompareCorrelation.csv"  #Change to ProjectSizeFixesCompareCorrelation.csv for ModuleSize,
                                      #ProjectCostFixesCompareCorrelation.csv for AC/Cost
                                      #ProjectSizeFixesBeforeCompareCorrelation.csv for SizeF-ixes before Comparing size
  , sep=";")
# my.new.cor <- na.omit(my.cor.res_)

plot.project = function(project_id, title_=NULL) {
  my.p <- ggplot(filter(my.cor.res_1, ProjectId==project_id), 
                 mapping=aes(y=Correlation, x=Date, color=ModuleLevel))+
    theme_bw()+
    ylim(-1,1)+
    geom_point(size=3) + 
    theme(axis.text.x=element_text(size=8, angle = 90, margin= margin(t = 0, r = 20, b = 0, l = 0)),
          axis.title.x=element_text(size=16),
          axis.text.y=element_text(size=14), axis.title.y=element_text(size=16),
          plot.title=element_text(size=14, face="bold", color="darkgreen")) +
    scale_colour_gradientn(limits = c(2, 4), breaks = c(2, 3, 4),
                           guide = guide_colorbar(ticks = TRUE, ticks.linewidth = 2),
                           colours=rainbow(3))
  
  if (!is.null(title_)) {
    my.p <- my.p + ggtitle(title_)
  }
  return (my.p)
}
  
save.plot = function(my.plot, type, project_id) {
  png_file <- paste("ProjectCombine", type, "-Fixes-CompareCorrelation.png", sep="")
  if (project_id) {
    png_file <- paste("Project",project_id , type,"-Fixes-CompareCorrelation.png", sep="")
  } 
  png(png_file, unit=imgunit, width=300, height=200, res=600)
  try(print(my.plot))
  dev.off()
}


my.p1 <- plot.project(1)
my.p4 <- plot.project(4)
my.p6 <- plot.project(6)
my.p9 <- plot.project(9)

my.p9

# multiplot() function is down this script ;)
my.pmulti <- multiplot(my.p1+ggtitle('JQuery'),
                       my.p4+ggtitle('Font-Awesome'),
                       my.p6+ggtitle('ReactJS'),
                       my.p9+ggtitle('Atom'), cols=2)

save.plot(my.p1, 'Size', 1) # Size for ModuleSize-Fixes plot, while Cost for Cost-Fixes plot
save.plot(my.p4, 'Size', 4)
save.plot(my.p6, 'Size', 6)
save.plot(my.p9, 'Size', 9)
# save.plot(my.plot = my.p9, type = NULL, project_id = NULL)

my.pmulti










max(my.cor.res_$Correlation)
plot(density(my.cor.res_$Correlation))
lines(df(sort(my.cor.res_$Correlation)))

as.character.da







cor_res <- c() #initial  to keep the data
for ( c_ in c(1:20)) {
  nrow_ata <- nrow(ata)
  rand_ata <- ata[sample(nrow_ata, nrow_ata*.03), ]
  
  ata.cor.test <- cor.test(
    x=rand_ata$fixesCompare,
    y=rand_ata$costCompare,
    method = "kendall",
    exact = FALSE
  )
  
  cor_res[c_] <- ata.cor.test$estimate
    print(ata.cor.test$estimate)
    print(paste("----------------", c_, "-----------------"))

}
summary(cor_res)

sort(cor_res)

plot(cor_res)
lines(cor_res)
# ata.cor.test


# Kendall's rank correlation tau
# 
# data:  rand_ata$fixesCompare and rand_ata$costCompare
# z = -32.122, p-value < 2.2e-16
# alternative hypothesis: true tau is not equal to 0
# sample estimates:
# tau 
# -0.06984939 
# 






# c(
#   count_all = 25636756,
#   count_lessthan8 = 24315500, #less than 8 bugs 
#   count_inclu8 = 24435772, #min 8 bugs
#   
#   count_incl4 = 23652992, #min 8 bugs
#   count_lessthan4 = 23028100, #less than 4 bugs
#   
#   count_lessthan2 = 17803506,
#   count_incl2 = 21522546,
#   
#   ninetypercent = 23073080, # for 90 % 4;
#   
#   count_incl2/count_all
# )

# check_at_percent(round(percent_threshold(99.9)));

















tgc <- summarySE(ata, measurevar="fixesDifference", groupvars = c('ModulePath2', 'ModulePath'))
tgc


# The errorbars overlapped, so use position_dodge to move them horizontally
pd <- position_dodge(0.1) # move them .05 to the left and right


ggplot(tgc, aes(x=ModulePath, y=fixesDifference, group=ModulePath2, colour=ModulePath2)) + 
  geom_errorbar(aes(ymin=fixesDifference-ci, ymax=fixesDifference+ci), color="black", width=.1, position=pd) +
  geom_line(position=pd) +
  geom_point(position=pd, size=3, shape=21, fill="white") +

scale_colour_hue(name="Supplement type",    # Legend label, use darker colors
                 breaks=c("OJ", "VC"),
                 labels=c("Orange juice", "Ascorbic acid"),
                 l=40) +                    # Use darker colors, lightness=40
  ggtitle("The Effect of Vitamin C on\nTooth Growth in Guinea Pigs") +
  expand_limits(y=0) +                        # Expand y range
  scale_y_continuous(breaks=0:20*4) +         # Set tick every 4
  theme_bw() +
  theme(legend.justification=c(1,0),
        legend.position=c(1,0))               # Position legend in bottom right


# plot(sd(ata$fixesDifference))
ata.cor.test
# head(ata)

# quantile(ata$fixesDifference, 0.95)

# plot(ata$fixesDifference);

# summary(ata$fixesDifference)

# boxplot(ata$fixesDifference)

# quantile(ata$fixesDifference, c(.95)) #8

boxplot(ata$fixesDifference)

curve(qt(.975,x), from = 2 , to = 100, ylab = "Quantile 0.975 ", xlab = "Degrees of freedom", main = "Student t distribution")
abline(h=qnorm(.975), col = 2)

summary(density(ata$fixesDifference))

# my.bfs <- read.csv2(paste("issuesminer_projectmoduleachurnhistory.csv", sep=";"))

View(head(my.bfs))

max(my.bfs$fixes)
# my.fil.bf <- filter(my.bfs, ProjectId==6 & ModuleLevel == 4)
# my.fil.bf$fixes_percm = my.fil.bf$fixes / sum(my.fil.bf$fixes)
# & as.Date(Date) > as.Date('2017-01-01')
ggplot(my.bfs, aes(x=fixes, y=Ids))+geom_line()
qqnorm(x);
density(my.bfs$fixes)

par(new = FALSE)
(95 /100) * max(my.fil.bf$fixes)

sd()




percent_threshold = function(percent = 90, total = 25636756) {
  return ((percent/100) * total);
}

check_at_percent = function(at_value, field = 'fixesDifference') {
  rs_ = dbSendQuery(mydb, paste("select ", field," from projectcostdifference order by fixesDifference ASC limit 1 offset ", at_value))
  return (fetch(rs_, n=-1))
}



# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
# http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}
