# setwd("/Users/OMITOBISAM/Desktop/Thesis Data Import") #New Project 9 Cost Churn
setwd("/Users/OMITOBISAM/Desktop/Thesis Data Import/New Projects/New Project 9 Cost Churn")

library("ggplot2")
imgres <- 600
imgwidth <- 120 #180
imgheight <- 60 #70
imgunit <- "mm"

# pids <- c(126,137,139,140,147,149,168,170)
pids <- c(9)

# my.module.data <- read.csv2("../ModuleSizeAndChurn.csv")
# my.modules <- read.csv2("../VCSModules.csv")

# for(moduleid in unique(as.character(my.module.data[,"ModuleId"]))) {
	# my.module.data[as.character(my.module.data[,"ModuleId"]) == as.character(moduleid),"ModulePath"] <- my.modules[as.character(my.modules[,"ModuleId"]) == as.character(moduleid), "Path"]
# }
# my.modulechurn <- read.csv2("ModuleChurn_utf8.csv")

min.sign.alt.cost <- 4000
sign.alt.cost <- data.frame(pid=numeric(), ModulePath=character(),ModuleLevel=numeric(),AlternativeCost=numeric())
my.data._<- read.csv2(paste("Project", 9, "ChurnEstimationsg.csv", sep=""))

View(head(my.data._))

for(pid in pids) {
	print(pid)
	# my.data.m <- read.csv2(paste("Project", pid, "ModuleChurnEstimations.csv", sep=""))
	# my.data.e <- read.csv2(paste("Project", pid, "ChurnEstimations.csv", sep=""))
	
	# my.module.data.prj <- my.module.data[my.module.data[,"ProjectId"] == pid,]
  my.data.t <- read.csv2(paste("Project", pid, "ChurnEstimationsg.csv", sep=""))
  
  my.data.m <- my.data.t[my.data.t[,"ModuleDateRevisionId"]>0, ]
  my.data.e <- my.data.t[my.data.t[,"ModuleDateRevisionId"]<0, ]
	
	my.data.m <- my.data.m[as.numeric(my.data.m[["ModuleLevel"]]) < 5,]
	my.data.m[my.data.m[["Estimation"]] < 0, "Estimation"] <- 0.0
	my.data.e[my.data.e[["Estimation"]] < 0, "Estimation"] <- 0.0
	my.data.m[["Imp"]] <- 0.0
	
	my.data.lvls <- unique(as.numeric(my.data.m[["ModuleLevel"]]))
	print(my.data.lvls)

  my.data.pid.est <- as.list(my.data.e[,"Estimation"])
  names(my.data.pid.est) <- as.character(my.data.e[,"ProjectDateRevisionId"])
  
  my.data.m[,"ProjectChurnEstimation"] <- unlist(my.data.pid.est[as.character(my.data.m[,"ProjectDateRevisionId"])])
  
  if("Date" %in% colnames(my.data.e)) {
    try(my.data.pid.est <- as.list(as.character(my.data.e[,"Date"])))
    try(names(my.data.pid.est) <- as.character(my.data.e[,"ProjectDateRevisionId"]))
    
    try(my.data.m[,"Date"] <- unlist(my.data.pid.est[as.character(my.data.m[,"ProjectDateRevisionId"])]))
  }
  
  my.data.m[,"AlternativeCost"] <- my.data.m[,"Estimation"] - my.data.m[,"ProjectChurnEstimation"]
  
  write.csv2(file=paste("Project",pid,"Data.csv", sep=""),as.data.frame(my.data.m))
  
  my.alt.cost.pid <- my.data.m[abs(my.data.m[,"AlternativeCost"]) >= min.sign.alt.cost,c("ModulePath","ModuleLevel","AlternativeCost")]
  if(nrow(my.alt.cost.pid) > 0) {
    my.alt.cost.pid[,"pid"] <- pid
    sign.alt.cost <- rbind(sign.alt.cost, my.alt.cost.pid)
  }
  
	# my.data.m[, "Imp2"] <- my.data.m[, "AlternativeCost"]
	# for(i in 1:nrow(my.data.m)){
		# try(my.data.m[i, "Imp2"] <- my.module.data.prj[as.character(my.module.data.prj[,"ModulePath"]) == as.character(my.data.m[i,"ModulePath"]) & my.module.data.prj[,"ProjectDateRevisionId"] == my.data.m[i,"ProjectDateRevisionId"],"YearlyModuleChurn"] - my.data.m[i, "Imp"])
	# }
	my.ta <- tapply(as.numeric(my.data.m[["AlternativeCost"]]), as.character(my.data.m[["ModulePath"]]), mean)
	sort(my.ta)
	write.csv2(file=paste("Project",pid,"meanAC.csv", sep=""),as.data.frame(my.ta))
	
	# my.ta <- tapply(as.numeric(my.data.m[["Imp2"]]), as.character(my.data.m[["ModulePath"]]), mean)
	# sort(my.ta)
	# write.csv2(file=paste("Project",pid,"meanImp2.csv", sep=""),as.data.frame(my.ta))
		

	print(paste(pid, "AC calculated"))
	
	for(my.data.lvl in my.data.lvls) {
		print(paste(pid, my.data.lvl))
		my.data <- my.data.m[my.data.m[["ModuleLevel"]] == my.data.lvl, ]
		
		my.ta <- tapply(as.numeric(my.data[["AlternativeCost"]]), as.character(my.data[["ModulePath"]]), mean)
		sort(my.ta)
		write.csv2(file=paste("Project",pid,"meanACL",my.data.lvl,".csv", sep=""),as.data.frame(my.ta))
		
		# my.ta <- tapply(as.numeric(my.data[["Imp2"]]), as.character(my.data[["ModulePath"]]), mean)
		# sort(my.ta)
		# write.csv2(file=paste("Project",pid,"meanImp2L",my.data.lvl,".csv", sep=""),as.data.frame(my.ta))
		
		png(paste("Project", pid, "ModuleAChurnLevel", my.data.lvl, ".png", sep=""), unit=imgunit, width=imgwidth, height=imgheight*2, res=imgres)
		my.p <- ggplot(my.data, aes(x=ModulePath, y=AlternativeCost)) + ylab("Alternative Churn Cost") + theme_bw() + theme(title=element_text(size=9), text=element_text(size=8))
		print(my.p + geom_jitter(scale="width") + theme(axis.text.x=element_text(angle=90)))
		dev.off()
		
    png(paste("Project", pid, "ModuleAChurnLevelX-X", my.data.lvl, ".png", sep=""), unit=imgunit, width=imgwidth, height=imgheight*2, res=imgres)
		my.p <- ggplot(my.data, aes(x=ModulePath, y=AlternativeCost, fill=ModuleLevel)) + ylab("Alternative Churn Cost") + theme_bw() + theme(title=element_text(size=9), text=element_text(size=8))
		print(my.p + geom_jitter() + scale_x_discrete(limits=rev(sort(unique(as.character(my.data[,"ModulePath"]))))) + theme(axis.text.y=element_text(hjust=0), legend.position="none") + coord_flip())
		dev.off()
    
    # png(paste("Project", pid, "ModuleAChurnLevel", my.data.lvl, "History.png", sep=""), unit=imgunit, width=imgwidth, height=imgheight, res=imgres)
		# my.p <- ggplot(my.data, aes(x=Date, colour=ModulePath, y=Estimation)) + theme_bw() + theme(title=element_text(size=9), text=element_text(size=8))
		# try(print(my.p + geom_line() + theme(axis.text.x=element_text(angle=90))))
		# dev.off()
    
		
		
    if("Date" %in% colnames(my.data)) {
      # print(my.data[,"Date"])
      my.data[,"Date"] <- as.Date(my.data[,"Date"])
      
      my.ddata <- my.data
      my.ddata[, "Date"] <- format(as.Date(my.ddata[,"Date"]), "%Y-%m-%d")
      
      my.dt <- as.data.frame(my.ddata[,c("Date","ModulePath","ModuleLevel","AlternativeCost")])
      my.dtframe <- aggregate(AlternativeCost ~ Date+ModulePath+ModuleLevel, my.dt, sum)
      # my.dt = my.data[["ModulePath",["AlternativeCost"]];
      write.csv2(file=paste("Project",pid,"ModuleAChurnLevel",my.data.lvl,"History.csv", sep=""),my.dtframe)
      
      png(paste("Project", pid, "ModuleAChurnLevel", my.data.lvl, "History.png", sep=""), unit=imgunit, width=300, height=200, res=imgres)
      my.p <- ggplot(my.data, aes(x=Date, colour=ModulePath, y=AlternativeCost)) + theme_bw() + theme(title=element_text(size=9), text=element_text(size=8))
      try(print(my.p + geom_step() + theme(axis.text.x=element_text(angle=90))))
      dev.off()
		}
	}
}

write.csv2(file=paste("SignificantAC.csv", sep=""),sign.alt.cost)
    