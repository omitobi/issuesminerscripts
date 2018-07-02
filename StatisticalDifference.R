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

my.bfreqchurns <- read.csv2(paste("issuesminer_projectmoduleachurnhistory.csv", sep=";"))
View(head(my.bfreqchurns))

proj_ids <- c(1) #,1,6,9
mod_levs <- c(3) #,3,4

my.statdiff.result <- data.frame() #initial datafrome to keep the data

for (proj_id in proj_ids)
{
  for (mod_lev in mod_levs)
  {
    project.by.level <- filter(my.bfreqchurns, ProjectId == proj_id & ModuleLevel == mod_lev)
    proj.ord.dates <- project.by.level[order(project.by.level$Date),]
    cdates <- unique(proj.ord.dates$Date)
    proj.date.mods <- unique(proj.ord.dates$ModulePath)
    
    for(cnt in 1:length(cdates))
    {
      for (cnt.mod in 1:length(proj.date.mods))
      {
        proj.date.mods2 <- proj.date.mods[-cnt.mod]
        proj.wo.mod <- filter(proj.ord.dates, ModulePath == proj.date.mods[cnt.mod] & Date == cdates[cnt])
        
        if ( NROW(proj.wo.mod) == 1) {
  
        # setdiff(proj.date.mods,  proj.date.mods2)
        # print(paste('at mods count...', proj.date.mods[cnt.mod], 'mod_path ', proj.wo.mod))
        
          for (cnt.mod2 in 1:length(proj.date.mods2))
          {
            proj.other.mod <- filter(proj.ord.dates, ModulePath == proj.date.mods2[cnt.mod2] & Date == cdates[cnt])
            if (NROW(proj.other.mod) == 1) {
            
              my.statdiff.result[nrow(my.statdiff.result)+1, "ProjectId"] <- proj_id
              my.statdiff.result[nrow(my.statdiff.result), "ModuleLevel"] <- mod_lev
              
              my.statdiff.result[nrow(my.statdiff.result), "ModulePath"] <- proj.wo.mod$ModulePath
              
              # next;
              my.statdiff.result[nrow(my.statdiff.result), "ModulePath2"] <- proj.other.mod$ModulePath
              
              my.statdiff.result[nrow(my.statdiff.result), "Date"] <- cdates[cnt]
              my.statdiff.result[nrow(my.statdiff.result), "AlternativeCost"] <- proj.wo.mod$AlternativeCost
              my.statdiff.result[nrow(my.statdiff.result), "AlternativeCost2"] <- proj.other.mod$AlternativeCost
    
              #finding Absolute difference of these two modules
              my.statdiff.result[nrow(my.statdiff.result), "AlternativeCostDiff"] <- abs(proj.wo.mod$AlternativeCost - proj.other.mod$AlternativeCost)
              my.statdiff.result[nrow(my.statdiff.result), "loc"] <- proj.wo.mod$loc
              my.statdiff.result[nrow(my.statdiff.result), "loc2"] <- proj.other.mod$loc
            } #endif
  
          }
        } #if ends
        print(paste("Project: ", proj_id, " level: ", mod_lev, " date:count: ", cnt, " is: ", cdates[cnt], " module: ", proj.date.mods[cnt.mod]))
        # break;
      }
      # break;
    }
    #break;
  }
  
}

# __---------------__>
# The last is:

# [1] "Project:  1  level:  3  date:count:  477  is:  2016-01-25  module:  jquery/build/build/"
# [1] "Project:  1  level:  3  date:count:  477  is:  2016-01-25  module:  jquery/build/docs/"
# [1] "Project:  1  level:  3  date:count:  477  is:  2016-01-25  module:  jquery/build/js/"
# [1] "Project:  1  level:  3  date:count:  477  is:  2016-01-25  module:  jquery/build/lib/"
# [1] "Project:  1  level:  3  date:count:  477  is:  2016-01-25  module:  jquery/build/runtest/"
# [1] "Project:  1  level:  3  date:count:  477  is:  2016-01-25  module:  jquery/build/speed/"
# [1] "Project:  1  level:  3  date:count:  477  is:  2016-01-25  module:  jquery/build/test/"
# [1] "Project:  1  level:  3  date:count:  477  is:  2016-01-25  module:  jquery/docs/build/"

# __---------------__>

write.csv2(x=my.statdiff.result, file = 'StatDiffProject1Level3.csv')
# View(my.statdiff.result)
# write.csv2(x=my.cor.result, file = 'CorResultProjectCostFixesCorr.csv')
