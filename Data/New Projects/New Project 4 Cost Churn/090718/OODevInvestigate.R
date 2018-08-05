# ['.cpp', '.cs', '.php', '.java', '.cxx', '.hpp','.js','.d', '.fs', '.vb', '.ts', '.py'];

setwd("/Users/omitobisam/Desktop/Thesis Data Import/New Projects/New Project 4 Cost Churn/090718")

library('dplyr')
library('ggplot2')
library(RMySQL)

#------------------------------------
#               Examples
#---------------------------------------------------------------------------------
# by fixesDifference ASC limit 1 offset 24354918

# rs = dbSendQuery(mydb, "select count(*) from projectcostdifference where fixesDifference <= 2")

#---------------------------------------------------


# -------------------------
mydb = dbConnect(MySQL(), user='root', password='Sch..l1234', dbname='issuesminer', host='localhost')
# -------------------------
pid_ = 1;

#--------------------------
rs_1 = dbSendQuery(mydb,"select 
                   Id,
                   ProjectId,
                   Date,
                   Alias,
                   ProjectLOC,
                   CommitterId,
                   Extension,
                   CommitId,
                   AuthorEmail,
                   AddedCodeLines,
                   RemovedCodeLines,
                   LinesOfCode,
                   status from VCSFileRevision where projectId = 4"
                   # AND
                   # Extension IN ('.cpp', '.cs', '.php', '.java', '.cxx', '.hpp','.js','.d', '.fs', '.vb', '.ts', '.py')
                   )
count_of_record = fetch(rs_1, n=-1)
count_by_date = count_of_record[order(count_of_record$Date), ]
head(count_of_record)
developers <- unique(count_of_record$AuthorEmail)
developers
ggplot(count_by_date, aes(y=LinesOfCode, x=Date))+geom_point()
#------------------

#---- /Method -----