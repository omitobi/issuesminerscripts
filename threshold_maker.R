setwd("/Users/OMITOBISAM/Desktop/Thesis Data Import/Progress_6_Dec")

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
rs_1 = dbSendQuery(mydb,'select count(Id) from projectcostdifference')
count_of_record = fetch(rs_1, n=-1)
count_of_record #==25636756
count_of_record <- 25636756

#------------------
mid_position = count_of_record/2
mid_position #=== 12818378

rs_2 = dbSendQuery(mydb,'select max(locDifference) from projectcostdifference order by locDifference')
max_locDiff = fetch(rs_2, n=-1)
max_locDiff #===277975
max_locDiff <- 277975
#-----------------

rs_3 = dbSendQuery(mydb,'select locDifference from projectcostdifference order by locDifference limit 1 offset 12818378')
mid_locDiff = fetch(rs_3, n=-1) #mid_locDiff
mid_locDiff #=== 62
#-----------------

rs_4 = dbSendQuery(mydb,'select count(*) from projectcostdifference where fixesDifference<62')
count_lessthanmid = fetch(rs_4, n=-1)

count_lessthanmid#==25460908
count_lessthanmid = 25460908
#----------------------------
ninety_perc = check_at_percent(round(percent_threshold(90))); #at 90_percent = 8038
#-------

ninety5_perc = check_at_percent(round(percent_threshold(95))); #at 95_percent = 25270

ninety5_perc = check_at_percent(round(percent_threshold(99))); #at 95_percent = 25270
ninety5_perc

#-------

count_lessthanmid/count_of_record

(ninety5_perc/max_locDiff) *100
#------------

#--------Experiments-------
total__ = get_total_records(); #25636756
perc__total = round(percent_threshold(85, total_record=total__))
perc__ = check_at_percent(perc__total)
perc__

#LocDifferences compare
#|--perc--|--value--|
#| .80**  |  1658   |
#-------------------|
#| .85    |  2782   |
#-------------------|
#| .90*   |  8038   |
#-------------------|
#| .95    | 25270   |
#-------------------|
#| .99    | 112858  |
#|------------------|
#| .999   | 267558  |
#|------------------|

from_value = check_from_value(value = 8038, operator = '<=')
from_value
from_value/total__
#--------/Experiments------

#--- Method ------

get_total_records = function(table ='projectcostdifference', field = 'Id') {
  rss = dbSendQuery(mydb, paste("select count(", field,") from ",table))
  return (fetch(rss, n=-1))
}

percent_threshold = function(percent = 90, total_record) {
  return ((percent/100) * total_record);
}

check_at_percent = function(at_value, field = 'locDifference') {
  rs_s = dbSendQuery(mydb, paste("select ", field," from projectcostdifference order by ",field," ASC limit 1 offset ", at_value))
  return (fetch(rs_s, n=-1))
}

check_from_value = function(value, operator = '=', field = 'locDifference') {
  rs_ss = dbSendQuery(mydb, paste("select count(Id) from projectcostdifference where ",field, " ",operator," ", value))
  return (fetch(rs_ss, n=-1))
}

#---- /Method -----