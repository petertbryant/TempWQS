library(chron)
library(reshape)
library(plyr)
library(zoo)
load('example_df.Rdata')
source('funCalculateSDADM.R')
source('funEvaluateTempWQS.R')
source('funPlotTempWQS.R')

options(stringsAsFactors = FALSE)

sdadm <- Calculate.sdadm(df = df.all, 
                         result_column_name = "Result", 
                         station_column_name =  "Station_ID", 
                         datetime_column_name = "Sampled", 
                         datetime_format = "%Y-%m-%d %H:%M:%S")

spwn <- data.frame(id = unique(sdadm$id), 
                   spwn_dates = c(rep('August 15-June 15',9)), 
                   ben_use_des = c(rep('Core Cold Water Habitat',5),
                                   rep('No Salmonid Use/Out of State',4)))

sdadm <- merge(sdadm, spwn, by = 'id', all.x = TRUE)

sdadm_evaluated <- EvaluateTempWQS(sdadm)

attr(sdadm_evaluated, 'result_summary')

sdadm_one_station <- sdadm_evaluated[sdadm_evaluated$id == 36195,]

plotTempWQS(sdadm_one_station, df.all = df.all)
