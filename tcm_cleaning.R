rm(list = ls())
library(dplyr)

df = read.csv("data_tcm/all_data.csv")
data_api_out = read.csv("data_tcm/data_api_joined.csv")

colnames(df)

which(colnames(df)=="RID")

temporary = df[,c(552,193:248)]

tcm = temporary[!is.na(temporary$tc1),]
tcm_treatments = tcm[!is.na(tcm$tc_map),]

tcm_treatments = left_join(tcm_treatments,data_api_out,by="RID")
ds = tcm_treatments

ds$stated_time = ds$hours_travel+ds$minutes_travel/60
ds$estimated_time = ds$mean_time/3600

ds = ds[!is.na(ds$estimated_time),]



plot(density(ds$estimated_time))
lines(density(ds$stated_time[!is.na(ds$stated_time)]), col = "red")



plot(density(ds$estimated_time[ds$natvisit_last12m>0 & ds$estimated_time<5]), main="")
lines(density(ds$stated_time[!is.na(ds$stated_time) & ds$natvisit_last12m>0]), col = "red")
title("estimated (black) vs stated (red) travel time")
####
###
####distance
plot(density(ds$mean_dist[ds$natvisit_last12m>0]/1000), main="")

quantile(ds$mean_dist/1000, probs = 0.95, na.rm = TRUE)

plot(density(ds$mean_dist[ds$natvisit_last12m>0 & ds$mean_dist<50000]/1000), main="")

title("estimated (black) distance")

ds = ds[ds$estimated_time<5 & ds$mean_dist<50000,]
ds = ds[ds$mean_time<18000 & ds$mean_dist<50000 & ds$mean_dist>0 & !is.na(ds$mean_dist),]

#we went from
length(tcm_treatments$RID)
#to
length(ds$RID)

#dropped
length(tcm_treatments$RID) - length(ds$RID)

