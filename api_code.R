#api
library(dplyr)


df = read.csv("data_tcm/all_data.csv")

table(df$natvis_vhc_1[!is.na(df$tc1)]) #bike
table(df$natvis_vhc_2[!is.na(df$tc1)])#car
table(df$natvis_vhc_3[!is.na(df$tc1)]) #motorbike
table(df$natvis_vhc_4[!is.na(df$tc1)]) #bus
table(df$natvis_vhc_5[!is.na(df$tc1)]) #train
table(df$natvis_vhc_6[!is.na(df$tc1)]) #on foot
table(df$natvis_vhc_7[!is.na(df$tc1)]) #other

#for now assume car (default) bike or on foot.
#later correct for train

df$on_foot = 0
df$on_foot = ifelse(df$natvis_vhc_6==2 & !is.na(df$natvis_vhc_6),1,0)

df$bike = 0
df$bike = ifelse(df$natvis_vhc_1==2 & !is.na(df$natvis_vhc_1),1,0)

df$car = 0
df$car = ifelse(df$natvis_vhc_2==2 & !is.na(df$natvis_vhc_2),1,0)

#older wave, thats ok
#a = temporary[is.na(df$tc1),]


data_for_api = df[!is.na(df$tc1) & !is.na(df$lon_tc),c("RID","lat", "lon","lat_tc","lon_tc", "on_foot","bike", "car")]

data_for_api$start = NA
data_for_api$end = NA

for (i in 1:length(data_for_api$RID)) {
  data_for_api$start[i] = paste(data_for_api$lat[i],data_for_api$lon[i], sep =", ")
  data_for_api$end[i] = paste(data_for_api$lat_tc[i],data_for_api$lon_tc[i], sep =", ")
}


data_for_api_on_foot = data_for_api[data_for_api$on_foot==1,c("RID","start", "end")]
data_for_api_bike = data_for_api[data_for_api$bike==1,c("RID","start", "end")]
data_for_api_car = data_for_api[(data_for_api$on_foot==0 & data_for_api$bike==0) | data_for_api$car==1,c("RID","start", "end")]

write.csv(data_for_api_on_foot, "data_tcm/for_api_walk.csv", row.names = FALSE)
write.csv(data_for_api_bike, "data_tcm/for_api_bike.csv", row.names = FALSE)
write.csv(data_for_api_car, "data_tcm/for_api_car.csv", row.names = FALSE)


#___________
#google matrix api using python
#__________
api_out_walk = read.csv("data_tcm/API_out/API_out_walk.csv")
#a = api_out_walk[api_out_walk$Distance<0,]
api_out_walk = api_out_walk[,c("RID","Distance","Time")]

api_out_bike = read.csv("data_tcm/API_out/API_out_bike.csv")
api_out_bike = api_out_bike[,c("RID","Distance","Time")]

api_out_car = read.csv("data_tcm/API_out/API_out_car.csv")
api_out_car = api_out_car[,c("RID","Distance","Time")]


data_api_out = data_for_api[,c("RID","on_foot","bike", "car")]


colnames(api_out_walk) <- c("RID", "distance_walk", "time_walk")
colnames(api_out_bike) <- c("RID", "distance_bike", "time_bike")
colnames(api_out_car) <- c("RID", "distance_car", "time_car")

data_api_out = left_join(data_api_out,api_out_walk, by = "RID")
data_api_out = left_join(data_api_out,api_out_bike, by = "RID")
data_api_out = left_join(data_api_out,api_out_car, by = "RID")

data_api_out$mean_dist = NA
data_api_out$mean_time = NA

for (i in 1:length(data_api_out$RID)) {
  data_api_out$mean_dist[i] = mean(c(data_api_out$distance_walk[i], data_api_out$distance_bike[i], data_api_out$distance_car[i]), na.rm = TRUE)
  data_api_out$mean_time[i] = mean(c(data_api_out$time_walk[i], data_api_out$time_bike[i], data_api_out$time_car[i]), na.rm = TRUE)
}

write.csv(data_api_out,"data_tcm/data_api_joined.csv", row.names = FALSE)


