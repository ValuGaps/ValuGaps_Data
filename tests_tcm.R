#tests

wilcox_print <- function(ds1,ds2, alt ="t", dg_rank=3) {
  a = round(mean(ds1), digits = 3)
  a_s = round(sd(ds1), digits = 3)
  a_n = length(ds1)
  b = round(mean(ds2), digits = 3)
  b_s = round(sd(ds2), digits = 3)
  b_n = length(ds2)
  c = round(as.numeric(wilcox.test(ds1,ds2, alternative=alt, digits.rank = dg_rank)['p.value']), digits = 3)
  return(paste(a," (",a_s,"); "," N=",a_n, "; ",b," (",b_s,"); ","N=",b_n, "; ",c, sep = ""))
}

wilcox_print(ds$mean_dist[sub_sample_1]/1000,ds$mean_dist[sub_sample_2]/1000)

## explore distance
#map
sub_sample_1 = ds$mean_dist[ds$tc_map==1]/1000
sub_sample_2 = ds$mean_dist[ds$tc_map==2]/1000
wilcox_print(sub_sample_1,sub_sample_2)

#### T

sub_sample_1 = ds$mean_dist[which(ds$tc_map==1 & ds$tc_timeline==2 & ds$tc_calc==2 & ds$tc_avg==2)]/1000
sub_sample_2 = ds$mean_dist[which(ds$tc_map==2 & ds$tc_timeline==2 & ds$tc_calc==2 & ds$tc_avg==2)]/1000
wilcox_print(sub_sample_1,sub_sample_2)

sub_sample_1 = ds$mean_dist[which(ds$tc_map==1 & ds$tc_timeline==1 & ds$tc_calc==2 & ds$tc_avg==2)]/1000
sub_sample_2 = ds$mean_dist[which(ds$tc_map==2 & ds$tc_timeline==1 & ds$tc_calc==2 & ds$tc_avg==2)]/1000
wilcox_print(sub_sample_1,sub_sample_2)

sub_sample_1 = ds$mean_dist[which(ds$tc_map==1 & ds$tc_timeline==2 & ds$tc_calc==1 & ds$tc_avg==2)]/1000
sub_sample_2 = ds$mean_dist[which(ds$tc_map==2 & ds$tc_timeline==2 & ds$tc_calc==1 & ds$tc_avg==2)]/1000
wilcox_print(sub_sample_1,sub_sample_2)

sub_sample_1 = ds$mean_dist[which(ds$tc_map==1 & ds$tc_timeline==2 & ds$tc_calc==2 & ds$tc_avg==1)]/1000
sub_sample_2 = ds$mean_dist[which(ds$tc_map==2 & ds$tc_timeline==2 & ds$tc_calc==2 & ds$tc_avg==1)]/1000
wilcox_print(sub_sample_1,sub_sample_2)

sub_sample_1 = ds$mean_dist[which(ds$tc_map==1 & ds$tc_timeline==1 & ds$tc_calc==1 & ds$tc_avg==2)]/1000
sub_sample_2 = ds$mean_dist[which(ds$tc_map==2 & ds$tc_timeline==1 & ds$tc_calc==1 & ds$tc_avg==2)]/1000
wilcox_print(sub_sample_1,sub_sample_2)

sub_sample_1 = ds$mean_dist[which(ds$tc_map==1 & ds$tc_timeline==2 & ds$tc_calc==1 & ds$tc_avg==1)]/1000
sub_sample_2 = ds$mean_dist[which(ds$tc_map==2 & ds$tc_timeline==2 & ds$tc_calc==1 & ds$tc_avg==1)]/1000
wilcox_print(sub_sample_1,sub_sample_2)

sub_sample_1 = ds$mean_dist[which(ds$tc_map==1 & ds$tc_timeline==1 & ds$tc_calc==2 & ds$tc_avg==1)]/1000
sub_sample_2 = ds$mean_dist[which(ds$tc_map==2 & ds$tc_timeline==1 & ds$tc_calc==2 & ds$tc_avg==1)]/1000
wilcox_print(sub_sample_1,sub_sample_2)

sub_sample_1 = ds$mean_dist[which(ds$tc_map==1 & ds$tc_timeline==1 & ds$tc_calc==1 & ds$tc_avg==1)]/1000
sub_sample_2 = ds$mean_dist[which(ds$tc_map==2 & ds$tc_timeline==1 & ds$tc_calc==1 & ds$tc_avg==1)]/1000
wilcox_print(sub_sample_1,sub_sample_2)
####

## explore distance
sub_sample_1 = ds$mean_dist[ds$tc_timeline==1]/1000
sub_sample_2 = ds$mean_dist[ds$tc_timeline==2]/1000
wilcox_print(sub_sample_1,sub_sample_2)

sub_sample_1 = ds$mean_dist[ds$tc_calc==1]/1000
sub_sample_2 = ds$mean_dist[ds$tc_calc==2]/1000
wilcox_print(sub_sample_1,sub_sample_2)

sub_sample_1 = ds$mean_dist[ds$tc_avg==1]/1000
sub_sample_2 = ds$mean_dist[ds$tc_avg==2]/1000
wilcox_print(sub_sample_1,sub_sample_2)

#______
sub_sample_1 = ds$mean_dist[ds$tc_avg==1]/1000
sub_sample_2 = ds$mean_dist[ds$tc_avg==2]/1000
wilcox_print(sub_sample_1,sub_sample_2)

###
#explore number of trips
#skip map
sub_sample_1 = ds$natvisit_last12m[ds$tc_timeline==1]
sub_sample_2 = ds$natvisit_last12m[ds$tc_timeline==2]
wilcox_print(sub_sample_1,sub_sample_2)

sub_sample_1 = ds$natvisit_last12m[ds$tc_calc==1]
sub_sample_2 = ds$natvisit_last12m[ds$tc_calc==2]
wilcox_print(sub_sample_1,sub_sample_2)

sub_sample_1 = ds$natvisit_last12m[ds$tc_avg==1]
sub_sample_2 = ds$natvisit_last12m[ds$tc_avg==2]
wilcox_print(sub_sample_1,sub_sample_2)

sub_sample_1 = ds$natvisit_last12m[ds$tc_timeline==1 & ds$tc_calc==1]
sub_sample_2 = ds$natvisit_last12m[ds$tc_timeline==2 & ds$tc_calc==1]
wilcox_print(sub_sample_1,sub_sample_2)

sub_sample_1 = ds$natvisit_last12m[ds$tc_timeline==1 & ds$tc_calc==2]
sub_sample_2 = ds$natvisit_last12m[ds$tc_timeline==2 & ds$tc_calc==2]
wilcox_print(sub_sample_1,sub_sample_2)

sub_sample_1 = ds$natvisit_last12m[ds$tc_calc==1 & ds$tc_timeline==1]
sub_sample_2 = ds$natvisit_last12m[ds$tc_calc==2 & ds$tc_timeline==1]
wilcox_print(sub_sample_1,sub_sample_2)

sub_sample_1 = ds$natvisit_last12m[ds$tc_calc==1 & ds$tc_timeline==2]
sub_sample_2 = ds$natvisit_last12m[ds$tc_calc==2 & ds$tc_timeline==2]
wilcox_print(sub_sample_1,sub_sample_2)

###fav trips
#explore number of trips
sub_sample_1 = ds$natvis_fav_fin[ds$tc_map==1 & !is.na(ds$natvis_fav_fin)]
sub_sample_2 = ds$natvis_fav_fin[ds$tc_map==2]
wilcox_print(sub_sample_1,sub_sample_2)

sub_sample_1 = ds$natvis_fav_fin[ds$tc_timeline==1 & !is.na(ds$natvis_fav_fin)]
sub_sample_2 = ds$natvis_fav_fin[ds$tc_timeline==2 & !is.na(ds$natvis_fav_fin)]
wilcox_print(sub_sample_1,sub_sample_2)

sub_sample_1 = ds$natvisit_last12m[ds$tc_calc==1& !is.na(ds$natvis_fav_fin)]
sub_sample_2 = ds$natvisit_last12m[ds$tc_calc==2& !is.na(ds$natvis_fav_fin)]
wilcox_print(sub_sample_1,sub_sample_2)

sub_sample_1 = ds$natvisit_last12m[ds$tc_avg==1& !is.na(ds$natvis_fav_fin)]
sub_sample_2 = ds$natvisit_last12m[ds$tc_avg==2& !is.na(ds$natvis_fav_fin)]
wilcox_print(sub_sample_1,sub_sample_2)

#_____
#
CrossTable(ds$changed_12m,ds$tc_map, prop.t=FALSE, prop.r=F, chisq=T, prop.chisq=F)
CrossTable(ds$changed_12m,ds$tc_timeline, prop.t=FALSE, prop.r=F, chisq=T, prop.chisq=F)
CrossTable(ds$changed_12m,ds$tc_calc, prop.t=FALSE, prop.r=F, chisq=T, prop.chisq=F)
CrossTable(ds$changed_12m,ds$tc_avg, prop.t=FALSE, prop.r=F, chisq=T, prop.chisq=F)
###

ds$time_on_site = ds$hours_spend + ds$minutes_spend/60

sub_sample_1 = ds$time_on_site[ds$tc_map==1& !is.na(ds$time_on_site)]
sub_sample_2 = ds$time_on_site[ds$tc_map==2& !is.na(ds$time_on_site)]
wilcox_print(sub_sample_1,sub_sample_2)

sub_sample_1 = ds$time_on_site[ds$tc_timeline==1& !is.na(ds$time_on_site)]
sub_sample_2 = ds$time_on_site[ds$tc_timeline==2& !is.na(ds$time_on_site)]
wilcox_print(sub_sample_1,sub_sample_2)

sub_sample_1 = ds$time_on_site[ds$tc_calc==1& !is.na(ds$time_on_site)]
sub_sample_2 = ds$time_on_site[ds$tc_calc==2& !is.na(ds$time_on_site)]
wilcox_print(sub_sample_1,sub_sample_2)

sub_sample_1 = ds$time_on_site[ds$tc_avg==1& !is.na(ds$time_on_site)]
sub_sample_2 = ds$time_on_site[ds$tc_avg==2& !is.na(ds$time_on_site)]
wilcox_print(sub_sample_1,sub_sample_2)


sub_sample_1 = ds$natvisit_next12m[ds$tc_avg==1& !is.na(ds$natvisit_next12m)]
sub_sample_2 = ds$natvisit_next12m[ds$tc_avg==2& !is.na(ds$natvisit_next12m)]
wilcox_print(sub_sample_1,sub_sample_2)
