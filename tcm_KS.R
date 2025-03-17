#TCM_analysis
library(dplyr)
library(gmodels)
#write.csv(all_data, "data_tcm/all_data.csv")

df = read.csv("data_tcm/all_data.csv")
data_api_out = read.csv("data_tcm/data_api_joined.csv")

colnames(df)


which(colnames(df)=="RID")

temporary = df[,c(552,193:248)]

tcm = temporary[!is.na(temporary$tc1),]
tcm_treatments = tcm[!is.na(tcm$tc_map),]

tcm_treatments = left_join(tcm_treatments,data_api_out,by="RID")
ds = tcm_treatments

ds = ds[ds$mean_time<18000 & ds$mean_dist<50000 & ds$mean_dist>0 & !is.na(ds$mean_dist),]

#### map
wilcox.test(ds$mean_dist[ds$tc_map==1]/1000,ds$mean_dist[ds$tc_map==2]/1000, paired = FALSE, alternative = "t", digits.rank=3)
wilcox.test(ds$mean_dist[ds$tc_map==1]/1000,ds$mean_dist[ds$tc_map==2]/1000, paired = FALSE, alternative = "l", digits.rank=3)

mean(ds$mean_dist[ds$tc_map==1]/1000, na.rm = TRUE)
mean(ds$mean_dist[ds$tc_map==2]/1000, na.rm = TRUE)
length(ds$mean_dist[ds$tc_map==1 & !is.na(ds$mean_dist)]/1000)
length(ds$mean_dist[ds$tc_map==2 & !is.na(ds$mean_dist)]/1000)

#sign. difference

wilcox.test(ds$mean_time[ds$tc_map==1]/3600,ds$mean_time[ds$tc_map==2]/3600, paired = FALSE, alternative = "t", digits.rank=3)
wilcox.test(ds$mean_time[ds$tc_map==1]/3600,ds$mean_time[ds$tc_map==2]/3600, paired = FALSE, alternative = "l", digits.rank=3)
mean(ds$mean_time[ds$tc_map==1]/3600, na.rm = TRUE)
mean(ds$mean_time[ds$tc_map==2]/3600, na.rm = TRUE)

#signif difference

### compare with stated distance
ds$stated_time = ds$hours_travel+ds$minutes_travel/60

wilcox.test(ds$stated_time[ds$tc_map==1]/3600,ds$stated_time[ds$tc_map==2]/3600, paired = FALSE, alternative = "t", digits.rank=3)
wilcox.test(ds$stated_time[ds$tc_map==1]/3600,ds$stated_time[ds$tc_map==2]/3600, paired = FALSE, alternative = "l", digits.rank=3)
#still holds!
#signif difference - but  probs omit stated for the paper
########

ds$natvis_fav_og = ds$natvisit_fav
ds$natvis_fav_fin =  ds$natvisit_fav1
ds$natvis_fav_fin = ifelse(is.na(ds$natvis_fav_fin), ds$natvisit_fav2, ds$natvis_fav_fin)

ds$changed_12m = ifelse(ds$natvisit_last12m==ds$natvisit_last12m_og,0,1)
ds$changed_fav = ifelse(ds$natvis_fav_fin==ds$natvis_fav_og,0,1)


#do people correct their responses?
wilcox.test(ds$natvisit_last12m_og, ds$natvisit_last12m, paired = TRUE, alternative = "t")
wilcox.test(ds$natvisit_last12m_og, ds$natvisit_last12m, paired = TRUE, alternative = "l")
#yes

wilcox.test(ds$natvis_fav_og, ds$natvis_fav_fin, paired = TRUE, alternative = "t")
wilcox.test(ds$natvis_fav_og, ds$natvis_fav_fin, paired = TRUE, alternative = "l")
#yes

#do people correct their responses no matter the treatment?
#do a model? logit?
wilcox.test(ds$natvisit_last12m_og[ds$tc_avg==1], ds$natvisit_last12m[ds$tc_avg==1], paired = TRUE, alternative = "l")
wilcox.test(ds$natvisit_last12m_og[ds$tc_avg==2], ds$natvisit_last12m[ds$tc_avg==2], paired = TRUE, alternative = "l")

#explore impact on intial response all visits (so we skip map & avg)
wilcox.test(ds$natvisit_last12m_og[ds$tc_timeline==1], ds$natvisit_last12m_og[ds$tc_timeline==2], paired = FALSE, alternative = "t")
wilcox.test(ds$natvisit_last12m_og[ds$tc_timeline==1], ds$natvisit_last12m_og[ds$tc_timeline==2], paired = FALSE, alternative = "l")

wilcox.test(ds$natvisit_last12m_og[ds$tc_timeline==1 & ds$tc_calc==2], ds$natvisit_last12m_og[ds$tc_timeline==2 & ds$tc_calc==2], paired = FALSE, alternative = "t")

#no evidence for timeline

wilcox.test(ds$natvisit_last12m_og[ds$tc_calc==1], ds$natvisit_last12m_og[ds$tc_calc==2], paired = FALSE, alternative = "t")
wilcox.test(ds$natvisit_last12m_og[ds$tc_calc==1], ds$natvisit_last12m_og[ds$tc_calc==2], paired = FALSE, alternative = "g")

#explore avg on initial vs corrected response
wilcox.test(ds$natvisit_last12m_og[ds$tc_avg==1], ds$natvisit_last12m[ds$tc_avg==1], paired = TRUE, alternative = "l")
wilcox.test(ds$natvisit_last12m_og[ds$tc_avg==2], ds$natvisit_last12m[ds$tc_avg==2], paired = TRUE, alternative = "l")

#
wilcox.test(ds$natvisit_last12m[ds$tc_timeline==1 & ds$tc_avg==1], ds$natvisit_last12m[ds$tc_timeline==2 & ds$tc_avg==1], paired = FALSE, alternative = "t")
wilcox.test(ds$natvisit_last12m[ds$tc_timeline==1 & ds$tc_avg==2], ds$natvisit_last12m[ds$tc_timeline==2 & ds$tc_avg==2], paired = FALSE, alternative = "t")

wilcox.test(ds$natvisit_last12m[ds$tc_timeline==1], ds$natvisit_last12m[ds$tc_timeline==2], paired = FALSE, alternative = "t")
wilcox.test(ds$natvisit_last12m[ds$tc_timeline==1], ds$natvisit_last12m[ds$tc_timeline==2], paired = FALSE, alternative = "l")

wilcox.test(ds$natvisit_last12m[ds$tc_calc==1], ds$natvisit_last12m[ds$tc_calc==2], paired = FALSE, alternative = "t")
wilcox.test(ds$natvisit_last12m[ds$tc_calc==1], ds$natvisit_last12m[ds$tc_calc==2], paired = FALSE, alternative = "g")

#######
CrossTable(ds$nv_2a==3,ds$tc_timeline, prop.t=FALSE, prop.r=F, chisq=T, prop.chisq=F)
CrossTable(ds$nv_2a==2,ds$tc_calc, prop.t=FALSE, prop.r=F, chisq=T, prop.chisq=F)


#######
CrossTable(ds$nv_4a==3,ds$tc_timeline, prop.t=FALSE, prop.r=F, chisq=T, prop.chisq=F)
CrossTable(ds$nv_4a==2,ds$tc_calc, prop.t=FALSE, prop.r=F, chisq=T, prop.chisq=F)
CrossTable(ds$nv_4a,ds$tc_map, prop.t=FALSE, prop.r=F, chisq=T, prop.chisq=F)
######
#explore impact on intial response favourite visits (so we skip avg)

wilcox.test(ds$natvis_fav_og[ds$tc_map==1], ds$natvis_fav_og[ds$tc_map==2], paired = FALSE, alternative = "t")
wilcox.test(ds$natvis_fav_fin[ds$tc_map==1], ds$natvis_fav_fin[ds$tc_map==2], paired = FALSE, alternative = "t")

wilcox.test(ds$natvis_fav_og[ds$tc_map==1 & ds$tc_timeline==2 & ds$tc_calc==2], ds$natvis_fav_og[ds$tc_map==2& ds$tc_timeline==2& ds$tc_calc==2], paired = FALSE, alternative = "t")
wilcox.test(ds$natvis_fav_fin[ds$tc_map==1& ds$tc_timeline==2& ds$tc_calc==2], ds$natvis_fav_fin[ds$tc_map==2& ds$tc_timeline==2& ds$tc_calc==2], paired = FALSE, alternative = "t")

wilcox.test(ds$natvis_fav_og[ds$tc_timeline==1 & ds$tc_map==2 ], ds$natvis_fav_og[ds$tc_timeline==2& ds$tc_map==2 ], paired = FALSE, alternative = "t")
wilcox.test(ds$natvis_fav_fin[ds$tc_timeline==1& ds$tc_map==2 ], ds$natvis_fav_fin[ds$tc_timeline==2& ds$tc_map==2 ], paired = FALSE, alternative = "t")

wilcox.test(ds$natvis_fav_og[ds$tc_calc==1 ], ds$natvis_fav_og[ds$tc_calc==2], paired = FALSE, alternative = "t")
wilcox.test(ds$natvis_fav_fin[ds$tc_calc==1], ds$natvis_fav_fin[ds$tc_calc==2], paired = FALSE, alternative = "t")


#######
CrossTable(ds$nv_4a,ds$dog==1, prop.t=FALSE, prop.r=F, chisq=T, prop.chisq=F)
wilcox.test(ds$natvis_fav_og[ds$dog==1 ], ds$natvis_fav_og[ds$dog!=1], paired = FALSE, alternative = "g")
wilcox.test(ds$natvis_fav_og[ds$dog==1 ], ds$natvis_fav_og[ds$dog!=1], paired = FALSE, alternative = "g")

#####
CrossTable(ds$changed_12m,ds$nv_4a, prop.t=FALSE, prop.r=F, chisq=T, prop.chisq=F)
CrossTable(ds$changed_12m,ds$nv_4a==4, prop.t=FALSE, prop.r=F, chisq=T, prop.chisq=F)
CrossTable(ds$changed_12m,ds$nv_4a==3, prop.t=FALSE, prop.r=F, chisq=T, prop.chisq=F)

CrossTable(ds$nv_4a,ds$changed_fav, prop.t=FALSE, prop.r=F, chisq=T, prop.chisq=F)
#small sample for fav, so skip for now

#CrossTable(ds$tc_map,ds$changed_12m, prop.t=FALSE, prop.r=F, chisq=T, prop.chisq=F) no sense
CrossTable(ds$changed_fav,ds$tc_map, prop.t=FALSE, prop.r=F, chisq=T, prop.chisq=F)
#no

CrossTable(ds$changed_12m,ds$tc_timeline, prop.t=FALSE, prop.r=F, chisq=T, prop.chisq=F)

CrossTable(ds$changed_12m,ds$tc_calc, prop.t=FALSE, prop.r=F, chisq=T, prop.chisq=F)

CrossTable(ds$changed_12m,ds$tc_avg,prop.t=FALSE, prop.r=F, chisq=T, prop.chisq=F)

######
library(MASS)

ds$map= abs(ds$tc_map-2)
ds$timeline= abs(ds$tc_timeline-2)
ds$calc= abs(ds$tc_calc-2)
ds$avg= abs(ds$tc_avg-2)

summary(m1 <- glm.nb(natvis_fav_fin ~ mean_dist + map *timeline *calc + avg , data = ds))

summary(m1 <- glm.nb(natvisit_last12m ~ mean_dist + timeline *calc + avg , data = ds))

ds$me
#####

table(ds$nv_2a)

#####
#treatments summary
table(tcm$tc_map)
table(tcm$tc_timeline)
table(tcm$tc_calc)
table(tcm$tc_avg)

table(tcm$tc_calc,tcm$tc_avg) #ok!




###################### messy code below ####################
#idea
#funkcja wybierajaca subsety?

#hypoteses exploration - easy
wilcox.test(tcm$natvisit_last12m_og, tcm$natvisit_last12m, paired = TRUE, alternative = "t")
wilcox.test(tcm$natvisit_last12m_og, tcm$natvisit_last12m, paired = TRUE, alternative = "l")

wilcox.test(tcm$natvis_fav_og, tcm$natvis_fav_fin, paired = TRUE, alternative = "t")
wilcox.test(tcm$natvis_fav_og, tcm$natvis_fav_fin, paired = TRUE, alternative = "l")

colnames(tcm)
#treatment analysis - aggregated
#map
wilcox.test(tcm$natvisit_last12m[tcm$tc_map==1],tcm$natvisit_last12m[tcm$tc_map==2], paired = FALSE, alternative = "t")
wilcox.test(tcm$natvisit_last12m_og[tcm$tc_map==1],tcm$natvisit_last12m_og[tcm$tc_map==2], paired = FALSE, alternative = "t")

wilcox.test(tcm$natvis_fav_fin[tcm$tc_map==1],
            tcm$natvis_fav_fin[tcm$tc_map==2], paired = FALSE, alternative = "t")



wilcox.test(tcm$natvis_fav_fin[tcm$tc_map==1],
            tcm$natvis_fav_fin[tcm$tc_map==2], paired = FALSE, alternative = "l")

wilcox.test(tcm$natvis_fav_og[tcm$tc_map==1],tcm$natvis_fav_og[tcm$tc_map==2], paired = FALSE, alternative = "t")
wilcox.test(tcm$natvis_fav_og[tcm$tc_map==1],tcm$natvis_fav_og[tcm$tc_map==2], paired = FALSE, alternative = "l")




#________
#timeline
wilcox.test(tcm$natvisit_last12m[tcm$tc_timeline==1],
            tcm$natvisit_last12m[tcm$tc_timeline==2], paired = FALSE, alternative = "t")
wilcox.test(tcm$natvisit_last12m_og[tcm$tc_timeline==1],
            tcm$natvisit_last12m_og[tcm$tc_timeline==2], paired = FALSE, alternative = "t")

wilcox.test(tcm$natvis_fav_fin[tcm$tc_timeline==1],
            tcm$natvis_fav_fin[tcm$tc_timeline==2], paired = FALSE, alternative = "t")

wilcox.test(tcm$natvis_fav_fin[tcm$tc_timeline==1],
            tcm$natvis_fav_fin[tcm$tc_timeline==2], paired = FALSE, alternative = "g")


wilcox.test(tcm$natvis_fav_og[tcm$tc_timeline==1],tcm$natvis_fav_og[tcm$tc_timeline==2], paired = FALSE, alternative = "t")
wilcox.test(tcm$natvis_fav_og[tcm$tc_timeline==1],tcm$natvis_fav_og[tcm$tc_timeline==2], paired = FALSE, alternative = "g")


#more complex
wilcox.test(tcm$natvis_fav_fin[tcm$tc_timeline==1 & tcm$tc_map==1],
            tcm$natvis_fav_fin[tcm$tc_timeline==2 & tcm$tc_map==1], paired = FALSE, alternative = "t")

wilcox.test(tcm$natvis_fav_fin[tcm$tc_timeline==1 & tcm$tc_map==2],
            tcm$natvis_fav_fin[tcm$tc_timeline==2 & tcm$tc_map==2], paired = FALSE, alternative = "t")

wilcox.test(tcm$natvis_fav_fin[tcm$tc_map==1 & tcm$tc_timeline==1],
            tcm$natvis_fav_fin[tcm$tc_map==2 & tcm$tc_timeline==1], paired = FALSE, alternative = "t")

wilcox.test(tcm$natvis_fav_fin[tcm$tc_map==1 & tcm$tc_timeline==2],
            tcm$natvis_fav_fin[tcm$tc_map==2 & tcm$tc_timeline==2], paired = FALSE, alternative = "t")


wilcox.test(tcm$natvis_fav_fin[tcm$tc_map==1 & tcm$tc_timeline==2],
            tcm$natvis_fav_fin[tcm$tc_map==2 & tcm$tc_timeline==2], paired = FALSE, alternative = "l")


#________


wilcox.test(tcm$natvisit_last12m[tcm$tc_calc==1],
            tcm$natvisit_last12m[tcm$tc_calc==2], paired = FALSE, alternative = "t")
wilcox.test(tcm$natvisit_last12m[tcm$tc_calc==1],
            tcm$natvisit_last12m[tcm$tc_calc==2], paired = FALSE, alternative = "g")

wilcox.test(tcm$natvisit_last12m_og[tcm$tc_calc==1],tcm$natvisit_last12m_og[tcm$tc_calc==2], paired = FALSE, alternative = "t")
wilcox.test(tcm$natvisit_last12m_og[tcm$tc_calc==1],tcm$natvisit_last12m_og[tcm$tc_calc==2], paired = FALSE, alternative = "g")

wilcox.test(tcm$natvisit_last12m[tcm$tc_avg==1],tcm$natvisit_last12m[tcm$tc_avg==2], paired = FALSE, alternative = "t")
wilcox.test(tcm$natvisit_last12m[tcm$tc_avg==1],tcm$natvisit_last12m[tcm$tc_avg==2], paired = FALSE, alternative = "g")


wilcox.test(tcm$natvis_fav_fin[tcm$tc_avg==1],
            tcm$natvis_fav_fin[tcm$tc_avg==2], paired = FALSE, alternative = "t")
wilcox.test(tcm$natvis_fav_fin[tcm$tc_avg==1],
            tcm$natvis_fav_fin[tcm$tc_avg==2], paired = FALSE, alternative = "g")

wilcox.test(tcm$natvis_fav_fin[tcm$tc_avg==1 & tcm$tc_calc==1],
            tcm$natvis_fav_fin[tcm$tc_avg==2 & tcm$tc_calc==1], paired = FALSE, alternative = "t")
wilcox.test(tcm$natvis_fav_fin[tcm$tc_avg==1 & tcm$tc_calc==2],
            tcm$natvis_fav_fin[tcm$tc_avg==2 & tcm$tc_calc==2], paired = FALSE, alternative = "t")

wilcox.test(tcm$natvis_fav_fin[tcm$tc_avg==1 & tcm$tc_calc==2],
            tcm$natvis_fav_fin[tcm$tc_avg==2 & tcm$tc_calc==2], paired = FALSE, alternative = "g")




