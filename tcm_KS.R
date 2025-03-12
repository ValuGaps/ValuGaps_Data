#TCM_analysis
library(dplyr)
#write.csv(all_data, "data_tcm/all_data.csv")

df = read.csv("data_tcm/all_data.csv")
data_api_out = read.csv("data_tcm/data_api_joined.csv")

colnames(df)

# temporary = df[, c("tc1", "tc_map", "tc_timeline", "tc_calc", "tc_avg",
#                    "natvisit_last12m", "natvisit_last12m_og", "natvisit_monthly",
#                    "natvisit_weekly", "natvisit_last12m_m", "natvisit_last12m_w", 
#                    "nv_2a", "nv_2a_other", "nv_2a_RAND", "lat_tc", "lon_tc", "q91test_1",
#                    "q91test_2", "q91test_3", "q91test_1", "q91test_1"
#                    )]
which(colnames(df)=="RID")

temporary = df[,c(551,191:262)]



tcm = temporary[!is.na(temporary$tc1),]
tcm_treatments = tcm[!is.na(tcm$tc_map),]

tcm_treatments = left_join(tcm_treatments,data_api_out,by="RID")

###################### messy code below ####################

tcm_treatments

#treatments summary
table(tcm$tc_map)
table(tcm$tc_timeline)
table(tcm$tc_calc)
table(tcm$tc_avg)

table(tcm$tc_calc,tcm$tc_avg) #ok!

tcm$natvis_fav_og = tcm$natvisit_fav
tcm$natvis_fav_fin =  tcm$natvisit_fav1
tcm$natvis_fav_fin = ifelse(is.na(tcm$natvis_fav_fin), tcm$natvisit_fav2, tcm$natvis_fav_fin)

read.csv()

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




