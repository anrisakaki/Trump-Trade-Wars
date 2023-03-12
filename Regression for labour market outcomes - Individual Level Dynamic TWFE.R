###############################################################
# SETING UP FOR DYNAMIC TWFE FOR INDIVIDUAL-LEVEL REGRESSIONS # 
###############################################################

for(i in LFS_1520){
  assign(i, full_join(get(i), us_chn_tariff, by = "ISIC"))
  
  assign(i, get(i) %>% 
           mutate(
             first_treated = recode(effective_mdate, 
                                    '697' = '201802',
                                    '698' = '201803',
                                    '702' = '201807',
                                    '703' = '201808',
                                    '704' = '201809',
                                    '716' = '201909')
           ))  
}

LFS1520 <- bind_rows(list(LFS_2015, LFS_2016, LFS_2017, LFS_2018, LFS_2019))

LFS1520$first_treated <- as.numeric(LFS1520$first_treated)

LFS1520 <- LFS1520 %>% mutate(first_treated = ifelse(is.na(first_treated), 0, first_treated))

dynamic_lfs <- att_gt(
  yname = "work",
  gname = "first_treated",
  idname = "ISIC",
  tname = "effective_mdate",
  data = LFS1520,
  panel = FALSE)

summary(dynamic_lfs)
