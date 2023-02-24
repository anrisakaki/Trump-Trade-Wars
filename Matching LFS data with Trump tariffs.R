##################################################
# MATCHING LFS DATA WITH HS-10 TRUMP TARIFF DATA #
##################################################

# 2018 

LFS_2018 <- left_join(LFS_2018, us_chn_tariffs_18, by =c("ISIC", "month"))

# 2019 
## Tariffs in some sectors were raised in September 2019 so it is necessary to match workers to pre-post new tariffs based on month 

us_chn_tariffs_19_old <- us_chn_tariffs_19 %>% 
  filter(is.na(month)) %>% 
  select(-"month")
LFS_2019_presept <- LFS_2019 %>% 
  filter(month < 9)

us_chn_tariffs_19_new <- us_chn_tariffs_19 %>% 
  filter(is.na(month)) %>% 
  select(-"month")
LFS_2019_postsept <- LFS_2019 %>% 
  filter(month > 8)

LFS_2019_presept <- left_join(LFS_2019_presept, us_chn_tariffs_19_old, by = "ISIC") %>% 
  distinct()
LFS_2019_postsept <- left_join(LFS_2019_postsept, us_chn_tariffs_19_new, by = "ISIC") %>% 
  distinct()

LFS_2019 <- bind_rows(LFS_2019_presept, LFS_2019_postsept)

for(i in LFS_1520){
  
  if (i %in% c("LFS_2015", "LFS_2016", "LFS_2017")){
    assign(i, get(i) %>% 
             mutate(treated = 0,
                    tariff_max = 0,
                    tariff_scaled = 0))
  }
}

  