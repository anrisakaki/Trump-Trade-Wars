##############################
# CLEANING TRUMP TARIFF DATA #
##############################
us_chn_tariffs_18 <- us_tariffs %>% 
  filter(iso3 == "CHN",
         year == 2018) %>% 
  select(hs10, tariff_max, tariff_scaled, effective_mdate) %>% 
  mutate(month = recode(effective_mdate,
                        "704" = "9",
                        "702" = "2",
                        "298" = "3",
                        "702" = "7",
                        "703" = "8")) %>% 
  filter(!is.na(month))

us_chn_tariffs_19 <- us_tariffs %>% 
  filter(iso3 == "CHN",
         year == 2019) %>% 
  select(hs10, tariff_max, tariff_scaled, effective_mdate) %>% 
  mutate(month = recode(effective_mdate,
                        "716" = "9",
                        "704" = "2018_9",
                        "702" = "2018_2",
                        "298" = "2018_3",
                        "702" = "2018_7",
                        "703" = "2018_8"))  

#####################################
# CONVERTING ISIC4 IN LFS TO ISIC 3 #
#####################################

us_chn_tariffs_18$HS6 <- as.numeric(substr(as.character(us_chn_tariffs_18$hs10), 1, 6))
us_chn_tariffs_19$HS6 <- as.numeric(substr(as.character(us_chn_tariffs_19$hs10), 1, 6))

trump_tariffs <- c("us_chn_tariffs_18", "us_chn_tariffs_19")

for(i in trump_tariffs){
  assign(i, merge(get(i), hs_i3, by = "HS6"))
  
  assign(i, merge(get(i), i4_i3, by = "ISIC"))
  
  assign(i, get(i) %>% 
           rename(ISIC3 = ISIC,
                  ISIC = ISIC4) %>% 
           select(ISIC3, HS6, tariff_max, tariff_scaled, effective_mdate, month, ISIC) %>% 
           mutate(across(month, as.numeric)))
}


