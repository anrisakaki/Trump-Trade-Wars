################################
# CONCORD BETWEEN MASP AND HS8 #
################################

masp_hs8 <- masp_hs8 %>% 
  filter(masp != "SP mới",
         masp != "sp mới",
         masp != "không có",
         masp != "chưa có",
         HS8 != "ko có đối ứng", 
         HS8 != "Không có mã HS",
         HS8 != "không có mã HS"
  ) %>%  
  mutate(masp = gsub("\\*", "", masp),
         HS8 = gsub("\\*", "", HS8),
         HS8 = gsub("\\.", "", HS8),
         HS8 = gsub("\\'", "", HS8)) %>% 
  mutate(HS8 = str_trim(HS8)) %>%  # Remove leading/trailing spaces
  separate_rows(HS8, sep = "\\s+") %>% 
  mutate(masp = str_trim(masp)) %>% 
  separate_rows(masp, sep = "\\s+") %>% 
  mutate(masp = recode(masp, # recoding based on updates made by GSO 
                       "20119283" = "20110684",
                       "23103201" = "23100429",
                       "28110402" = "28110403",
                       "20232419" = "20232410",
                       "20232411" = "32900642",
                       "20232412" = "32900643",
                       "20290930" = "20290920"
  )) %>% 
  mutate(HS8 = str_pad(HS8, width = 8, side = "left", pad = "0"),
         masp = str_pad(masp, width = 8, side = "left", pad = "0"))

write_dta(masp_hs8, "masp_hs8.dta")

hs8_trump <- us_tariffs %>% 
  filter(country == "CHINA") %>% 
  mutate(HS8 = substr(hs10, 1, 8),
         treated = ifelse(tariff_max > 0, 1, 0)) %>% 
  select(year, HS8, treated, tariff_max, tariff_scaled) %>% 
  distinct()

hs8_trump_18 <- us_tariffs %>% 
  filter(country == "CHINA" & year == 2018) %>% 
  mutate(HS8 = substr(hs10, 1, 8),
         treated = ifelse(tariff_max > 0, 1, 0)) %>% 
  select(year, HS8, treated, tariff_max, tariff_scaled) %>% 
  distinct()

hs8_trump_19 <- us_tariffs %>% 
  filter(country == "CHINA" & year == 2019) %>% 
  mutate(HS8 = substr(hs10, 1, 8),
         treated = ifelse(tariff_max > 0, 1, 0)) %>% 
  select(year, HS8, treated, tariff_max, tariff_scaled) %>% 
  distinct()

willbe_treated <- hs8_trump %>% 
  filter (treated == 1) %>% 
  select(HS8, treated) %>% 
  distinct() %>% 
  rename(willbe_treated = treated)

#####################
# CLEANING SP FILES #
#####################

sp1418 <- c("SP_2014", "SP_2015", "SP_2016", "SP_2017", "SP_2018")

for(i in sp1418){
  
  assign(i, get(i) %>% 
           rename(total_volume = kl_spsx,
                  total_value = trigia) %>% 
           select(tinh, ma_thue, masp, total_volume, total_value) %>% 
           group_by(ma_thue, masp) %>%
           summarise(total_value = sum(total_value),
                     total_volume = sum(total_volume)))
  
}

SP_2019 <- SP_2019 %>% 
  rename(ma_thue = masothue,
         masp = masanpha,
         total_volume = khoiluon,
         total_value = trigiasp) %>% 
  select(ma_thue, masp, total_volume, total_value) %>% 
  group_by(ma_thue, masp) %>%
  summarise(total_value = sum(total_value),
            total_volume = sum(total_volume))  

SP_2014 <- SP_2014 %>% mutate(year = 2014)
SP_2015 <- SP_2015 %>% mutate(year = 2015)
SP_2016 <- SP_2016 %>% mutate(year = 2016)
SP_2017 <- SP_2017 %>% mutate(year = 2017)
SP_2018 <- SP_2018 %>% mutate(year = 2018)
SP_2019 <- SP_2019 %>% mutate(year = 2019)

sp_1419 <- c("SP_2014", "SP_2015", "SP_2016", "SP_2017", "SP_2018", "SP_2019")

for(i in sp_1419){
  
  assign(i, left_join(get(i), masp_hs8, by = "masp"))
  
  assign(i, left_join(get(i), willbe_treated, by = "HS8"))
  
}


SP_2018 <- left_join(SP_2018, hs8_trump_18, by = c("HS8", "year"))
SP_2019 <- left_join(SP_2019, hs8_trump_19, by = c("HS8", "year"))

sp1419 <- bind_rows(SP_2014, SP_2015, SP_2016, SP_2017, SP_2018, SP_2019) %>% 
  mutate(willbe_treated = ifelse(is.na(willbe_treated), 0, willbe_treated),
         treated = ifelse(is.na(treated), 0, treated)) %>% 
  select(-"masp") %>% 
  distinct()

#####################################
# CREATING DATASET OF TREATED FIRMS #
#####################################

# firms are considered treated if they produce treated products 

dataframes_list <- list(
  SP_2014,
  SP_2015,
  SP_2016,
  SP_2017,
  SP_2018,
  SP_2019
)

treated_firms <- list()

for (i in 1:length(dataframes_list)) {
  df <- dataframes_list[[i]]
  result <- df %>%
    group_by(ma_thue) %>%
    mutate(willbe_treated = ifelse(is.na(willbe_treated), 0, willbe_treated)) %>% 
    summarise(treated = sum(willbe_treated)) %>% 
    mutate(treated = ifelse(treated > 1, 1, 0))
  
  # Append the result to the results list
  treated_firms[[i]] <- result
}
