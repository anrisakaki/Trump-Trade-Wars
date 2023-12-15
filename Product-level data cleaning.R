################################
# CONCORD BETWEEN MASP AND HS8 #
################################

masp_hs6 <- masp_hs8 %>% 
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
         masp = str_pad(masp, width = 8, side = "left", pad = "0")) %>% 
  mutate(HS6 = substr(HS8, 1, 6),
         cap6 = substr(masp, 1, 6),
         cap5 = substr(masp, 1, 5)) %>% 
  select(cap5, cap6, HS6, masp) %>% 
  distinct()

write_dta(masp_hs6, "masp_hs6.dta")

# List HS8 treated products and first treated year 
hs6_trump <- us_tariffs %>% 
  filter(country == "CHINA") %>% 
  mutate(HS6 = substr(hs10, 1, 6),
         treated = ifelse(tariff_max > 0, 1, 0),
         first_treated = ifelse(effective_mdate < 716, 2018, 2019)) %>% 
  select(first_treated, HS6, treated, tariff_max, tariff_scaled) %>% 
  distinct() %>%
  group_by(first_treated, HS6) %>% 
  summarise(tariff_max = max(tariff_max),
            tariff_scaled = max(tariff_scaled)) %>% 
  mutate(treated = 1) %>% 
  ungroup()

hs5_trump <- us_tariffs %>% 
  filter(country == "CHINA") %>% 
  mutate(HS5 = substr(hs10, 1, 5),
         treated = ifelse(tariff_max > 0, 1, 0)) %>% 
  filter(treated == 1) %>% 
  select(HS5, treated)

cap5_hs5 <- masp_hs6 %>% select(cap5, HS6) %>% 
  mutate(HS5 = substr(HS6, 1, 5)) %>% 
  select(cap5, HS5) %>% 
  distinct()

cap5_hs5 <- left_join(cap5_hs5, hs5_trump, by = "HS5") %>% distinct()

# Services 

services <- services %>%
  filter(str_detect(Description, "Dịch vụ"),
         !is.na(masp8)) %>% 
  mutate(service = 1,
         masp8 = str_pad(masp8, width = 8, side = "left", pad = "0"),
         cap5 = substr(masp8, 1, 5)) %>% 
  select(cap5, everything())

services <- left_join(services, cap5_hs5, by = "cap5") %>% distinct() %>% 
  select(masp8, service, treated) %>% 
  rename(treated_service = treated,
         masp = masp8)

#####################
# CLEANING SP FILES #
#####################

sp1418 <- c("SP_2014", "SP_2015", "SP_2016", "SP_2017", "SP_2018")

for(i in sp1418){
  
  assign(i, get(i) %>% 
           rename(total_value = trigia) %>% 
           select(tinh, ma_thue, masp, total_value) %>% 
           group_by(ma_thue, masp) %>%
           summarise(total_value = sum(total_value)) %>%
           ungroup())
  
}

SP_2019 <- SP_2019 %>% 
  rename(ma_thue = masothue,
         masp = masanpha,
         total_value = trigiasp) %>% 
  select(ma_thue, masp, total_value) %>% 
  group_by(ma_thue, masp) %>%
  summarise(total_value = sum(total_value)) %>% 
  ungroup()

SP_2014 <- SP_2014 %>% mutate(year = 2014)
SP_2015 <- SP_2015 %>% mutate(year = 2015)
SP_2016 <- SP_2016 %>% mutate(year = 2016)
SP_2017 <- SP_2017 %>% mutate(year = 2017)
SP_2018 <- SP_2018 %>% mutate(year = 2018)
SP_2019 <- SP_2019 %>% mutate(year = 2019)

sp_1419 <- c("SP_2014", "SP_2015", "SP_2016", "SP_2017", "SP_2018", "SP_2019")

for(i in sp_1419){
  
  assign(i, left_join(get(i), masp_hs6, by = "masp"))
  
  assign(i, left_join(get(i), hs6_trump, by = "HS6"))
  
  assign(i, left_join(get(i), services, by = "masp"))
  
  assign(i, get(i) %>% 
           mutate(treated = ifelse(is.na(treated), 0 , treated)))
  
}

# Dataframe which gives the firms which were created treated products in year t

sp1419 <- bind_rows(SP_2014, SP_2015, SP_2016, SP_2017, SP_2018, SP_2019) %>% 
  group_by(ma_thue) %>% 
  mutate(
    # prod_treated = 1 if firm produces treated products in any year before 2018
    prod_treated = ifelse(any(treated == 1 & year < 2018), 1, 0),
    # prod_treated17 = 1 if firm i produces treated products in the year immediately preceding the first wave of tariff hikes (i.e. 2017)
    prod_treated17 = ifelse(any(treated == 1 & year == 2017), 1, 0),
    treated_service = ifelse(any(treated_service == 1 & year < 2018), 1, 0),
    treated_service17 = ifelse(any(treated_service == 1 & year == 2017), 1, 0))

sp1419_clean <- sp1419 %>% 
  select(year, ma_thue, prod_treated, prod_treated17, treated_service, treated_service17) %>% 
  distinct() %>% 
  mutate(treated_service = ifelse(is.na(treated_service), 0, treated_service),
         treated_service17 = ifelse(is.na(treated_service17), 0, treated_service17))

withinfirm <- sp1419 %>% 
  group_by(ma_thue, year) %>% 
  summarise(
    total_products = n(),
    treated_products = sum(treated == 1)
  ) %>% 
  mutate(share_treated = treated_products/total_products)

productlevel_firms <- sp1419 %>%
  mutate(treated = ifelse(any(treated == 1), 1, 0)) %>% 
  select(ma_thue, year, treated) %>% 
  distinct() %>% 
  group_by(year) %>% 
  summarise(total_firms = n(),
            treated_firms = sum(treated == 1)) %>% 
  mutate(share = treated_firms/total_firms)

long_questionnaire <- sp1419 %>%
  select(ma_thue, year, prod_treated) %>% 
  mutate(long = 1) %>% 
  distinct()
