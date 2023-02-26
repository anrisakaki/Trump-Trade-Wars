##############################
# CLEANING TRUMP TARIFF DATA #
##############################
us_chn_tariffs_18 <- us_tariffs %>% 
  filter(iso3 == "CHN",
         year == 2018,
         effective_mdate != 716) %>% 
  select(hs10, tariff_max, tariff_scaled, effective_mdate) %>% 
  mutate(month = recode(effective_mdate, 
                        '697' = '2',
                        '698' = '3',
                        '702' = '7',
                        '703' = '8',
                        '704' = '9'
                        ),
         treated = 1)

us_chn_tariffs_19 <- us_tariffs %>% 
  filter(iso3 == "CHN",
         year == 2019) %>% 
  select(hs10, tariff_max, tariff_scaled, effective_mdate) %>% 
  mutate(month = recode(effective_mdate,
                        "716" = "9"),
         treated = 1)

#####################################
# CONVERTING ISIC4 IN LFS TO ISIC 3 #
#####################################
trump_tariffs <- c("us_chn_tariffs_18", "us_chn_tariffs_19")

us_chn_tariffs_18$hs10 <- sprintf(fmt = "%-10.0f", us_chn_tariffs_18$hs10)
us_chn_tariffs_19$hs10 <- sprintf(fmt = "%-10.0f", us_chn_tariffs_19$hs10)

us_chn_tariffs_18$HS6 <- as.numeric(substr(format(us_chn_tariffs_18$hs10, scientific = F), 1, 6))
us_chn_tariffs_19$HS6 <- as.numeric(substr(format(us_chn_tariffs_19$hs10, scientific = F), 1, 6))

HS6_18 <- us_chn_tariffs_18 %>% 
  select(HS6) %>% 
  distinct()

HS6_ISIC_18 <- data.frame(concord(sourcevar = HS6_18$HS6,
        origin = "HS6", destination = "ISIC4",
        dest.digit = 4, all = FALSE))

HS6_ISIC_18 <- HS6_ISIC_18 %>% 
  rename(ISIC = concord.sourcevar...HS6_18.HS6..origin....HS6...destination....ISIC4...)

HS6_ISIC_18 <- bind_cols(HS6_18, HS6_ISIC_18)

trump_tariffs <- c("us_chn_tariffs_18", "us_chn_tariffs_19")

for(i in trump_tariffs){
  
  assign(i, get(i) %>% 
           select(-"hs10"))
  
  assign(i, merge(get(i), hs_i3, by = "HS6"))
  
  assign(i, get(i) %>%
           select(tariff_max, tariff_scaled, effective_mdate, month, ISIC, treated) %>%
           group_by(effective_mdate, ISIC) %>%
           distinct()) # ISIC in this dataframe is ISIC3
  
  assign(i, merge(get(i), i4_i3, by = "ISIC")) %>%
    group_by(effective_mdate, ISIC4) %>%
    distinct()
  
  assign(i, get(i) %>%
           rename(ISIC3 = ISIC,
                  ISIC = ISIC4) %>%
           mutate(across(month, as.numeric)))
}





