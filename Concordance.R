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

# 2018 
us_chn_tariffs_18$HS6 <- as.numeric(substr(trimws(format(us_chn_tariffs_18$hs10, scientific = F)), 1, 6))

HS6_18 <- us_chn_tariffs_18 %>% 
  select(HS6) %>% 
  distinct()

HS6_ISIC_18 <- data.frame(concord(sourcevar = HS6_18$HS6,
        origin = "HS5", destination = "ISIC4",
        dest.digit = 4, all = FALSE))

HS6_ISIC_18 <- HS6_ISIC_18 %>% 
  rename(ISIC = concord.sourcevar...HS6_18.HS6..origin....HS5...destination....ISIC4...)

HS6_ISIC_18 <- bind_cols(HS6_18, HS6_ISIC_18)

us_chn_tariffs_18 <- left_join(us_chn_tariffs_18, HS6_ISIC_18, by = "HS6")

# 2019 
us_chn_tariffs_19$HS6 <- as.numeric(substr(trimws(format(us_chn_tariffs_19$hs10, scientific = F)), 1, 6))

HS6_19 <- us_chn_tariffs_19 %>% 
  select(HS6) %>% 
  distinct()

HS6_ISIC_19 <- data.frame(concord(sourcevar = HS6_19$HS6,
                                  origin = "HS5", destination = "ISIC4",
                                  dest.digit = 4, all = FALSE))

HS6_ISIC_19 <- bind_cols(HS6_19, HS6_ISIC_19)

HS6_ISIC_19 <- HS6_ISIC_19 %>% 
  rename(ISIC = concord.sourcevar...HS6_19.HS6..origin....HS5...destination....ISIC4...)

us_chn_tariffs_19 <- left_join(us_chn_tariffs_19, HS6_ISIC_19, by = "HS6")

us_chn_tariffs_1819 <- c("us_chn_tariffs_18", "us_chn_tariffs_19")

for(i in us_chn_tariffs_1819){
  assign(i, get(i) %>% 
           select(-c(HS6, hs10)) %>% 
           group_by(month, ISIC) %>% 
           distinct() %>% 
           mutate(ISIC = recode(ISIC,
                                '0111' = "111",
                                '0113' = '113',
                                '0121' = '121',
                                '0143' = '143',
                                '0311' = '311',
                                '0510' = '510',
                                '0610' = '610',
                                '0729' = '729',
                                '0810' = '810',
                                '0891' = '891',
                                '0892' = '892',
                                '0893' = '893',
                                '0520' = '520',
                                '0141' = '141'))))
}
