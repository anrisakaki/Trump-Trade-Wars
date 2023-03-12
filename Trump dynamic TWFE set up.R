##########################################
# SETTING UP FOR DYNAMIC TWFE REGRESSION # 
##########################################

us_chn_tariffs <- us_tariffs %>% 
  filter(iso3 == "CHN") %>% 
  select(hs10, effective_mdate) %>% 
  distinct()

us_chn_tariffs$HS6 <- as.numeric(substr(trimws(format(us_chn_tariffs$hs10, scientific = F)), 1, 6))

us_chn_tariffs_HS6 <- us_chn_tariffs %>% 
  select(HS6)

HS6_ISIC <- data.frame(concord(sourcevar = us_chn_tariffs_HS6$HS6,
                               origin = "HS5", destination = "ISIC4",
                               dest.digit = 4, all = FALSE))

us_chn_tariffs <- bind_cols(us_chn_tariffs, HS6_ISIC) 

us_chn_tariffs <- us_chn_tariffs %>% 
  rename(ISIC = concord.sourcevar...us_chn_tariffs_HS6.HS6..origin....HS5...destination....ISIC4...) %>% 
  select(effective_mdate, ISIC) %>% 
  distinct()

us_chn_tariffs <- us_chn_tariffs %>% mutate(ISIC = recode(ISIC,
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
                                                          '0141' = '141',
                                                          '0910' = '910'))

# For sectors were products were hit at different periods, I take the earliest time period which that sector was hit 

us_chn_tariffs_feb18 <- us_chn_tariffs %>% 
  filter(effective_mdate == 697) %>% 
  rename(feb18 = effective_mdate)

us_chn_tariffs_mar18 <- us_chn_tariffs %>% 
  filter(effective_mdate == 698) %>% 
  rename(mar18 = effective_mdate)

us_chn_tariffs_jul18 <- us_chn_tariffs %>% 
  filter(effective_mdate == 702) %>% 
  rename(jul18 = effective_mdate)  

us_chn_tariffs_aug18 <- us_chn_tariffs %>% 
  filter(effective_mdate == 703) %>% 
  rename(aug18 = effective_mdate)

us_chn_tariffs_sept18 <- us_chn_tariffs %>% 
  filter(effective_mdate == 704) %>% 
  rename(sept18 = effective_mdate)

us_chn_tariffs_sept19 <- us_chn_tariffs %>% 
  filter(effective_mdate == 716) %>% 
  rename(sept19 = effective_mdate)

us_chn_tariff <- list(us_chn_tariffs_feb18, us_chn_tariffs_mar18, us_chn_tariffs_jul18, us_chn_tariffs_aug18, us_chn_tariffs_sept18, us_chn_tariffs_sept19) %>% 
  reduce(full_join, by = "ISIC")

write.csv(us_chn_tariff, "us_chn_tariff.csv")

# Manually edited on excel to retain month of sector was first targeted 