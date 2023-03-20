##############################
# SETTING UP FOR STATIC TWFE #
##############################

us_chn_tariff <- us_chn_tariff %>% 
  select(ISIC, effective_mdate)

sector_agg <- c("sector_15", "sector_16", "sector_17", "sector_18", "sector_19", "sector_20")

for(i in sector_agg){
  assign(i, full_join(get(i), us_chn_tariff, by = "ISIC"))
    
  assign(i, get(i) %>% 
           mutate(
             first_treated = recode(effective_mdate, 
                                    '697' = '201802',
                                    '698' = '201803',
                                    '702' = '201807',
                                    '703' = '201808',
                                    '704' = '201809',
                                    '716' = '201909'
             ),
             treated = ifelse(year_month > first_treated, 1, 0)
                  ))

}

sector_1519 <- bind_rows(list(sector_15, sector_16, sector_17, sector_18, sector_19, sector_20))

sector_1519$year_ft <- as.numeric(substr(trimws(format(sector_1519$first_treated, scientific = F)), 1, 4))

sector_1519$first_treated <- as.numeric(sector_1519$first_treated)

sector_1519 <- sector_1519 %>%
  mutate(first_treated = ifelse(is.na(first_treated), 0, first_treated),
         treated = ifelse(is.na(treated), 0, treated),
         time_to_treated = year - year_ft,
         treat = ifelse(first_treated > 0, 1, 0))

##############
# SIMPLE TWFE#
##############

etable(list(
  feols(n_workers ~ i(year, treat, 2018) | ISIC^month + year, sector_1519),
  feols(hours ~ i(year, treat, 2018) | ISIC^month + year, sector_1519),
  feols(log(wage) ~ i(year, treat, 2018) | ISIC^month + year, sector_1519)), tex = T)
 
etable(list(
  feols(female_ratio ~ i(year, treat, 2018) | ISIC^month + year, sector_1519),
  feols(formal_ratio ~ i(year, treat, 2018) | ISIC^month + year, sector_1519)
), tex = T)

dict = c("2015" = "t-3")

setFixest_coefplot(dict = dict, grid = F)

png("hours_TWFE.png")
iplot(feols(hours ~ i(year, treat, 2018) | ISIC^month + year, sector_1519))
axis(1, at = c(2015:2020), labels = c("t-3", "t-2", "t-1", "0", "t+1", "t+2"))
dev.off()

png("wages_TWFE.png")
iplot(feols(log(wage) ~ i(year, treat, 2018) | ISIC^month + year, sector_1519))
dev.off()

png("n_workers_TWFE.png")
iplot(feols(n_workers ~ i(year, treat, 2018) | ISIC^month + year, sector_1519))
dev.off()

png("f_ratio_TWFE.png")
iplot(feols(female_ratio ~ i(year, treat, 2018) | ISIC^month + year, sector_1519))
dev.off()

png("formal_ratio.png")
iplot(feols(formal_ratio ~ i(year, treat, 2018) | ISIC^month + year, sector_1519))
dev.off()

########################
# SUB AND ABRAHAM TWFE #
########################

etable(list(
  feols(hours ~ treated + sunab(year_ft, year) | ISIC^month + year, sector_1519),
  feols(log(wage) ~ treated + sunab(year_ft, year) | ISIC^month + year, sector_1519),
  feols(n_workers ~ treated + sunab(year_ft, year) | ISIC^month + year, sector_1519),
  feols(formal_ratio ~ treated + sunab(year_ft, year) | ISIC^month + year, sector_1519),
  feols(female_ratio ~ treated + sunab(year_ft, year) | ISIC^month + year, sector_1519)
), agg = "att", tex = TRUE)

#######################
# WITH LEADS AND LAGS # 
#######################

etable(list(
  feols(hours ~ l(treated, -3:2), sector_1519, panel.id = ~ISIC+year_month),
  feols(log(wage) ~ l(treated, -3:2), sector_1519, panel.id = ~ISIC+year_month)
))
