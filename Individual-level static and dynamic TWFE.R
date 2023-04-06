for (i in LFS_1520){
  # assign(i, full_join(get(i), us_chn_tariff, by = "ISIC"))
  
  assign(i, get(i) %>% 
           mutate(first_treated = recode(effective_mdate, 
                                         '697' = '201802',
                                         '698' = '201803',
                                         '702' = '201807',
                                         '703' = '201808',
                                         '704' = '201809',
                                         '716' = '201909'),
                  treat = ifelse(year_month > first_treated, 1, NA),
                  treat = ifelse(is.na(treat), 0, treat),
                  treated = ifelse(is.na(effective_mdate), 0, 1)) %>% 
           mutate(across(month, as.numeric)))
}

LFS1520 <- bind_rows(list(LFS_2015, LFS_2016, LFS_2017, LFS_2018, LFS_2019, LFS_2020))

LFS1520$year_ft <- as.numeric(substr(trimws(format(LFS1520$first_treated, scientific = F)), 1, 4))

LFS1520$month_hit <- as.numeric(substr(trimws(format(LFS1520$first_treated, scientific = F)), 5, 6))

LFS1520 <- LFS1520 %>% 
  mutate(ttt = year - year_ft,
         ttt = ifelse(month < month_hit, ttt-1, ttt),
         ttt = ifelse(treated == 0, 0, ttt),
         ytt = year - ttt,
         ytt = ifelse(treated == 0, 10000, ytt)) # Following Sun and Abraham, we give our never-treated units a fake "treatment" date far outside the relevant study period.

#######################
# BASIC EVENT STUDIES #
#######################

etable(list(
  feols(work ~ i(ttt, treated, ref = -1) | ISIC^month + year,
        weights = ~ weight,
        vcov = ~ISIC,
        LFS1520),
  feols(hours ~ i(ttt, treated, ref = -1) | ISIC^month + year,
        weights = ~ weight,
        vcov = ~ISIC,
        LFS1520),
  feols(log(wage) ~ i(ttt, treated, ref = -1) | ISIC^month + year,
        weights = ~ weight,
        vcov = ~ISIC,
        LFS1520),
  feols(log(wage_perh) ~ i(ttt, treated, ref = -1) | ISIC^month + year,
        weights = ~ weight,
        vcov = ~ISIC,
        LFS1520)  
), tex = TRUE)

# Plus controls 

etable(list(
  feols(work ~ i(ttt, treated, ref = -1) + age + age^2 + educ + Female + urban | ISIC^month + year,
        weights = ~ weight,
        vcov = ~ISIC,
        LFS1520),
  feols(hours ~ i(ttt, treated, ref = -1) + age + age^2 + educ + Female + urban | ISIC^month + year,
        weights = ~ weight,
        vcov = ~ISIC,
        LFS1520),
  feols(log(wage) ~ i(ttt, treated, ref = -1) + age + age^2 + educ + Female + urban | ISIC^month + year,
        weights = ~ weight,
        vcov = ~ISIC,
        LFS1520),
  feols(log(wage_perh) ~ i(ttt, treated, ref = -1) + age + age^2 + educ + Female + urban | ISIC^month + year,
        weights = ~ weight,
        vcov = ~ISIC,
        LFS1520)  
), tex = TRUE)

########################
# SUN AND ABRAHAM TWFE #
########################

etable(list(
  feols(work ~ sunab(ytt, year) + age + age^2 + educ + Female + urban | ISIC^month + year,
        weights = ~ weight,
        vcov = ~ISIC,
        LFS1520),
  feols(hours ~ sunab(ytt, year) + age + age^2 + educ + Female + urban | ISIC^month + year,
        weights = ~ weight,
        vcov = ~ISIC,
        LFS1520),
  feols(log(wage) ~ sunab(ytt, year) + age + age^2 + educ + Female + urban | ISIC^month + year,
        weights = ~ weight,
        vcov = ~ISIC,
        LFS1520),
  feols(log(wage_perh) ~ sunab(ytt, year) + age + age^2 + educ + Female + urban | ISIC^month + year,
        weights = ~ weight,
        vcov = ~ISIC,
        LFS1520)  
), tex = TRUE)

etable(list(
  feols(work ~ sunab(year_ft, year) + age + age^2 + educ + Female + urban | ISIC^month + year,
        weights = ~ weight,
        vcov = ~ISIC,
        LFS1520),
  feols(hours ~ sunab(year_ft, year) + age + age^2 + educ + Female + urban | ISIC^month + year,
        weights = ~ weight,
        vcov = ~ISIC,
        LFS1520),
  feols(log(wage) ~ sunab(year_ft, year) + age + age^2 + educ + Female + urban | ISIC^month + year,
        weights = ~ weight,
        vcov = ~ISIC,
        LFS1520),
  feols(log(wage_perh) ~ sunab(year_ft, year) + age + age^2 + educ + Female + urban | ISIC^month + year,
        weights = ~ weight,
        vcov = ~ISIC,
        LFS1520)  
), agg = "att", tex = TRUE)
