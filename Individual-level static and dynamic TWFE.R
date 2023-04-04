for (i in LFS_1520){
  assign(i, full_join(get(i), us_chn_tariff, by = "ISIC"))
  
  assign(i, get(i) %>% 
           mutate(first_treated = recode(effective_mdate, 
                                         '697' = '201802',
                                         '698' = '201803',
                                         '702' = '201807',
                                         '703' = '201808',
                                         '704' = '201809',
                                         '716' = '201909'),
                  treat = ifelse(year_month > first_treated, 1, NA),
                  first_treated = ifelse(is.na(first_treated), 0, first_treated),
                  treat = ifelse(is.na(treat), 0, treat),
                  treated = ifelse(first_treated > 0, 1, 0)))
}

LFS1520 <- bind_rows(list(LFS_2015, LFS_2016, LFS_2017, LFS_2018, LFS_2019, LFS_2020))

LFS1520$year_ft <- as.numeric(substr(trimws(format(LFS1520$first_treated, scientific = F)), 1, 4))

###############
# STATIC TWFE #
###############

etable(list(
  feols(work ~ i(year, treated, 2018) | ISIC^month + year,
        weights = ~ weight,
        vcov = ~ ISIC,
        LFS1520),
  feols(hours ~ i(year, treated, 2018) | ISIC^month + year,
        weights = ~ weight,
        vcov = ~ ISIC,
        LFS1520),
  feols(log(wage) ~ i(year, treated, 2018) | ISIC^month + year,
        weights = ~ weight,
        vcov = ~ ISIC,
        LFS1520)
), tex = T)

etable(list(
  feols(work ~ i(year, treated, 2018) + age + age^2 + educ + Female + urban | ISIC^month + year,
        weights = ~ weight,
        vcov = ~ ISIC,
        LFS1520)),
  feols(hours ~ i(year, treated, 2018) + age + age^2 + educ + Female + urban | ISIC^month + year,
        weights = ~ weight,
        vcov = ~ ISIC,
        LFS1520),
  feols(log(wage) ~ i(year, treated, 2018) + age + age^2 + educ + Female + urban | ISIC^month + year,
        weights = ~ weight,
        vcov = ~ ISIC,
        LFS1520), tex = T)

etable(list(
  feols(formal ~ i(year, treat, 2018) + age + age^2 + educ + Female + urban | ISIC^month + year,
        weights = ~ weight,
        vcov = ~ ISIC,
        LFS1520)),
  feols(casual_contract ~ i(year, treat, 2018) + age + age^2 + educ + Female + urban | ISIC^month + year,
        weights = ~ weight,
        vcov = ~ ISIC,
        LFS1520), tex = T)

########################
# SUN AND ABRAHAM TWFE #
########################

etable(list(
  feols(work ~ sunab(year_ft, year) | ISIC^month + year,
        weights = ~ weight,
        vcov = ~ISIC,
        LFS1520),
  feols(hours ~ sunab(year_ft, year) | ISIC^month + year,
        weights = ~ weight,
        vcov = ~ISIC,
        LFS1520),
  feols(log(wage) ~ sunab(year_ft, year) | ISIC^month + year,
        weights = ~ weight,
        vcov = ~ISIC,
        LFS1520)
), agg = "att", tex = TRUE)

etable(list(
  feols(frormal ~ sunab(year_ft, year) | ISIC^month + year,
        weights = ~ weight,
        vcov = ~ISIC,
        LFS1520),
  feols(hours ~ sunab(year_ft, year) | ISIC^month + year, LFS1520)
), agg = "att", tex = TRUE)

