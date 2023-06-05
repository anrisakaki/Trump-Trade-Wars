
setwd("C:/Users/Anri Sakakibara/OneDrive/PhD Political Economy/Trump China Trade War/")

here() # check that gives correct path

LFS1520 <- read_dta("LFS1520_main.dta")

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
), agg = "att", tex = TRUE)
