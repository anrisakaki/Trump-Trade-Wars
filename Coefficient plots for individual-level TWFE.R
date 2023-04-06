png("work_ES_SA.png")
iplot(list(
  feols(work ~ i(ttt, treated, ref = -1) + age + age^2 + educ + Female + urban | ISIC^month + year,
        weights = ~ weight,
        vcov = ~ISIC,
        LFS1520),
  feols(work ~ sunab(year_ft, year) + age + age^2 + educ + Female + urban | ISIC^month + year,
        weights = ~ weight,
        vcov = ~ISIC,
        LFS1520)), sep = 0.2, ref.line = -1,
  xlab = 'Time to treatment',
  main = 'Effect on work')
legend("bottomleft", col = c(1, 2), pch = c(20, 17), cex = 0.5,
       legend = c("TWFE", "Sun & Abraham (2020)"))
dev.off()

png("hours_ES_SA.png")
iplot(list(
  feols(hours ~ i(ttt, treated, ref = -1) + age + age^2 + educ + Female + urban | ISIC^month + year,
        weights = ~ weight,
        vcov = ~ISIC,
        LFS1520),
  feols(hours ~ sunab(year_ft, year) + age + age^2 + educ + Female + urban | ISIC^month + year,
        weights = ~ weight,
        vcov = ~ISIC,
        LFS1520)), sep = 0.2, ref.line = -1,
  xlab = 'Time to treatment',
  main = 'Effect on hours worked')
legend("bottomleft", col = c(1, 2), pch = c(20, 17), cex = 0.6,
       legend = c("TWFE", "Sun & Abraham (2020)"))
dev.off()

png("wages_ES_SA.png")
iplot(list(
  feols(log(wage) ~ i(ttt, treated, ref = -1) + age + age^2 + educ + Female + urban | ISIC^month + year,
        weights = ~ weight,
        vcov = ~ISIC,
        LFS1520),
  feols(log(wage) ~ sunab(year_ft, year) + age + age^2 + educ + Female + urban | ISIC^month + year,
        weights = ~ weight,
        vcov = ~ISIC,
        LFS1520)), sep = 0.2, ref.line = -1,
  xlab = 'Time to treatment',
  main = 'Effect on (log) wages')
legend("bottomleft", col = c(1, 2), pch = c(20, 17), cex = 0.6,
       legend = c("TWFE", "Sun & Abraham (2020)"))
dev.off()

png("wageperh_ES_SA.png")
iplot(list(
  feols(log(wage_perh) ~ i(ttt, treated, ref = -1) + age + age^2 + educ + Female + urban | ISIC^month + year,
        weights = ~ weight,
        vcov = ~ISIC,
        LFS1520),
  feols(log(wage_perh) ~ sunab(year_ft, year) + age + age^2 + educ + Female + urban | ISIC^month + year,
        weights = ~ weight,
        vcov = ~ISIC,
        LFS1520)), sep = 0.2, ref.line = -1,
  xlab = 'Time to treatment',
  main = 'Effect on (log) wages per hour')
legend("bottomleft", col = c(1, 2), pch = c(20, 17), cex = 0.6,
       legend = c("TWFE", "Sun & Abraham (2020)"))
dev.off()

iplot(list(
  feols(log(formal) ~ i(ttt, treated, ref = -1) + age + age^2 + educ + Female + urban | ISIC^month + year,
        weights = ~ weight,
        vcov = ~ISIC,
        LFS1520),
  feols(log(formal) ~ sunab(year_ft, year) + age + age^2 + educ + Female + urban | ISIC^month + year,
        weights = ~ weight,
        vcov = ~ISIC,
        LFS1520)), sep = 0.2, ref.line = -1,
  xlab = 'Time to treatment',
  main = 'Effect on formal')
