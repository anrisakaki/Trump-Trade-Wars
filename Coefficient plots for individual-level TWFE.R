png("work_ES_SA.png")
iplot(list(
  feols(work ~ i(ttt, treated, ref = -1) + age + age^2 + educ + Female + urban | ISIC^month + year,
        weights = ~ weight,
        vcov = ~ISIC,
        LFS1520),
  feols(work ~ sunab(ytt, year) + age + age^2 + educ + Female + urban | ISIC^month + year,
        weights = ~ weight,
        vcov = ~ISIC,
        LFS1520)), sep = 0.2, ref.line = -1,
  main = "",
  xlab = 'Time to treatment')
legend("bottomleft", col = c(1, 2), pch = c(20, 17), cex = 0.5,
       legend = c("TWFE", "Sun & Abraham (2020)"))
dev.off()

png("hours_ES_SA.png")
iplot(list(
  feols(hours ~ i(ttt, treated, ref = -1) + age + age^2 + educ + Female + urban | ISIC^month + year,
        weights = ~ weight,
        vcov = ~ISIC,
        LFS1520),
  feols(hours ~ sunab(ytt, year) + age + age^2 + educ + Female + urban | ISIC^month + year,
        weights = ~ weight,
        vcov = ~ISIC,
        LFS1520)), sep = 0.2, ref.line = -1,
  main = "",  
  xlab = 'Time to treatment')
legend("bottomleft", col = c(1, 2), pch = c(20, 17), cex = 0.6,
       legend = c("TWFE", "Sun & Abraham (2020)"))
dev.off()

png("wages_ES_SA.png")
iplot(list(
  feols(log(wage) ~ i(ttt, treated, ref = -1) + age + age^2 + educ + Female + urban | ISIC^month + year,
        weights = ~ weight,
        vcov = ~ISIC,
        LFS1520),
  feols(log(wage) ~ sunab(ytt, year) + age + age^2 + educ + Female + urban | ISIC^month + year,
        weights = ~ weight,
        vcov = ~ISIC,
        LFS1520)), sep = 0.2, ref.line = -1,
  main = "",  
  xlab = 'Time to treatment')
legend("bottomleft", col = c(1, 2), pch = c(20, 17), cex = 0.6,
       legend = c("TWFE", "Sun & Abraham (2020)"))
dev.off()

png("wageperh_ES_SA.png")
iplot(list(
  feols(log(wage_perh) ~ i(ttt, treated, ref = -1) + age + age^2 + educ + Female + urban | ISIC^month + year,
        weights = ~ weight,
        vcov = ~ISIC,
        LFS1520),
  feols(log(wage_perh) ~ sunab(ytt, year) + age + age^2 + educ + Female + urban | ISIC^month + year,
        weights = ~ weight,
        vcov = ~ISIC,
        LFS1520)), sep = 0.2, ref.line = -1,
  main = "",  
  xlab = 'Time to treatment')
legend("bottomleft", col = c(1, 2), pch = c(20, 17), cex = 0.6,
       legend = c("TWFE", "Sun & Abraham (2020)"))
dev.off()

# GENDER DIFFERENCES 
png("work_ES_gender.png")
iplot(list(
  feols(work ~ i(ttt, treated, ref = -1) + age + age^2 + educ + urban | ISIC^month + year,
        weights = ~ weight,
        vcov = ~ISIC,
        subset(LFS1520, Female == 0)),
  feols(work ~ i(ttt, treated, ref = -1) + age + age^2 + educ + urban | ISIC^month + year,
        weights = ~ weight,
        vcov = ~ISIC,
        subset(LFS1520, Female == 1))), sep = 0.2, ref.line = -1,
  main = "",  
  xlab = 'Time to treatment')
legend("bottomleft", col = c(1, 2), pch = c(20, 17), cex = 0.5,
       legend = c("Male", "Female"))
dev.off()

png("hours_ES_gender.png")
iplot(list(
  feols(hours ~ i(ttt, treated, ref = -1) + age + age^2 + educ + urban | ISIC^month + year,
        weights = ~ weight,
        vcov = ~ISIC,
        subset(LFS1520, Female == 0)),
  feols(hours ~ i(ttt, treated, ref = -1) + age + age^2 + educ + urban | ISIC^month + year,
        weights = ~ weight,
        vcov = ~ISIC,
        subset(LFS1520, Female == 1))), sep = 0.2, ref.line = -1,
  main = "",  
  xlab = 'Time to treatment')
legend("bottomleft", col = c(1, 2), pch = c(20, 17), cex = 0.5,
       legend = c("Male", "Female"))
dev.off()

png("wages_ES_gender.png")
iplot(list(
  feols(log(wage) ~ i(ttt, treated, ref = -1) + age + age^2 + educ + urban | ISIC^month + year,
        weights = ~ weight,
        vcov = ~ISIC,
        subset(LFS1520, Female == 0)),
  feols(log(wage) ~ i(ttt, treated, ref = -1) + age + age^2 + educ + urban | ISIC^month + year,
        weights = ~ weight,
        vcov = ~ISIC,
        subset(LFS1520, Female == 1))), sep = 0.2, ref.line = -1,
  main = "",  
  xlab = 'Time to treatment')
legend("bottomleft", col = c(1, 2), pch = c(20, 17), cex = 0.5,
       legend = c("Male", "Female"))
dev.off()

png("wageperh_ES_gender.png")
iplot(list(
  feols(log(wage_perh) ~ i(ttt, treated, ref = -1) + age + age^2 + educ + urban | ISIC^month + year,
        weights = ~ weight,
        vcov = ~ISIC,
        subset(LFS1520, Female == 0)),
  feols(log(wage_perh) ~ i(ttt, treated, ref = -1) + age + age^2 + educ + urban | ISIC^month + year,
        weights = ~ weight,
        vcov = ~ISIC,
        subset(LFS1520, Female == 1))), sep = 0.2, ref.line = -1,
  main = "",  
  xlab = 'Time to treatment')
legend("bottomleft", col = c(1, 2), pch = c(20, 17), cex = 0.5,
       legend = c("Male", "Female"))
dev.off()

# Sun and Abraham 
png("work_SA_gender.png")
iplot(list(
  feols(work ~ sunab(ytt, year) + age + age^2 + educ + urban | ISIC^month + year,
        weights = ~ weight,
        vcov = ~ISIC,
        subset(LFS1520, Female == 0)),
  feols(work ~ sunab(ytt, year) + age + age^2 + educ + urban | ISIC^month + year,
        weights = ~ weight,
        vcov = ~ISIC,
        subset(LFS1520, Female == 1))), sep = 0.2, ref.line = -1,
  xlab = 'Time to treatment')
legend("bottomleft", col = c(1, 2), pch = c(20, 17), cex = 0.5,
       legend = c("Male", "Female"))
dev.off()

png("hours_SA_gender.png")
iplot(list(
  feols(hours ~ sunab(ytt, year) + age + age^2 + educ + urban | ISIC^month + year,
        weights = ~ weight,
        vcov = ~ISIC,
        subset(LFS1520, Female == 0)),
  feols(hours ~ sunab(ytt, year) + age + age^2 + educ + urban | ISIC^month + year,
        weights = ~ weight,
        vcov = ~ISIC,
        subset(LFS1520, Female == 1))), sep = 0.2, ref.line = -1,
  xlab = 'Time to treatment')
legend("bottomleft", col = c(1, 2), pch = c(20, 17), cex = 0.5,
       legend = c("Male", "Female"))
dev.off()

png("wages_ES_gender.png")
iplot(list(
  feols(log(wage) ~ i(ttt, treated, ref = -1) + age + age^2 + educ + urban | ISIC^month + year,
        weights = ~ weight,
        vcov = ~ISIC,
        subset(LFS1520, Female == 0)),
  feols(log(wage) ~ i(ttt, treated, ref = -1) + age + age^2 + educ + urban | ISIC^month + year,
        weights = ~ weight,
        vcov = ~ISIC,
        subset(LFS1520, Female == 1))), sep = 0.2, ref.line = -1,
  xlab = 'Time to treatment')
legend("bottomleft", col = c(1, 2), pch = c(20, 17), cex = 0.5,
       legend = c("Male", "Female"))
dev.off()

png("wageperh_ES_gender.png")
iplot(list(
  feols(log(wage_perh) ~ i(ttt, treated, ref = -1) + age + age^2 + educ + urban | ISIC^month + year,
        weights = ~ weight,
        vcov = ~ISIC,
        subset(LFS1520, Female == 0)),
  feols(log(wage_perh) ~ i(ttt, treated, ref = -1) + age + age^2 + educ + urban | ISIC^month + year,
        weights = ~ weight,
        vcov = ~ISIC,
        subset(LFS1520, Female == 1))), sep = 0.2, ref.line = -1,
  xlab = 'Time to treatment')
legend("bottomleft", col = c(1, 2), pch = c(20, 17), cex = 0.5,
       legend = c("Male", "Female"))
dev.off()
