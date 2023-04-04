png("work_TWFE.png")
iplot(feols(work ~ i(year, treat, 2018) + age + age^2 + educ + Female + urban | ISIC^month + year,
            weights = ~ weight,
            vcov = ~ ISIC,
            LFS1520))
dev.off()

png("hours_TWFE.png")
iplot(feols(hours ~ i(year, treat, 2018) + age + age^2 + educ + Female + urban | ISIC^month + year,
              weights = ~ weight,
              vcov = ~ ISIC,
              LFS1520))
dev.off()

png("wages_TWFE.png")
iplot(feols(log(wage) ~ i(year, treat, 2018) + age + age^2 + educ + Female + urban | ISIC^month + year,
            weights = ~ weight,
            vcov = ~ ISIC,
            LFS1520))
dev.off()

png("work_sunab.png")
iplot(feols(work ~ sunab(year_ft, year) | ISIC^month + year,
        weights = ~ weight,
        vcov = ~ISIC,
        LFS1520))
dev.off()

png("hours_sunab.png")
iplot(feols(hours ~ sunab(year_ft, year) | ISIC^month + year,
            weights = ~ weight,
            vcov = ~ISIC,
            LFS1520))
dev.off()

png("wages_sunab.png")
iplot(feols(log(wage) ~ sunab(year_ft, year) | ISIC^month + year,
            weights = ~ weight,
            vcov = ~ISIC,
            LFS1520))
dev.off()
