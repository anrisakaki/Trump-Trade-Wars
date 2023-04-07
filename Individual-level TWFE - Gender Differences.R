# Male 

etable(list(
  feols(work ~ i(ttt, treated, ref = -1) + age + age^2 + educ + urban | ISIC^month + year,
        weights = ~ weight,
        vcov = ~ISIC,
        subset(LFS1520, Female == 0)),
  feols(hours ~ i(ttt, treated, ref = -1) + age + age^2 + educ + urban | ISIC^month + year,
        weights = ~ weight,
        vcov = ~ISIC,
        subset(LFS1520, Female == 0)),
  feols(log(wage) ~ i(ttt, treated, ref = -1) + age + age^2 + educ + urban | ISIC^month + year,
        weights = ~ weight,
        vcov = ~ISIC,
        subset(LFS1520, Female == 0)),
  feols(log(wage_perh) ~ i(ttt, treated, ref = -1) + age + age^2 + educ + urban | ISIC^month + year,
        weights = ~ weight,
        vcov = ~ISIC,
        subset(LFS1520, Female == 0))  
), tex = TRUE)

# Female 