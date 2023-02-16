#####################################
# SETTING UP FOR FIRST-DIFFERENCING #
#####################################

sector_18 <- full_join(sector_18, isic_trump_18, by = c("ISIC", "month"))

sector_19 <- full_join(sector_19, isic_trump_19, by = c("ISIC", "month"))

sector_1819 <- bind_rows(sector_18, sector_19)

etable(list(
  feols(female_ratio ~ tariff_maxCHN | ISIC + month,
        sector_1819, 
        vcov = ~ISIC),
  feols(formal_ratio ~ tariff_maxCHN | ISIC + month,
        sector_1819, 
        vcov = ~ISIC),
  feols(n_workers ~ tariff_maxCHN | ISIC + month,
        sector_1819, 
        vcov = ~ISIC)  
), tex = TRUE)
