#####################################
# SETTING UP FOR FIRST-DIFFERENCING #
#####################################

sector_17 <- sector_17 %>% 
  mutate(treated = 0,
         product_lines_affectedCHN = 0,
         nb_of_linesCHN = 0)

sector_18 <- full_join(sector_18, isic_trump_18, by = c("ISIC", "month"))

sector_19 <- full_join(sector_19, isic_trump_19, by = c("ISIC", "month"))

sector_1719 <- bind_rows(sector_17, sector_19) %>% 
  mutate(treated = ifelse(is.na(treated), 0 , treated),
         product_lines_affectedCHN = ifelse(is.na(product_lines_affected), 0, product_lines_affected),
         nb_of_linesCHN = ifelse(is.na(nb_of_lines), 0, nb_of_lines))  

sector_1819 <- bind_rows(sector_18, sector_19) %>% 
  mutate(treated = ifelse(is.na(treated), 0 , treated),
         product_lines_affectedCHN = ifelse(is.na(product_lines_affected), 0, product_lines_affected),
         nb_of_linesCHN = ifelse(is.na(nb_of_lines), 0, nb_of_lines))

##################################
# FIRST-DIFFERENCING REGRESSIONS # 
##################################

# 2017 - 2019 
etable(list(
  feols(female_ratio ~ treated | ISIC + month,
        sector_1719, 
        vcov = ~ISIC),
  feols(formal_ratio ~ treated | ISIC + month,
        sector_1719, 
        vcov = ~ISIC),
  feols(n_workers ~ treated | ISIC + month,
        sector_1719, 
        vcov = ~ISIC)  
), tex = TRUE)

etable(list(
  feols(female_ratio ~ product_lines_affected | ISIC + month,
        sector_1719, 
        vcov = ~ISIC),
  feols(formal_ratio ~ product_lines_affected | ISIC + month,
        sector_1719, 
        vcov = ~ISIC),
  feols(n_workers ~ product_lines_affected | ISIC + month,
        sector_1719, 
        vcov = ~ISIC)  
), tex = TRUE)

etable(list(
  feols(female_ratio ~ nb_of_lines | ISIC + month,
        sector_1719, 
        vcov = ~ISIC),
  feols(formal_ratio ~ nb_of_lines | ISIC + month,
        sector_1719, 
        vcov = ~ISIC),
  feols(n_workers ~ nb_of_lines | ISIC + month,
        sector_1719, 
        vcov = ~ISIC)  
), tex = TRUE)

# 2018 - 2019 
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

etable(list(
  feols(female_ratio ~ treated | ISIC + month,
        sector_1819, 
        vcov = ~ISIC),
  feols(formal_ratio ~ treated | ISIC + month,
        sector_1819, 
        vcov = ~ISIC),
  feols(n_workers ~ treated | ISIC + month,
        sector_1819, 
        vcov = ~ISIC)  
), tex = TRUE)

