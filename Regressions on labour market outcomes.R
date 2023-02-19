##############################
# SETTING UP FOR TWFE MODELS # 
##############################

LFS_2017 <- LFS_2017 %>% 
  mutate(across(educ, as.numeric))

LFS_2019 <- LFS_2019 %>% 
  mutate(treated = ifelse(is.na(treated), 0 , treated),
         product_lines_affected = ifelse(is.na(product_lines_affected), 0, product_lines_affected),
         nb_of_lines = ifelse(is.na(nb_of_lines), 0, nb_of_lines))

LFS_1719 <- bind_rows(LFS_2017, LFS_2019)

LFS_1819 <- bind_rows(LFS_2018, LFS_2019)

################################
# TWO-WAY FIXED EFFECTS MODELS #
################################

y <- c("formal", "casual_contract", "log(hours)", "log(wage)")

# 2017 - 2019 
dummy_models_wcontrols <- list()

for(i in y){
  formula <- as.formula(paste(i, "~ as.factor(treated) + Female + age + age^2 + educ + urban | month + ISIC"))
  
  model <- feols(formula,
                 subset(LFS_1719, age > 17 & age < 66),
                 vcov = ~ISIC,
                 weights = ~weight)
  
  dummy_models_wcontrols[[i]] <- model
  
}

dummy_models_wcontrols_f <- list()

for(i in y){
  formula <- as.formula(paste(i, "~ as.factor(treated) + as.factor(Female) + age + age^2 + educ + urban | month + ISIC"))
  
  model <- feols(formula,
                 subset(LFS_1719, age > 17 & age < 66 & Female == 1),
                 vcov = ~ISIC,
                 weights = ~weight)
  
  dummy_models_wcontrols_f[[i]] <- model
  
}

dummy_models_wcontrols_m <- list()

for(i in y){
  formula <- as.formula(paste(i, "~ as.factor(treated) + age + age^2 + educ + urban | month + ISIC"))
  
  model <- feols(formula,
                 subset(LFS_1719, age > 17 & age < 66 & Female == 0),
                 vcov = ~ISIC,
                 weights = ~weight)
  
  dummy_models_wcontrols_m[[i]] <- model
  
}

productlines_models_controls <- list()

for(i in y){
  formula <- as.formula(paste(i, "~ product_lines_affected + as.factor(Female) + age + age^2 + educ + urban | month + ISIC"))
  
  model <- feols(formula,
                 subset(LFS_1719, age > 17 & age < 66),
                 vcov = ~ISIC,
                 weights = ~weight)
  
  productlines_models_controls[[i]] <- model
  
}

productlines_models_controls_f <- list()

for(i in y){
  formula <- as.formula(paste(i, "~ product_lines_affected + age + age^2 + educ + urban | month + ISIC"))
  
  model <- feols(formula,
                 subset(LFS_1719, age > 17 & age < 66 & Female == 1),
                 vcov = ~ISIC,
                 weights = ~weight)
  
  productlines_models_controls_f[[i]] <- model
  
}

nb_lines_affected <- list()

for(i in y){
  formula <- as.formula(paste(i, "~ nb_of_lines + as.factor(Female) + age + age^2 + educ + urban | month + ISIC"))
  
  model <- feols(formula,
                 subset(LFS_1719, age > 17 & age < 65),
                 vcov = ~ISIC,
                 weights = ~weight)
  
  nb_lines_affected[[i]] <- model
  
}

nb_lines_affected_f <- list()

for(i in y){
  formula <- as.formula(paste(i, "~ nb_of_lines + age + age^2 + educ + urban | month + ISIC"))
  
  model <- feols(formula,
                 subset(LFS_1719, age > 17 & age < 65 & Female == 1),
                 vcov = ~ISIC,
                 weights = ~weight)
  
  nb_lines_affected_f[[i]] <- model
  
}

nb_lines_affected_m <- list()

for(i in y){
  formula <- as.formula(paste(i, "~ nb_of_lines + age + age^2 + educ + urban | month + ISIC"))
  
  model <- feols(formula,
                 subset(LFS_1719, age > 17 & age < 65 & Female == 0),
                 vcov = ~ISIC,
                 weights = ~weight)
  
  nb_lines_affected_m[[i]] <- model
  
}



share_HS_lines_affected <- list()

for(i in y){
  formula <- as.formula(paste(i, "~ share_HS_lines_affected + age + age^2 + educ + urban | month + ISIC"))
  
  model <- feols(formula,
                 subset(LFS_1719, age > 17 & age < 65),
                 vcov = ~ISIC,
                 weights = ~weight)
  
  share_HS_lines_affected[[i]] <- model
  
}

# 2018 - 2019 

tariff_CHN_1819 <- list()

for(i in y){
  formula <- as.formula(paste(i, "~ tariff_maxCHN + as.factor(Female) + age + age^2 + educ + urban | month + ISIC"))
  
  model <- feols(formula,
                 subset(LFS_1819, age > 17 & age < 65),
                 vcov = ~ISIC,
                 weights = ~weight)
  
  tariff_CHN_1819[[i]] <- model
  
}

tariff_CHN_1819_f <- list()

for(i in y){
  formula <- as.formula(paste(i, "~ tariff_maxCHN + age + age^2 + educ + urban | month + ISIC"))
  
  model <- feols(formula,
                 subset(LFS_1819, age > 17 & age < 65 & Female == 1),
                 vcov = ~ISIC,
                 weights = ~weight)
  
  tariff_CHN_1819_f[[i]] <- model
  
}

tariff_CHN_1819_m <- list()

for(i in y){
  formula <- as.formula(paste(i, "~ tariff_maxCHN + age + age^2 + educ + urban | month + ISIC"))
  
  model <- feols(formula,
                 subset(LFS_1819, age > 17 & age < 65 & Female == 0),
                 vcov = ~ISIC,
                 weights = ~weight)
  
  tariff_CHN_1819_m[[i]] <- model
  
}

