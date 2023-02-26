##############################
# SETTING UP FOR TWFE MODELS # 
##############################

LFS_2017 <- LFS_2017 %>% 
  mutate(across(educ, as.numeric))

LFS_2019 <- LFS_2019 %>% 
  mutate(treated = ifelse(is.na(treated), 0 , treated))

LFS_1719 <- bind_rows(LFS_2017, LFS_2019)

LFS_1819 <- bind_rows(LFS_2018, LFS_2019)

################################
# TWO-WAY FIXED EFFECTS MODELS #
################################

y <- c("formal", "casual_contract", "work", "log(hours)", "log(wage)")

# 2017 - 2019 
dummy_models_wcontrols <- list()

for(i in y){
  formula <- as.formula(paste(i, "~ i(treated) + Female + age + age^2 + educ + urban | month^ISIC"))
  
  model <- feols(formula,
                 subset(LFS_1719, age > 17 & age < 66),
                 vcov = ~ISIC,
                 weights = ~weight)
  
  dummy_models_wcontrols[[i]] <- model
  
}

dummy_models_wcontrols_f <- list()

for(i in y){
  formula <- as.formula(paste(i, "~ i(treated) + age + age^2 + educ + urban | month^ISIC"))
  
  model <- feols(formula,
                 subset(LFS_1719, age > 17 & age < 66 & Female == 1),
                 vcov = ~ISIC,
                 weights = ~weight)
  
  dummy_models_wcontrols_f[[i]] <- model
  
}

dummy_models_wcontrols_m <- list()

for(i in y){
  formula <- as.formula(paste(i, "~ as.factor(treated) + age + age^2 + educ + urban | month^ISIC"))
  
  model <- feols(formula,
                 subset(LFS_1719, age > 17 & age < 66 & Female == 0),
                 vcov = ~ISIC,
                 weights = ~weight)
  
  dummy_models_wcontrols_m[[i]] <- model
  
}

tariffmax_models <- list()

for(i in y){
  formula <- as.formula(paste(i, "~ tariff_max + i(Female) + age + age^2 + educ + urban | month^ISIC"))
  
  model <- feols(formula,
                 subset(LFS_1719, age > 17 & age < 66),
                 vcov = ~ISIC,
                 weights = ~weight)
  
  tariffmax_models[[i]] <- model
  
}

tariffmax_models_f <- list()

for(i in y){
  formula <- as.formula(paste(i, "~ tariff_max + age + age^2 + educ + urban | month^ISIC"))
  
  model <- feols(formula,
                 subset(LFS_1719, age > 17 & age < 66 & Female == 1),
                 vcov = ~ISIC,
                 weights = ~weight)
  
  tariffmax_models_f[[i]] <- model
  
}

tariffmax_models_m <- list()

for(i in y){
  formula <- as.formula(paste(i, "~ tariff_max + age + age^2 + educ + urban | month^ISIC"))
  
  model <- feols(formula,
                 subset(LFS_1719, age > 17 & age < 65 & Female == 0),
                 vcov = ~ISIC,
                 weights = ~weight)
  
  tariffmax_models_m[[i]] <- model
  
}