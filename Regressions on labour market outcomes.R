##############################
# SETTING UP FOR TWFE MODELS # 
##############################

LFS_2017 <- LFS_2017 %>% 
  mutate(year = 2017) %>% 
  mutate(across(educ, as.numeric))

LFS_2019 <- LFS_2019 %>% 
  mutate(year = 2019,
         treated = ifelse(is.na(treated), 0 , treated))

LFS_1719 <- bind_rows(LFS_2017, LFS_2019)

################################
# TWO-WAY FIXED EFFECTS MODELS #
################################

y <- c("work", "formal", "casual_contract", "hours", "log(wage)")

simple_dummy_models <- list()

for(i in y){
  formula <- as.formula(paste(i, "~ as.factor(treated) | year^month + ISIC"))
  
  model <- feols(formula,
                 LFS_1719,
                 vcov = ~ISIC,
                 weights = ~weight)
  
  simple_dummy_models[[i]] <- model
  
}

dummy_models_wcontrols <- list()

for(i in y){
  formula <- as.formula(paste(i, "~ as.factor(treated) + age + age^2 + educ + urban | year^month + ISIC"))
  
  model <- feols(formula,
                 subset(LFS_1719, age > 17 & age < 65),
                 vcov = ~ISIC,
                 weights = ~weight)
  
  dummy_models_wcontrols[[i]] <- model
  
}

productlines_models <- list()

for(i in y){
  formula <- as.formula(paste(i, "~ product_lines_affected | year^month + ISIC"))
  
  model <- feols(formula,
                 subset(LFS_1719, age > 17 & age < 65),
                 vcov = ~ISIC,
                 weights = ~weight)
  
  productlines_models[[i]] <- model
  
}

productlines_models_controls <- list()

for(i in y){
  formula <- as.formula(paste(i, "~ product_lines_affected + age + age^2 + educ + urban | year^month + ISIC"))
  
  model <- feols(formula,
                 subset(LFS_1719, age > 17 & age < 65),
                 vcov = ~ISIC,
                 weights = ~weight)
  
  productlines_models_controls[[i]] <- model
  
}

###################
# MODEL SUMMARIES #
###################

etable(list(
  simple_dummy_models[[1]],
  simple_dummy_models[[2]],
  simple_dummy_models[[3]],
  simple_dummy_models[[4]],
  simple_dummy_models[[5]]
), tex = TRUE)

etable(list(
  dummy_models_wcontrols[[1]],
  dummy_models_wcontrols[[2]],
  dummy_models_wcontrols[[3]],
  dummy_models_wcontrols[[4]],
  dummy_models_wcontrols[[5]]
), tex = TRUE)

etable(list(
  productlines_models[[1]],
  productlines_models[[2]],
  productlines_models[[3]],
  productlines_models[[4]],
  productlines_models[[5]]
), tex = TRUE)

etable(list(
  productlines_models_controls[[1]],
  productlines_models_controls[[2]],
  productlines_models_controls[[3]],
  productlines_models_controls[[4]],
  productlines_models_controls[[5]]
), tex = TRUE)
