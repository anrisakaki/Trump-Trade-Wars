##############################
# SETTING UP FOR TWFE MODELS # 
##############################

LFS_2017 <- LFS_2017 %>% 
  mutate(year = 2017,
         across(educ, as.numeric))

LFS_2019 <- LFS_2019 %>% 
  mutate(year = 2019,
         treated = ifelse(is.na(treated), 0 , treated),
         product_lines_affected = ifelse(is.na(product_lines_affected), 0, product_lines_affected),
         nb_of_lines = ifelse(is.na(nb_of_lines), 0, nb_of_lines),
         share_HS_lines_affected = ifelse(is.na(share_HS_lines_affected), 0, share_HS_lines_affected))

LFS_1719 <- bind_rows(LFS_2017, LFS_2019)

################################
# TWO-WAY FIXED EFFECTS MODELS #
################################

y <- c("work", "formal", "casual_contract", "hours", "log(wage)")

dummy_models_wcontrols <- list()

for(i in y){
  formula <- as.formula(paste(i, "~ as.factor(treated) + age + age^2 + educ + urban | year^month + ISIC"))
  
  model <- feols(formula,
                 subset(LFS_1719, age > 17 & age < 65),
                 vcov = ~ISIC,
                 weights = ~weight)
  
  dummy_models_wcontrols[[i]] <- model
  
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

nb_lines_affected <- list()

for(i in y){
  formula <- as.formula(paste(i, "~ nb_of_lines + age + age^2 + educ + urban | year^month + ISIC"))
  
  model <- feols(formula,
                 subset(LFS_1719, age > 17 & age < 65),
                 vcov = ~ISIC,
                 weights = ~weight)
  
  nb_lines_affected[[i]] <- model
  
}

share_HS_lines_affected <- list()

for(i in y){
  formula <- as.formula(paste(i, "~ share_HS_lines_affected + age + age^2 + educ + urban | year^month + ISIC"))
  
  model <- feols(formula,
                 subset(LFS_1719, age > 17 & age < 65),
                 vcov = ~ISIC,
                 weights = ~weight)
  
  share_HS_lines_affected[[i]] <- model
  
}


###################
# MODEL SUMMARIES #
###################

etable(dummy_models_wcontrols)

etable(list(
  productlines_models_controls[[1]],
  productlines_models_controls[[2]],
  productlines_models_controls[[3]],
  productlines_models_controls[[4]],
  productlines_models_controls[[5]]
))

etable(nb_lines_affected)

etable(share_HS_lines_affected)
