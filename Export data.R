###################################
# SETTING UP DATA FOR FIRM EXPORT #
###################################

exp14 <- DN_2014 %>% 
  rename(export = co_xk,
         exp_value = tg_xktt) %>% 