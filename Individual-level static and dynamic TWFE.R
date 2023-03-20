for (i in LFS_1520){
  assign(i, full_join(get(i), us_chn_tariff, by = "ISIC"))
  
  assign(i, get(i) %>% 
           mutate(first_treated = recode(effective_mdate, 
                                         '697' = '201802',
                                         '698' = '201803',
                                         '702' = '201807',
                                         '703' = '201808',
                                         '704' = '201809',
                                         '716' = '201909'),
                  treated = ifelse(year_month > first_treated, 1, 0),
                  first_treated = ifelse(is.na(first_treated), 0, first_treated),
                  treated = ifelse(is.na(treated), 0, treated),
                  treat = ifelse(first_treated > 0, 1, 0)))
}

LFS1520 <- bind_rows()