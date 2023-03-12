##############################
# SETTING UP FOR STATIC TWFE #
##############################

us_chn_tariff <- us_chn_tariff %>% 
  select(ISIC, effective_mdate)

sector_agg <- c("sector_17", "sector_18", "sector_19")

for(i in sector_agg){
  assign(i, full_join(get(i), us_chn_tariff, by = "ISIC"))
    
  assign(i, get(i) %>% 
           mutate(
             first_treated = recode(effective_mdate, 
                                   '697' = '201802',
                                   '698' = '201802',
                                   '702' = '201802',
                                   '703' = '201802',
                                   '704' = '201802',
                                   '716' = '201902'),
             treated = ifelse(effective_mdate > 0, 1, 0)
                  ))
  
  if(i %in% c("sector_17")){
    assign(i, get(i) %>%
             mutate(year_month = year)
    )
  }
  
  if(i %in% c("sector_18")){
      assign(i, get(i) %>%
               mutate(year_month = ifelse(year_month < 201807, 201801, 201802))
      )
  }
    
  if(i %in% c("sector_19")){
      assign(i, get(i) %>%
               mutate(year_month = ifelse(year_month < 201909, 201901, 201902))
      )    
  }
}

sector_1519 <- bind_rows(list(sector_17, sector_18, sector_19))

sector_1519$first_treated <- as.numeric(sector_1519$first_treated)

sector_1519 <- sector_1519 %>% mutate(first_treated = ifelse(is.na(first_treated), 0, first_treated))

################
# DYNAMIC TWFE #
################

sector_dynamic_twfe_a <- att_gt(
  yname = "n_workers",
  gname = "first_treated",
  idname = "ISIC",
  tname = "year_month",
  data = sector_1519,
)

summary(sector_dynamic_twfe_a)

# aggregate group-time ATE 

sector_dynamic_twfe_agg <- aggte(sector_dynamic_twfe, type = "dynamic")

summary(sector_dynamic_twfe_agg)

ggdid(sector_dynamic_twfe_agg)
