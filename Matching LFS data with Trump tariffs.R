##################################################
# MATCHING LFS DATA WITH HS-10 TRUMP TARIFF DATA #
##################################################

# Cleaning ISIC_Trump file 

LFS_2018 <- left_join(LFS_2018, us_chn_tariffs_18, by =c("ISIC", "month"))
LFS_2019 <- left_join(LFS_2019, isic_trump_19, by = c("ISIC", "month", "year"))

for(i in LFS_1520){
  
  if (i %in% c("LFS_2015", "LFS_2016", "LFS_2017")){
    assign(i, get(i) %>% 
             mutate(treated = 0,
                    product_lines_affected = 0,
                    share_HS_lines_affected = 0,
                    nb_of_lines = 0))
  }
}

  