# ##################################################
# # MATCHING LFS DATA WITH HS-10 TRUMP TARIFF DATA #
# ##################################################
# 
# # Converting HS10 into HS6 data 
# HS_trump_17 <- hs10_trump %>% 
#   filter(year == 2017)
# 
# HS_trump_18 <- hs10_trump %>% 
#   filter(year == 2018)
# 
# HS_trump_19 <- hs10_trump %>% 
#   filter(year == 2019)
# 
# # Creating HS6 codes in HS10 data 
# 
# HS_1520 <- c("HS_trump_17", "HS_trump_18", "HS_trump_19")
# 
# for(i in HS_1520){
#   assign(i, get(i) %>% 
#            mutate(hs6 = substr(hs10,1,6)))
# }

# Cleaning ISIC_Trump file 
isic_trump_18 <- isic_trump %>% 
  filter(year == 2018) %>% 
  filter(!is.na(ISIC)) %>% 
  mutate(treated = as.numeric(tariff_maxCHN > 0))

isic_trump_19 <- isic_trump %>% 
  filter(year == 2019) %>% 
  filter(!is.na(ISIC)) %>% 
  mutate(treated = as.numeric(tariff_maxCHN > 0))

LFS_2018 <- left_join(LFS_2018, isic_trump_18, by =c("ISIC", "month"))
LFS_2019 <- left_join(LFS_2019, isic_trump_19, by = c("ISIC", "month"))

for(i in LFS_1520){
  
  if (i %in% c("LFS_2015", "LFS_2016", "LFS_2017", "LFS_2020")){
    assign(i, get(i) %>% 
             mutate(treated = 0,
                    product_lines_affected = 0,
                    share_HS_lines_affected = 0,
                    nb_of_lines = 0))
  }
}
