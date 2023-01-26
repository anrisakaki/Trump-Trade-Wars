isic_trump <- isic_trump %>% 
  rename(year_tariff = year)

for (i in LFS_1520){
  assign(i, left_join(get(i), isic_trump, by = "ISIC"))
}
