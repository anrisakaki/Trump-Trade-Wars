###################################
# CLEANING ENTERPRISE CENSUS DATA # 
###################################

DN_2019 <- DN_2019 %>% rename(nganh_kd = manganhc)
DN_2019a <- DN_2019a %>% rename(nganh_kd = manganhc)

DN1417 <- c("DN_2014", "DN_2015", "DN_2016", "DN_2017", "DN_2018")

for(i in DN1417){
  
  assign(i, get(i) %>% 
           mutate(vsic07 = as.numeric(nganh_kd)))
  
  assign(i, left_join(get(i), vsic0793, by = "vsic07")) %>% distinct()
}

vsic0793 <- vsic0793 %>% 
  mutate(vsic07 = ifelse(vsic07 < 10101, str_sub(as.character(vsic07), start = 1, end = 3),
                                    str_sub(as.character(vsic07), start = 1, end = 4))) %>% 
  distinct()

vsic0793$vsic07 <- as.numeric(vsic0793$vsic07)

vsic1807 <- vsic1807 %>% mutate(vsic07 = as.numeric(vsic07))

vsic1893 <- left_join(vsic1807, vsic0793, by = "vsic07") %>% select(vsic18, vsic93) %>% distinct()

DN1819 <- c("DN_2019", "DN_2019a")

for(i in DN1819){
  
  assign(i, get(i) %>% 
           mutate(vsic18 = ifelse(nganh_kd < 10101, str_sub(as.character(nganh_kd), start = 1, end = 3),
                                  str_sub(as.character(nganh_kd), start = 1, end = 4))) %>% 
           mutate(vsic18 = as.numeric(vsic18)))
  
  assign(i, left_join(get(i), vsic1893, by = "vsic18")) %>% distinct()
}

dn14 <- DN_2014 %>% 
  rename(n_workers = tsld,
         n_fworkers = tsldnu,
         n_workers_eoy = ld11,
         n_fworkers_eoy = ld12,
         n_informal = ld31,
         n_finformal = ld32,
         export_value = tgxk_tt, 
         wage = tn1,
         net_turnover = kqkd5,
         net_turnover1 = kqkdc,
         pretax_profit = kqkd22,
         exp_value = tgxk_tt) %>% 
  mutate(year = 2014,
         manu = ifelse(vsic93 < 4010 & vsic93 > 500, 1, 0),
         export = ifelse(co_xk < 2, 1, 0),
         nganh_kd = as.double(nganh_kd)) %>% 
  select("year", nganh_kd, "vsic93", "tinh", "huyen", "xa", "ma_thue", "lhdn", "n_workers", "n_fworkers", "n_workers_eoy", "n_fworkers_eoy", "n_informal",
         "wage", "net_turnover", "pretax_profit", export, exp_value, manu)

dn15 <- DN_2015 %>% 
  rename(n_workers = tsld,
         n_fworkers = tsldnu,
         n_workers_eoy = ld11,
         n_fworkers_eoy = ld12,
         n_informal = ld31,
         n_finformal = ld32,
         wage = tn1,
         exp_value = tgxk_tt, 
         net_turnover = kqkd5,
         net_turnover1 = kqkdc,         
         pretax_profit = kqkd20) %>% 
  mutate(year = 2015,
         export = ifelse(exp_value > 0, 1, 0),
         manu = ifelse(vsic93 < 4010 & vsic93 > 500, 1, 0),
         nganh_kd = as.double(nganh_kd)) %>% 
  select("year", nganh_kd, "vsic93", "tinh", "huyen", "xa", "ma_thue", "lhdn", "n_workers", "n_fworkers", "n_workers_eoy", "n_fworkers_eoy", "n_informal",
         "wage", "net_turnover", "pretax_profit", export, exp_value, manu)

dn_16fdi <- DN_2016_fdi %>% 
  mutate(FDI_oc = nvpd1,
         FDI_share = vpdn11/ vpd11) %>% 
  select(ma_thue, ma_thue2, FDI_oc, FDI_share)

DN_2016 <- left_join(DN_2016, dn_16fdi, by = c("ma_thue", "ma_thue2")) %>% distinct()

dn16 <- DN_2016 %>% 
  rename(n_workers = tsld,
         n_fworkers = tsldnu,
         n_workers_eoy = ld11,
         n_fworkers_eoy = ld21,
         n_informal = ld41,
         wage = tn1,
         net_turnover = kqkd5,
         net_turnover1 = kqkdc,         
         pretax_profit = kqkd20) %>% 
  mutate(year = 2016,
         manu = ifelse(vsic93 < 4010 & vsic93 > 500, 1, 0),
         nganh_kd = as.double(nganh_kd)) %>% 
  select("year", nganh_kd, "vsic93", "tinh", "huyen", "xa", "ma_thue", ma_thue2, "lhdn", "n_workers", "n_fworkers", "n_workers_eoy", "n_fworkers_eoy", "n_informal",
         "wage", "net_turnover", "pretax_profit", "FDI_share", "FDI_oc", manu)

dn17 <- DN_2017 %>% 
  rename(n_workers = tsld,
         n_fworkers = tsldnu,
         n_workers_eoy = ld11,
         n_fworkers_eoy = ld21,
         n_informal = ld41,
         wage = tn1,
         net_turnover = kqkd3,
         net_turnover1 = kqkdc,
         FDI_share = vpd63/vpd13,
         FDI_oc = nvpd1,
         pretax_profit = kqkd7) %>% 
  mutate(year = 2017,
         manu = ifelse(vsic93 < 4010 & vsic93 > 500, 1, 0),
         nganh_kd = as.double(nganh_kd)) %>% 
  select("year", nganh_kd, "vsic93", "tinh", "huyen", "xa", "ma_thue", ma_thue2, "lhdn", "n_workers", "n_fworkers", "n_workers_eoy", "n_fworkers_eoy", "n_informal",
         "wage", "net_turnover", "pretax_profit", "FDI_share", "FDI_oc", manu)

dn18 <- DN_2018 %>% 
  rename(n_workers = tsld,
         n_fworkers = tsldnu,
         n_workers_eoy = ld11,
         n_fworkers_eoy = ld21,
         n_informal = ld41,
         wage = tn1,
         net_turnover = kqkd3,
         net_turnover1 = kqkdc,
         FDI_share = vpd63/vpd13,
         FDI_oc = nvpd1,         
         pretax_profit = kqkd7) %>% 
  mutate(year = 2018,
         manu = ifelse(vsic93 < 4010 & vsic93 > 500, 1, 0),
         nganh_kd = as.double(nganh_kd)) %>% 
  select("year", nganh_kd, "vsic93", "tinh", "huyen", "xa", "ma_thue", ma_thue2, "lhdn", "n_workers", "n_fworkers", "n_workers_eoy", "n_fworkers_eoy", "n_informal",
         "wage", "net_turnover", "pretax_profit", "FDI_share", "FDI_oc", manu)

dn19_fdi <- DN_2019_fdi %>% 
  rename(FDI_oc = manuoc) %>% 
  select(masothue, FDI_oc)

DN_2019 <- bind_rows(DN_2019, DN_2019a)

DN_2019 <- left_join(DN_2019, dn19_fdi, by = "masothue", multiple = "all")

dn19 <- DN_2019 %>% 
  rename(tinh = matinh_d,
         huyen = mahuyen,
         xa = maxa_die,
         ma_thue = masothue,
         lhdn = loaihinh,
         n_workers = solaodon,
         n_fworkers = v23_a,
         n_workers_eoy = v24_a,
         n_fworkers_eoy = v25_a,
         n_informal = v27_a,
         wage = v30_a,
         net_turnover = doanhthu,
         pretax_profit = loinhuan,
         FDI_share = v63_a/vondieul) %>% 
  mutate(year = 2019,
         manu = ifelse(vsic93 < 4010 & vsic93 > 500, 1, 0),
         nganh_kd = as.double(nganh_kd)) %>% 
  select("year", nganh_kd, "vsic93", "tinh", "huyen", "xa", "ma_thue", "lhdn", "n_workers", "n_fworkers", "n_workers_eoy", "n_fworkers_eoy", "n_informal",
         "wage", "net_turnover", "pretax_profit", "FDI_share", "FDI_oc",manu)

###################################
# COMPILING INTO SINGLE DATAFRAME #
###################################

dn_list <- list(dn14, dn15, dn16, dn17, dn18, dn19)

dn <- list()

my_function <- function(df) {
  result <- df %>%
    mutate(
      vsic93 = sprintf("%04d", vsic93),
      isic3 = case_when(
        vsic93 == "0112" ~ "0111",
        vsic93 == "0113" ~ "0111",
        vsic93 == "0114" ~ NA_character_,
        vsic93 == "0115" ~ "0113",
        vsic93 == "0116" ~ "0112",
        vsic93 == "0117" ~ "0111",
        vsic93 == "0123" ~ "0122",
        vsic93 == "9011" ~ "9211",
        vsic93 == "9014" ~ "9214",
        TRUE ~ vsic93
      ))
    
  return(result)
}

for (i in 1:length(dn_list)) {
  df <- dn_list[[i]]
  result <- my_function(df)
  
  # Append the result to the results list
  dn[[i]] <- result
}

dn1419 <- bind_rows(dn[[1]], dn[[2]], dn[[3]], dn[[4]], dn[[5]], dn[[6]]) %>% 
  group_by(tinh, huyen, xa, ma_thue) %>% 
  mutate(id = cur_group_id()) %>% 
  mutate(
         export = ifelse(exp_value > 0, 1, 0),
         export = ifelse(is.na(export), 0 , export),
         exp_value = ifelse(exp_value == 0, NA, exp_value)) %>%
  mutate(
    n_fworkers = ifelse(is.na(n_fworkers), 0 , n_fworkers),
    n_fworkers_eoy = ifelse(is.na(n_fworkers_eoy), 0 , n_fworkers_eoy),
    n_workers = ifelse(is.na(n_workers), 0 , n_workers),
    n_workers_eoy = ifelse(is.na(n_workers_eoy), 0 , n_workers_eoy),
    n_informal = ifelse(is.na(n_informal), 0, n_informal),
    fworkers = n_fworkers/ n_workers,
    fworkers_eoy = n_fworkers_eoy/n_workers_eoy,
    fworkers = ifelse(n_workers == 0, NA, fworkers),
    fworkers = ifelse(n_workers > 0 & n_fworkers == 0, 0, fworkers),
    fworkers_eoy = ifelse(n_workers_eoy == 0, NA, fworkers_eoy),
    fworkers_eoy = ifelse(n_workers_eoy > 0 & n_fworkers_eoy == 0, 0, fworkers_eoy),
    fworkers = ifelse(fworkers > 1 | fworkers < 0, NA, fworkers),
    fworkers_eoy = ifelse(fworkers_eoy > 1 | fworkers_eoy < 0, NA, fworkers_eoy),
    wage = ifelse(is.na(wage), 0, wage),    
    wage = ifelse(n_workers == 0 & wage == 0, NA, wage)
    ) %>% 
  select(-c("ma_thue2")) %>%
  distinct() %>% 
  select(id, year, isic3, everything()) %>% 
  ungroup()

dn1419 <- left_join(dn1419, sp1419_clean, by = c("ma_thue", "year"))

##############################
# CLEANING TRUMP TARIFF DATA #
##############################

us_chn_tariffs <- us_tariffs %>% 
  filter(iso3 == "CHN") %>% 
  select(hs10, effective_mdate) %>% 
  distinct()

us_chn_tariffs$HS6 <- as.numeric(substr(trimws(format(us_chn_tariffs$hs10, scientific = F)), 1, 6))

us_chn_tariffs_HS6 <- us_chn_tariffs %>% 
  select(HS6, effective_mdate)


us_chn_tariffs <- merge(us_chn_tariffs_HS6, hs_i3, by = "HS6") %>% 
  select(HS6, ISIC, effective_mdate)

us_chn_tariffs <- us_chn_tariffs %>%
  mutate(ISIC = str_pad(ISIC, width = 4, pad = "0"),
         first_treated = ifelse(effective_mdate < 716, 2018, 2019)) %>% 
  select(ISIC, first_treated) %>% 
  distinct()
