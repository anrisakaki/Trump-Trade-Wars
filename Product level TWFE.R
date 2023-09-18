# TWFE USING ENTERPRISE CENSUS # 

load("dn1419.rda")

dn1419 <- dn1419 %>% 
  # replacing never treated units per fixest package 
  mutate(first_treated = ifelse(is.na(first_treated), 10000, first_treated))

# event studies 

iplot(feols(log(wage) ~ i(ttt, treated17, -1)| id + year,
            dn1419), main = "Effect on labour cost")

iplot(feols(log(wage) ~ i(ttt, treated17, -1)| id + year,
            subset(dn1419, manu == 1)), main = "Effect on labour cost")

iplot(feols(log(n_workers) ~ i(ttt, treated17, -1)| id + year,
            dn1419), main = "Effect on firm size")

iplot(feols(log(n_workers) ~ i(ttt, treated17, -1)| id + year,
            subset(dn1419, manu == 1)), main = "Effect on firm size")

iplot(feols(fworkers ~ i(ttt, treated17, -1)| id + year,
            dn1419), main = "Effect on share of \nfemale workers")

iplot(feols(fworkers ~ i(ttt, treated17, -1)| id + year,
            subset(dn1419, manu == 1)), main = "Effect on share of \nfemale workers")

iplot(feols(export ~ i(ttt, treated, -1)| id + year,
            dn1419), main = "Effect on exporting")

iplot(feols(export ~ i(ttt, treated, -1)| id + year,
            subset(dn1419, manu == 1)), main = "Effect on exporting")


# switching to making new products and entering / exiting 

