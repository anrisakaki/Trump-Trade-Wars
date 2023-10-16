# TWFE USING ENTERPRISE CENSUS # 

load("dn1419.rda")

dn1419 <- dn1419 %>% 
  # replacing never treated units per fixest package 
  mutate(first_treated = ifelse(willbe_treated == 0, 10000, first_treated))

dn1419_balanced <- dn1419_balanced %>% 
  # replacing never treated units per fixest package 
  mutate(first_treated = ifelse(willbe_treated == 0, 10000, first_treated))


# Event studies 

## wage cost

etable(list(
  feols(log(wage) ~ i(ttt, treated17, -1)| id + year,
        dn1419),
  feols(log(wage) ~ i(ttt, willbe_treated, -1)| id + year,
        dn1419)))

iplot(feols(log(wage) ~ i(ttt, treated, -1)| id + year,
            dn1419), main = "Effect on labour cost")

### with Sun and Abraham 

etable(feols(log(wage) ~ sunab(first_treated, year)| id + year,
            dn1419))

iplot(feols(log(wage) ~ sunab(first_treated, year)| id + year,
            subset(dn1419, manu == 1)), main = "Effect on labour cost")

## firm size 

iplot(feols(log(n_workers) ~ i(ttt, willbe_treated, -1)| id + year,
            dn1419), main = "Effect on firm size")

iplot(feols(log(n_workers) ~ i(ttt, willbe_treated, -1)| id + year,
            subset(dn1419, manu == 1)), main = "Effect on firm size")

### with Sun and Abraham 

iplot(feols(log(n_workers) ~ sunab(first_treated, year)| id + year,
            dn1419), main = "Effect on firm size")

iplot(feols(log(n_workers) ~ sunab(first_treated, year)| id + year,
            subset(dn1419, manu == 1)), main = "Effect on firm size")

# Share of female workers 

iplot(feols(fworkers ~ i(ttt, willbe_treated, -1)| id + year,
            dn1419), main = "Effect on share of female workers")

iplot(feols(fworkers ~ i(ttt, willbe_treated, -1)| id + year,
            subset(dn1419, manu == 1)), main = "Effect on share of female workers")


# switching to making new products and entering / exiting 

iplot(feols(treated ~ i(ttt, willbe_treated, -1)| id + year,
            dn1419))

# Exporting 

iplot(feols(export ~ i(ttt, willbe_treated, -1)| id + year,
            dn1419))
