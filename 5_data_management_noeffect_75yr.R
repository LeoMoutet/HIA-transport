# Packages
pacman::p_load(dplyr,
               tidyr,
               readxl,
               ggplot2,
               reshape2,
               stringr,
               readr,
               ggpubr,
               here,
               ggnewscale)

# Data
data_ademe_vae <- read_excel(here("Data brut","data ademe vae.xlsx"))
projinsee <- readRDS(here("Data brut","Insee_data.drs"))
distrib_velo_EMP19_par_age <- read.csv(here("Data brut","distri_velo_EMT19_inf50km_jour.csv"))
distrib_marche_EMP2019 <- read_excel(here("Data brut","distrib_marche_EMP2019.xlsx"))
monetarisation <- read.csv2(here("Codes kevin","publication_MT","monetarization_yll.csv"), dec=".")
den <- read.csv(here("Data brut","den.csv"), sep=";")

# Functions
source(here("functions.R"))

# Parameter
Cycle_speed <- 14.9 
walk_speed <- 4.8
eCycle_speed <- 18.1

pop_fr_2015_md <- 0.06655

Cycle_km_2015 <- 6.8/pop_fr_2015_md/52.25
eCycle_km_2015 <- 0.36/pop_fr_2015_md/52.25
walk_km_2015 <- 20/pop_fr_2015_md/52.25
mean_min_2015 <- (Cycle_km_2015/Cycle_speed)*60 + (walk_km_2015/walk_speed)*60 + (eCycle_km_2015/eCycle_speed)*60

Cycle_km_2019 <- 2.24
Walk_km_2019 <- 6.28
mean_min_2019 <- (Cycle_km_2019/Cycle_speed)*60 + (Walk_km_2019/walk_speed)*60

RR_cycle = 0.9
RR_cycle_low = 0.94
RR_cycle_high = 0.87

RR_walk = 0.89
RR_walk_low = 0.96
RR_walk_high = 0.83

Walk_MET = 4
Cycle_MET = 6.8

ratio_eCycle_MET = 0.9 # Bouscasse & al : 4.5/5.8

RR_ecycle = 1-((1-RR_cycle)*ratio_eCycle_MET)
RR_ecycle_low = RR_cycle_low
RR_ecycle_high = RR_cycle_high

Ref_velo = 100
Ref_marche = 168

coef_pi_a <- 0.01
ordo_pi_a <- 0.1

obj_pi_TEND <- 0.75
obj_pi_s1 <- 0.6
obj_pi_s2 <- 0.8
obj_pi_s3 <- 0.8
obj_pi_s4 <- 0.75
obj_delta <- 6.7

#################################### Interpol distances for all years
yr = (2015:2050)

data_ademe = data.frame()

for(s in unique(data_ademe_vae$scenario)){
  for (t in unique(data_ademe_vae$type)) {
    
    Scenarii <- subset(data_ademe_vae,  type == t & scenario == s) %>%
      mutate(gpkm = as.numeric(gpkm), 
             type = t, scenario = s)
    
    dist_tot = approx(Scenarii$year, Scenarii$gpkm, xout = yr)
    
    Scenarii <- Scenarii %>%
      add_row(year = dist_tot$x, gpkm = dist_tot$y)%>%
      mutate (type = t, scenario = s) %>%
      distinct() %>%
      arrange(year)
    
    
    data_ademe = rbind(data_ademe, Scenarii)
  }
}


rm(dist_tot, Scenarii)
rm(s,t,yr)


#################################### Create rows for each ages
data_ademe <- data_ademe %>% select(type, scenario, year, gpkm)

# Ajout des ages
all_age <- (0:100)
df <- data.frame(matrix(NA,100,4)) %>%
  rename(type = X1,
         scenario= X2,
         year= X3,
         gpkm= X4)

data_ademe_age <- data.frame()

for(t in unique(data_ademe$type)){
  for (s in unique(data_ademe$scenario)) {
    for (y in unique(data_ademe$year)) {
      
      clage <- subset(data_ademe,  type == t & scenario == s & year == y) %>%
        mutate(gpkm = as.numeric(gpkm))
      
      clage <- rbind(clage, df)  %>%
        mutate(gpkm = clage$gpkm,
               type = t, scenario = s, year = y,
               age = all_age)
      
      data_ademe_age <- rbind(data_ademe_age,clage)
    }}}

rm(clage,df)
rm(all_age,s,t,y)


# Add ages for each type

data_velo <- subset(data_ademe_age,data_ademe_age$type =="velo")
data_marche <- subset(data_ademe_age,data_ademe_age$type =="marche")
data_vae <-  subset(data_ademe_age,data_ademe_age$type =="VAE")

rm(data_ademe_age)

#################################### Merge INSEE and ADEME projections for each modes

projinsee <- projinsee %>% filter(sexe == "Both")

data_marche <- data_marche %>% filter(year > "2020")
data_velo <- data_velo %>% filter(year > "2020")
data_vae <- data_vae %>% filter(year > "2020")

ademe_marche <- merge(data_marche, projinsee, by = c("age", "year"), all.x = T)
ademe_velo <- merge(data_velo, projinsee, by = c("age", "year"), all.x = T)
ademe_vae <- merge(data_vae, projinsee, by = c("age", "year"), all.x = T)

rm(data_marche,data_velo, data_vae)

##############################################################################################################################
# Distribution of km for each age groups and modes : Rho
# Formating data
distrib_velo_EMP19_par_age <- distrib_velo_EMP19_par_age %>%
  select(c(mean_km_week,age_grp)) %>%
  rename(mean_km = mean_km_week)

# Walk
distrib_marche_fr <- distrib_marche_EMP2019 %>%
  mutate(mean_ind = as.numeric(mean_ind), rho = mean_ind /mean_ind[age_grp == "15-19"]) %>%
  filter (age_grp !=  "90-94",age_grp !=  "95-99") %>%
  select(-c(mean_ind),) %>%
  add_row(age_grp = "0-4", rho = 0 )%>%
  add_row(age_grp = "5-9",  rho = 0 )%>%
  add_row(age_grp = "10-14",  rho = 0 )%>%
  add_row(age_grp = "90-94" ,  rho = 0 )%>%
  add_row(age_grp = "95-99", rho = 0 )%>%
  add_row(age_grp = "100-104", rho = 0 )

ademe_marche <- merge(ademe_marche,distrib_marche_fr, by = "age_grp", all.x = T)


# Cycle
distrib_velo_fr <- distrib_velo_EMP19_par_age %>%
  mutate(rho = mean_km /mean_km[age_grp == "15-19"]) %>%
  select(-c(mean_km),) %>%
  add_row(age_grp = "0-4", rho = 0 )

ademe_velo <- merge(ademe_velo,distrib_velo_fr, by = "age_grp", all.x = T)

# E-Cycle
distrib_vae_fr <- distrib_velo_EMP19_par_age %>%
  mutate(rho = mean_km /mean_km[age_grp == "15-19"]) %>%
  select(-c(mean_km),) %>%
  add_row(age_grp = "0-4", rho = 0 )

ademe_vae <- merge(ademe_vae,distrib_velo_fr, by = "age_grp", all.x = T)
#######################################################################

rm(distrib_velo_EMP19_par_age, distrib_marche_EMP2019)
##############################################################################################################################

# Remplace NA by 0
ademe_marche[is.na(ademe_marche)] <- 0
ademe_velo[is.na(ademe_velo)] <- 0
ademe_vae[is.na(ademe_velo)] <- 0

#################################### Calculation of km per person per year (km_pp_y)
# Walk 
ademe_marche_km <- data.frame()
ademe_marche_km <- km_pp_y(ademe_marche, ademe_marche_km) 

# Cycle
ademe_velo_km <- data.frame()
ademe_velo_km <- km_pp_y(ademe_velo, ademe_velo_km)

# E-Cycle
ademe_vae_km <- data.frame()
ademe_vae_km <- km_pp_y(ademe_vae, ademe_vae_km)

rm(ademe_marche, ademe_vae, ademe_velo)

##############################################################################################################################
# For E-cycle, rho depends on the proportion of e-cycle for each age groups : pi_age 

ademe_vae_km_pi_age <- data.frame()
for (y in unique(ademe_vae_km$year)) {
  for (s in unique(ademe_vae_km$scenario)) {
    
    df <- ademe_vae_km %>% filter(scenario == s, year == y)
    
    # Optimisation of pi and delta
    res <- optim(par = c(coef_pi_a, ordo_pi_a), fn = allocate_km_vae, data = df, obj_pi=ifelse(s=="TEND",obj_pi_TEND,ifelse(s=="S1", obj_pi_s1, ifelse(s=="S2",obj_pi_s2, ifelse(s=="S3",obj_pi_s3, obj_pi_s4)))), obj_delta=obj_delta)
    # Calcul of pi_age
    df$pi_age <- df$age * res$par[1] + res$par[2]
    ademe_vae_km_pi_age <- rbind(ademe_vae_km_pi_age, df)
  }
}

rm(df, res, s, y)

# pi_age = 0 for <15 years and >89 years 
ademe_vae_km_pi_age$age <- as.numeric(ademe_vae_km_pi_age$age)

ademe_vae_km_pi_age$pi_age[ademe_vae_km_pi_age$age <15 | ademe_vae_km_pi_age$age >89 ] <- 0

ademe_vae_km_pi_age <- ademe_vae_km_pi_age[order(ademe_vae_km_pi_age$age,ademe_vae_km_pi_age$year,ademe_vae_km_pi_age$scenario),] 

# Calculation of "pi_corr" that allows distribution of km e-cycled integrating pi and delta
ademe_vae_km_pi_age$pi_corr <- ademe_vae_km_pi_age$rho * ademe_vae_km_pi_age$pi_age

# Calculation of km travelled with pi_corr 
# E-bike
ademe_vae_km <- data.frame()
for (s in unique(ademe_vae_km_pi_age$scenario)) {
  for (y in unique(ademe_vae_km_pi_age$year)) {
    
    df <- ademe_vae_km_pi_age %>%
      filter(scenario == s, year == y)
    
    x <- tapply(df$pop, df$age_grp == "15-19", sum)
    pop_ref <- x [["TRUE"]]
    
    df$popref <-   pop_ref   
    df$rhopop <- df$pi_corr * df$pop
    sum_rhopop <- sum(round(df$rhopop))
    df$km_pp_y <- df$pi_corr*unique(df$gpkm*10^9)/(sum_rhopop)
    
    ademe_vae_km <- rbind(ademe_vae_km,df)
    
  }
}

rm(pop_ref, sum_rhopop, s,x,y, df)
rm(ademe_vae_km_pi_age)

##############################################################################################################################

#################################### Calculation of km per person per week : km_pp_week

ademe_marche_km$km_pp_w <- (((ademe_marche_km$km_pp_y)/(365.25/7)))
ademe_velo_km$km_pp_w <- (((ademe_velo_km$km_pp_y)/(365.25/7)))
ademe_vae_km$km_pp_w <- (((ademe_vae_km$km_pp_y)/(365.25/7)))

#################################### Calculation of min per person per week : min_pp_w

ademe_marche_km$min_pp_w <- (((ademe_marche_km$km_pp_y)/(365.25/7))/walk_speed)*60
ademe_velo_km$min_pp_w <- (((ademe_velo_km$km_pp_y)/(365.25/7))/Cycle_speed)*60
ademe_vae_km$min_pp_w <- (((ademe_vae_km$km_pp_y)/(365.25/7))/eCycle_speed)*60

#################################### Calculation of number of deaths prevented for each scenario

ademe_marche_km$death_prev <- round(n_prev(ademe_marche_km, RR_walk, Ref_marche))
ademe_velo_km$death_prev <- round(n_prev(ademe_velo_km, RR_cycle, Ref_velo))
ademe_vae_km$death_prev <- round(n_prev(ademe_vae_km, RR_ecycle, Ref_velo))   #### RR and Ref_volume of cycle

# Ic 95%
ademe_marche_km$death_prev_low <- round(n_prev(ademe_marche_km, RR_walk_low, Ref_marche))
ademe_velo_km$death_prev_low <- round(n_prev(ademe_velo_km, RR_cycle_low, Ref_velo))
ademe_vae_km$death_prev_low <- round(n_prev(ademe_vae_km, RR_ecycle_low, Ref_velo))   #### RR and Ref_volume of cycle

ademe_marche_km$death_prev_high <- round(n_prev(ademe_marche_km, RR_walk_high, Ref_marche))
ademe_velo_km$death_prev_high <- round(n_prev(ademe_velo_km, RR_cycle_high, Ref_velo))
ademe_vae_km$death_prev_high <- round(n_prev(ademe_vae_km, RR_ecycle_high, Ref_velo))   #### RR and Ref_volume of cycle

# Combine modes
ademe_vae_km <- ademe_vae_km %>% select(-c(pi_corr,pi_age))

ademe_velo_marche <- rbind(ademe_marche_km,ademe_velo_km,ademe_vae_km)

#################################### Order data

ademe_velo_marche$age_grp  <- reorder(ademe_velo_marche$age_grp, as.numeric(gsub("-\\d+","",ademe_velo_marche$age_grp)))
ademe_velo_marche <- ademe_velo_marche[order(ademe_velo_marche$scenario,ademe_velo_marche$age,ademe_velo_marche$year),] 

ademe_marche_km$age_grp  <- reorder(ademe_marche_km$age_grp, as.numeric(gsub("-\\d+","",ademe_marche_km$age_grp)))
ademe_marche_km <- ademe_marche_km[order(ademe_marche_km$scenario,ademe_marche_km$age,ademe_marche_km$year),]  

ademe_velo_km$age_grp  <- reorder(ademe_velo_km$age_grp, as.numeric(gsub("-\\d+","",ademe_velo_km$age_grp)))
ademe_velo_km <- ademe_velo_km[order(ademe_velo_km$scenario,ademe_velo_km$age,ademe_velo_km$year),]  

ademe_vae_km$age_grp  <- reorder(ademe_vae_km$age_grp, as.numeric(gsub("-\\d+","",ademe_vae_km$age_grp)))
ademe_vae_km <- ademe_vae_km[order(ademe_vae_km$scenario,ademe_vae_km$age,ademe_vae_km$year),]  


#################################### Number of deaths prevented compared to BAU
ademe_velo_marche$diff_death <- diff_death(ademe_velo_marche,5)
ademe_velo_marche$diff_death_low <- diff_death_low(ademe_velo_marche,5)
ademe_velo_marche$diff_death_high <- diff_death_high(ademe_velo_marche,5)

#################################### YLL
ademe_velo_marche$yll <- ademe_velo_marche$diff_death * (ademe_velo_marche$life_exp - ademe_velo_marche$age)
ademe_velo_marche$yll_low <- ademe_velo_marche$diff_death_low * (ademe_velo_marche$life_exp - ademe_velo_marche$age)
ademe_velo_marche$yll_high <- ademe_velo_marche$diff_death_high * (ademe_velo_marche$life_exp - ademe_velo_marche$age)

# No YLL if age > life expectancy
ademe_velo_marche$yll_max <- ifelse(ademe_velo_marche$life_exp < ademe_velo_marche$age, 0,1)

ademe_velo_marche$yll_corr <- ademe_velo_marche$yll * ademe_velo_marche$yll_max
ademe_velo_marche$yll_low_corr <- ademe_velo_marche$yll_low * ademe_velo_marche$yll_max
ademe_velo_marche$yll_high_corr <- ademe_velo_marche$yll_high * ademe_velo_marche$yll_max

ademe_velo_marche <- ademe_velo_marche %>% select(-c(yll, yll_max))


#################################### life expectancy

# Compilation of all types
ademe_lifexp <- ademe_velo_marche %>%
  filter(type == "marche")

ademe_velo <- ademe_velo_marche %>%
  filter(type == "velo")

ademe_vae <- ademe_velo_marche %>%
  filter(type == "VAE") 

# add diff_death of each modes
ademe_lifexp$diff_death_all_type <- ademe_lifexp$diff_death + ademe_velo$diff_death + ademe_vae$diff_death
ademe_lifexp$diff_death_all_type_low <- ademe_lifexp$diff_death_low + ademe_velo$diff_death_low + ademe_vae$diff_death_low
ademe_lifexp$diff_death_all_type_high <- ademe_lifexp$diff_death_high + ademe_velo$diff_death_high + ademe_vae$diff_death_high

rm(ademe_velo,ademe_vae)

# Mortality rate (MR) for each scenario
ademe_lifexp$MR_s <- (ademe_lifexp$MR*ademe_lifexp$pop - ademe_lifexp$diff_death_all_type)/ademe_lifexp$pop
ademe_lifexp$MR_s_low <- (ademe_lifexp$MR*ademe_lifexp$pop - ademe_lifexp$diff_death_all_type_low)/ademe_lifexp$pop
ademe_lifexp$MR_s_high <- (ademe_lifexp$MR*ademe_lifexp$pop - ademe_lifexp$diff_death_all_type_high)/ademe_lifexp$pop

ademe_velo_marche_lifexp <- data.frame()
for (y in unique(ademe_lifexp$year)) {
  for (s in unique(ademe_lifexp$scenario)) {
    
    df <- ademe_lifexp %>%
      filter(scenario == s & year == y)
    
    prop_alive = c(1, cumprod((1 - df$MR_s) ))
    deaths_S <- -diff(prop_alive)
    life_exp_S = sum(deaths_S * 0:(max(df$age)) ) 
    
    df$life_exp_s <- life_exp_S
    
    # IC 95
    prop_alive = c(1, cumprod((1 - df$MR_s_low) ))
    deaths_S <- -diff(prop_alive)
    life_exp_S = sum(deaths_S * 0:(max(df$age)) ) 
    
    df$life_exp_s_low <- life_exp_S
    
    prop_alive = c(1, cumprod((1 - df$MR_s_high) ))
    deaths_S <- -diff(prop_alive)
    life_exp_S = sum(deaths_S * 0:(max(df$age)) ) 
    
    df$life_exp_s_high <- life_exp_S
    
    ademe_velo_marche_lifexp <- rbind(ademe_velo_marche_lifexp, df)
    
  }
}

# Gain in life expectancy for each year (valid for one year and for all ages)
ademe_lifexp <- data.frame()
for (y in unique(ademe_velo_marche_lifexp$year)) {
  
  df <- ademe_velo_marche_lifexp %>%
    filter(year == y)
  df$lifexp_TEND <- df$life_exp_s[df$scenario == "TEND"]
  df$diff_lifexp <- df$life_exp_s - df$lifexp_TEND
  
  # IC 95
  df$lifexp_TEND_low <- df$life_exp_s_low[df$scenario == "TEND"]
  df$diff_lifexp_low <- df$life_exp_s_low - df$lifexp_TEND_low
  
  df$lifexp_TEND_high <- df$life_exp_s_high[df$scenario == "TEND"]
  df$diff_lifexp_high <- df$life_exp_s_high - df$lifexp_TEND_high
  
  ademe_lifexp <- rbind(ademe_lifexp,df)
  
}

ademe_lifexp$diff_lifexp_mois <- 12*ademe_lifexp$diff_lifexp
ademe_lifexp$diff_lifexp_mois_low <- 12*ademe_lifexp$diff_lifexp_low
ademe_lifexp$diff_lifexp_mois_high <- 12*ademe_lifexp$diff_lifexp_high

rm(df, ademe_velo_marche_lifexp, deaths_S, life_exp_S, prop_alive, s, y)


#################################### Monetizing

ademe_velo_marche <- merge (ademe_velo_marche,monetarisation, by = "year", all.x = T)

ademe_velo_marche$cout <- ademe_velo_marche$yll_corr*ademe_velo_marche$euro_yll
ademe_velo_marche$cout_low <- ademe_velo_marche$yll_low_corr*ademe_velo_marche$euro_yll
ademe_velo_marche$cout_high <- ademe_velo_marche$yll_high_corr*ademe_velo_marche$euro_yll

# No impacts for the <20 and >74
ademe_velo_marche$death_prev[ademe_velo_marche$age < 20]<- 0 # No impacts for >74
ademe_velo_marche$death_prev[ademe_velo_marche$age > 74] <- 0 # No impacts for >74
ademe_velo_marche$death_prev_low[ademe_velo_marche$age < 20] <- 0 # No impacts for >74
ademe_velo_marche$death_prev_low[ademe_velo_marche$age > 74] <- 0 # No impacts for >74
ademe_velo_marche$death_prev_high[ademe_velo_marche$age < 20] <- 0 # No impacts for >74
ademe_velo_marche$death_prev_high[ademe_velo_marche$age > 74] <- 0 # No impacts for >74

ademe_velo_marche$diff_death[ademe_velo_marche$age < 20]<- 0  # No impacts for >74
ademe_velo_marche$diff_death[ademe_velo_marche$age > 74] <- 0 # No impacts for >74
ademe_velo_marche$diff_death_low[ademe_velo_marche$age < 20] <- 0 # No impacts for >74
ademe_velo_marche$diff_death_low[ademe_velo_marche$age > 74] <- 0 # No impacts for >74
ademe_velo_marche$diff_death_high[ademe_velo_marche$age < 20] <- 0 # No impacts for >74
ademe_velo_marche$diff_death_high[ademe_velo_marche$age > 74] <- 0 # No impacts for >74

ademe_velo_marche$yll_corr[ademe_velo_marche$age < 20] <- 0 # No impacts for >74
ademe_velo_marche$yll_corr[ademe_velo_marche$age > 74] <- 0 # No impacts for >74
ademe_velo_marche$yll_low_corr[ademe_velo_marche$age < 20] <- 0 # No impacts for >74
ademe_velo_marche$yll_low_corr[ademe_velo_marche$age > 74] <- 0 # No impacts for >74
ademe_velo_marche$yll_high_corr[ademe_velo_marche$age < 20] <- 0 # No impacts for >74
ademe_velo_marche$yll_high_corr[ademe_velo_marche$age > 74] <- 0 # No impacts for >74

ademe_velo_marche$diff_lifexp_mois[ademe_velo_marche$age < 20 ] <- 0 # No impacts for >74
ademe_velo_marche$diff_lifexp_mois[ademe_velo_marche$age > 74] <- 0 # No impacts for >74
ademe_velo_marche$diff_lifexp_mois_low[ademe_velo_marche$age < 20] <- 0 # No impacts for >74
ademe_velo_marche$diff_lifexp_mois_low[ademe_velo_marche$age > 74] <- 0 # No impacts for >74
ademe_velo_marche$diff_lifexp_mois_high[ademe_velo_marche$age < 20] <- 0 # No impacts for >74
ademe_velo_marche$diff_lifexp_mois_high[ademe_velo_marche$age > 74] <- 0 # No impacts for >74

ademe_velo_marche$cout[ademe_velo_marche$age < 20] <- 0 # No impacts for >74
ademe_velo_marche$cout[ademe_velo_marche$age > 74] <- 0 # No impacts for >74
ademe_velo_marche$cout_low[ademe_velo_marche$age < 20] <- 0 # No impacts for >74
ademe_velo_marche$cout_low[ademe_velo_marche$age  > 74] <- 0 # No impacts for >74
ademe_velo_marche$cout_high[ademe_velo_marche$age < 20] <- 0 # No impacts for >74
ademe_velo_marche$cout_high[ademe_velo_marche$age > 74] <- 0 # No impacts for >74

##### Create age group category of 10 years
ademe_velo_marche$age_grp_10 <- age_grp_10(ademe_velo_marche$age)
ademe_marche_km$age_grp_10 <- age_grp_10(ademe_marche_km$age)
ademe_velo_km$age_grp_10 <- age_grp_10(ademe_velo_km$age)
ademe_vae_km$age_grp_10 <- age_grp_10(ademe_vae_km$age)

#########################################
### Data to analyses

# English
ademe_velo_marche$scenario <- gsub("TEND","BAU", ademe_velo_marche$scenario)

ademe_velo_marche$type <- gsub("marche","Walk", ademe_velo_marche$type)
ademe_velo_marche$type <- gsub("velo","Cycle", ademe_velo_marche$type)
ademe_velo_marche$type <- gsub("VAE","E-cycle", ademe_velo_marche$type)


ademe_lifexp$scenario <- gsub("TEND","BAU", ademe_lifexp$scenario)

ademe_lifexp$type <- gsub("marche","Walk", ademe_lifexp$type)
ademe_lifexp$type <- gsub("velo","Cycle", ademe_lifexp$type)
ademe_lifexp$type <- gsub("VAE","E-cycle", ademe_lifexp$type)

ademe_marche_km$scenario <- gsub("TEND","BAU", ademe_marche_km$scenario)
ademe_velo_km$scenario <- gsub("TEND","BAU", ademe_velo_km$scenario)
ademe_vae_km$scenario <- gsub("TEND","BAU", ademe_vae_km$scenario)

# Get only data of interest for health impact (20-89 years old without BAU)
ademe_velo_marche_no_BAU_age <- ademe_velo_marche %>%
  filter(age > 19 & age < 90 & scenario != "BAU")

ademe_lifex_no_BAU_age <- ademe_lifexp %>%
  filter(age > 19 & age < 90 & scenario != "BAU")

# YLL in thousand
ademe_velo_marche_no_BAU_age$yll_mil <- ademe_velo_marche_no_BAU_age$yll_corr /1000
ademe_velo_marche_no_BAU_age$yll_low_mil <- ademe_velo_marche_no_BAU_age$yll_low_corr /1000
ademe_velo_marche_no_BAU_age$yll_high_mil <- ademe_velo_marche_no_BAU_age$yll_high_corr /1000

# Create age group of 10 years
ademe_velo_marche_no_BAU_age$age_grp_10 <- age_grp_10(ademe_velo_marche_no_BAU_age$age)

# Cost in thousand
ademe_velo_marche_no_BAU_age$cout_md <- ademe_velo_marche_no_BAU_age$cout /10^9
ademe_velo_marche_no_BAU_age$cout_low_md <- ademe_velo_marche_no_BAU_age$cout_low /10^9
ademe_velo_marche_no_BAU_age$cout_high_md <- ademe_velo_marche_no_BAU_age$cout_high /10^9

ademe_velo_marche_no_BAU_age_2035 <- ademe_velo_marche_no_BAU_age %>%
  filter(year == "2035")
ademe_velo_marche_no_BAU_age_2050 <- ademe_velo_marche_no_BAU_age %>%
  filter(year == "2050")

ademe_lifexp_no_BAU_age_2035 <- ademe_lifex_no_BAU_age %>%
  filter(year == "2035")
ademe_lifexp_no_BAU_age_2050 <- ademe_lifex_no_BAU_age %>%
  filter(year == "2050")

# Classify types

ademe_velo_marche$type <- factor(ademe_velo_marche$type, levels = c("Walk", "Cycle","E-cycle"))

#####
#  Dataframe with "total bike" (Bike + e-bike)
ademe_totalcycle_km <- ademe_velo_marche %>% 
  filter(type == "Cycle")

ademe_totalcycle_km$type <- gsub("Cycle", "Total cycle", ademe_totalcycle_km$type)
ademe_vae_km$scenario <- gsub("TEND","BAU", ademe_vae_km$scenario)

ademe_totalcycle_km <- ademe_totalcycle_km[order(ademe_totalcycle_km$scenario,ademe_totalcycle_km$age,ademe_totalcycle_km$year),]
ademe_vae_km <- ademe_vae_km[order(ademe_vae_km$scenario,ademe_vae_km$age,ademe_vae_km$year),]

ademe_totalcycle_km$gpkm <- ademe_totalcycle_km$gpkm + ademe_vae_km$gpkm
ademe_totalcycle_km$km_pp_w <- ademe_totalcycle_km$km_pp_w + ademe_vae_km$km_pp_w
ademe_totalcycle_km$min_pp_w <- ademe_totalcycle_km$min_pp_w + ademe_vae_km$min_pp_w

ademe_velo_marche_totalcycle <- rbind(ademe_velo_marche, ademe_totalcycle_km)

# Create a dataframe for each scenario
ademe_all_type_BAU <- ademe_velo_marche_totalcycle %>% filter (scenario== "BAU")
ademe_all_type_S1 <- ademe_velo_marche_totalcycle %>% filter (scenario== "S1")
ademe_all_type_S2 <- ademe_velo_marche_totalcycle %>% filter (scenario== "S2")
ademe_all_type_S3 <- ademe_velo_marche_totalcycle %>% filter (scenario== "S3")
ademe_all_type_S4 <- ademe_velo_marche_totalcycle %>% filter (scenario== "S4")

###
# Compilation of all types
ademe_all_type <- ademe_marche_km %>%
  select(year,age,scenario,age_grp,min_pp_w)

ademe_all_type <- ademe_all_type[order(ademe_all_type$scenario,ademe_all_type$age,ademe_all_type$year),]
ademe_velo_km <- ademe_velo_km[order(ademe_velo_km$scenario,ademe_velo_km$age,ademe_velo_km$year),]
ademe_vae_km <- ademe_vae_km[order(ademe_vae_km$scenario,ademe_vae_km$age,ademe_vae_km$year),]

ademe_all_type$min_pp_w <- ademe_all_type$min_pp_w + ademe_velo_km$min_pp_w + ademe_vae_km$min_pp_w

# Get only years 2035 and 2050 
ademe_velo_marche_3550_no_0 <- ademe_all_type %>% 
  filter(year == "2035" | year == "2050", min_pp_w > 0) 

###
