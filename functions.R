# function that create categories based on an age column
# 5 year category
age_grp = function(age){
  age_grp = gsub(",", "-",
                 gsub("\\[|\\]|\\(|\\)", "",
                      cut( age, breaks = seq(0,150, by = 5), include.lowest = T, right = F)))
  post = sub(".*-","",age_grp)
  age_grp = paste0(sub("-.*", "", age_grp),
                   "-", as.numeric(post)-1)
  return(age_grp)
}

# 10 year category
age_grp_10 = function(age){
  age_grp = gsub(",", "-",
                 gsub("\\[|\\]|\\(|\\)", "",
                      cut( age, breaks = seq(0,150, by = 10), include.lowest = T, right = F)))
  post = sub(".*-","",age_grp)
  age_grp = paste0(sub("-.*", "", age_grp),
                   "-", as.numeric(post)-1)
  return(age_grp)
}

# Calcul des km_pp_y
km_pp_y = function(data, data_km){
  
  for (s in unique(data$scenario)) {
    for (y in unique(data$year)) {
      
      df <- data %>%
        filter(scenario == s, year == y)
      
      x <- tapply(df$pop, df$age_grp == "15-19", sum)
      pop_ref <- x [["TRUE"]]
      
      df$popref <-   pop_ref   
      df$rhopop <- df$rho * df$pop
      sum_rhopop <- sum(round(df$rhopop))
      df$km_pp_y <- df$rho*unique(df$gpkm*10^9)/(sum_rhopop)
      
      data_km <- rbind(data_km,df)
      
    }
  }
  return(data_km)
}



# Calcul de rho_age
allocate_km_vae <- function(data, par=c(coef_pi_a,ordo_pi_a), obj_pi, obj_delta){
  
  data$rho_a <- data$age * par[1] + par[2]
  data["rho_a"][data["rho_a"] >=1] <- 0.99 # Remplace les rho_a >= 1 par 0.99
  data["rho_a"][data["rho_a"] < 0] <- 0 #  # Remplace les rho_a <0 par 0
  # Calcul de km_a
  data$km_a <- data$pop * data$km_pp_y
  # Calcul km_a_vae
  data$km_a_vae <- data$km_a * data$rho_a
  # Calcul a_km_pp_rho_a
  data$a_km_pp_rho_a <- data$age * data$km_pp_y * data$rho_a
  # Calcul km_pp_rho_a
  data$km_pp_rho_a <- data$km_pp_y * data$rho_a
  # Calcul a_km_pp_1-rho_a
  data$a_km_pp_1_rho_a <- data$age * data$km_pp_y *(1-data$rho_a)
  # Calcul km_pp_1-rho_a
  data$km_pp_1_rho_a <- data$km_pp_y *(1-data$rho_a)
  
  delta <- (sum(data$a_km_pp_rho_a)/sum(data$km_pp_rho_a)) - (sum(data$a_km_pp_1_rho_a)/sum(data$km_pp_1_rho_a))
  pi <- sum(data$km_a_vae)/ sum(data$km_a)
  critere <- 5*abs(pi - obj_pi) + abs(delta - obj_delta)
  
  return(critere)
}




# Number of deaths prevented
n_prev = function(data, RR, Ref_volume){
  # calculate the number of death prevented 
  res = (1-RR)*(data$min_pp_w/Ref_volume)*data$MR*data$pop
  return(res)
}



# Nombre de mort prévenu par rapport à TEND
diff_death = function(data,n_scenario){
  
  a = data$death_prev[data$scenario=="TEND"]
  a = rep(a, n_scenario)
  
  for (y in unique(data$year)) {
    for (s in data$scenario) {
      data$diff_death = data$death_prev - a
    }
  }
  data <- data %>% select (c(diff_death),)
  return(data$diff_death)
}

# IC 95 %
diff_death_low = function(data,n_scenario){
  
  a = data$death_prev_low[data$scenario=="TEND"]
  a = rep(a, n_scenario)
  
  for (y in unique(data$year)) {
    for (s in data$scenario) {
      data$diff_death = data$death_prev_low - a
    }
  }
  data <- data %>% select (c(diff_death),)
  return(data$diff_death)
}

diff_death_high = function(data,n_scenario){
  
  a = data$death_prev_high[data$scenario=="TEND"]
  a = rep(a, n_scenario)
  
  for (y in unique(data$year)) {
    for (s in data$scenario) {
      data$diff_death = data$death_prev_high - a
    }
  }
  data <- data %>% select (c(diff_death),)
  return(data$diff_death)
}


# Get life expectancy by year
life_exp = function(df, MR ,age){
  test = df %>% 
    mutate(prop_alive = cumprod(1 - MR),
           deaths =  -(prop_alive - lag(prop_alive)),
           life_exp = sum(age *deaths, na.rm = T)) 
  return(test)
}





