#Function to extract variables from FRIDA uncertainty output 
#To be called from the plotting script "plot_FRIDA_vs_other_IAMs.R"

#Muralidhar Adakudlu, 2025
#Division for Ocean and Ice
#Norwegian Meteorological Institute


get.frida.vars <- function() {

filepath.frida.emb  <- ("./FRIDA_sensi_output/plotData_EMB")
filepath.frida.calib <- ("./FRIDA_sensi_output")

file.names   <- c("demographics_population-fit uncertainty-equaly-weighted.RDS",
                  "food_demand_animal_products_demand-fit uncertainty-equaly-weighted.RDS",
                  "demographics_real_gdp_per_person-fit uncertainty-equaly-weighted.RDS",
                  "energy_balance_model_surface_temperature_anomaly-fit uncertainty-equaly-weighted.RDS",
                  "land_use_forest_land-fit uncertainty-equaly-weighted.RDS", 
                  "renewable_energy_renewable_energy_output-fit uncertainty-equaly-weighted.RDS",
                  "fossil_energy_secondary_fossil_energy_output-fit uncertainty-equaly-weighted.RDS",
                  "energy_supply_total_energy_output-fit uncertainty-equaly-weighted.RDS",
                  "sea_level_total_global_sea_level_anomaly-fit uncertainty-equaly-weighted.RDS")

variable.names <- c("Population","Animal_Products_Demand","GDP_per_person","Surface_temperature_anomaly",
                    "Forest_land","Renewable_energy","Secondary_fossil_energy","Total_energy","Sea_level_rise") 

emb.data <- list(NA)
emb.data <- sapply(1:length(file.names), function(i) {
  data <- readRDS(paste(filepath.frida.emb,"/",file.names[i],sep=""))
  emb.data[[variable.names[i]]] <- as.data.frame(data$ciBounds)}, 
  simplify=FALSE, USE.NAMES = TRUE)
names(emb.data)<- variable.names
emb.data.df <- as.data.frame(do.call(rbind,emb.data), stringAsFactors=True)
colnames(emb.data.df) <- c("low2","low1","median","high1","high2")
emb.data.df$variable <- rownames(emb.data.df)
emb.data.df <- tidyr::separate(emb.data.df, variable, into = c('variable', 'year'), sep = '\\.')
emb.data.df$Scenario <- "EMB"

# We combine any other experiments/policy results here
frida.data.df <- map_df(.x=list("FRIDA"=emb.data.df),
                        .f=bind_rows,.id="Model")

# Extract the medians for each variable in FRIDA
population.frida.df <- data.frame(frida.data.df$year[frida.data.df$variable==variable.names[1]], 
                                  frida.data.df$Model[frida.data.df$variable==variable.names[1]],
                                  frida.data.df$Scenario[frida.data.df$variable==variable.names[1]],
                                  frida.data.df$median[frida.data.df$variable==variable.names[1]]*1e-3)
colnames(population.frida.df) <- c("Year","Model","Scenario","Data")

animal.products.demand.frida.df <- data.frame(frida.data.df$year[frida.data.df$variable==variable.names[2]],
                                              frida.data.df$Model[frida.data.df$variable==variable.names[2]],
                                              frida.data.df$Scenario[frida.data.df$variable==variable.names[2]],
                                              frida.data.df$median[frida.data.df$variable==variable.names[2]]*1e-3)
colnames(animal.products.demand.frida.df) <- c("Year","Model","Scenario","Data")

deflator.value.2005  <- 1.784793301  # Deflating the GDP to year 2005 to be consistent with rest of the IAMs
gdp.per.person.frida.df <- data.frame(frida.data.df$year[frida.data.df$variable==variable.names[3]],
                                      frida.data.df$Model[frida.data.df$variable==variable.names[3]],
                                      frida.data.df$Scenario[frida.data.df$variable==variable.names[3]],
                                      frida.data.df$median[frida.data.df$variable==variable.names[3]]/deflator.value.2005)
colnames(gdp.per.person.frida.df) <- c("Year","Model","Scenario","Data")

sta.frida.df <- data.frame(frida.data.df$year[frida.data.df$variable==variable.names[4]],
                           frida.data.df$Model[frida.data.df$variable==variable.names[4]],
                           frida.data.df$Scenario[frida.data.df$variable==variable.names[4]],
                           frida.data.df$median[frida.data.df$variable==variable.names[4]])
colnames(sta.frida.df) <- c("Year","Model","Scenario","Data")

forest.land.frida.df <- data.frame(frida.data.df$year[frida.data.df$variable==variable.names[5]],
                                   frida.data.df$Model[frida.data.df$variable==variable.names[5]],
                                   frida.data.df$Scenario[frida.data.df$variable==variable.names[5]],
                                   frida.data.df$median[frida.data.df$variable==variable.names[5]]*1e-3)
colnames(forest.land.frida.df) <- c("Year","Model","Scenario","Data")

energy.TWh.to.EJ <- 1/277.8   # Unit conversion to be consistent with rest of the IAMs
ren.energy.frida.df <- data.frame(frida.data.df$year[frida.data.df$variable==variable.names[6]],
                                  frida.data.df$Model[frida.data.df$variable==variable.names[6]],
                                  frida.data.df$Scenario[frida.data.df$variable==variable.names[6]],
                                  frida.data.df$median[frida.data.df$variable==variable.names[6]]*energy.TWh.to.EJ)
colnames(ren.energy.frida.df) <- c("Year","Model","Scenario","Data")


fossil.energy.frida.df <- data.frame(frida.data.df$year[frida.data.df$variable==variable.names[7]],
                                     frida.data.df$Model[frida.data.df$variable==variable.names[7]],
                                     frida.data.df$Scenario[frida.data.df$variable==variable.names[7]],
                                     frida.data.df$median[frida.data.df$variable==variable.names[7]]*energy.TWh.to.EJ)
colnames(fossil.energy.frida.df) <- c("Year","Model","Scenario","Data")

total.energy.frida.df <- data.frame(frida.data.df$year[frida.data.df$variable==variable.names[8]],
                                    frida.data.df$Model[frida.data.df$variable==variable.names[8]],
                                    frida.data.df$Scenario[frida.data.df$variable==variable.names[8]],
                                    frida.data.df$median[frida.data.df$variable==variable.names[8]]*energy.TWh.to.EJ)
colnames(total.energy.frida.df) <- c("Year","Model","Scenario","Data")

slr.frida.df <- data.frame(frida.data.df$year[frida.data.df$variable==variable.names[9]],
                           frida.data.df$Model[frida.data.df$variable==variable.names[9]],
                           frida.data.df$Scenario[frida.data.df$variable==variable.names[9]],
                           frida.data.df$median[frida.data.df$variable==variable.names[9]])
colnames(slr.frida.df) <- c("Year","Model","Scenario","Data")

# Calibration data
len.frida.data <- length(seq(1980,2150,1))
calib.df <- readRDS(paste(filepath.frida.calib,"/","calDat.RDS",sep=""))
gdp.per.person.calibration <- data.frame(as.factor(seq(1980,2150,by=1)),abind(calib.df$calDat$demographics_real_gdp_per_person/deflator.value.2005,rep(NA,len.frida.data-44)))
sta.calibration <- data.frame(as.factor(seq(1980,2150,by=1)),abind(calib.df$calDat$energy_balance_model_surface_temperature_anomaly,rep(NA,len.frida.data-44)))
forest.land.calibration <- data.frame(as.factor(seq(1980,2150,by=1)),abind(calib.df$calDat$land_use_forest_land*1e-3,rep(NA,len.frida.data-44)))
population.calibration <- data.frame(as.factor(seq(1980,2150,by=1)),abind(calib.df$calDat$demographics_population*1e-3,rep(NA,len.frida.data-44)))
ren.energy.calibration <- data.frame(as.factor(seq(1980,2150,by=1)),abind(calib.df$calDat$renewable_energy_renewable_energy_output*energy.TWh.to.EJ,rep(NA,len.frida.data-44)))
fossil.energy.calibration <- data.frame(as.factor(seq(1980,2150,by=1)),abind(calib.df$calDat$fossil_energy_secondary_fossil_energy_output*energy.TWh.to.EJ,rep(NA,len.frida.data-44)))
total.energy.calibration  <- data.frame(as.factor(seq(1980,2150,by=1)),abind(calib.df$calDat$energy_supply_total_energy_output*energy.TWh.to.EJ,rep(NA,len.frida.data-44)))
animal.products.demand.calibration <- data.frame(as.factor(seq(1980,2150,by=1)),abind(calib.df$calDat$food_demand_animal_products_demand*1e-3,rep(NA,len.frida.data-44)))
slr.calibration <- data.frame(as.factor(seq(1980,2150,by=1)),abind(calib.df$calDat$sea_level_total_global_sea_level_anomaly,rep(NA,len.frida.data-44)))

colnames(population.calibration) <- colnames(sta.calibration) <- colnames(forest.land.calibration) <- colnames(gdp.per.person.calibration) <-
  colnames(ren.energy.calibration) <- colnames(fossil.energy.calibration) <- colnames(total.energy.calibration) <- colnames(animal.products.demand.calibration) <-
  colnames(slr.calibration) <- c("Year","Data")

population.calibration$Scenario <- 
sta.calibration$Scenario <- 
  forest.land.calibration$Scenario <- 
  ren.energy.calibration$Scenario <- 
  fossil.energy.calibration$Scenario <- total.energy.calibration$Scenario <-
  animal.products.demand.calibration$Scenario <- 
  gdp.per.person.calibration$Scenario <- 
  slr.calibration$Scenario <- NA #"Calibration"

population.calibration$Model <-
  sta.calibration$Model <-  forest.land.calibration$Model <-
  ren.energy.calibration$Model <-
  fossil.energy.calibration$Model <- 
  total.energy.calibration$Model <-  
  animal.products.demand.calibration$Model <- 
  gdp.per.person.calibration$Model <- slr.calibration$Model <- 
 "Calibration"

# confidence intervals data for shading
population.ci <- data.frame(frida.data.df$year[frida.data.df$variable==variable.names[1]], 
                                 frida.data.df$Model[frida.data.df$variable==variable.names[1]],
                                 frida.data.df$Scenario[frida.data.df$variable==variable.names[1]],
                                 frida.data.df$low2[frida.data.df$variable==variable.names[1]]*1e-3,
                                 frida.data.df$low1[frida.data.df$variable==variable.names[1]]*1e-3,
                                 frida.data.df$high1[frida.data.df$variable==variable.names[1]]*1e-3,
                                 frida.data.df$high2[frida.data.df$variable==variable.names[1]]*1e-3)

animal.products.demand.ci <- data.frame(frida.data.df$year[frida.data.df$variable==variable.names[2]], 
                                             frida.data.df$Model[frida.data.df$variable==variable.names[2]],
                                             frida.data.df$Scenario[frida.data.df$variable==variable.names[2]],
                                             frida.data.df$low2[frida.data.df$variable==variable.names[2]]*1e-3,
                                             frida.data.df$low1[frida.data.df$variable==variable.names[2]]*1e-3,
                                             frida.data.df$high1[frida.data.df$variable==variable.names[2]]*1e-3,
                                             frida.data.df$high2[frida.data.df$variable==variable.names[2]]*1e-3)
forest.land.ci <- data.frame(frida.data.df$year[frida.data.df$variable==variable.names[5]], 
                                  frida.data.df$Model[frida.data.df$variable==variable.names[5]],
                                  frida.data.df$Scenario[frida.data.df$variable==variable.names[5]],
                                  frida.data.df$low2[frida.data.df$variable==variable.names[5]]*1e-3,
                                  frida.data.df$low1[frida.data.df$variable==variable.names[5]]*1e-3,
                                  frida.data.df$high1[frida.data.df$variable==variable.names[5]]*1e-3,
                                  frida.data.df$high2[frida.data.df$variable==variable.names[5]]*1e-3)
sta.ci <- data.frame(frida.data.df$year[frida.data.df$variable==variable.names[4]], 
                          frida.data.df$Model[frida.data.df$variable==variable.names[4]],
                          frida.data.df$Scenario[frida.data.df$variable==variable.names[4]],
                          frida.data.df$low2[frida.data.df$variable==variable.names[4]],
                          frida.data.df$low1[frida.data.df$variable==variable.names[4]],
                          frida.data.df$high1[frida.data.df$variable==variable.names[4]],
                          frida.data.df$high2[frida.data.df$variable==variable.names[4]])
slr.ci <- data.frame(frida.data.df$year[frida.data.df$variable==variable.names[9]], 
                          frida.data.df$Model[frida.data.df$variable==variable.names[9]],
                          frida.data.df$Scenario[frida.data.df$variable==variable.names[9]],
                          frida.data.df$low2[frida.data.df$variable==variable.names[9]],
                          frida.data.df$low1[frida.data.df$variable==variable.names[9]],
                          frida.data.df$high1[frida.data.df$variable==variable.names[9]],
                          frida.data.df$high2[frida.data.df$variable==variable.names[9]])
ren.energy.ci <- data.frame(frida.data.df$year[frida.data.df$variable==variable.names[6]], 
                                 frida.data.df$Model[frida.data.df$variable==variable.names[6]],
                                 frida.data.df$Scenario[frida.data.df$variable==variable.names[6]],
                                 frida.data.df$low2[frida.data.df$variable==variable.names[6]]*energy.TWh.to.EJ,
                                 frida.data.df$low1[frida.data.df$variable==variable.names[6]]*energy.TWh.to.EJ,
                                 frida.data.df$high1[frida.data.df$variable==variable.names[6]]*energy.TWh.to.EJ,
                                 frida.data.df$high2[frida.data.df$variable==variable.names[6]]*energy.TWh.to.EJ)
fossil.energy.ci <- data.frame(frida.data.df$year[frida.data.df$variable==variable.names[7]], 
                                    frida.data.df$Model[frida.data.df$variable==variable.names[7]],
                                    frida.data.df$Scenario[frida.data.df$variable==variable.names[7]],
                                    frida.data.df$low2[frida.data.df$variable==variable.names[7]]*energy.TWh.to.EJ,
                                    frida.data.df$low1[frida.data.df$variable==variable.names[7]]*energy.TWh.to.EJ,
                                    frida.data.df$high1[frida.data.df$variable==variable.names[7]]*energy.TWh.to.EJ,
                                    frida.data.df$high2[frida.data.df$variable==variable.names[7]]*energy.TWh.to.EJ)
total.energy.ci <- data.frame(frida.data.df$year[frida.data.df$variable==variable.names[8]], 
                                   frida.data.df$Model[frida.data.df$variable==variable.names[8]],
                                   frida.data.df$Scenario[frida.data.df$variable==variable.names[8]],
                                   frida.data.df$low2[frida.data.df$variable==variable.names[8]]*energy.TWh.to.EJ,
                                   frida.data.df$low1[frida.data.df$variable==variable.names[8]]*energy.TWh.to.EJ,
                                   frida.data.df$high1[frida.data.df$variable==variable.names[8]]*energy.TWh.to.EJ,
                                   frida.data.df$high2[frida.data.df$variable==variable.names[8]]*energy.TWh.to.EJ)
gdp.per.person.ci <-  data.frame(frida.data.df$year[frida.data.df$variable==variable.names[3]], 
                                      frida.data.df$Model[frida.data.df$variable==variable.names[3]],
                                      frida.data.df$Scenario[frida.data.df$variable==variable.names[3]],
                                      frida.data.df$low2[frida.data.df$variable==variable.names[3]]/deflator.value.2005,
                                      frida.data.df$low1[frida.data.df$variable==variable.names[3]]/deflator.value.2005,
                                      frida.data.df$high1[frida.data.df$variable==variable.names[3]]/deflator.value.2005,
                                      frida.data.df$high2[frida.data.df$variable==variable.names[3]]/deflator.value.2005)

colnames(sta.ci) <- colnames(forest.land.ci) <- colnames(animal.products.demand.ci) <-
  colnames(gdp.per.person.ci) <- colnames(population.ci) <-
  colnames(slr.ci) <- colnames(total.energy.ci) <- 
  colnames(ren.energy.ci) <- colnames(fossil.energy.ci) <- c("Year","Model","Scenario","low2","low1","high1","high2")

frida.output <- list(population.frida.df, population.calibration, 
                     sta.frida.df, sta.calibration,
                     slr.frida.df,
                     slr.calibration,
                     gdp.per.person.frida.df,
                     gdp.per.person.calibration,
                     animal.products.demand.frida.df,
                     animal.products.demand.calibration,
                     forest.land.frida.df,
                     forest.land.calibration,
                     ren.energy.frida.df,
                     ren.energy.calibration,
                     fossil.energy.frida.df,
                     fossil.energy.calibration,
                     total.energy.frida.df,
                     total.energy.calibration,
#                    data for confidence bounds
                     population.ci,
                     sta.ci,
                     slr.ci,
                     gdp.per.person.ci,
                     animal.products.demand.ci,
                     forest.land.ci,
                     ren.energy.ci,
                     fossil.energy.ci,
                     total.energy.ci)

names(frida.output) <- c("population.data","Population.cal",
                         "sta.data","sta.cal",
                         "slr.data","slr.cal",
                         "gdp.data","gdp.cal",
                         "animal.products.demand.data","animal.products.cal",
                         "forest.land.data","forest.land.cal",
                         "ren.energy.data","ren.energy.cal",
                         "fossil.energy.data","fossil.energy.cal",
                         "total.energy.data","total.energy.cal",
                         "population.ci","sta.ci","slr.ci","gdp.ci","animal.products.demand.ci","forest.land.ci",
                         "ren.energy.ci","fossil.energy.ci","total.energy.ci")


return(frida.output)  }