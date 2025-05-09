#Function to extract variables from selected IAMs (SSP baselines/RCP combinations)
#To be called from the plotting script "plot_FRIDA_vs_other_IAMs.R"

#Muralidhar Adakudlu, 2025
#Division for Ocean and Ice
#Norwegian Meteorological Institute


get.iams.vars <- function() {

filepath.iams       <- "./SSP_baseline_IAMs/"

# Population
population      <- read_excel(paste(filepath.iams, "population.xlsx",sep=""))
population.iams   <- head(population,-2)[,!names(population) %in% c("Notes","Region","Variable","Unit")]  
colnames(population.iams) <- c(colnames(population.iams)[1:2],as.character(c(2005,seq(2010,2100,by=10))))
population.iams.long <- reshape2::melt(population.iams, id.vars=c("Model","Scenario"))                   
colnames(population.iams.long) = c("Model","Scenario","Year","Data")
population.iams.long$Data <- population.iams.long$Data*1e-3

# Real GDP per person
gdp.per.person   <- read_excel(paste(filepath.iams, "gdp_per_capita.xlsx",sep=""))
gdp.per.person.iams         <- head(gdp.per.person,-2)[,!names(gdp.per.person) %in% c("Notes","Region","Variable","Unit")]
colnames(gdp.per.person.iams) <- c(colnames(gdp.per.person.iams)[1:2],as.character(c(2005,seq(2010,2100,by=10))))
gdp.per.person.iams.long <- reshape2::melt(gdp.per.person.iams, id.vars=c("Model","Scenario"))                   
colnames(gdp.per.person.iams.long) = c("Model","Scenario","Year","Data")

# surface temperature anomaly
sta      <- read_excel(paste(filepath.iams, "sta.xlsx",sep=""))
sta.iams   <- head(sta,-2)[,!names(sta) %in% c("Notes","Region","Variable","Unit")]  
colnames(sta.iams) <- c(colnames(sta.iams)[1:2],as.character(c(2005,seq(2010,2100,by=10))))
sta.iams.long <- reshape2::melt(sta.iams, id.vars=c("Model","Scenario"))                   
colnames(sta.iams.long) = c("Model","Scenario","Year","Data")

# Forest land
forest.land      <- read_excel(paste(filepath.iams, "forest.xlsx",sep=""))
forest.land.iams   <- head(forest.land,-2)[,!names(forest.land) %in% c("Notes","Region","Variable","Unit")]  
colnames(forest.land.iams) <- c(colnames(forest.land.iams)[1:2],as.character(c(2005,seq(2010,2100,by=10))))
forest.land.iams.long <- reshape2::melt(forest.land.iams, id.vars=c("Model","Scenario"))                   
colnames(forest.land.iams.long) = c("Model","Scenario","Year","Data")
forest.land.iams.long$Data <- forest.land.iams.long$Data*1e-3

# Secondary fossil energy
sec.energy.liq.oil      <- read_excel(paste(filepath.iams, "se_liquids_oil.xlsx",sep=""))
sec.energy.liq.coal     <- read_excel(paste(filepath.iams, "se_liquids_coal.xlsx",sep=""))
sec.energy.liq.gas      <- read_excel(paste(filepath.iams, "se_liquids_gas.xlsx",sep=""))
sec.energy.coal         <- read_excel(paste(filepath.iams, "se_gases_coal.xlsx",sep=""))
sec.energy.nat.gas      <- read_excel(paste(filepath.iams, "se_gases_natural_gas.xlsx",sep=""))
sec.energy.liq.oil <- head(sec.energy.liq.oil,-2)[,!names(sec.energy.liq.oil) %in% c("Notes","Region","Variable","Unit")]
sec.energy.liq.coal <- head(sec.energy.liq.coal,-2)[,!names(sec.energy.liq.coal) %in% c("Notes","Region","Variable","Unit")]
sec.energy.liq.gas <- head(sec.energy.liq.gas,-2)[,!names(sec.energy.liq.gas) %in% c("Notes","Region","Variable","Unit")]
sec.energy.nat.gas <- head(sec.energy.nat.gas,-2)[,!names(sec.energy.nat.gas) %in% c("Notes","Region","Variable","Unit")]
sec.energy.coal <- head(sec.energy.coal,-2)[,!names(sec.energy.coal) %in% c("Notes","Region","Variable","Unit")]
colnames(sec.energy.liq.oil) <- colnames(sec.energy.liq.coal) <- colnames(sec.energy.liq.gas) <-
  colnames(sec.energy.coal) <- colnames(sec.energy.nat.gas) <-c(c("Model","Scenario"),as.character(c(2005,seq(2010,2100,by=10))))

# Variables for total secondary fossil energy - take only oil/gas/coal - because that is what we have in FRIDA
sec.fossil.energy <-  rbind(sec.energy.liq.oil,sec.energy.nat.gas,sec.energy.coal)
sec.fossil.energy.long <-  reshape2::melt(sec.fossil.energy, id.vars=c("Model","Scenario"))        

# Total secondary fossil energy
total.sec.fossil.energy <- aggregate(sec.fossil.energy.long$value,by=list(sec.fossil.energy.long$Model,sec.fossil.energy.long$Scenario,sec.fossil.energy.long$variable),FUN=sum)
colnames(total.sec.fossil.energy) <- c("Model","Scenario","Year","Data")

# renewable energy
ren.energy.solar      <- read_excel(paste(filepath.iams,  "pe_solar.xlsx",sep="")) 
ren.energy.hydro      <- read_excel(paste(filepath.iams, "pe_hydro.xlsx",sep="")) 
ren.energy.wind       <- read_excel(paste(filepath.iams, "pe_wind.xlsx",sep=""))
ren.energy.solar      <- head(ren.energy.solar,-2)[,!names(ren.energy.solar) %in% c("Notes","Region","Variable","Unit")]  
ren.energy.hydro      <- head(ren.energy.hydro,-2)[,!names(ren.energy.hydro) %in% c("Notes","Region","Variable","Unit")]  
ren.energy.wind      <- head(ren.energy.wind,-2)[,!names(ren.energy.wind) %in% c("Notes","Region","Variable","Unit")]  
colnames(ren.energy.hydro) <- colnames(ren.energy.solar) <- colnames(ren.energy.wind) <- c(c("Model","Scenario"),as.character(c(2005,seq(2010,2100,by=10))))
ren.energy <-   rbind(ren.energy.solar,ren.energy.wind,ren.energy.hydro)
ren.energy.long <-  reshape2::melt(ren.energy, id.vars=c("Model","Scenario"))        

# Total renewables
total.ren.energy <- aggregate(ren.energy.long$value,by=list(ren.energy.long$Model,ren.energy.long$Scenario,ren.energy.long$variable),FUN=sum)
colnames(total.ren.energy) <- c("Model","Scenario","Year","Data")

# Total energy (renewables + fossils)
energy.output.iams <- rbind(total.sec.fossil.energy,total.ren.energy) 
total.energy.output.iams <- aggregate(energy.output.iams$Data, by=list(energy.output.iams$Model,energy.output.iams$Scenario,energy.output.iams$Year),FUN=sum) 
colnames(total.energy.output.iams) <- c("Model","Scenario","Year","Data")

# Check if the energy output makes sense
#data.check <- cbind(total.energy.output.iams$Model,total.energy.output.iams$Scenario, as.character(total.energy.output.iams$Year), 
#                            total.energy.output.iams$Data, total.sec.fossil.energy$Data, total.ren.energy$Data)
#colnames(data.check) <-  c("Model","Scenario","Year","Total energy output", "Total fossil energy", "Total renewable energy")

iams.output <- list(population.iams.long,
                    sta.iams.long,
                    gdp.per.person.iams.long,
                    forest.land.iams.long,
                    total.ren.energy,
                    total.sec.fossil.energy,
                    total.energy.output.iams)

names(iams.output) <- c("population","sta","gdp","forest.land","ren.energy","sec.fossil.energy","total.energy")

return(iams.output) }