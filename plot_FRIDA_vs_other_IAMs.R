# Script to plot Figure 3 in the FRIDA paper

#Muralidhar Adakudlu, 2025
#Division for Ocean and Ice
#Norwegian Meteorological Institute

library(tidyverse)
library("readxl")
library(abind)
library(ggtext)   # for highlighting legend title
library(gridExtra) # nice arrangement of panels
library(lemon)
library(ggpubr)
library(ragg)

#setwd('G:/My Drive/R/r-scripts/worldTrans/make_plots_from_FRIDA_output')
remove(list=ls())

# Get the energy variables from other IAMs
source('./get_variables_from_other_IAMs.R')
source('./get_variables_from_FRIDA.R')

frida.output <- get.frida.vars() 
iams.output <- get.iams.vars()


# ---- plotting preprocessing -----

plot.population <- map_df(.x=list(iams.output$population,
                                  frida.output$population.data,
                                  frida.output$Population.cal),
                          .f=bind_rows)

plot.sta <- map_df(.x=list(iams.output$sta,
                           frida.output$sta.data,
                           frida.output$sta.cal),
                   .f=bind_rows)

plot.slr     <- map_df(.x=list(frida.output$slr.data,
                               frida.output$slr.cal),
                       .f=bind_rows)

plot.gdp.per.person <- map_df(.x=list(iams.output$gdp,
                                      frida.output$gdp.data,
                                      frida.output$gdp.cal),
                              .f=bind_rows)

plot.animal.products.demand <- map_df(.x=list(frida.output$animal.products.demand.data,
                                              frida.output$animal.products.cal),
                                      .f=bind_rows)

plot.forest.land <- map_df(.x=list(iams.output$forest.land,
                                   frida.output$forest.land.data,
                                   frida.output$forest.land.cal),
                           .f=bind_rows)

plot.ren.energy <- map_df(.x=list(iams.output$ren.energy,
                                  frida.output$ren.energy.data,
                                  frida.output$ren.energy.cal),
                          .f=bind_rows)

plot.fossil.energy <- map_df(.x=list(iams.output$sec.fossil.energy,
                                     frida.output$fossil.energy.data,
                                     frida.output$fossil.energy.cal),
                             .f=bind_rows)

plot.total.energy <- map_df(.x=list(iams.output$total.energy,
                                    frida.output$total.energy.data,
                                    frida.output$total.energy.cal),
                            .f=bind_rows)


####################################################################################################################
# Merge all data together
plot.data.all <- map_df(.x=list("(a) Surf. Temp. Anomaly (°C)"=plot.sta,
                                "(b) GDP per per. (2005b$/mp/Yr)"=plot.gdp.per.person,
                                "(c) Sea Level Rise (meters)"=plot.slr,
                                "(d) Population (Billion)"=plot.population,
                                "(e) Animal Pr. Dem. (ECal/Yr)"=plot.animal.products.demand,
                                "(f) Forest (bHa)"= plot.forest.land,
                                "(g) Ren. Energy Output (EJ/Yr)"=plot.ren.energy,
                                "(h) Fossil Energy Output (EJ/Yr)"=plot.fossil.energy,
                                "(i) Total Energy Output (EJ/Yr)"=plot.total.energy),
                        .f=bind_rows, .id = "name")

plot.data.ci <- map_df(.x=list("(a) Surf. Temp. Anomaly (°C)"=frida.output$sta.ci,
                               "(b) GDP per per. (2005b$/mp/Yr)"=frida.output$gdp.ci,
                               "(c) Sea Level Rise (meters)"=frida.output$slr.ci,
                               "(d) Population (Billion)"= frida.output$population.ci,
                               "(e) Animal Pr. Dem. (ECal/Yr)"=frida.output$animal.products.demand.ci,
                               "(f) Forest (bHa)"=frida.output$forest.land.ci,
                               "(g) Ren. Energy Output (EJ/Yr)"=frida.output$ren.energy.ci,
                               "(h) Fossil Energy Output (EJ/Yr)"=frida.output$fossil.energy.ci,
                               "(i) Total Energy Output (EJ/Yr)"=frida.output$total.energy.ci),
                       .f=bind_rows, .id="name")


# --- Edited by Jeff for high resolution image ---
# Separate into rows, isolating energy variables from the rest for the sake of adjusting y-scales in the plot.

panel.r1.emb.ci <-  plot.data.ci[plot.data.ci$name %in% c("(a) Surf. Temp. Anomaly (°C)",
                                                          "(b) GDP per per. (2005b$/mp/Yr)",
                                                          "(c) Sea Level Rise (meters)") & plot.data.ci$Scenario %in% "EMB",]
panel.r2.emb.ci <-  plot.data.ci[plot.data.ci$name %in% c("(d) Population (Billion)",
                                                          "(e) Animal Pr. Dem. (ECal/Yr)",
                                                          "(f) Forest (bHa)") & plot.data.ci$Scenario %in% "EMB",]
panel.energy.emb.ci <-  plot.data.ci[plot.data.ci$name %in% c("(g) Ren. Energy Output (EJ/Yr)",
                                                              "(h) Fossil Energy Output (EJ/Yr)",
                                                              "(i) Total Energy Output (EJ/Yr)") & plot.data.ci$Scenario %in% "EMB",]

panel.r1.median <- plot.data.all[plot.data.all$name %in% c("(a) Surf. Temp. Anomaly (°C)",
                                                           "(b) GDP per per. (2005b$/mp/Yr)",
                                                           "(c) Sea Level Rise (meters)"),]
panel.r2.median <- plot.data.all[plot.data.all$name %in% c("(d) Population (Billion)",
                                                           "(e) Animal Pr. Dem. (ECal/Yr)",
                                                           "(f) Forest (bHa)"),]
panel.energy.median <- plot.data.all[plot.data.all$name %in% c("(g) Ren. Energy Output (EJ/Yr)",
                                                               "(h) Fossil Energy Output (EJ/Yr)",
                                                               "(i) Total Energy Output (EJ/Yr)"),]


titles.r1 <- c("(a) Surface Temperature Anomaly (°C)", "(b) GDP per capita (2005B$/Mp/yr)", "(c) Sea Level Rise (m)")
names(titles.r1) <- unique(panel.r1.median$name)

titles.r2 <- c("(d) Total Population (B)", "(e) Animal Products Demand (Ecal/yr)", "(f) Forest (Bha)")
names(titles.r2) <- unique(panel.r2.median$name)

titles.energy <- c("(g) Renewable Energy Output (EJ/yr)", "(h) Fossil Energy Output (EJ/yr)", "(i) Total Energy Output (EJ/yr)")
names(titles.energy) <- unique(panel.energy.median$name)

# Plot r1
panel.plot.r1 <- ggplot()+
  geom_line(data=panel.r1.median,mapping=aes(x=as.numeric(Year),y=Data,
                                             color=Scenario,linetype=Model,linewidth=Scenario))+
  geom_point(data=panel.r1.median,mapping=aes(x=as.numeric(Year),y=Data,
                                              shape=Model, size=Model, fill=Model))+
  facet_wrap(~name,scale="free_y", labeller = labeller(name = titles.r1))+
  geom_ribbon(data=panel.r1.emb.ci,aes(x=as.numeric(Year),ymax=high1,ymin=low1), alpha=0.25)+
  geom_ribbon(data=panel.r1.emb.ci, aes(x=as.numeric(Year),ymax=high2,ymin=low2), alpha=0.15)+
  theme_bw()+
  scale_color_manual(values=c("black","green","green4","magenta","orange","blue"),
                     breaks=c("EMB","SSP1-Baseline","SSP2-Baseline","SSP3-Baseline","SSP4-Baseline","SSP5-Baseline"),   # actual values of the "Scenario" column
                     labels=c("EMB","SSP1-Baseline","SSP2-Baseline","SSP3-Baseline","SSP4-Baseline","SSP5-Baseline"))+  # names we want to display on the legend
  scale_linetype_manual(values=c("dotted","dotted","solid","dashed","longdash","dotdash","twodash","dotted"))+
  scale_shape_manual(values=c(0,21,NA,5,1,2,3,4))+
  scale_size_manual(values=c(2,2,NA,2,2,2,2,2,2))+
  scale_fill_manual(values=c(NA,"red",NA,NA,NA,NA,NA,NA,NA))+
  scale_linewidth_manual(values=c(1,1,1,1,1,1))+
  labs(x=NULL,y=NULL,title=NULL)+
  scale_x_continuous(breaks = seq(1980,2150,20),expand=c(0,0))+
  theme(strip.background =element_blank(),
        strip.text = element_text(size=14, face="bold"),
        axis.text.x = element_text(size=11, angle = 45, hjust = 1),
        axis.text.y = element_text(size=11),
        axis.ticks.x = element_line(),
        panel.grid.major = element_line(color = "gray90", linetype = "dotted"),
        panel.grid.minor = element_line(color = "gray90", linetype = "dotted"),
        plot.margin = margin(t=10, r=10, b=10, l=10),
        legend.title = element_text(face = "bold", size=13),
        legend.text = element_text(size=12), 
        legend.position = "bottom"
  )+
  guides(color = guide_legend(nrow = 2, ncol = 8, position = "bottom", override.aes = list(linewidth = 1)),
         linetype = guide_legend(nrow = 2, ncol = 8, position = "bottom"),
         linewidth = "none")

# Plot r2
panel.plot.r2 <- ggplot()+
  geom_line(data=panel.r2.median,mapping=aes(x=as.numeric(Year),y=Data,
                                             color=Scenario,linetype=Model,linewidth=Scenario))+
  geom_point(data=panel.r2.median,mapping=aes(x=as.numeric(Year),y=Data,
                                              shape=Model, size=Model, fill=Model))+
  facet_wrap(~name,scale="free_y", labeller = labeller(name = titles.r2))+
  geom_ribbon(data=panel.r2.emb.ci,aes(x=as.numeric(Year),ymax=high1,ymin=low1), alpha=0.25)+
  geom_ribbon(data=panel.r2.emb.ci, aes(x=as.numeric(Year),ymax=high2,ymin=low2), alpha=0.15)+
  theme_bw()+
  scale_color_manual(values=c("black","green","green4","magenta","orange","blue"),
                     breaks=c("EMB","SSP1-Baseline","SSP2-Baseline","SSP3-Baseline","SSP4-Baseline","SSP5-Baseline"),
                     labels=c("EMB","SSP1-Baseline","SSP2-Baseline","SSP3-Baseline","SSP4-Baseline","SSP5-Baseline")
  )+
  scale_linetype_manual(values=c("dotted","dotted","solid","dashed","longdash","dotdash","twodash","dotted"))+
  scale_shape_manual(values=c(0,21,NA,5,1,2,3,4))+
  scale_size_manual(values=c(2,2,NA,2,2,2,2,2,2))+
  scale_fill_manual(values=c(NA,"red",NA,NA,NA,NA,NA,NA,NA))+
  scale_linewidth_manual(values=c(1,1,1,1,1,1))+
  labs(x=NULL,y=NULL,title=NULL)+
  scale_x_continuous(breaks = seq(1980,2150,20),expand=c(0,0))+
  theme(strip.background =element_blank(),
        strip.text = element_text(size=14, face="bold"),
        axis.text.x = element_text(size=11, angle = 45, hjust = 1),
        axis.text.y = element_text(size=11),
        axis.ticks.x = element_line(),
        panel.grid.major = element_line(color = "gray90", linetype = "dotted"),
        panel.grid.minor = element_line(color = "gray90", linetype = "dotted"),
        plot.margin = margin(t=10, r=10, b=10, l=10),
        legend.title = element_text(face = "bold", size=13),
        legend.text = element_text(size=12), 
        legend.position = "none"
  )+
  guides(color = guide_legend(nrow = 2, ncol = 7, position = "bottom", override.aes = list(linewidth = 1)),
         linetype = guide_legend(nrow = 2, ncol = 8, position = "bottom"),
         linewidth = "none")

#Plot energy
panel.plot.energy <- ggplot()+
  geom_line(data=panel.energy.median,mapping=aes(x=as.numeric(Year),y=Data,
                                                 color=Scenario,linetype=Model,linewidth=Scenario))+
  geom_point(data=panel.energy.median,mapping=aes(x=as.numeric(Year),y=Data,
                                                  shape=Model, size=Model, fill=Model))+
  facet_rep_wrap(~name,scales="fixed", repeat.tick.labels = "left", labeller = labeller(name = titles.energy))+
  geom_ribbon(data=panel.energy.emb.ci,aes(x=as.numeric(Year),ymax=high1,ymin=low1),alpha=0.25)+
  geom_ribbon(data=panel.energy.emb.ci, aes(x=as.numeric(Year),ymax=high2,ymin=low2),alpha=0.15)+
  theme_bw()+
  scale_color_manual(values=c("black","green","green4","magenta","orange","blue"),
                     breaks=c("EMB","SSP1-Baseline","SSP2-Baseline","SSP3-Baseline","SSP4-Baseline","SSP5-Baseline"),
                     labels=c("EMB","SSP1-Baseline","SSP2-Baseline","SSP3-Baseline","SSP4-Baseline","SSP5-Baseline")
  )+
  scale_linetype_manual(values=c("dotted","dotted","solid","dashed","longdash","dotdash","twodash","dotted"))+
  scale_shape_manual(values=c(0,21,NA,5,1,2,3,4))+
  scale_size_manual(values=c(2,2,NA,2,2,2,2,2))+
  scale_fill_manual(values=c(NA,"red",NA,NA,NA,NA,NA,NA,NA))+
  scale_linewidth_manual(values=c(1,1,1,1,1,1))+
  labs(x=NULL,y=NULL,title=NULL)+
  scale_x_continuous(breaks = seq(1980,2150,20),expand=c(0,0))+
  theme(strip.background =element_blank(),
        strip.text = element_text(size=14, face="bold"),
        axis.text.x = element_text(size=11, angle = 45, hjust = 1),
        axis.text.y = element_text(size=11),
        axis.ticks.x = element_line(),
        legend.text = element_text(size=12), 
        legend.title = element_text(face = "bold", size=13),
        legend.position = "bottom",
        panel.grid.major = element_line(color = "gray90", linetype = "dotted"),
        panel.grid.minor = element_line(color = "gray90", linetype = "dotted"),
        plot.margin = margin(t=10, r=10, b=10, l=10)
  )+
  guides(color = guide_legend(nrow = 2, ncol = 7, position = "bottom", override.aes = list(linewidth = 1)),
         linetype = guide_legend(nrow = 2, ncol = 8, position = "bottom"),
         linewidth = "none")

#Combine plots
combined_plot <- ggarrange(panel.plot.r1,panel.plot.r2,panel.plot.energy,
                           nrow=3, ncol=1,
                           heights= c(1,1,1),
                           align= "v", #align vertically
                           common.legend = TRUE,
                           legend = "bottom")+
  theme(plot.background = element_rect(fill = "white", color = NA),
        legend.background = element_rect(fill = "white"),
        legend.box.background = element_rect(fill = "white"))

# Save in high resolution 
ggsave(
  filename = "EMBvsSSP.png",
  plot = combined_plot,
  width = 16,
  height = 12
)

# Scale the figure to even higher resolution and save 
agg_png("EMBvsSSP-Baseline.png", width = 16*3, height = 12*3, units = "in", res = 300, scaling = 3)
print(combined_plot)
dev.off()
