rm(list = ls()) ##CLEAN EVERYTHING
setwd('U:\\Manuscript\\Bifurcations') ##CHANGE THIS FOLDER TO YOUR OWN LOCATION
source("U:\\Manuscript\\Code_figures\\Functions.R") #Load script#
source("U:\\Manuscript\\Code_figures\\Script_functions_MJ.R") #Load script#

library(ggpubr)
library(ggplot2)

#----------------------------------------------------------------------------------------#
# Result 1: reduced food availability in focal nursery
#----------------------------------------------------------------------------------------#
xlabel = expression(Food ~ productivity ~ focal ~ nursery ~ (mg ~ L^-1 ~ day^-1))

FoodFocalBif = read.table('test_R1maxbif.avg.out', header=F)
GiveNames(FoodFocalBif, bif = T, bifname = "bif_par", MinMax = F)
FoodFocalBif$Bio_tot = FoodFocalBif$Ad_bm + FoodFocalBif$Juv_bm
FoodFocalBif$bif_par = 1 - FoodFocalBif$bif_par

FoodFocalBifPlot <- ggplot(data=FoodFocalBif, aes(bif_par, y=Larv_bm*1000, color='Juveniles in focal nursery')) +
  geom_point(aes(y=Bio_tot*1000, color='Subadults + Adults')) + 
  geom_point() + labs(y = expression(Biomass ~ (mg ~ L^-1)), x = xlabel) + #theme(legend.position="bottom") + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  # theme(legend.position = c(0.9, 0.9)) +
  theme(legend.key=element_blank()) + 
  scale_color_manual(breaks = c('Juveniles in focal nursery', 'Subadults + Adults'),
                     values = c("Juveniles in focal nursery" = "#4B0055", "Subadults + Adults" = "#36E9A7"), 
                     name=NULL) + 
  guides(color = guide_legend(override.aes = list(size = 3))) +
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=20),
        legend.text=element_text(size=18)) + 
  scale_x_continuous(breaks = c(0.900, 0.925, 0.950, 0.975, 1.000), 
                     labels = c(0.1, 0.075, 0.05, 0.025, 0))

FoodFocalBifPlot


#----------------------------------------------------------------------------------------#
# Result 2: increased mortality in focal nursery
#----------------------------------------------------------------------------------------#
xlabel = expression(Mortality ~ rate ~ focal ~ nursery ~ (day^-1))

MortFocalBif = read.table('FocalMortBif.avg.out', header=F)
GiveNames(MortFocalBif, bif = T, bifname = "bif_par", MinMax = F)
MortFocalBif$Bio_tot = MortFocalBif$Ad_bm + MortFocalBif$Juv_bm

MortFocalBifPlot <- ggplot(data=MortFocalBif, aes(bif_par, y=Larv_bm*1000, color='Juveniles in focal nursery')) +
  geom_point(aes(y=Bio_tot*1000, color='Subadults + Adults')) + 
  geom_point() + labs(y = expression(Biomass ~ (mg ~ L^-1)), x = xlabel) + #theme(legend.position="bottom") + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  # theme(legend.position = c(0.9, 0.9)) +
  theme(legend.key=element_blank()) + 
  scale_color_manual(breaks = c('Juveniles in focal nursery', 'Subadults + Adults'),
                     values = c("Juveniles in focal nursery" = "#4B0055", "Subadults + Adults" = "#36E9A7"), 
                     name=NULL) + 
  guides(color = guide_legend(override.aes = list(size = 3))) +
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=20),
        legend.text=element_text(size=18))

MortFocalBifPlot


#----------------------------------------------------------------------------------------#
# Result 3: increased mortality in offshore habitat
#----------------------------------------------------------------------------------------#

## High quality of focal nursery
xlabel = expression(Mortality ~ rate ~ offshore ~ habitat ~ (day^-1))

MortAdultBif = read.table('AdultMortBif_HighR1max.avg.out', header=F)
GiveNames(MortAdultBif, bif = T, bifname = "bif_par", MinMax = F)
MortAdultBif$Bio_tot = MortAdultBif$Ad_bm + MortAdultBif$Juv_bm

MortAdultBifPlot <- ggplot(data=MortAdultBif, aes(bif_par, y=Larv_bm*1000, color='Juveniles in focal nursery')) +
  geom_point(aes(y=Bio_tot*1000, color='Subadults + Adults')) + 
  geom_point() + labs(y = expression(Biomass ~ (mg ~ L^-1)), x = xlabel) + #theme(legend.position="bottom") + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  # theme(legend.position = c(0.9, 0.9)) +
  theme(legend.key=element_blank()) + 
  scale_color_manual(breaks = c('Juveniles in focal nursery', 'Subadults + Adults'),
                     values = c("Juveniles in focal nursery" = "#4B0055", "Subadults + Adults" = "#36E9A7"), 
                     name=NULL) + 
  guides(color = guide_legend(override.aes = list(size = 3))) +
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=20),
        legend.text=element_text(size=18))

MortAdultBifPlot


## Low quality of focal nursery
MortAdultBif_lowQ = read.table('AdultMortBif_LowR1max.avg.out', header=F)
GiveNames(MortAdultBif_lowQ, bif = T, bifname = "bif_par", MinMax = F)
MortAdultBif_lowQ$Bio_tot = MortAdultBif_lowQ$Ad_bm + MortAdultBif_lowQ$Juv_bm

MortAdultBifPlot_lowQ <- ggplot(data=MortAdultBif_lowQ, aes(bif_par, y=Larv_bm*1000, color='Juveniles in focal nursery')) +
  geom_point(aes(y=Bio_tot*1000, color='Subadults + Adults')) + 
  geom_point() + labs(y = expression(Biomass ~ (mg ~ L^-1)), x = xlabel) + #theme(legend.position="bottom") + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  # theme(legend.position = c(0.9, 0.9)) +
  theme(legend.key=element_blank()) + 
  scale_color_manual(breaks = c('Juveniles in focal nursery', 'Subadults + Adults'),
                     values = c("Juveniles in focal nursery" = "#4B0055", "Subadults + Adults" = "#36E9A7"), 
                     name=NULL) + 
  guides(color = guide_legend(override.aes = list(size = 3))) +
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=20),
        legend.text=element_text(size=18))

MortAdultBifPlot_lowQ

#----------------------------------------------------------------------------------------#
# Result 4A: Decreased connectivity to focal nursery with good quality of all habitats
#----------------------------------------------------------------------------------------#
xlabel = "Connectivity to focal nursery"

ConnFigA = read.table('Connectivity_figA.avg.out', header=F)
GiveNames(ConnFigA, bif = T, bifname = "bif_par", MinMax = F)
ConnFigA$Bio_tot = ConnFigA$Ad_bm + ConnFigA$Juv_bm
ConnFigA$bif_par = 1 - ConnFigA$bif_par

ConnFigAPlot <- ggplot(data=ConnFigA, aes(bif_par, y=Larv_bm*1000, color='Juveniles in focal nursery')) +
  geom_point(aes(y=Bio_tot*1000, color='Subadults + Adults')) + 
  geom_point() + labs(y = expression(Biomass ~ (mg ~ L^-1)), x = xlabel) + #theme(legend.position="bottom") + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  # theme(legend.position = c(0.9, 0.9)) +
  theme(legend.key=element_blank()) + 
  scale_color_manual(breaks = c('Juveniles in focal nursery', 'Subadults + Adults'),
                     values = c("Juveniles in focal nursery" = "#4B0055", "Subadults + Adults" = "#36E9A7"), 
                     name=NULL) + 
  guides(color = guide_legend(override.aes = list(size = 3))) +
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=20),
        legend.text=element_text(size=18)) +
  scale_x_continuous(breaks = c(0, 0.25, 0.50, 0.75, 1), 
                     labels = c(1, 0.75, 0.50, 0.25, 0))

ConnFigAPlot


#-----------------------------------------------------------------------------------------------#
# Result 4B: Decreased connectivity to focal nursery with low food productivity in focal nursery
#-----------------------------------------------------------------------------------------------#
xlabel = "Connectivity to focal nursery"

ConnFigB = read.table('Connectivity_figB.avg.out', header=F)
GiveNames(ConnFigB, bif = T, bifname = "bif_par", MinMax = F)
ConnFigB$Bio_tot = ConnFigB$Ad_bm + ConnFigB$Juv_bm
ConnFigB$bif_par = 1 - ConnFigB$bif_par

ConnFigBPlot <- ggplot(data=ConnFigB, aes(bif_par, y=Larv_bm*1000, color='Juveniles')) +
  geom_point(aes(y=Bio_tot*1000, color='Subadults + Adults')) + 
  geom_point() + labs(y = expression(Biomass ~ (mg ~ L^-1)), x = xlabel) + #theme(legend.position="bottom") + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  # theme(legend.position = c(0.9, 0.9)) +
  theme(legend.key=element_blank()) + 
  scale_color_manual(breaks = c('Juveniles', 'Subadults + Adults'),
                     values = c("Juveniles" = "#4B0055", "Subadults + Adults" = "#36E9A7"), 
                     name=NULL) + 
  guides(color = guide_legend(override.aes = list(size = 3))) +
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=20),
        legend.text=element_text(size=18)) +
  scale_x_continuous(breaks = c(0, 0.25, 0.50, 0.75, 1), 
                     labels = c(1, 0.75, 0.50, 0.25, 0))

ConnFigBPlot


#----------------------------------------------------------------------------------------#
# Result 4C: Decreased connectivity to focal nursery with high mortality in focal nursery
#----------------------------------------------------------------------------------------#
xlabel = "Connectivity to focal nursery"

ConnFigC = read.table('Connectivity_figC.avg.out', header=F)
GiveNames(ConnFigC, bif = T, bifname = "bif_par", MinMax = F)
ConnFigC$Bio_tot = ConnFigC$Ad_bm + ConnFigC$Juv_bm
ConnFigC$bif_par = 1 - ConnFigC$bif_par

ConnFigCPlot <- ggplot(data=ConnFigC, aes(bif_par, y=Larv_bm*1000, color='Juveniles in focal nursery')) +
  geom_point(aes(y=Bio_tot*1000, color='Subadults + Adults')) + 
  geom_point() + labs(y = expression(Biomass ~ (mg ~ L^-1)), x = xlabel) + #theme(legend.position="bottom") + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  # theme(legend.position = c(0.9, 0.9)) +
  theme(legend.key=element_blank()) + 
  scale_color_manual(breaks = c('Juveniles in focal nursery', 'Subadults + Adults'),
                     values = c("Juveniles in focal nursery" = "#4B0055", "Subadults + Adults" = "#36E9A7"), 
                     name=NULL) + 
  guides(color = guide_legend(override.aes = list(size = 3))) +
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=20),
        legend.text=element_text(size=18)) +
  scale_x_continuous(breaks = c(0, 0.25, 0.50, 0.75, 1), 
                     labels = c(1, 0.75, 0.50, 0.25, 0))

ConnFigCPlot


#-------------------------------------------------------------------------------------------#
# Result 4D: Decreased connectivity to focal nursery with high mortality in offshore habitat
#-------------------------------------------------------------------------------------------#
xlabel = "Connectivity to focal nursery"

ConnFigD = read.table('Connectivity_figD.avg.out', header=F)
GiveNames(ConnFigD, bif = T, bifname = "bif_par", MinMax = F)
ConnFigD$Bio_tot = ConnFigD$Ad_bm + ConnFigD$Juv_bm
ConnFigD$bif_par = 1 - ConnFigD$bif_par

ConnFigDPlot <- ggplot(data=ConnFigD, aes(bif_par, y=Larv_bm*1000, color='Juveniles in focal nursery')) +
  geom_point(aes(y=Bio_tot*1000, color='Subadults + Adults')) + 
  geom_point() + labs(y = expression(Biomass ~ (mg ~ L^-1)), x = xlabel) + #theme(legend.position="bottom") + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  # theme(legend.position = c(0.9, 0.9)) +
  theme(legend.key=element_blank()) + 
  scale_color_manual(breaks = c('Juveniles in focal nursery', 'Subadults + Adults'),
                     values = c("Juveniles in focal nursery" = "#4B0055", "Subadults + Adults" = "#36E9A7"), 
                     name=NULL) + 
  guides(color = guide_legend(override.aes = list(size = 3))) +
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=20),
        legend.text=element_text(size=18)) +
  scale_x_continuous(breaks = c(0, 0.25, 0.50, 0.75, 1), 
                     labels = c(1, 0.75, 0.50, 0.25, 0))

ConnFigDPlot
