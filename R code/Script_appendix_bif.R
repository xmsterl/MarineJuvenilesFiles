rm(list = ls()) ##CLEAN EVERYTHING
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) #Set wd to current directory
source("Script_functions.R") #Load script#

#Set location output files#
Appendix_files = "../EBTfiles/Files_Appendix/"

###Appendix bif Fig1: food productivity nursery####
xlabel = expression(Food ~ productivity ~ focal ~ nursery ~ (mg ~ L^-1 ~ day^-1))

FoodFocalBif = read.table(paste(Appendix_files, 'App_bif_Fig1_r1max.avg.out', sep = ""), header=F)
GiveNames(FoodFocalBif, bif = T, bifname = "bif_par", MinMax = F)
FoodFocalBif$Bio_tot = FoodFocalBif$Ad_bm + FoodFocalBif$Juv_bm
FoodFocalBif$bif_par = 1 - FoodFocalBif$bif_par

FoodFocalBifPlot <- ggplot(data=FoodFocalBif, aes(bif_par, y=Larv_bm*1000, color='Juveniles in focal nursery')) +
  geom_point(size = 3, aes(y=Bio_tot*1000, color='Subadults + Adults')) + 
  geom_point(size = 3) + labs(y = expression(Biomass ~ (mg ~ L^-1)), x = xlabel) + #theme(legend.position="bottom") + 
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

#FoodFocalBifPlot
# 
# ggsave("../Plots/App_bif_Fig1_R1max.png", plot = FoodFocalBifPlot,
#   width = 1920, height = 1107,
#        scale = 2.5,
#        units = "px")



###Appendix bif Fig2: mortality rate nursery####
xlabel = expression(Mortality ~ rate ~ focal ~ nursery ~ (day^-1))

#
MortFocalBif = read.table(paste(Appendix_files, 'App_bif_Fig2_nursmort.avg.out', sep = ""), header=F)
GiveNames(MortFocalBif, bif = T, bifname = "bif_par", MinMax = F)
MortFocalBif$Bio_tot = MortFocalBif$Ad_bm + MortFocalBif$Juv_bm

MortFocalBifPlot <- ggplot(data=MortFocalBif, aes(bif_par, y=Larv_bm*1000, color='Juveniles in focal nursery')) +
  geom_point(aes(y=Bio_tot*1000, color='Subadults + Adults'), size = 3) + 
  geom_point(size = 3) + labs(y = expression(Biomass ~ (mg ~ L^-1)), x = xlabel) + #theme(legend.position="bottom") + 
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

#MortFocalBifPlot
# ggsave("../Plots/App_bif_Fig2_MortNurs.png", plot = MortFocalBifPlot,
#        width = 1920, height = 1107,
#        scale = 2.5,
#        units = "px")


###Appendix bif Fig3: mortality offshore, high quality nurs####
xlabel = expression(Mortality ~ rate ~ offshore ~ habitat ~ (day^-1))

MortAdultBif = read.table(paste(Appendix_files, 'App_bif_Fig3_HighR1max_offshoremort.avg.out', sep = ""), header=F)
GiveNames(MortAdultBif, bif = T, bifname = "bif_par", MinMax = F)
MortAdultBif$Bio_tot = MortAdultBif$Ad_bm + MortAdultBif$Juv_bm

MortAdultBifPlot <- ggplot(data=MortAdultBif, aes(bif_par, y=Larv_bm*1000, color='Juveniles in focal nursery')) +
  geom_point(size = 3, aes(y=Bio_tot*1000, color='Subadults + Adults')) + 
  geom_point(size = 3) + labs(y = expression(Biomass ~ (mg ~ L^-1)), x = xlabel) + #theme(legend.position="bottom") + 
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

#MortAdultBifPlot
# ggsave("../Plots/App_bif_Fig3_HighR1max_ MortOffshore.png", plot = MortAdultBifPlot,
#        width = 1920, height = 1107,
#        scale = 2.5,
#        units = "px")

###Appendix bif Fig4: mortality offshore, low quality nurs####
MortAdultBif_lowQ = read.table(paste(Appendix_files, 'App_bif_Fig4_LowR1max_offshoremort.avg.out', sep = ""), header=F)
GiveNames(MortAdultBif_lowQ, bif = T, bifname = "bif_par", MinMax = F)
MortAdultBif_lowQ$Bio_tot = MortAdultBif_lowQ$Ad_bm + MortAdultBif_lowQ$Juv_bm

MortAdultBifPlot_lowQ <- ggplot(data=MortAdultBif_lowQ, aes(bif_par, y=Larv_bm*1000, color='Juveniles in focal nursery')) +
  geom_point(size = 3, aes(y=Bio_tot*1000, color='Subadults + Adults')) + 
  geom_point(size = 3) + labs(y = expression(Biomass ~ (mg ~ L^-1)), x = xlabel) + #theme(legend.position="bottom") + 
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

#MortAdultBifPlot_lowQ
# ggsave("../Plots/App_bif_Fig4_LowR1max_MortOffshore.png", plot = MortAdultBifPlot_lowQ,
#        width = 1920, height = 1107,
#        scale = 2.5,
#        units = "px")


###Appendix bif Fig5: loss of connectivity high quality nurs####
xlabel = "Connectivity to focal nursery"
ConnFigA = read.table(paste(Appendix_files, 'App_bif_Fig5_HighQ_connectivity.avg.out', sep = ""), header=F)
GiveNames(ConnFigA, bif = T, bifname = "bif_par", MinMax = F)
ConnFigA$Bio_tot = ConnFigA$Ad_bm + ConnFigA$Juv_bm
ConnFigA$bif_par = 1 - ConnFigA$bif_par

ConnFigAPlot <- ggplot(data=ConnFigA, aes(bif_par, y=Larv_bm*1000, color='Juveniles in focal nursery')) +
  geom_point(size = 3, aes(y=Bio_tot*1000, color='Subadults + Adults')) + 
  geom_point(size = 3) + labs(y = expression(Biomass ~ (mg ~ L^-1)), x = xlabel) + #theme(legend.position="bottom") + 
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

#ConnFigAPlot
# ggsave("../Plots/App_bif_Fig5_HighQ_Connectivity.png", plot = ConnFigAPlot,
#        width = 1920, height = 1107,
#        scale = 2.5,
#        units = "px")



###Appendix bif Fig6: loss of connectivity low food nurs####
xlabel = "Connectivity to focal nursery"
ConnFigB = read.table(paste(Appendix_files, 'App_bif_Fig6_LowR1max_connectivity.avg.out', sep = ""), header=F)
GiveNames(ConnFigB, bif = T, bifname = "bif_par", MinMax = F)
ConnFigB$Bio_tot = ConnFigB$Ad_bm + ConnFigB$Juv_bm
ConnFigB$bif_par = 1 - ConnFigB$bif_par

ConnFigBPlot <- ggplot(data=ConnFigB, aes(bif_par, y=Larv_bm*1000, color='Juveniles')) +
  geom_point(size = 3, aes(y=Bio_tot*1000, color='Subadults + Adults')) + 
  geom_point(size = 3) + labs(y = expression(Biomass ~ (mg ~ L^-1)), x = xlabel) + #theme(legend.position="bottom") + 
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

#ConnFigBPlot
# ggsave("../Plots/App_bif_Fig6_LowR1max_Connectivity.png", plot = ConnFigBPlot,
#        width = 1920, height = 1107,
#        scale = 2.5,
#        units = "px")


###Appendix bif Fig7: loss of connectivity high mort nurs####
xlabel = "Connectivity to focal nursery"

ConnFigC = read.table(paste(Appendix_files, 'App_bif_Fig7_nursmort_connectivity.avg.out', sep = ""), header=F)

GiveNames(ConnFigC, bif = T, bifname = "bif_par", MinMax = F)
ConnFigC$Bio_tot = ConnFigC$Ad_bm + ConnFigC$Juv_bm
ConnFigC$bif_par = 1 - ConnFigC$bif_par

ConnFigCPlot <- ggplot(data=ConnFigC, aes(bif_par, y=Larv_bm*1000, color='Juveniles in focal nursery')) +
  geom_point(size = 3, aes(y=Bio_tot*1000, color='Subadults + Adults')) + 
  geom_point(size = 3) + labs(y = expression(Biomass ~ (mg ~ L^-1)), x = xlabel) + #theme(legend.position="bottom") + 
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

#ConnFigCPlot
# ggsave("../Plots/App_bif_Fig7_MortNurs_Connectivity.png", plot = ConnFigCPlot,
#        width = 1920, height = 1107,
#        scale = 2.5,
#        units = "px")


###Appendix bif Fig8: loss of connectivity high mort offshore####
xlabel = "Connectivity to focal nursery"
ConnFigD = read.table(paste(Appendix_files, 'App_bif_Fig8_offshoremort_connectivity.avg.out', sep = ""), header=F)

GiveNames(ConnFigD, bif = T, bifname = "bif_par", MinMax = F)
ConnFigD$Bio_tot = ConnFigD$Ad_bm + ConnFigD$Juv_bm
ConnFigD$bif_par = 1 - ConnFigD$bif_par

ConnFigDPlot <- ggplot(data=ConnFigD, aes(bif_par, y=Larv_bm*1000, color='Juveniles in focal nursery')) +
  geom_point(size = 3, aes(y=Bio_tot*1000, color='Subadults + Adults')) + 
  geom_point(size = 3) + labs(y = expression(Biomass ~ (mg ~ L^-1)), x = xlabel) + #theme(legend.position="bottom") + 
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

#ConnFigDPlot

# ggsave("../Plots/App_bif_Fig8_MortOffShore_Connectivity.png", plot = ConnFigDPlot,
#        width = 1920, height = 1107,
#        scale = 2.5,
#        units = "px")
