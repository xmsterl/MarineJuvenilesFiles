rm(list = ls()) ##CLEAN EVERYTHING
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) #Set wd to current directory
source("Script_functions.R") #Load script#

#Set location output files#
Appendix_files = "../EBTfiles/Files_Appendix/Bifurcations/"

#----------------------------------------------------------------------------------------#
# Fig B1: reduced food availability in focal nursery
#----------------------------------------------------------------------------------------#
xlabel = expression(Food ~ productivity ~ focal ~ nursery ~ (mg ~ L^-1 ~ day^-1))

FoodFocalBif = read.table('../EBTfiles/Files_Appendix/Bifurcations/test_R1maxbif.avg.out', header=F)
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
# 
ggsave("../Plots/FoodFocalBif.png", plot = FoodFocalBifPlot,
  width = 1920, height = 1107,
       scale = 2.5, dpi = 300,
       units = "px")

#----------------------------------------------------------------------------------------#
# Fig B2: decreasing food productivity in focal nursery with outside recruitment
#----------------------------------------------------------------------------------------#
xlabel = expression(Food ~ productivity ~ focal ~ nursery ~ (mg ~ L^-1 ~ day^-1))

FoodFocalBif_recr = read.table('../EBTfiles/Files_Appendix/Bifurcations/app_bif_r1max+adrecr.avg.out', header=F)
FoodFocalBif_recr_back = read.table('../EBTfiles/Files_Appendix/Bifurcations/app_bif_r1max+adrecr_back.avg.out', header=F)
FoodFocalBif_recr = rbind(FoodFocalBif_recr, FoodFocalBif_recr_back)

GiveNames(FoodFocalBif_recr, bif = T, bifname = "bif_par", MinMax = F)
FoodFocalBif_recr$Bio_tot = FoodFocalBif_recr$Ad_bm + FoodFocalBif_recr$Juv_bm
FoodFocalBif_recr$bif_par = 1 - FoodFocalBif_recr$bif_par

FoodFocalBifPlot_recr <- ggplot(data=FoodFocalBif_recr, aes(bif_par, y=Larv_bm*1000, color='Juveniles in focal nursery')) +
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

FoodFocalBifPlot_recr
ggsave("../Plots/FoodFocalBif_recr.png", plot = FoodFocalBifPlot_recr,
       width = 1920, height = 1107,
       scale = 2.5, dpi = 300,
       units = "px")

#----------------------------------------------------------------------------------------#
# Fig B3: increased mortality in focal nursery
#----------------------------------------------------------------------------------------#
xlabel = expression(Mortality ~ rate ~ focal ~ nursery ~ (day^-1))

MortFocalBif = read.table('../EBTfiles/Files_Appendix/Bifurcations/FocalMortBif.avg.out', header=F)
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

#MortFocalBifPlot
ggsave("../Plots/MortFocalBif.png", plot = MortFocalBifPlot,
       width = 1920, height = 1107,
       scale = 2.5, dpi = 300,
       units = "px")


#----------------------------------------------------------------------------------------#
# Fig B4: increased mortality in offshore habitat, high quality alternative nurseries
#----------------------------------------------------------------------------------------#

## High quality of focal nursery
xlabel = expression(Mortality ~ rate ~ offshore ~ habitat ~ (day^-1))

MortAdultBif = read.table('../EBTfiles/Files_Appendix/Bifurcations/AdultMortBif_HighR1max.avg.out', header=F)
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

#MortAdultBifPlot
ggsave("../Plots/MortOffshoreBif_highQ.png", plot = MortAdultBifPlot,
       width = 1920, height = 1107,
       scale = 2.5, dpi = 300,
       units = "px")

#----------------------------------------------------------------------------------------#
# Fig B5: increased mortality in offshore habitat, low quality alternative nurseries
#----------------------------------------------------------------------------------------#
xlabel = expression(Mortality ~ rate ~ offshore ~ habitat ~ (day^-1))

MortAdultBif_lowQ = read.table('../EBTfiles/Files_Appendix/Bifurcations/AdultMortBif_LowR1max.avg.out', header=F)
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

ggsave("../Plots/MortOffshoreBif_lowQ.png", plot = MortAdultBifPlot_lowQ,
       width = 1920, height = 1107,
       scale = 2.5, dpi = 300,
       units = "px")

#----------------------------------------------------------------------------------------#
# Fig B6: increasing mortality in offshore habitat with outside recruitment
#----------------------------------------------------------------------------------------#
xlabel = expression(Mortality ~ rate ~ offshore ~ habitat ~ (day^-1))

MortAdultBif_recr = read.table('../EBTfiles/Files_Appendix/Bifurcations/App_bif_LowR1max_offshoremort+adrecr.avg.out', header=F)
GiveNames(MortAdultBif_recr, bif = T, bifname = "bif_par", MinMax = F)
MortAdultBif_recr$Bio_tot = MortAdultBif_recr$Ad_bm + MortAdultBif_recr$Juv_bm

MortAdultBifPlot_recr <- ggplot(data=MortAdultBif_recr, aes(bif_par, y=Larv_bm*1000, color='Juveniles in focal nursery')) +
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

MortAdultBifPlot_recr

ggsave("../Plots/MortOffshoreBif_recr.png", plot = MortAdultBifPlot_recr,
       width = 1920, height = 1107,
       scale = 2.5, dpi = 300,
       units = "px")


#----------------------------------------------------------------------------------------#
# Fig B7: Decreased connectivity to focal nursery with good quality of all habitats
#----------------------------------------------------------------------------------------#
xlabel = "Connectivity to focal nursery"

ConnFigA = read.table('../EBTfiles/Files_Appendix/Bifurcations/Connectivity_figA.avg.out', header=F)
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

#ConnFigAPlot
ggsave("../Plots/ConnFigA_bif.png", plot = ConnFigAPlot,
       width = 1920, height = 1107,
       scale = 2.5, dpi = 300,
       units = "px")



#-----------------------------------------------------------------------------------------------#
# Fig B8: Decreased connectivity to focal nursery with low food productivity in focal nursery
#-----------------------------------------------------------------------------------------------#
xlabel = "Connectivity to focal nursery"

ConnFigB = read.table('../EBTfiles/Files_Appendix/Bifurcations/Connectivity_figB.avg.out', header=F)
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

#ConnFigBPlot
ggsave("../Plots/ConnFigB_bif.png", plot = ConnFigBPlot,
       width = 1920, height = 1107,
       scale = 2.5, dpi = 300,
       units = "px")


#----------------------------------------------------------------------------------------#
# Fig B9: Decreased connectivity to focal nursery with high mortality in focal nursery
#----------------------------------------------------------------------------------------#
xlabel = "Connectivity to focal nursery"

ConnFigC = read.table('../EBTfiles/Files_Appendix/Bifurcations/Connectivity_figC.avg.out', header=F)
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

#ConnFigCPlot
ggsave("../Plots/ConnFigC_bif.png", plot = ConnFigCPlot,
       width = 1920, height = 1107,
       scale = 2.5, dpi = 300,
       units = "px")


#-------------------------------------------------------------------------------------------#
# Fig B10: Decreased connectivity to focal nursery with high mortality in offshore habitat
#-------------------------------------------------------------------------------------------#
xlabel = "Connectivity to focal nursery"

ConnFigD = read.table('../EBTfiles/Files_Appendix/Bifurcations/Connectivity_figD.avg.out', header=F)
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

ggsave("../Plots/ConnFigD_bif.png", plot = ConnFigDPlot,
       width = 1920, height = 1107,
       scale = 2.5, dpi = 300,
       units = "px")

#----------------------------------------------------------------------------------------#
# Fig B11: increasing egg mortality
#----------------------------------------------------------------------------------------#
xlabel = "Egg mortality"

MuEgg = read.table('../EBTfiles/Files_Appendix/Bifurcations/App_bif_mu_egg.avg.out', header=F)
GiveNames(MuEgg, bif = T, bifname = "bif_par", MinMax = F)
MuEgg$Bio_tot = MuEgg$Ad_bm + MuEgg$Juv_bm

MuEggPlot <- ggplot(data=MuEgg, aes(bif_par, y=Larv_bm*1000, color='Juveniles in focal nursery')) +
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
MuEggPlot

ggsave("../Plots/MuEgg.png", plot = MuEggPlot,
       width = 1920, height = 1107,
       scale = 2.5, dpi = 300,
       units = "px")

