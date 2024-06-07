rm(list = ls()) ##CLEAN EVERYTHING
setwd('U:\\Manuscript\\Timeseries') ##CHANGE THIS FOLDER TO YOUR OWN LOCATION
source("U:\\Manuscript\\Code_figures\\Script_functions_MJ.R") #Load script#
xlabtime = expression(atop("",atop("Time (years)","")))
library(ggpubr)

#Set locations#
FigMain_loc = "U:\\Manuscript\\Figures_main"
FigAppendix_loc = "Figures/Figures_appendx"
Bif_loc = "D:\\Internship\\Manuscript\\Files_Hanna\\EBTRuns\\Bifurcations\\"
Time_loc = "U:\\Manuscript\\Timeseries\\"

#----------------------------------------------------------------------------------------#
# Result 1: reduced food in focal nursery
#----------------------------------------------------------------------------------------#

DataNames = c("HighQ_HighR1max",
              "HighQ_InterR1max",
              "HighQ_LowR1max")
BifPar = c(0.075, 0.025, 0.005)
minMaxTime <- data.frame()
AllTimeData <- data.frame()

for (i in 1:length(BifPar)) {
  filename <- paste0(Time_loc, DataNames[i], ".out", sep = '')
  GetData <- read.table(filename, header = F)
  GiveNames(GetData, maxcohorts = CohNr)
  GetData$Bif = BifPar[i]
  NewData = data.frame(BifPar = BifPar[i], MinTime = min(GetData$Time),
                       MaxTime = max(GetData$Time))
  minMaxTime = rbind(minMaxTime, NewData)
  AllTimeData = rbind(AllTimeData, GetData)
}

rm(GetData)
AllTimeData_R1maxHighQ = AllTimeData

label1 = expression(paste("7.5 mg ", L^-1, day^-1))
label2 = expression(paste("2.5 mg ", L^-1, day^-1))
label3 = expression(paste("0.5 mg ", L^-1, day^-1))
xlabtime = expression(atop("",atop("Time (years)","")))

# Add column with combined Subadult and Adult biomass
AllTimeData_R1maxHighQ$Juv_Ad_bm = AllTimeData_R1maxHighQ$Juv_bm + 
  AllTimeData_R1maxHighQ$Ad_bm

######Make plot of AllTimeData#####
SBMTimePlotR1maxHighQ = ggplot(data =  AllTimeData_R1maxHighQ,
                               aes(x = Time/Season, y = Larv_bm * 1000)) +
  geom_point(data = subset(AllTimeData_R1maxHighQ, Larv_bm > 0), size = 0.5, aes(colour = "aJuveniles")) +
  #geom_point(data = subset(AllTimeData_R1maxHighQ, Est_bm > 0), size = 0.1, aes(colour = "aAJuveniles", y = Est_bm * 1000)) +
  geom_point(data = subset(AllTimeData_R1maxHighQ, Juv_Ad_bm > 0), 
             size = 0.5, aes(colour = "bJuveniles", y = Juv_Ad_bm * 1000)) +
  #geom_point(data = subset(AllTimeData_R1maxHighQ, Ad_bm > 0), size = 0.1, aes(colour = "cAdults", y = Ad_bm * 1000)) + 
  layout + 
  scale_color_manual(values = c("#4B0055", "#36E9A7"),
                     labels = c("Juveniles in focal nursery", "Subadults + Adults"))+
  ylab(expression(atop("", atop("Fish biomass",  paste("(mg ", L^-1, ")"))))) +
  xlab(xlabtime) +
  guides(color = guide_legend(byrow = TRUE,  override.aes = list(size = 3))) + 
  geom_vline(xintercept = (subset(minMaxTime, BifPar < 0.075)$MinTime) / Season, 
             linetype = 'dashed') +
  theme(legend.position = c(0.83, 0.75),
        legend.background = element_blank(),
        legend.text = element_text(size=11),
        axis.text=element_text(size=14),
        axis.title=element_text(size=20)) +
  annotate(geom = "text", x = 25, y = 300, label = 'High', size = 6) + 
  annotate(geom = "text", x = 75, y = 300, label = 'Intermediate', size = 6) + 
  annotate(geom = "text", x = 125, y = 300, label = 'Low', size = 6) +
  NULL
#SBMTimePlotR1maxHighQ


SBMTimePlotR1maxHighQ_res = ggplot(data =  AllTimeData_R1maxHighQ,
                                   aes(x = Time/Season, y = R1 * 1000)) +
  geom_point(data = subset(AllTimeData_R1maxHighQ, R1 > 0), size = 0.5, aes(colour = "R1")) +
  #geom_point(data = subset(AllTimeData_R1maxHighQ, Est_bm > 0), size = 0.1, aes(colour = "aAJuveniles", y = Est_bm * 1000)) +
  geom_point(data = subset(AllTimeData_R1maxHighQ, R2 > 0), 
             size = 0.5, aes(colour = "R2", y = R2 * 1000)) +
  #geom_point(data = subset(AllTimeData_R1maxHighQ, Ad_bm > 0), size = 0.1, aes(colour = "cAdults", y = Ad_bm * 1000)) + 
  layout + 
  scale_color_manual(values = c("#4B0055", "#36E9A7"),
                     labels = c("Focal nursery", "Adult habitat"))+
  ylab(expression(atop("", atop("Food density",  paste("(mg ", L^-1, ")"))))) +
  xlab(xlabtime) +
  guides(color = guide_legend(byrow = TRUE,  override.aes = list(size = 3))) + 
  geom_vline(xintercept = (subset(minMaxTime, BifPar < 0.075)$MinTime) / Season, 
             linetype = 'dashed') +
  theme(legend.position = c(0.83, 0.75),
        legend.background = element_blank(),
        legend.text = element_text(size=11),
        axis.text=element_text(size=14),
        axis.title=element_text(size=20)) +
  annotate(geom = "text", x = 125, y = 8, label = 'Starvation threshold', size = 4) + 
  # annotate(geom = "text", x = 60, y = 100, label = label2, size = 5) + 
  # annotate(geom = "text", x = 100, y = 100, label = label3, size = 5) +
  geom_hline(yintercept = 5, linetype = "dashed") + 
  scale_y_continuous(breaks = c(0, 5, 20, 40, 60), 
                     labels = c(0, 5, 20, 40, 60)) +
  NULL
#SBMTimePlotR1maxHighQ_res  


# Combine plots, make sure to also have growthcurves plots
ggarrange(SBMTimePlotR1maxHighQ +
            theme(axis.title.x=element_blank(),
                  axis.text.x=element_blank(),
                  axis.ticks.x=element_blank(),
                  plot.margin = unit(c(0,1,0,0), 'lines')),
          HighR1max_growth + theme(plot.margin = unit(c(0,1,0,0), 'lines')) +
            xlim(0, 20) + 
            theme(axis.title.x=element_blank(),
                  axis.text.x=element_blank(),
                  axis.ticks.x=element_blank(),
                  plot.margin = unit(c(0,1,0,0), 'lines')), 
          SBMTimePlotR1maxHighQ_res + theme(plot.margin = unit(c(0,1,0,0), 'lines')),
          InterR1max_growth + xlim(0,20) + 
            theme(plot.margin = unit(c(0,1,0,0) , 'lines')),
          nrow = 2, ncol = 2, labels = c("A", "C", "B", "D"),
          align = "hv")


#----------------------------------------------------------------------------------------#
# Result 2: increased mortality in focal nursery
#----------------------------------------------------------------------------------------#
Time_loc = "U:\\Manuscript\\Runs timeserie\\Runs timeserie\\"

DataNames = c("HighQ_noMort",
              "HighQ_InterMort",
              "HighQ_HighMort")
BifPar = c(0, 0.003, 0.006)
minMaxTime <- data.frame()
AllTimeData <- data.frame()

for (i in 1:length(BifPar)) {
  filename <- paste0(Time_loc, DataNames[i], ".out", sep = '')
  GetData <- read.table(filename, header = F)
  GiveNames(GetData, maxcohorts = CohNr)
  GetData$Bif = BifPar[i]
  NewData = data.frame(BifPar = BifPar[i], MinTime = min(GetData$Time),
                       MaxTime = max(GetData$Time))
  minMaxTime = rbind(minMaxTime, NewData)
  AllTimeData = rbind(AllTimeData, GetData)
}

rm(GetData)
AllTimeData_HighQ = AllTimeData

# Add column with combined Subadult and Adult biomass
AllTimeData_HighQ$Juv_Ad_bm = AllTimeData_HighQ$Juv_bm + 
  AllTimeData_HighQ$Ad_bm

###Make plot mortality series High Q#####
MortTimePlotHighQ = ggplot(data =  AllTimeData_HighQ,
                          aes(x = Time/Season, y = Larv_bm * 1000)) +
  geom_point(data = subset(AllTimeData_HighQ, Larv_bm > 0), size = 0.5, aes(colour = "aJuveniles")) +
  #geom_point(data = subset(AllTimeData_R1maxHighQ, Est_bm > 0), size = 0.1, aes(colour = "aAJuveniles", y = Est_bm * 1000)) +
  geom_point(data = subset(AllTimeData_HighQ, Juv_Ad_bm > 0), 
             size = 0.5, aes(colour = "bJuveniles", y = Juv_Ad_bm * 1000)) +
  #geom_point(data = subset(AllTimeData_R1maxHighQ, Ad_bm > 0), size = 0.1, aes(colour = "cAdults", y = Ad_bm * 1000)) + 
  layout + 
  scale_color_manual(values = c("#4B0055", "#36E9A7"),
                     labels = c("Juveniles in focal nursery", "Subadults + Adults")) +
  ylab(expression(atop("", atop("Fish biomass",  paste("(mg ", L^-1, ")"))))) +
  xlab(xlabtime) +
  guides(color = guide_legend(byrow = TRUE,  override.aes = list(size = 3))) + 
  geom_vline(xintercept = (subset(minMaxTime, BifPar > 0)$MinTime) / Season, 
             linetype = 'dashed') +
  theme(legend.position = c(0.83, 0.75),
        legend.background = element_blank(),
        legend.text = element_text(size=11),
        axis.text=element_text(size=14),
        axis.title=element_text(size=20)) +
  annotate(geom = "text", x = 25, y = 300, label = 'Low', size = 6) +
  annotate(geom = "text", x = 75, y = 300, label = "Intermediate", size = 6) + 
  annotate(geom = "text", x = 125, y = 300, label = "High", size = 6) +
  NULL
#MortTimePlotHighQ


MortTimePlotHighQ_res = ggplot(data =  AllTimeData_HighQ,
                           aes(x = Time/Season, y = R1 * 1000)) +
  geom_point(data = subset(AllTimeData_HighQ, R1 > 0), size = 0.5, aes(colour = "aJuveniles")) +
  #geom_point(data = subset(AllTimeData_R1maxHighQ, Est_bm > 0), size = 0.1, aes(colour = "aAJuveniles", y = Est_bm * 1000)) +
  geom_point(data = subset(AllTimeData_HighQ, R2 > 0), 
             size = 0.5, aes(colour = "bJuveniles", y = R2 * 1000)) +
  #geom_point(data = subset(AllTimeData_R1maxHighQ, Ad_bm > 0), size = 0.1, aes(colour = "cAdults", y = Ad_bm * 1000)) + 
  layout + 
  scale_color_manual(values = c("#4B0055", "#36E9A7"),
                     labels = c("Focal nursery", "Adult habitat"))+
  ylab(expression(atop("", atop("Food density",  paste("(mg ", L^-1, ")"))))) +
  xlab(xlabtime) +
  guides(color = guide_legend(byrow = TRUE,  override.aes = list(size = 3))) + 
  geom_vline(xintercept = (subset(minMaxTime, BifPar > 0)$MinTime) / Season, 
             linetype = 'dashed') +
  theme(legend.position = c(0.83, 0.80),
        legend.background = element_blank(),
        legend.text = element_text(size=11),
        axis.text=element_text(size=14),
        axis.title=element_text(size=20)) +
  annotate(geom = "text", x = 125, y = 8, label = 'Starvation threshold', size = 4) +
  # annotate(geom = "text", x = 60, y = 150, label = "0.003 / Day", size = 5) + 
  # annotate(geom = "text", x = 100, y = 150, label = "0.006 / Day", size = 5) +
  geom_hline(yintercept = 5, linetype = "dashed") +   
  scale_y_continuous(breaks = c(0, 5, 25, 50, 75, 100), 
                     labels = c(0, 5, 25, 50, 75, 100)) +
  NULL
#MortTimePlotHighQ

ggarrange(MortTimePlotHighQ +
            theme(axis.title.x=element_blank(),
                  axis.text.x=element_blank(),
                  axis.ticks.x=element_blank(),
                  plot.margin = unit(c(0,1,0,0), 'lines')),
          NoMortFocal_growth + theme(plot.margin = unit(c(0,1,0,0), 'lines')) +
            xlim(0,7) + 
            theme(axis.title.x=element_blank(),
                  axis.text.x=element_blank(),
                  axis.ticks.x=element_blank(),
                  plot.margin = unit(c(0,1,0,0), 'lines')), 
          MortTimePlotHighQ_res + theme(plot.margin = unit(c(0,1,0,0), 'lines')),
          InterMortFocal_growth + xlim(0,7) +
            theme(plot.margin = unit(c(0,1,0,0), 'lines')),
          nrow = 2, ncol = 2, labels = c("A", "C", "B", "D"),
          align = "hv")

#----------------------------------------------------------------------------------------#
# Result 3: increased mortality in offshore habitat
#----------------------------------------------------------------------------------------#

DataNames = c("HighQ_HighR1max_NoNSMort",
              "HighQ_HighR1max_InterNSMort",
              "HighQ_HighR1max_HighNSMort")
BifPar = c(0, 0.005, 0.01)
minMaxTime <- data.frame()
AllTimeData <- data.frame()

for (i in 1:length(BifPar)) {
  filename <- paste0(Time_loc, DataNames[i], ".out", sep = '')
  GetData <- read.table(filename, header = F)
  GiveNames(GetData, maxcohorts = CohNr)
  GetData$Bif = BifPar[i]
  NewData = data.frame(BifPar = BifPar[i], MinTime = min(GetData$Time),
                       MaxTime = max(GetData$Time))
  minMaxTime = rbind(minMaxTime, NewData)
  AllTimeData = rbind(AllTimeData, GetData)
}

rm(GetData)
AllTimeData_HighQ = AllTimeData

# Add column with combined Subadult and Adult biomass
AllTimeData_HighQ$Juv_Ad_bm = AllTimeData_HighQ$Juv_bm + 
  AllTimeData_HighQ$Ad_bm

###Make plot mortality series High R1 max#####
MortTimePlotHighQ = ggplot(data =  AllTimeData_HighQ,
                           aes(x = Time/Season, y = Larv_bm * 1000)) +
  geom_point(data = subset(AllTimeData_HighQ, Larv_bm > 0), size = 0.5, aes(colour = "aJuveniles")) +
  #geom_point(data = subset(AllTimeData_R1maxHighQ, Est_bm > 0), size = 0.1, aes(colour = "aAJuveniles", y = Est_bm * 1000)) +
  geom_point(data = subset(AllTimeData_HighQ, Juv_Ad_bm > 0), 
             size = 0.5, aes(colour = "bJuveniles", y = Juv_Ad_bm * 1000)) +
  #geom_point(data = subset(AllTimeData_R1maxHighQ, Ad_bm > 0), size = 0.1, aes(colour = "cAdults", y = Ad_bm * 1000)) + 
  layout + 
  scale_color_manual(values = c("#4B0055", "#36E9A7"),
                     labels = c("Juveniles in focal nursery", "Subadults + Adults")) +
  ylab(expression(atop("", atop("Fish biomass",  paste("(mg ", L^-1, ")"))))) +
  xlab(xlabtime) +
  guides(color = guide_legend(byrow = TRUE,  override.aes = list(size = 3))) + 
  geom_vline(xintercept = (subset(minMaxTime, BifPar > 0)$MinTime) / Season, 
             linetype = 'dashed') +
  theme(legend.position = c(0.82, 0.75),
        legend.background = element_blank(),
        legend.text = element_text(size=11),
        axis.text=element_text(size=14),
        axis.title=element_text(size=20)) +
  annotate(geom = "text", x = 25, y = 300, label = 'Low', size = 10) +
  annotate(geom = "text", x = 75, y = 300, label = "Intermediate", size = 10) + 
  annotate(geom = "text", x = 125, y = 300, label = "High", size = 10) +
  xlim(0, 150) + ylim(0, 310) +
  NULL
#MortTimePlotHighQ


# For low quality of WS

Time_loc = "U:\\Manuscript\\Runs timeserie\\Runs timeserie\\"

DataNames = c("HighQ_LowR1max_NoNSMort",
              "HighQ_LowR1max_InterNSMort",
              "HighQ_LowR1max_HighNSMort")
BifPar = c(0, 0.005, 0.01)
minMaxTime <- data.frame()
AllTimeData <- data.frame()

for (i in 1:length(BifPar)) {
  filename <- paste0(Time_loc, DataNames[i], ".out", sep = '')
  GetData <- read.table(filename, header = F)
  GiveNames(GetData, maxcohorts = CohNr)
  GetData$Bif = BifPar[i]
  NewData = data.frame(BifPar = BifPar[i], MinTime = min(GetData$Time),
                       MaxTime = max(GetData$Time))
  minMaxTime = rbind(minMaxTime, NewData)
  AllTimeData = rbind(AllTimeData, GetData)
}

rm(GetData)
AllTimeData_HighQ = AllTimeData

# Add column with combined Subadult and Adult biomass
AllTimeData_HighQ$Juv_Ad_bm = AllTimeData_HighQ$Juv_bm + 
  AllTimeData_HighQ$Ad_bm

###Make plot mortality series High R1 max#####
MortTimePlotHighQ_lowR1 = ggplot(data =  AllTimeData_HighQ,
                           aes(x = Time/Season, y = Larv_bm * 1000)) +
  geom_point(data = subset(AllTimeData_HighQ, Larv_bm > 0), size = 0.5, aes(colour = "aJuveniles")) +
  #geom_point(data = subset(AllTimeData_R1maxHighQ, Est_bm > 0), size = 0.1, aes(colour = "aAJuveniles", y = Est_bm * 1000)) +
  geom_point(data = subset(AllTimeData_HighQ, Juv_Ad_bm > 0), 
             size = 0.5, aes(colour = "bJuveniles", y = Juv_Ad_bm * 1000)) +
  #geom_point(data = subset(AllTimeData_R1maxHighQ, Ad_bm > 0), size = 0.1, aes(colour = "cAdults", y = Ad_bm * 1000)) + 
  layout + 
  scale_color_manual(values = c("#4B0055", "#36E9A7"),
                     labels = c("Juveniles in focal nursery", "Subadults + Adults")) +
  ylab(expression(atop("", atop("Fish biomass",  paste("(mg ", L^-1, ")"))))) +
  xlab(xlabtime) +
  guides(color = guide_legend(byrow = TRUE,  override.aes = list(size = 3))) + 
  geom_vline(xintercept = (subset(minMaxTime, BifPar > 0)$MinTime) / Season, 
             linetype = 'dashed') +
  theme(legend.position = 'none',
        legend.background = element_blank(),
        legend.text = element_text(size=11),
        axis.text=element_text(size=14),
        axis.title=element_text(size=20)) +
  # annotate(geom = "text", x = 75, y = 100, label = "0.005 / Day", size = 4) + 
  # annotate(geom = "text", x = 125, y = 100, label = "0.01 / Day", size = 4) +
  scale_x_continuous(breaks = c(0, 25, 50, 75, 100, 125, 150),
                                labels = c(0, 25, 50, 75, 100, 125, 150), 
                                limits = c(0,150)) +
  NULL
#MortTimePlotHighQ_lowR1

ggarrange(MortTimePlotHighQ +
            theme(aspect.ratio = 0.3,
                  axis.title.x=element_blank(),
                  axis.text.x=element_blank(),
                  axis.ticks.x=element_blank(),
                  plot.margin = margin(t = 0, r = 0, b = 0, l = 0),
                  plot.title = element_text(size=20)) +
            ggtitle('High quality focal nursery'),
          MortTimePlotHighQ_lowR1 + theme(aspect.ratio = 0.3,
                                          plot.margin = margin(t = 0, r = 0, b = 0, l = 0),
                                          plot.title = element_text(size=20)) +
            ggtitle('Low quality focal nursery'),
          nrow = 2, ncol = 1, labels = c("A", "B"),
          align = "hv")


#----------------------------------------------------------------------------------------#
# Result 4: reduced connectivity to focal nursery
#----------------------------------------------------------------------------------------#

# For high quality of other nurseries
DataNames = c("HighQ_HighConnect",
              "HighQ_InterConnect",
              "HighQ_LowConnect")
BifPar = c(1, 0.5, 0.1)
minMaxTime <- data.frame()
AllTimeData <- data.frame()

for (i in 1:length(BifPar)) {
  filename <- paste0(Time_loc, DataNames[i], ".out", sep = '')
  GetData <- read.table(filename, header = F)
  GiveNames(GetData, maxcohorts = CohNr)
  GetData$Bif = BifPar[i]
  NewData = data.frame(BifPar = BifPar[i], MinTime = min(GetData$Time),
                       MaxTime = max(GetData$Time))
  minMaxTime = rbind(minMaxTime, NewData)
  AllTimeData = rbind(AllTimeData, GetData)
}

rm(GetData)
AllTimeData_HighQ = AllTimeData

# Add column with combined Subadult and Adult biomass
AllTimeData_HighQ$Juv_Ad_bm = AllTimeData_HighQ$Juv_bm + 
  AllTimeData_HighQ$Ad_bm


ConnTimePlotHighQ = ggplot(data =  AllTimeData_HighQ,
                          aes(x = Time/Season, y = Larv_bm * 1000)) +
  geom_point(data = subset(AllTimeData_HighQ, Larv_bm > 0), size = 0.5, aes(colour = "aJuveniles")) +
  #geom_point(data = subset(AllTimeData_R1maxHighQ, Est_bm > 0), size = 0.1, aes(colour = "aAJuveniles", y = Est_bm * 1000)) +
  geom_point(data = subset(AllTimeData_HighQ, Juv_Ad_bm > 0), 
             size = 0.5, aes(colour = "bJuveniles", y = Juv_Ad_bm * 1000)) +
  #geom_point(data = subset(AllTimeData_R1maxHighQ, Ad_bm > 0), size = 0.1, aes(colour = "cAdults", y = Ad_bm * 1000)) + 
  layout + 
  scale_color_manual(values = c("#4B0055", "#36E9A7"),
                     labels = c("Juveniles in focal nursery", "Subadults + Adults")) +
  ylab(expression(atop("", atop("Fish biomass",  paste("(mg ", L^-1, ")"))))) +
  xlab(xlabtime) +
  guides(color = guide_legend(byrow = TRUE,  override.aes = list(size = 3))) + 
  geom_vline(xintercept = (subset(minMaxTime, 1-BifPar > 0)$MinTime) / Season, 
             linetype = 'dashed') +
  theme(legend.position = c(0.83, 0.85),
        legend.background = element_blank(),
        legend.text = element_text(size=11),
        axis.text=element_text(size=14),
        axis.title=element_text(size=20)) +
  annotate(geom = "text", x = 25, y = 290, label = "High", size = 6) +
  annotate(geom = "text", x = 75, y = 290, label = "Intermediate", size = 6) + 
  annotate(geom = "text", x = 125, y = 290, label = "Low", size = 6) +
  scale_x_continuous(breaks = c(0, 25, 50, 75, 100, 125, 150),
                     labels = c(0, 25, 50, 75, 100, 125, 150), 
                     limits = c(0,150)) +
  NULL
#ConnTimePlotHighQ

# High quality other nurseries, high mortality NS
Time_loc = "U:\\Manuscript\\Runs timeserie\\Runs timeserie\\"

DataNames = c("HighQ_HighNSMort_HighConnect",
              "HighQ_HighNSMort_InterConnect",
              "HighQ_HighNSMort_LowConnect")
BifPar = c(1, 0.5, 0.1)
minMaxTime <- data.frame()
AllTimeData <- data.frame()

for (i in 1:length(BifPar)) {
  filename <- paste0(Time_loc, DataNames[i], ".out", sep = '')
  GetData <- read.table(filename, header = F)
  GiveNames(GetData, maxcohorts = CohNr)
  GetData$Bif = BifPar[i]
  NewData = data.frame(BifPar = BifPar[i], MinTime = min(GetData$Time),
                       MaxTime = max(GetData$Time))
  minMaxTime = rbind(minMaxTime, NewData)
  AllTimeData = rbind(AllTimeData, GetData)
}

rm(GetData)
AllTimeData_HighQ = AllTimeData

# Add column with combined Subadult and Adult biomass
AllTimeData_HighQ$Juv_Ad_bm = AllTimeData_HighQ$Juv_bm + 
  AllTimeData_HighQ$Ad_bm


ConnHighQHighNSMort = ggplot(data =  AllTimeData_HighQ,
                            aes(x = Time/Season, y = Larv_bm * 1000)) +
  geom_point(data = subset(AllTimeData_HighQ, Larv_bm > 0), size = 0.5, aes(colour = "aJuveniles")) +
  #geom_point(data = subset(AllTimeData_R1maxHighQ, Est_bm > 0), size = 0.1, aes(colour = "aAJuveniles", y = Est_bm * 1000)) +
  geom_point(data = subset(AllTimeData_HighQ, Juv_Ad_bm > 0), 
             size = 0.5, aes(colour = "bJuveniles", y = Juv_Ad_bm * 1000)) +
  #geom_point(data = subset(AllTimeData_R1maxHighQ, Ad_bm > 0), size = 0.1, aes(colour = "cAdults", y = Ad_bm * 1000)) + 
  layout + 
  scale_color_manual(values = c("#4B0055", "#36E9A7"),
                     labels = c("Juveniles in focal nursery", "Subadults + Adults")) +
  ylab(expression(atop("", atop("Fish biomass",  paste("(mg ", L^-1, ")"))))) +
  xlab(xlabtime) +
  guides(color = guide_legend(byrow = TRUE,  override.aes = list(size = 3))) + 
  geom_vline(xintercept = (subset(minMaxTime, 1-BifPar > 0)$MinTime) / Season, 
             linetype = 'dashed') +
  theme(legend.position = c(0.83, 0.85),
        legend.background = element_blank(),
        legend.text = element_text(size=11),
        axis.text=element_text(size=14),
        axis.title=element_text(size=20)) +
  annotate(geom = "text", x = 25, y = 170, label = "High", size = 6) +
  annotate(geom = "text", x = 75, y = 170, label = "Intermediate", size = 6) + 
  annotate(geom = "text", x = 125, y = 170, label = "Low", size = 6) +
  scale_x_continuous(breaks = c(0, 25, 50, 75, 100, 125, 150),
                     labels = c(0, 25, 50, 75, 100, 125, 150), 
                     limits = c(0,150)) +
  NULL
#ConnHighQHighNSMort


# High quality other nurseries, high mortality WS
Time_loc = "U:\\Manuscript\\Timeseries\\"

DataNames = c("HighQ_HighWSMort_HighConnect",
              "HighQ_HighWSMort_InterConnect",
              "HighQ_HighWSMort_LowConnect")
BifPar = c(1, 0.5, 0.1)
minMaxTime <- data.frame()
AllTimeData <- data.frame()

for (i in 1:length(BifPar)) {
  filename <- paste0(Time_loc, DataNames[i], ".out", sep = '')
  GetData <- read.table(filename, header = F)
  GiveNames(GetData, maxcohorts = CohNr)
  GetData$Bif = BifPar[i]
  NewData = data.frame(BifPar = BifPar[i], MinTime = min(GetData$Time),
                       MaxTime = max(GetData$Time))
  minMaxTime = rbind(minMaxTime, NewData)
  AllTimeData = rbind(AllTimeData, GetData)
}

rm(GetData)
AllTimeData_HighQ = AllTimeData

# Add column with combined Subadult and Adult biomass
AllTimeData_HighQ$Juv_Ad_bm = AllTimeData_HighQ$Juv_bm + 
  AllTimeData_HighQ$Ad_bm


ConnHighQHighWSMort = ggplot(data =  AllTimeData_HighQ,
                            aes(x = Time/Season, y = Larv_bm * 1000)) +
  geom_point(data = subset(AllTimeData_HighQ, Larv_bm > 0), size = 0.5, aes(colour = "aJuveniles")) +
  #geom_point(data = subset(AllTimeData_R1maxHighQ, Est_bm > 0), size = 0.1, aes(colour = "aAJuveniles", y = Est_bm * 1000)) +
  geom_point(data = subset(AllTimeData_HighQ, Juv_Ad_bm > 0), 
             size = 0.5, aes(colour = "bJuveniles", y = Juv_Ad_bm * 1000)) +
  #geom_point(data = subset(AllTimeData_R1maxHighQ, Ad_bm > 0), size = 0.1, aes(colour = "cAdults", y = Ad_bm * 1000)) + 
  layout + 
  scale_color_manual(values = c("#4B0055", "#36E9A7"),
                     labels = c("Juveniles in focal nursery", "Subadults + Adults")) +
  ylab(expression(atop("", atop("Fish biomass",  paste("(mg ", L^-1, ")"))))) +
  xlab(xlabtime) +
  guides(color = guide_legend(byrow = TRUE,  override.aes = list(size = 3))) + 
  geom_vline(xintercept = (subset(minMaxTime, 1-BifPar > 0)$MinTime) / Season, 
             linetype = 'dashed') +
  theme(legend.position = c(0.83, 0.85),
        legend.background = element_blank(),
        legend.text = element_text(size=11),
        axis.text=element_text(size=14),
        axis.title=element_text(size=20)) +
  annotate(geom = "text", x = 25, y = 165, label = "High", size = 6) +
  annotate(geom = "text", x = 75, y = 165, label = "Intermediate", size = 6) + 
  annotate(geom = "text", x = 125, y = 165, label = "Low", size = 6) +
  scale_x_continuous(breaks = c(0, 25, 50, 75, 100, 125, 150),
                     labels = c(0, 25, 50, 75, 100, 125, 150), 
                     limits = c(0,150)) +
  NULL
#ConnHighQHighWSMort


# High quality other nurseries, low food availability focal nursery
setwd('U:\\Manuscript\\Appendix')
Time_loc = "U:\\Manuscript\\Appendix\\"
DataNames = c("HighQ_LowR1max_HighConnect_LongTime",
              "HighQ_LowR1max_InterConnect_LongTime",
              "HighQ_LowR1max_LowConnect_LongTime")
BifPar = c(1, 0.5, 0.1)
minMaxTime <- data.frame()
AllTimeData <- data.frame()

for (i in 1:length(BifPar)) {
  filename <- paste0(Time_loc, DataNames[i], ".out", sep = '')
  GetData <- read.table(filename, header = F)
  GiveNames(GetData, maxcohorts = CohNr)
  GetData$Bif = BifPar[i]
  NewData = data.frame(BifPar = BifPar[i], MinTime = min(GetData$Time),
                       MaxTime = max(GetData$Time))
  minMaxTime = rbind(minMaxTime, NewData)
  AllTimeData = rbind(AllTimeData, GetData)
}

rm(GetData)
AllTimeData_HighQ = AllTimeData

# Add column with combined Subadult and Adult biomass
AllTimeData_HighQ$Juv_Ad_bm = AllTimeData_HighQ$Juv_bm + 
  AllTimeData_HighQ$Ad_bm

AllTimeData_HighQ = AllTimeData_HighQ[37500:85003,]

###Make plot mortality series High Q#####
ConnHighQLowR1max = ggplot(data =  AllTimeData_HighQ,
                                   aes(x = Time/Season, y = Larv_bm * 1000)) +
  geom_point(data = subset(AllTimeData_HighQ, Larv_bm > 0), size = 0.5, aes(colour = "aJuveniles")) +
  #geom_point(data = subset(AllTimeData_R1maxHighQ, Est_bm > 0), size = 0.1, aes(colour = "aAJuveniles", y = Est_bm * 1000)) +
  geom_point(data = subset(AllTimeData_HighQ, Juv_Ad_bm > 0), 
             size = 0.5, aes(colour = "bJuveniles", y = Juv_Ad_bm * 1000)) +
  #geom_point(data = subset(AllTimeData_R1maxHighQ, Ad_bm > 0), size = 0.1, aes(colour = "cAdults", y = Ad_bm * 1000)) + 
  layout + 
  scale_color_manual(values = c("#4B0055", "#36E9A7"),
                     labels = c("Juveniles in focal nursery", "Subadults + Adults")) +
  ylab(expression(atop("", atop("Fish biomass",  paste("(mg ", L^-1, ")"))))) +
  xlab(xlabtime) +
  guides(color = guide_legend(byrow = TRUE,  override.aes = list(size = 3))) + 
  geom_vline(xintercept = (subset(minMaxTime, MinTime > 0)$MinTime) / Season, 
             linetype = 'dashed') +
  theme(legend.position = c(0.83, 0.85),
        legend.background = element_blank(),
        legend.text = element_text(size=11),
        axis.text=element_text(size=14),
        axis.title=element_text(size=20)) +
  annotate(geom = "text", x = 175, y = 150, label = 'Low', size = 6) +
  annotate(geom = "text", x = 225, y = 150, label = "Intermediate", size = 6) + 
  annotate(geom = "text", x = 275, y = 150, label = "High", size = 6) +
  xlim(150,300) +  
  NULL


ggarrange(ConnTimePlotHighQ +
            theme(axis.title.x=element_blank(),
                  axis.text.x=element_blank(),
                  axis.ticks.x=element_blank(),
                  plot.margin = unit(c(0,1,0,0), 'lines')),
          ConnHighQLowR1max + 
            theme(axis.title.x=element_blank(),
                  axis.text.x=element_blank(),
                  axis.ticks.x=element_blank(),
                  plot.margin = unit(c(0,1,0,0), 'lines')),
          ConnHighQHighWSMort + 
            theme(plot.margin = unit(c(0,1,0,0), 'lines')),
          ConnHighQHighNSMort + theme(plot.margin = unit(c(0,1,0,0), 'lines')),
          nrow = 2, ncol = 2, labels = c("A", "B", "C", "D"), align = 'hv')


#----------------------------------------------------------------------------------------#
# Appendix plots
#----------------------------------------------------------------------------------------#

# Plot 1: increased mortality focal nursery with low quality other nurseries
DataNames = c("LowQ_HighR1max",
              "LowQ_InterR1max",
              "LowQ_LowR1max")
BifPar = c(0.075, 0.025, 0.005)
minMaxTime <- data.frame()
AllTimeData <- data.frame()

for (i in 1:length(BifPar)) {
  filename <- paste0(Time_loc, DataNames[i], ".out", sep = '')
  GetData <- read.table(filename, header = F)
  GiveNames(GetData, maxcohorts = CohNr)
  GetData$Bif = BifPar[i]
  NewData = data.frame(BifPar = BifPar[i], MinTime = min(GetData$Time),
                       MaxTime = max(GetData$Time))
  minMaxTime = rbind(minMaxTime, NewData)
  AllTimeData = rbind(AllTimeData, GetData)
}

rm(GetData)
AllTimeData_R1maxHighQ = AllTimeData

label1 = expression(paste("7.5 mg ", L^-1, day^-1))
label2 = expression(paste("2.5 mg ", L^-1, day^-1))
label3 = expression(paste("0.5 mg ", L^-1, day^-1))
xlabtime = expression(atop("",atop("Time (years)","")))

# Add column with combined Subadult and Adult biomass
AllTimeData_R1maxHighQ$Juv_Ad_bm = AllTimeData_R1maxHighQ$Juv_bm + 
  AllTimeData_R1maxHighQ$Ad_bm

######Make plot of AllTimeData#####
SBMTimePlotR1maxLowQ = ggplot(data =  AllTimeData_R1maxHighQ,
                               aes(x = Time/Season, y = Larv_bm * 1000)) +
  geom_point(data = subset(AllTimeData_R1maxHighQ, Larv_bm > 0), size = 0.5, aes(colour = "aJuveniles")) +
  #geom_point(data = subset(AllTimeData_R1maxHighQ, Est_bm > 0), size = 0.1, aes(colour = "aAJuveniles", y = Est_bm * 1000)) +
  geom_point(data = subset(AllTimeData_R1maxHighQ, Juv_Ad_bm > 0), 
             size = 0.5, aes(colour = "bJuveniles", y = Juv_Ad_bm * 1000)) +
  #geom_point(data = subset(AllTimeData_R1maxHighQ, Ad_bm > 0), size = 0.1, aes(colour = "cAdults", y = Ad_bm * 1000)) + 
  layout + 
  scale_color_manual(values = c("#4B0055", "#36E9A7"),
                     labels = c("Juveniles in focal nursery", "Subadults + Adults"))+
  ylab(expression(atop("", atop("Fish biomass",  paste("(mg ", L^-1, ")"))))) +
  xlab(xlabtime) +
  guides(color = guide_legend(byrow = TRUE,  override.aes = list(size = 3))) + 
  geom_vline(xintercept = (subset(minMaxTime, BifPar < 0.075)$MinTime) / Season, 
             linetype = 'dashed') +
  theme(legend.position = c(0.82, 0.75),
        legend.background = element_blank(),
        legend.text = element_text(size=20),
        axis.text=element_text(size=18),
        axis.title=element_text(size=24)) +
  annotate(geom = "text", x = 25, y = 300, label = 'High', size = 10) + 
  annotate(geom = "text", x = 75, y = 300, label = 'Intermediate', size = 10) + 
  annotate(geom = "text", x = 125, y = 300, label = 'Low', size = 10) +
  NULL
SBMTimePlotR1maxLowQ

# Plot 2: increasing mortality focal nursery with low quality other nurseries
DataNames = c("LowQ_nomort",
              "LowQ_Intermort",
              "LowQ_Highmort")
BifPar = c(0, 0.003, 0.006)
minMaxTime <- data.frame()
AllTimeData <- data.frame()

for (i in 1:length(BifPar)) {
  filename <- paste0(Time_loc, DataNames[i], ".out", sep = '')
  GetData <- read.table(filename, header = F)
  GiveNames(GetData, maxcohorts = CohNr)
  GetData$Bif = BifPar[i]
  NewData = data.frame(BifPar = BifPar[i], MinTime = min(GetData$Time),
                       MaxTime = max(GetData$Time))
  minMaxTime = rbind(minMaxTime, NewData)
  AllTimeData = rbind(AllTimeData, GetData)
}

rm(GetData)
AllTimeData_HighQ = AllTimeData

# Add column with combined Subadult and Adult biomass
AllTimeData_HighQ$Juv_Ad_bm = AllTimeData_HighQ$Juv_bm + 
  AllTimeData_HighQ$Ad_bm

###Make plot mortality series High Q#####
MortTimePlotLowQ = ggplot(data =  AllTimeData_HighQ,
                           aes(x = Time/Season, y = Larv_bm * 1000)) +
  geom_point(data = subset(AllTimeData_HighQ, Larv_bm > 0), size = 0.5, aes(colour = "aJuveniles")) +
  #geom_point(data = subset(AllTimeData_R1maxHighQ, Est_bm > 0), size = 0.1, aes(colour = "aAJuveniles", y = Est_bm * 1000)) +
  geom_point(data = subset(AllTimeData_HighQ, Juv_Ad_bm > 0), 
             size = 0.5, aes(colour = "bJuveniles", y = Juv_Ad_bm * 1000)) +
  #geom_point(data = subset(AllTimeData_R1maxHighQ, Ad_bm > 0), size = 0.1, aes(colour = "cAdults", y = Ad_bm * 1000)) + 
  layout + 
  scale_color_manual(values = c("#4B0055", "#36E9A7"),
                     labels = c("Juveniles in focal nursery", "Subadults + Adults")) +
  ylab(expression(atop("", atop("Fish biomass",  paste("(mg ", L^-1, ")"))))) +
  xlab(xlabtime) +
  guides(color = guide_legend(byrow = TRUE,  override.aes = list(size = 3))) + 
  geom_vline(xintercept = (subset(minMaxTime, BifPar > 0)$MinTime) / Season, 
             linetype = 'dashed') +
  theme(legend.position = c(0.82, 0.75),
        legend.background = element_blank(),
        legend.text = element_text(size=20),
        axis.text=element_text(size=18),
        axis.title=element_text(size=24)) +
  annotate(geom = "text", x = 25, y = 330, label = 'Low', size = 10) +
  annotate(geom = "text", x = 75, y = 330, label = "Intermediate", size = 10) + 
  annotate(geom = "text", x = 125, y = 330, label = "High", size = 10) +
  NULL
MortTimePlotLowQ

## Plot 3: equilibrium in plot 5B
setwd('U:\\Manuscript\\Appendix')
Time_loc = "U:\\Manuscript\\Appendix\\"

DataNames = c("HighQ_LowR1max_HighConnect_LongTime",
              "HighQ_LowR1max_InterConnect_LongTime",
              "HighQ_LowR1max_LowConnect_LongTime")
BifPar = c(1, 0.5, 0.1)
minMaxTime <- data.frame()
AllTimeData <- data.frame()

for (i in 1:length(BifPar)) {
  filename <- paste0(Time_loc, DataNames[i], ".out", sep = '')
  GetData <- read.table(filename, header = F)
  GiveNames(GetData, maxcohorts = CohNr)
  GetData$Bif = BifPar[i]
  NewData = data.frame(BifPar = BifPar[i], MinTime = min(GetData$Time),
                       MaxTime = max(GetData$Time))
  minMaxTime = rbind(minMaxTime, NewData)
  AllTimeData = rbind(AllTimeData, GetData)
}

rm(GetData)
AllTimeData_HighQ = AllTimeData

# Add column with combined Subadult and Adult biomass
AllTimeData_HighQ$Juv_Ad_bm = AllTimeData_HighQ$Juv_bm + 
  AllTimeData_HighQ$Ad_bm

AllTimeData_HighQ = AllTimeData_HighQ[37500:85003,]

###Make plot mortality series High Q#####
MortTimePlotLowQ_LongTime = ggplot(data =  AllTimeData_HighQ,
                          aes(x = Time/Season, y = Larv_bm * 1000)) +
  geom_point(data = subset(AllTimeData_HighQ, Larv_bm > 0), size = 0.5, aes(colour = "aJuveniles")) +
  #geom_point(data = subset(AllTimeData_R1maxHighQ, Est_bm > 0), size = 0.1, aes(colour = "aAJuveniles", y = Est_bm * 1000)) +
  geom_point(data = subset(AllTimeData_HighQ, Juv_Ad_bm > 0), 
             size = 0.5, aes(colour = "bJuveniles", y = Juv_Ad_bm * 1000)) +
  #geom_point(data = subset(AllTimeData_R1maxHighQ, Ad_bm > 0), size = 0.1, aes(colour = "cAdults", y = Ad_bm * 1000)) + 
  layout + 
  scale_color_manual(values = c("#4B0055", "#36E9A7"),
                     labels = c("Juveniles in focal nursery", "Subadults + Adults")) +
  ylab(expression(atop("", atop("Fish biomass",  paste("(mg ", L^-1, ")"))))) +
  xlab(xlabtime) +
  guides(color = guide_legend(byrow = TRUE,  override.aes = list(size = 3))) + 
  geom_vline(xintercept = (subset(minMaxTime, MinTime > 0)$MinTime) / Season, 
             linetype = 'dashed') +
  theme(legend.position = c(0.82, 0.75),
        legend.background = element_blank(),
        legend.text = element_text(size=11),
        axis.text=element_text(size=14),
        axis.title=element_text(size=20)) +
  annotate(geom = "text", x = 175, y = 150, label = 'Low', size = 6) +
  annotate(geom = "text", x = 225, y = 150, label = "Intermediate", size = 6) + 
  annotate(geom = "text", x = 275, y = 150, label = "High", size = 6) +
  xlim(150,300) +  
  NULL
MortTimePlotLowQ_LongTime
