rm(list = ls()) ##CLEAN EVERYTHING
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) #Set wd to current directory
source("Script_functions.R") #Load script#

#Set location output files#
Appendix_files = "../EBTfiles/Files_Appendix/"

###______APPENDIX NO NURSERY______#####
####____NoNurs appendix Fig 1: (Result 1): reduced food in focal nursery____#######
#----------------------------------------------------------------------------------------#
DataNames = c("App_nonurs_Fig1_HighR1max",
              "App_nonurs_Fig1_InterR1max",
              "App_nonurs_Fig1_LowR1max")
BifPar = c(0.075, 0.025, 0.005)
minMaxTime <- data.frame()
AllTimeData <- data.frame()
CohNr = 21
for (i in 1:length(BifPar)) {
  filename <- paste0(Appendix_files, DataNames[i], ".out", sep = '')
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

######Make plot of AllTimeData###
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


####Appendix no nurs 1: Growth data figure 2####
#First get average juvenile period#
HighData <- subset(AllTimeData_R1maxHighQ, Bif == 0.075)
InterData <- subset(AllTimeData_R1maxHighQ, Bif == 0.025)


#Mgeom_point()#Mean period High#
mean(HighData$Larv_period, na.rm = T) / 250
mean(InterData$Larv_period, na.rm = T) / 250

####Appendix no nurs 1: Get pop data####
indexstart = which(colnames(HighData) == "LCoh_1")
indexend = CohNr * Varnr + indexstart - 1
HighData_pop <- HighData[, c(1,indexstart:indexend)]
PrepPopData(HighData_pop)
PopCalc(HighData_pop)

InterData_pop <- InterData[, c(1,indexstart:indexend)]
PrepPopData(InterData_pop)
PopCalc(InterData_pop)
####
index_mean = which.min(abs(HighData_pop$DietAge - mean(HighData$Larv_period, na.rm = T)))
bd_mean = HighData_pop$BirthDay[index_mean]
index_min = which.min(HighData_pop$DietAge)
bd_min = HighData_pop$BirthDay[index_min]
index_max = which.max(HighData_pop$DietAge)
bd_max = HighData_pop$BirthDay[index_max]


HighR1max_growth = ggplot(data = subset(HighData_pop,
                                        LCoh < (100-1E-9) & BirthDay == bd_mean),
                          aes(x = (ACoh) / 250, y = LCoh,
                              group = BirthDay)) +
  geom_path(alpha = .5, size = 2) +
  geom_path(data = subset(HighData_pop,
                          LCoh < (100-1E-9) & (BirthDay == bd_min | BirthDay == bd_max)), alpha = 0.3, size = 1) +
  xlab(expression(atop("", atop("Time (years)", "")))) +
  ylab("Size (gram)") +
  theme(legend.position = 'none',
        axis.title.y = element_text(size = 14, colour = 'black'),
        axis.title.x = element_text(size = 20, colour = 'black'),
        axis.text=element_text(size=14, colour = 'black'),
        plot.margin = margin(10, 5, 0, 5.5, "pt"),
        legend.background = element_blank(), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  NULL

#HighR1max_growth

index_mean = which.min(abs(InterData_pop$DietAge - mean(InterData$Larv_period, na.rm = T)))
bd_mean = InterData_pop$BirthDay[index_mean]
index_min = which.min(InterData_pop$DietAge)
bd_min = InterData_pop$BirthDay[index_min]
index_max = which.max(InterData_pop$DietAge)
bd_max = InterData_pop$BirthDay[index_max]

InterR1max_growth =
  ggplot(data = subset(InterData_pop,
                       LCoh < (100-1E-9) & BirthDay == bd_mean),
         aes(x = (ACoh) / 250, y = LCoh,
             group = BirthDay)) +
  geom_path(alpha = .5, size = 2) +
  geom_path(data = subset(InterData_pop,
                          LCoh < (100-1E-9) & (BirthDay == bd_min | BirthDay == bd_max)), alpha = 0.3, size = 1) +
  xlab(expression(atop("", atop("Time (years)", "")))) +
  ylab("Size (gram)") +
  theme(legend.position = 'none',
        axis.title.y = element_text(size = 14, colour = 'black'),
        axis.title.x = element_text(size = 20, colour = 'black'),
        axis.text=element_text(size=14, colour = 'black'),
        plot.margin = margin(10, 5, 0, 5.5, "pt"),
        legend.background = element_blank(), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  NULL
#InterR1max_growth

####Appendix no nurs 1: Combine panels figure 1######
# Combine plots, make sure to also have growthcurves plots
p1 = ggarrange(SBMTimePlotR1maxHighQ +
                 theme(axis.title.x=element_blank(),
                       axis.text.x=element_blank(),
                       axis.ticks.x=element_blank(),
                       plot.margin = unit(c(0,1,0,0), 'lines')),
               HighR1max_growth + theme(plot.margin = unit(c(0,1,0,0), 'lines')) +
                 xlim(0, 20), 
               SBMTimePlotR1maxHighQ_res + theme(plot.margin = unit(c(0,1,0,0), 'lines')),
               InterR1max_growth + xlim(0,20) + 
                 theme(plot.margin = unit(c(0,1,0,0) , 'lines')), 
               nrow = 2, ncol = 2, labels = c("A", "C", "B", "D"),
               align = "hv")

p1

ggsave("../Plots/App_nonurs_fig1.png", plot = p1, width = 1920, height = 1107,
       scale = 2.5,
       units = "px")



####____NoNurs appendix Fig 2: (Result 2): Increasing mortality in the focal nursery____#####
#----------------------------------------------------------------------------------------#
# Result 2: increased mortality in focal nursery
#----------------------------------------------------------------------------------------#
DataNames = c("App_nonurs_Fig2_nomort",
              "App_nonurs_Fig2_Intermort",
              "App_nonurs_Fig2_Highmort")
BifPar = c(0, 0.003, 0.006)
minMaxTime <- data.frame()
AllTimeData <- data.frame()

for (i in 1:length(BifPar)) {
  filename <- paste0(Appendix_files, DataNames[i], ".out", sep = '')
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



##Make plot mortality series High Q###
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
  annotate(geom = "text", x = 25, y = 330, label = 'Low', size = 6) +
  annotate(geom = "text", x = 75, y = 330, label = "Intermediate", size = 6) + 
  annotate(geom = "text", x = 125, y = 330, label = "High", size = 6) +
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
#MortTimePlotHighQ_res

####Appendix no nurs 2: Growth data figure ####
#First get average juvenile period#
HighData <- subset(AllTimeData_HighQ, Bif == 0)
InterData <- subset(AllTimeData_HighQ, Bif == 0.003)


#Mgeom_point()#Mean period High#
mean(HighData$Larv_period, na.rm = T) / 250
mean(InterData$Larv_period, na.rm = T) / 250

####Appendix no nurs 2: Get pop data####
indexstart = which(colnames(HighData) == "LCoh_1")
indexend = CohNr * Varnr + indexstart - 1
HighData_pop <- HighData[, c(1,indexstart:indexend)]
PrepPopData(HighData_pop)
PopCalc(HighData_pop)

InterData_pop <- InterData[, c(1,indexstart:indexend)]
PrepPopData(InterData_pop)
PopCalc(InterData_pop)
####
index_mean = which.min(abs(HighData_pop$DietAge - mean(HighData$Larv_period, na.rm = T)))
bd_mean = HighData_pop$BirthDay[index_mean]
index_min = which.min(HighData_pop$DietAge)
bd_min = HighData_pop$BirthDay[index_min]
index_max = which.max(HighData_pop$DietAge)
bd_max = HighData_pop$BirthDay[index_max]

NoMortFocal_growth = ggplot(data = subset(HighData_pop,
                                          LCoh < (100-1E-9) & BirthDay == bd_mean),
                            aes(x = (ACoh) / 250, y = LCoh,
                                group = BirthDay)) +
  geom_path(alpha = .5, size = 2) +
  geom_path(data = subset(HighData_pop,
                          LCoh < (100-1E-9) & (BirthDay == bd_min | BirthDay == bd_max)), alpha = 0.3, size = 1) +
  xlab(expression(atop("", atop("Time (years)", "")))) +
  ylab("Size (gram)") +
  theme(legend.position = 'none',
        axis.title.y = element_text(size = 14, colour = 'black'),
        axis.title.x = element_text(size = 20, colour = 'black'),
        axis.text=element_text(size=14, colour = 'black'),
        plot.margin = margin(10, 5, 0, 5.5, "pt"),
        legend.background = element_blank(), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  NULL

#NoMortFocal_growth

index_mean = which.min(abs(InterData_pop$DietAge - mean(InterData$Larv_period, na.rm = T)))
bd_mean = InterData_pop$BirthDay[index_mean]
index_min = which.min(InterData_pop$DietAge)
bd_min = InterData_pop$BirthDay[index_min]
index_max = which.max(InterData_pop$DietAge)
bd_max = InterData_pop$BirthDay[index_max]

InterMortFocal_growth =
  ggplot(data = subset(InterData_pop,
                       LCoh < (100-1E-9) & BirthDay == bd_mean),
         aes(x = (ACoh) / 250, y = LCoh,
             group = BirthDay)) +
  geom_path(alpha = .5, size = 2) +
  geom_path(data = subset(InterData_pop,
                          LCoh < (100-1E-9) & (BirthDay == bd_min | BirthDay == bd_max)), alpha = 0.3, size = 1) +
  xlab(expression(atop("", atop("Time (years)", "")))) +
  ylab("Size (gram)") +
  theme(legend.position = 'none',
        axis.title.y = element_text(size = 14, colour = 'black'),
        axis.title.x = element_text(size = 20, colour = 'black'),
        axis.text=element_text(size=14, colour = 'black'),
        plot.margin = margin(10, 5, 0, 5.5, "pt"),
        legend.background = element_blank(), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  NULL

#InterMortFocal_growth

####Appendix no nurs 2: Combine panels figure 2######
p2 = ggarrange(MortTimePlotHighQ +
                 theme(axis.title.x=element_blank(),
                       axis.text.x=element_blank(),
                       axis.ticks.x=element_blank(),
                       plot.margin = unit(c(0,1,0,0), 'lines')),
               NoMortFocal_growth + theme(plot.margin = unit(c(0,1,0,0), 'lines')) +
                 xlim(0,7), 
               MortTimePlotHighQ_res + theme(plot.margin = unit(c(0,1,0,0), 'lines')),
               InterMortFocal_growth + xlim(0,7) +
                 theme(plot.margin = unit(c(0,1,0,0), 'lines')),
               nrow = 2, ncol = 2, labels = c("A", "C", "B", "D"),
               align = "hv")

# ggsave("../Plots/App_nonurs_fig2.png", plot = p2, width = 1920, height = 1107,
#        scale = 2.5,
#        units = "px")

#
####____NoNurs appendix Fig 3: (result 3):increased mortality in offshore habitat____#####

DataNames = c("App_nonurs_Fig3_HighR1max_NoNSMort",
              "App_nonurs_Fig3_HighR1max_InterNSMort",
              "App_nonurs_Fig3_HighR1max_HighNSMort")
BifPar = c(0, 0.005, 0.01)
minMaxTime <- data.frame()
AllTimeData <- data.frame()

for (i in 1:length(BifPar)) {
  filename <- paste0(Appendix_files, DataNames[i], ".out", sep = '')
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

###Make plot mortality series High R1 max#
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
  annotate(geom = "text", x = 25, y = 300, label = 'Low', size = 6) +
  annotate(geom = "text", x = 75, y = 300, label = "Intermediate", size = 6) + 
  annotate(geom = "text", x = 125, y = 300, label = "High", size = 6) +
  xlim(0, 150) + ylim(0, 310) +
  NULL
#MortTimePlotHighQ


# For low quality of WS
DataNames = c("App_nonurs_Fig3_LowR1max_NoNSMort",
              "App_nonurs_Fig3_LowR1max_InterNSMort",
              "App_nonurs_Fig3_LowR1max_HighNSMort")
BifPar = c(0, 0.005, 0.01)
minMaxTime <- data.frame()
AllTimeData <- data.frame()

for (i in 1:length(BifPar)) {
  filename <- paste0(Appendix_files, DataNames[i], ".out", sep = '')
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

##Make plot mortality series High R1 max##
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
####Appendix no nurs 3: Combine panels figure 3######
p3 = ggarrange(MortTimePlotHighQ +
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

# ggsave("../Plots/App_nonurs_fig3.png", plot = p3, width = 1920, height = 1107,
#        scale = 2.5,
#        units = "px")

####____NoNurs appendix Fig 4: (Result 4) Effect of loss of connectivity____#####

#----------------------------------------------------------------------------------------#
# Result 4: reduced connectivity to focal nursery
#----------------------------------------------------------------------------------------#

####Appendix no nurs 4A: High quality####
# For high quality of other nurseries, panel A
DataNames = c("App_nonurs_fig4_high_A",
              "App_nonurs_fig4_inter_A",
              "App_nonurs_fig4_low_A")
BifPar = c(1, 0.5, 0.1)
minMaxTime <- data.frame()
AllTimeData <- data.frame()

for (i in 1:length(BifPar)) {
  filename <- paste0(Appendix_files, DataNames[i], ".out", sep = '')
  GetData <- read.table(filename, header = F)
  GiveNames(GetData, maxcohorts = CohNr)
  GetData$Bif = BifPar[i]
  NewData = data.frame(BifPar = BifPar[i], MinTime = min(GetData$Time),
                       MaxTime = max(GetData$Time))
  minMaxTime = rbind(minMaxTime, NewData)
  AllTimeData = rbind(AllTimeData, GetData)
}

rm(GetData)
AllTimeData_NoNurs = AllTimeData

# Add column with combined Subadult and Adult biomass
AllTimeData_NoNurs$Juv_Ad_bm = AllTimeData_NoNurs$Juv_bm + 
  AllTimeData_NoNurs$Ad_bm


ConnTimePlotHighQ = ggplot(data =  AllTimeData_NoNurs,
                           aes(x = Time/Season, y = Larv_bm * 1000)) +
  geom_point(data = subset(AllTimeData_NoNurs, Larv_bm > 0), size = 0.5, aes(colour = "aJuveniles")) +
  #geom_point(data = subset(AllTimeData_R1maxHighQ, Est_bm > 0), size = 0.1, aes(colour = "aAJuveniles", y = Est_bm * 1000)) +
  geom_point(data = subset(AllTimeData_NoNurs, Juv_Ad_bm > 0), 
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

####Appendix no nurs 4D: High mort offshore####
# High quality other nurseries, high mortality NS [Panel D]
DataNames = c("App_nonurs_fig4_high_D",
              "App_nonurs_fig4_inter_D",
              "App_nonurs_fig4_low_D")
BifPar = c(1, 0.5, 0.1)
minMaxTime <- data.frame()
AllTimeData <- data.frame()

for (i in 1:length(BifPar)) {
  filename <- paste0(Appendix_files, DataNames[i], ".out", sep = '')
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

####Appendix no nurs 4C: High mort nursery####
# High quality other nurseries, high mortality WS [PANEL C]
DataNames = c("App_nonurs_fig4_high_C",
              "App_nonurs_fig4_inter_C",
              "App_nonurs_fig4_low_C")
BifPar = c(1, 0.5, 0.1)
minMaxTime <- data.frame()
AllTimeData <- data.frame()

for (i in 1:length(BifPar)) {
  filename <- paste0(Appendix_files, DataNames[i], ".out", sep = '')
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

####Appendix no nurs 4B: Low food nursery####
# High quality other nurseries, low food availability focal nursery [Panel B]
DataNames = c("App_nonurs_fig4_high_B",
              "App_nonurs_fig4_inter_B",
              "App_nonurs_fig4_low_B")
BifPar = c(1, 0.5, 0.1)
minMaxTime <- data.frame()
AllTimeData <- data.frame()

for (i in 1:length(BifPar)) {
  filename <- paste0(Appendix_files, DataNames[i], ".out", sep = '')
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


#Make plot mortality series High Q###
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
  annotate(geom = "text", x = 25, y = 30, label = 'High', size = 6) +
  annotate(geom = "text", x = 75, y = 30, label = "Intermediate", size = 6) + 
  annotate(geom = "text", x = 125, y = 30, label = "Low", size = 6) +
  NULL
#ConnHighQLowR1max


####Appendix no nurs 4: Combine panels figure 4####
p4 = ggarrange(ConnTimePlotHighQ +
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
p4
ggsave("../Plots/App_nonurs_fig4.png", plot = p4, width = 1920, height = 1107,
       scale = 2.5,
       units = "px")
