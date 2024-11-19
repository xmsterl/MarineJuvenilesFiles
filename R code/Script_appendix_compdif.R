rm(list = ls()) ##CLEAN EVERYTHING
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) #Set wd to current directory
source("Script_functions.R") #Load script#

#Set location output files#
Appendix_files = "../EBTfiles/Files_Appendix/"

####____Competitive differences subadult>adult appendix Fig 1: (Result 1): reduced food in focal nursery____#######
#----------------------------------------------------------------------------------------#
DataNames = c("App_compdif_Fig1_HighQ_HighR1max_subadult",
              "App_compdif_Fig1_HighQ_InterR1max_subadult",
              "App_compdif_Fig1_HighQ_LowR1max_subadult")
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


####Appendix comp dif subadult 1: Growth data figure 2####
#First get average juvenile period#
HighData <- subset(AllTimeData_R1maxHighQ, Bif == 0.075)
InterData <- subset(AllTimeData_R1maxHighQ, Bif == 0.025)


#Mgeom_point()#Mean period High#
mean(HighData$Larv_period, na.rm = T) / 250
mean(InterData$Larv_period, na.rm = T) / 250

####Appendix comp dif subadult 1: Get pop data####
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

####Appendix comp dif subadult 1: Combine panels figure 1######
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

ggsave("../Plots/App_compdif_fig1.png", plot = p1, width = 1920, height = 1107,
       scale = 2.5,
       units = "px")



####____Competitive differences subadult>adult: (Result 2): Increasing mortality in the focal nursery____#####
#----------------------------------------------------------------------------------------#
# Result 2: increased mortality in focal nursery
#----------------------------------------------------------------------------------------#
DataNames = c("App_compdif_Fig2_HighQ_nomort_subadult",
              "App_compdif_Fig2_HighQ_Intermort_subadult",
              "App_compdif_Fig2_HighQ_Highmort_subadult")
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

####Appendix comp dif subadult 1: Growth data figure ####
#First get average juvenile period#
HighData <- subset(AllTimeData_HighQ, Bif == 0)
InterData <- subset(AllTimeData_HighQ, Bif == 0.003)


#Mgeom_point()#Mean period High#
mean(HighData$Larv_period, na.rm = T) / 250
mean(InterData$Larv_period, na.rm = T) / 250

####Appendix comp dif subadult 1: Get pop data####
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

####Appendix comp dif subadult 1: Combine panels figure 2######
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

ggsave("../Plots/App_compdif_fig2.png", plot = p2, width = 1920, height = 1107,
       scale = 2.5,
       units = "px")

#
####____Competitive differences adult>subadult appendix Fig 3: (Result 1): reduced food in focal nursery____#######
#----------------------------------------------------------------------------------------#
DataNames = c("App_compdif_Fig3_HighQ_HighR1max_adult",
              "App_compdif_Fig3_HighQ_InterR1max_adult",
              "App_compdif_Fig3_HighQ_LowR1max_adult")
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

# Add column with combined adult and Adult biomass
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
                     labels = c("Juveniles in focal nursery", "adults + Adults"))+
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


####Appendix comp dif adult 1: Growth data figure 3####
#First get average juvenile period#
HighData <- subset(AllTimeData_R1maxHighQ, Bif == 0.075)
InterData <- subset(AllTimeData_R1maxHighQ, Bif == 0.025)


#Mgeom_point()#Mean period High#
mean(HighData$Larv_period, na.rm = T) / 250
mean(InterData$Larv_period, na.rm = T) / 250

####Appendix comp dif adult 1: Get pop data####
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

####Appendix comp dif adult 1: Combine panels figure 3######
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

ggsave("../Plots/App_compdif_fig3.png", plot = p1, width = 1920, height = 1107,
       scale = 2.5,
       units = "px")



####____Competitive differences adult>subadult: appendix Fig 4: (Result 2): Increasing mortality in the focal nursery____#####
#----------------------------------------------------------------------------------------#
# Result 2: increased mortality in focal nursery
#----------------------------------------------------------------------------------------#
DataNames = c("App_compdif_Fig4_HighQ_nomort_adult",
              "App_compdif_Fig4_HighQ_Intermort_adult",
              "App_compdif_Fig4_HighQ_Highmort_adult")
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

# Add column with combined adult and Adult biomass
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
                     labels = c("Juveniles in focal nursery", "adults + Adults")) +
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

####Appendix comp dif adult 1: Growth data figure ####
#First get average juvenile period#
HighData <- subset(AllTimeData_HighQ, Bif == 0)
InterData <- subset(AllTimeData_HighQ, Bif == 0.003)


#Mgeom_point()#Mean period High#
mean(HighData$Larv_period, na.rm = T) / 250
mean(InterData$Larv_period, na.rm = T) / 250

####Appendix comp dif adult 1: Get pop data####
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

####Appendix comp dif adult 1: Combine panels figure 4######
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

ggsave("../Plots/App_compdif_fig4.png", plot = p2, width = 1920, height = 1107,
       scale = 2.5,
       units = "px")

#