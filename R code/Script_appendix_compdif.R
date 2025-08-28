rm(list = ls()) ##CLEAN EVERYTHING
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) #Set wd to current directory
source("Script_functions.R") #Load script#

#Set location output files#
Appendix_files = "../EBTfiles/Files_Appendix/"
###Reduced food#####
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
subadult_better = AllTimeData

subadult_HighData <- subset(subadult_better, Bif == 0.075)
subadult_InterData <- subset(subadult_better, Bif == 0.025)

indexstart = which(colnames(subadult_HighData) == "LCoh_1")
indexend = CohNr * Varnr + indexstart - 1

subadult_HighData_pop <- subadult_HighData[, c(1,indexstart:indexend)]
PrepPopData(subadult_HighData_pop)
PopCalc(subadult_HighData_pop)
subadult_InterData_pop <- subadult_InterData[, c(1,indexstart:indexend)]
PrepPopData(subadult_InterData_pop)
PopCalc(subadult_InterData_pop)


####____Competitive differences subadult<adult appendix Fig 1____#####
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
adult_better = AllTimeData


####____Competitive symmetry subadult = adult#####
Main_files = "../EBTfiles/Files_Main/"
DataNames = c("Fig2_HighQ_HighR1max",
              "Fig2_HighQ_InterR1max",
              "Fig2_HighQ_LowR1max")
BifPar = c(0.075, 0.025, 0.005)
minMaxTime <- data.frame()
AllTimeData <- data.frame()
CohNr = 21
for (i in 1:length(BifPar)) {
  filename <- paste0(Main_files, DataNames[i], ".out", sep = '')
  GetData <- read.table(filename, header = F)
  GiveNames(GetData, maxcohorts = CohNr)
  GetData$Bif = BifPar[i]
  NewData = data.frame(BifPar = BifPar[i], MinTime = min(GetData$Time),
                       MaxTime = max(GetData$Time))
  minMaxTime = rbind(minMaxTime, NewData)
  AllTimeData = rbind(AllTimeData, GetData)
}

rm(GetData)
symmetry = AllTimeData
####____Get population data for high and intermediate R1max#####
subadult_HighData <- subset(subadult_better, Bif == 0.075)
subadult_InterData <- subset(subadult_better, Bif == 0.025)
adult_HighData <- subset(adult_better, Bif == 0.075)
adult_InterData <- subset(adult_better, Bif == 0.025)
symmetry_HighData <- subset(symmetry, Bif == 0.075)
symmetry_InterData <- subset(symmetry, Bif == 0.025)

indexstart = which(colnames(subadult_HighData) == "LCoh_1")
indexend = CohNr * Varnr + indexstart - 1

#Subadult#
subadult_HighData_pop <- subadult_HighData[, c(1,indexstart:indexend)]
PrepPopData(subadult_HighData_pop)
PopCalc(subadult_HighData_pop)
subadult_InterData_pop <- subadult_InterData[, c(1,indexstart:indexend)]
PrepPopData(subadult_InterData_pop)
PopCalc(subadult_InterData_pop)
#adult
adult_HighData_pop <- adult_HighData[, c(1,indexstart:indexend)]
PrepPopData(adult_HighData_pop)
PopCalc(adult_HighData_pop)
adult_InterData_pop <- adult_InterData[, c(1,indexstart:indexend)]
PrepPopData(adult_InterData_pop)
PopCalc(adult_InterData_pop)
#symmetry
symmetry_HighData_pop <- symmetry_HighData[, c(1,indexstart:indexend)]
PrepPopData(symmetry_HighData_pop)
PopCalc(symmetry_HighData_pop)
symmetry_InterData_pop <- symmetry_InterData[, c(1,indexstart:indexend)]
PrepPopData(symmetry_InterData_pop)
PopCalc(symmetry_InterData_pop)

#Get indexes and birthdays#
subadult_High_index = which.min(abs(subadult_HighData_pop$DietAge - mean(subadult_HighData$Larv_period, na.rm = T)))
subadult_High_bd = subadult_HighData_pop$BirthDay[subadult_High_index]
subadult_Inter_index = which.min(abs(subadult_InterData_pop$DietAge - mean(subadult_InterData$Larv_period, na.rm = T)))
subadult_Inter_bd = subadult_InterData_pop$BirthDay[subadult_Inter_index]

adult_High_index = which.min(abs(adult_HighData_pop$DietAge - mean(adult_HighData$Larv_period, na.rm = T)))
adult_High_bd = adult_HighData_pop$BirthDay[adult_High_index]
adult_Inter_index = which.min(abs(adult_InterData_pop$DietAge - mean(adult_InterData$Larv_period, na.rm = T)))
adult_Inter_bd = adult_InterData_pop$BirthDay[adult_Inter_index]

symmetry_High_index = which.min(abs(symmetry_HighData_pop$DietAge - mean(symmetry_HighData$Larv_period, na.rm = T)))
symmetry_High_bd = symmetry_HighData_pop$BirthDay[symmetry_High_index]
symmetry_Inter_index = which.min(abs(symmetry_InterData_pop$DietAge - mean(symmetry_InterData$Larv_period, na.rm = T)))
symmetry_Inter_bd = symmetry_InterData_pop$BirthDay[symmetry_Inter_index]


#combine data#
Pop_subadult_high <- subset(subadult_HighData_pop,
                            LCoh < (100-1E-9) & BirthDay == subadult_High_bd)
Pop_subadult_high$food <- 'High food'
Pop_subadult_high$comp <- 'Subadults competitively superior'

Pop_subadult_inter <- subset(subadult_InterData_pop,
                            LCoh < (100-1E-9) & BirthDay == subadult_Inter_bd)
Pop_subadult_inter$food <- 'Intermediate food'
Pop_subadult_inter$comp <- 'Subadults competitively superior'

Pop_adult_high <- subset(adult_HighData_pop,
                         LCoh < (100-1E-9) & BirthDay == adult_High_bd)
Pop_adult_high$food <- 'High food'
Pop_adult_high$comp <- 'Adults competitively superior'

Pop_adult_inter <- subset(adult_InterData_pop,
                          LCoh < (100-1E-9) & BirthDay == adult_Inter_bd)
Pop_adult_inter$food <- 'Intermediate food'
Pop_adult_inter$comp <- 'Adults competitively superior'

Pop_symmetry_high <- subset(symmetry_HighData_pop,
                            LCoh < (100-1E-9) & BirthDay == symmetry_High_bd)
Pop_symmetry_high$food <- 'High food'
Pop_symmetry_high$comp <- 'Symmetric competition'

Pop_symmetry_inter <- subset(symmetry_InterData_pop,
                             LCoh < (100-1E-9) & BirthDay == symmetry_Inter_bd)
Pop_symmetry_inter$food <- 'Intermediate food'
Pop_symmetry_inter$comp <- 'Symmetric competition'

AllPop <- rbind(Pop_symmetry_high, Pop_symmetry_inter,
                Pop_adult_high, Pop_adult_inter,
                Pop_subadult_high, Pop_subadult_inter)
AllPop$comp <- as.factor(AllPop$comp)
AllPop$comp <- relevel(AllPop$comp, "Subadults competitively superior")
AllPop$comp <- relevel(AllPop$comp, "Symmetric competition")
AllPop$comp <- relevel(AllPop$comp, "Adults competitively superior")


GrowthCurves <- ggplot(data = AllPop,
                      aes(x = (ACoh) / 250, y = LCoh)) +
  geom_path(linewidth = 2, aes(colour = comp)) +
  
  facet_grid(.~food) +
  xlab(expression(atop("", atop("Time (years)", "")))) +
  ylab("Size (gram)") +
  scale_color_manual(values = c("darkgreen", "grey", "darkorange")) +
  theme(legend.position = 'right',
        legend.title = element_blank(),
        axis.title.y = element_text(size = 14, colour = 'black'),
        axis.title.x = element_text(size = 20, colour = 'black'),
        axis.text=element_text(size = 14, colour = 'black'),
        plot.margin = margin(10, 5, 0, 5.5, "pt"),
        legend.background = element_blank(), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  NULL
#GrowthCurves








###Plot time #####
subadult_better$Juv_Ad_bm = subadult_better$Juv_bm + 
  subadult_better$Ad_bm
adult_better$Juv_Ad_bm = adult_better$Juv_bm + 
  adult_better$Ad_bm
symmetry$Juv_Ad_bm = symmetry$Juv_bm + 
  symmetry$Ad_bm
subadult_better$comp = 'Subadults superior'
adult_better$comp = 'Adults superior'
symmetry$comp = 'Symmetric'
timedata_comp <- rbind(subadult_better, adult_better, symmetry)
timedata_comp$comp <- as.factor(timedata_comp$comp)
timedata_comp$comp <- relevel(timedata_comp$comp, "Subadults superior")
timedata_comp$comp <- relevel(timedata_comp$comp, "Symmetric")
timedata_comp$comp <- relevel(timedata_comp$comp, "Adults superior")



TimePlot_comp <- ggplot(data =  timedata_comp,
       aes(x = Time/Season, y = Larv_bm * 1000)) +  
  geom_path(aes(colour = "aJuveniles")) +
  geom_path(aes(colour = "bJuveniles", y = Juv_Ad_bm * 1000)) + 
  scale_color_manual(values = c("#4B0055", "#36E9A7"),
                     labels = c("Juveniles in focal nursery", "Subadults + Adults"))+
  theme_bw() +
  facet_grid(.~comp)+ 
  ylab(expression(atop("", atop("Fish biomass",  paste("(mg ", L^-1, ")"))))) +
  xlab(xlabtime) +
  guides(color = guide_legend(byrow = TRUE,  override.aes = list(size = 3))) + 
  geom_vline(xintercept = (subset(minMaxTime, BifPar < 0.075)$MinTime) / Season, 
             linetype = 'dashed') +
  theme(legend.position = 'none',#,c(0.83, 0.75),
        legend.background = element_blank(),
        legend.text = element_text(size=11),
        axis.text=element_text(size=14),
        axis.title=element_text(size=20)) +
  annotate(geom = "text", x = 25, y = 300, label = 'High', size = 6) + 
  annotate(geom = "text", x = 75, y = 300, label = 'Intermediate', size = 6) + 
  annotate(geom = "text", x = 125, y = 300, label = 'Low', size = 6) +
  NULL
#TimePlot_comp
#GrowthCurves

Appendix_competition_food <- ggarrange(
  TimePlot_comp + theme(plot.margin = unit(c(0,0,0,0), 'lines')), 
  GrowthCurves + theme(plot.margin = unit(c(0,0,0,1.5), 'lines')),
  ncol = 1)
Appendix_competition_food

ggsave("../Plots/App_compdif_fig1New.png", plot = Appendix_competition_food, width = 1920, height = 1107,
       scale = 2.5, dpi = 300,
       units = "px")

####____________________________________________________________________####
###Increased mortality nursery#####
####____Competitive differences subadult>adult appendix Fig 2: (Result 2): Increased mort in focal nursery____#######
#----------------------------------------------------------------------------------------#
DataNames = c("App_compdif_Fig2_HighQ_nomort_subadult",
              "App_compdif_Fig2_HighQ_Intermort_subadult",
              "App_compdif_Fig2_HighQ_Highmort_subadult")
BifPar = c(0, 0.003, 0.006)
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
subadult_better = AllTimeData

subadult_NoMortData <- subset(subadult_better, Bif == 0)
subadult_InterMortData <- subset(subadult_better, Bif == 0.003)

indexstart = which(colnames(subadult_NoMortData) == "LCoh_1")
indexend = CohNr * Varnr + indexstart - 1

subadult_NoMortData_pop <- subadult_NoMortData[, c(1,indexstart:indexend)]
PrepPopData(subadult_NoMortData_pop)
PopCalc(subadult_NoMortData_pop)
subadult_InterMortData_pop <- subadult_InterMortData[, c(1,indexstart:indexend)]
PrepPopData(subadult_InterMortData_pop)
PopCalc(subadult_InterMortData_pop)


####____Competitive differences subadult<adult appendix Fig 2____#####
DataNames = c("App_compdif_Fig4_HighQ_nomort_adult",
              "App_compdif_Fig4_HighQ_Intermort_adult",
              "App_compdif_Fig4_HighQ_Highmort_adult")
BifPar = c(0, 0.003, 0.006)
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
adult_better = AllTimeData

adult_NoMortData <- subset(adult_better, Bif == 0)
adult_InterMortData <- subset(adult_better, Bif == 0.003)

indexstart = which(colnames(adult_NoMortData) == "LCoh_1")
indexend = CohNr * Varnr + indexstart - 1

adult_NoMortData_pop <- adult_NoMortData[, c(1,indexstart:indexend)]
PrepPopData(adult_NoMortData_pop)
PopCalc(adult_NoMortData_pop)
adult_InterMortData_pop <- adult_InterMortData[, c(1,indexstart:indexend)]
PrepPopData(adult_InterMortData_pop)
PopCalc(adult_InterMortData_pop)


####____Competitive symmetry subadult = adult#####
Main_files = "../EBTfiles/Files_Main/"
DataNames = c("Fig3_HighQ_nomort",
              "Fig3_HighQ_Intermort",
              "Fig3_HighQ_Highmort")
BifPar = c(0, 0.003, 0.006)
minMaxTime <- data.frame()
AllTimeData <- data.frame()
CohNr = 21
for (i in 1:length(BifPar)) {
  filename <- paste0(Main_files, DataNames[i], ".out", sep = '')
  GetData <- read.table(filename, header = F)
  GiveNames(GetData, maxcohorts = CohNr)
  GetData$Bif = BifPar[i]
  NewData = data.frame(BifPar = BifPar[i], MinTime = min(GetData$Time),
                       MaxTime = max(GetData$Time))
  minMaxTime = rbind(minMaxTime, NewData)
  AllTimeData = rbind(AllTimeData, GetData)
}

rm(GetData)
symmetry = AllTimeData

symmetry_NoMortData <- subset(symmetry, Bif == 0)
symmetry_InterMortData <- subset(symmetry, Bif == 0.003)

indexstart = which(colnames(symmetry_NoMortData) == "LCoh_1")
indexend = CohNr * Varnr + indexstart - 1

symmetry_NoMortData_pop <- symmetry_NoMortData[, c(1, indexstart:indexend)]
PrepPopData(symmetry_NoMortData_pop)
PopCalc(symmetry_NoMortData_pop)
symmetry_InterMortData_pop <- symmetry_InterMortData[, c(1, indexstart:indexend)]
PrepPopData(symmetry_InterMortData_pop)
PopCalc(symmetry_InterMortData_pop)


####____Get population data for high and intermediate R1max#####


#Get indexes and birthdays#
subadult_NoMort_index = which.min(abs(subadult_NoMortData_pop$DietAge - mean(subadult_NoMortData$Larv_period, na.rm = T)))
subadult_NoMort_bd = subadult_NoMortData_pop$BirthDay[subadult_NoMort_index]
subadult_Inter_index = which.min(abs(subadult_InterMortData_pop$DietAge - mean(subadult_InterMortData$Larv_period, na.rm = T)))
subadult_Inter_bd = subadult_InterMortData_pop$BirthDay[subadult_Inter_index]

adult_NoMort_index = which.min(abs(adult_NoMortData_pop$DietAge - mean(adult_NoMortData$Larv_period, na.rm = T)))
adult_NoMort_bd = adult_NoMortData_pop$BirthDay[adult_NoMort_index]
adult_Inter_index = which.min(abs(adult_InterMortData_pop$DietAge - mean(adult_InterMortData$Larv_period, na.rm = T)))
adult_Inter_bd = adult_InterMortData_pop$BirthDay[adult_Inter_index]

symmetry_NoMort_index = which.min(abs(symmetry_NoMortData_pop$DietAge - mean(symmetry_NoMortData$Larv_period, na.rm = T)))
symmetry_NoMort_bd = symmetry_NoMortData_pop$BirthDay[symmetry_NoMort_index]
symmetry_Inter_index = which.min(abs(symmetry_InterMortData_pop$DietAge - mean(symmetry_InterMortData$Larv_period, na.rm = T)))
symmetry_Inter_bd = symmetry_InterMortData_pop$BirthDay[symmetry_Inter_index]


#combine data#
Pop_subadult_nomort <- subset(subadult_NoMortData_pop,
                              LCoh < (100-1E-9) & BirthDay == subadult_NoMort_bd)
Pop_subadult_nomort$mort <- 'Low'
Pop_subadult_nomort$comp <- 'Subadults competitively superior'

Pop_subadult_inter <- subset(subadult_InterMortData_pop,
                             LCoh < (100-1E-9) & BirthDay == subadult_Inter_bd)
Pop_subadult_inter$mort <- 'Intermediate'
Pop_subadult_inter$comp <- 'Subadults competitively superior'

Pop_adult_nomort <- subset(adult_NoMortData_pop,
                           LCoh < (100-1E-9) & BirthDay == adult_NoMort_bd)
Pop_adult_nomort$mort <- 'Low'
Pop_adult_nomort$comp <- 'Adults competitively superior'

Pop_adult_inter <- subset(adult_InterMortData_pop,
                          LCoh < (100-1E-9) & BirthDay == adult_Inter_bd)
Pop_adult_inter$mort <- 'Intermediate'
Pop_adult_inter$comp <- 'Adults competitively superior'

Pop_symmetry_nomort <- subset(symmetry_NoMortData_pop,
                              LCoh < (100-1E-9) & BirthDay == symmetry_NoMort_bd)
Pop_symmetry_nomort$mort <- 'Low'
Pop_symmetry_nomort$comp <- 'Symmetric competition'

Pop_symmetry_inter <- subset(symmetry_InterMortData_pop,
                             LCoh < (100-1E-9) & BirthDay == symmetry_Inter_bd)
Pop_symmetry_inter$mort <- 'Intermediate'
Pop_symmetry_inter$comp <- 'Symmetric competition'

AllPop <- rbind(Pop_symmetry_nomort, Pop_symmetry_inter,
                Pop_adult_nomort, Pop_adult_inter,
                Pop_subadult_nomort, Pop_subadult_inter)
AllPop$comp <- as.factor(AllPop$comp)
AllPop$comp <- relevel(AllPop$comp, "Subadults competitively superior")
AllPop$comp <- relevel(AllPop$comp, "Symmetric competition")
AllPop$comp <- relevel(AllPop$comp, "Adults competitively superior")
AllPop$mort <- as.factor(AllPop$mort)
AllPop$mort <- relevel(AllPop$mort, "Low")


GrowthCurves <- ggplot(data = AllPop,
                       aes(x = (ACoh) / 250, y = LCoh)) +
  geom_path(linewidth = 2, aes(colour = comp)) +
  facet_grid(.~mort) +
  xlab(expression(atop("", atop("Time (years)", "")))) +
  ylab("Size (gram)") +
  scale_color_manual(values = c("darkgreen", "grey", "darkorange")) +
  theme(legend.position = 'right',
        legend.title = element_blank(),
        axis.title.y = element_text(size = 14, colour = 'black'),
        axis.title.x = element_text(size = 20, colour = 'black'),
        axis.text=element_text(size = 14, colour = 'black'),
        plot.margin = margin(10, 5, 0, 5.5, "pt"),
        legend.background = element_blank(), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  NULL
#GrowthCurves




###Plot time #####
subadult_better$Juv_Ad_bm = subadult_better$Juv_bm + 
  subadult_better$Ad_bm
adult_better$Juv_Ad_bm = adult_better$Juv_bm + 
  adult_better$Ad_bm
symmetry$Juv_Ad_bm = symmetry$Juv_bm + 
  symmetry$Ad_bm
subadult_better$comp = 'Subadults superior'
adult_better$comp = 'Adults superior'
symmetry$comp = 'Symmetric'
timedata_comp <- rbind(subadult_better, adult_better, symmetry)
timedata_comp$comp <- as.factor(timedata_comp$comp)
timedata_comp$comp <- relevel(timedata_comp$comp, "Subadults superior")
timedata_comp$comp <- relevel(timedata_comp$comp, "Symmetric")
timedata_comp$comp <- relevel(timedata_comp$comp, "Adults superior")



TimePlot_comp <- ggplot(data =  timedata_comp,
                        aes(x = Time/Season, y = Larv_bm * 1000)) +  
  geom_path(aes(colour = "aJuveniles")) +
  geom_path(aes(colour = "bJuveniles", y = Juv_Ad_bm * 1000)) + 
  scale_color_manual(values = c("#4B0055", "#36E9A7"),
                     labels = c("Juveniles in focal nursery", "Subadults + Adults"))+
  theme_bw() +
  facet_grid(.~comp)+ 
  ylab(expression(atop("", atop("Fish biomass",  paste("(mg ", L^-1, ")"))))) +
  xlab(xlabtime) +
  guides(color = guide_legend(byrow = TRUE,  override.aes = list(size = 3))) + 
  geom_vline(xintercept = (subset(minMaxTime, BifPar < 0.075)$MinTime) / Season, 
             linetype = 'dashed') +
  theme(legend.position = 'none',#,c(0.83, 0.75),
        legend.background = element_blank(),
        legend.text = element_text(size=11),
        axis.text=element_text(size=14),
        axis.title=element_text(size=20)) +
  annotate(geom = "text", x = 25, y = 330, label = 'Low', size = 6) +
  annotate(geom = "text", x = 75, y = 330, label = "Intermediate", size = 6) + 
  annotate(geom = "text", x = 125, y = 330, label = "High", size = 6) +
  NULL
#TimePlot_comp
#GrowthCurves

Appendix_competition_mort <- ggarrange(
  TimePlot_comp + theme(plot.margin = unit(c(0,0,0,0), 'lines')), 
  GrowthCurves + theme(plot.margin = unit(c(0,0,0,1.5), 'lines')),
  ncol = 1)
Appendix_competition_mort

ggsave("../Plots/App_compdif_fig2New.png", plot = Appendix_competition_mort, width = 1920, height = 1107,
       scale = 2.5, dpi = 300,
       units = "px")
