rm(list = ls()) ##CLEAN EVERYTHING
setwd("U:\\Manuscript\\NewFiles_Hanna\\") ##CHANGE THIS FOLDER TO YOUR OWN LOCATION
source("U:\\Manuscript\\Code_figures\\Script_functions_MJ.R") #Load script#
CohNr = 21
xlabtime = expression(atop("",atop("Time (years)","")))
library(ggpubr)

#Set locations#
#FigMain_loc = "U:\\Manuscript\\Figures_main"
#FigAppendix_loc = "Figures/Figures_appendx"
#Bif_loc = "D:\\Internship\\Manuscript\\Files_Hanna\\EBTRuns\\Bifurcations\\"
Time_loc = "U:\\Manuscript\\NewFiles_Hanna\\"
#
####Better/simpler functions for getting pop data#####
PrepPopData <- function(DataPop,
                        maxcohorts = 21,
                        var = 3,
                        name1 = "LCoh_",
                        name2 = "NCoh_",
                        name3 = "BCoh_") {
  name = deparse(substitute(DataPop))
  print(name)
  AllData <- DataPop
  AllData[AllData > 10^10] <- NA
  
  for (i in 1:maxcohorts) {
    # Create the column names
    NCoh_name <- paste0(name2, i)
    BirthDay_name <- paste0("BirthDay_", i)
    
    # Find the position of NCoh_i column
    pos <- which(names(AllData) == NCoh_name)
    
    # Add the new column after NCoh_i
    AllData <- add_column(AllData, !!BirthDay_name := NA, .after = pos)
  }
  
  for (i in 1:maxcohorts) {
    LCoh_name <- paste0(name1, i)
    BirthDay_name <- paste0("BirthDay_", i)
    
    # Identify rows where ACoh_name is NA or 0
    na_indices <- is.na(AllData[, LCoh_name])
    zero_indices <- AllData[, LCoh_name] == 0.351
    zero_indices[is.na(zero_indices)] = FALSE 
    # Assign NA or Time value based on conditions
    AllData[zero_indices, BirthDay_name] <- AllData[zero_indices, "Time"]
    AllData <- AllData %>% fill(BirthDay_name)
  }
  
  # Create a vector of column names
  cols <- c(paste0(c(name1, name2, name3, "BirthDay_"), rep(1:maxcohorts, each = var+1)))
  
  # Reshape the data
  PopData_long <- AllData %>%
    pivot_longer(cols = all_of(cols), names_to = c(".value", "Group"), names_pattern = "(.*)_(.*)")
  PopData_long$Group = NULL
  PopData_long <- na.omit(PopData_long)
  assign(name, PopData_long, .GlobalEnv)
}

PopCalc <- function(PopData) {
  name = deparse(substitute(PopData))
  ###pars###
  Size_dietswitch = 100
  Size_mat = 200
  #####
  popdata <- data.frame()
  poptemp <- PopData 
  bd <- unique(poptemp$BirthDay)
  for(i in 1:length(bd)){
    subdata = subset(poptemp, BirthDay == bd[i])
    subdata$ACoh = seq(0,(nrow(subdata)-1))
    subdata$Day = subdata$Time %% Season
    subdata$Year = subdata$Time %/% Season
    subdata$LifeStage = "Juvenile"
    subdata$LifeStage[subdata$LCoh >= Size_dietswitch] = "Subadult"
    subdata$LifeStage[subdata$LCoh >= Size_mat] = "Adult"
    subdata$DietAge = NA
    subdata$MaxAge <- max(subdata$ACoh)
    if(max(subdata$LCoh >= Size_dietswitch)) {
      index <- which.min(abs(subdata$LCoh - Size_dietswitch))
      subdata$DietAge = subdata$ACoh[index]
    }
    popdata = rbind(popdata, subdata)
  }
  assign(name, popdata, .GlobalEnv)   
}


#----------------------------------------------------------------------------------------#
####Fig2: (Result 1): reduced food in focal nursery#######
#----------------------------------------------------------------------------------------#
DataNames = c("Fig2_HighQ_HighR1max",
              "Fig2_HighQ_InterR1max",
              "Fig2_HighQ_LowR1max")
BifPar = c(0.075, 0.025, 0.005)
minMaxTime <- data.frame()
AllTimeData <- data.frame()
CohNr = 21
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


####Fig2: Growth data figure 2####
#First get average juvenile period#
HighData <- subset(AllTimeData_R1maxHighQ, Bif == 0.075)
InterData <- read.table("Fig2_HighQ_InterR1maxLong.out", header = F)
GiveNames(InterData, maxcohorts = CohNr)

ggplot(data = subset(InterData, Time < 10000), aes(x = Time, y = Larv_period)) +
  geom_point()

#Mgeom_point()#Mean period High#
mean(HighData$Larv_period, na.rm = T) / 250
mean(InterData$Larv_period, na.rm = T) / 250

####Fig2: Get pop data####
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

####Fig3: Combine panels figure 2######
# Combine plots, make sure to also have growthcurves plots
ggarrange(SBMTimePlotR1maxHighQ +
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
####Fig 3

####Fig3  (Result 2): Increasing mortality in the focal nursery#####
#----------------------------------------------------------------------------------------#
# Result 2: increased mortality in focal nursery
#----------------------------------------------------------------------------------------#
DataNames = c("Fig3_HighQ_nomort",
              "Fig3_HighQ_Intermort",
              "Fig3_HighQ_Highmort")
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

####Fig3: Growth data figure ####
#First get average juvenile period#
HighData <- subset(AllTimeData_HighQ, Bif == 0)
InterData <- read.table("Fig3_HighQ_InterMortLong.out", header = F)
GiveNames(InterData, maxcohorts = CohNr)

ggplot(data = subset(InterData, Time < 10000), aes(x = Time, y = Larv_period)) +
  geom_point()

#Mgeom_point()#Mean period High#
mean(HighData$Larv_period, na.rm = T) / 250
mean(InterData$Larv_period, na.rm = T) / 250

####Fig3: Get pop data####
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

NoMortFocal_growth

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

InterMortFocal_growth




###Add everything###
ggarrange(MortTimePlotHighQ +
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





####Fig3: Combine panels#####
ggarrange(MortTimePlotHighQ +
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
