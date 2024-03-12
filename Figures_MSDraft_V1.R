#*_________________Figures ms draft 1_________________*#
#Last changes: HTB, 7 aug 2023#
######______SETTINGS______######
rm(list = ls()) ##CLEAN EVERYTHING
setwd('D:\\Internship\\Manuscript\\Files_Hanna') ##CHANGE THIS FOLDER TO YOUR OWN LOCATION
source("D:\\Internship\\Manuscript\\Files_Hanna\\Rscripts\\Script_functions_MJ.R") #Load script#

#Set locations#
FigMain_loc = "D:\\Internship\\Manuscript\\Files_Hanna\\Figures\\Figures_main\\"
FigAppendix_loc = "Figures/Figures_appendx"
Bif_loc = "EBTRuns/Bifurcations/"
Time_loc = "D:\\Internship\\Manuscript\\Files_Hanna\\EBTRuns\\Timeseries\\"
#
######______Result 1: Quality other affect nurseries______####
###__In case R1max low (bottleneck in focal nursery)__####
###Get Data Low R1max####
LowR1_LowQ_Name = "LowR1max_LowQ"
LowR1_HighQ_Name = "LowR1max_HighQ"
TimeData_all_LowR1_LowQ = read.table(paste(Time_loc, LowR1_LowQ_Name,".out", sep =''),
                          header = F)
TimeData_all_LowR1_HighQ = read.table(paste(Time_loc, LowR1_HighQ_Name,".out", sep =''),
                                     header = F)

GiveNames(TimeData_all_LowR1_LowQ, maxcohorts = CohNr)
indexStart = which(colnames(TimeData_all_LowR1_LowQ) == 'LCoh_1') #Get pop data
indexEnd = indexStart + CohNr * Varnr - 1
PopData_LowR1_LowQ <-  TimeData_all_LowR1_LowQ[, c(1, indexStart:indexEnd)]
TimeData_LowR1_LowQ <- TimeData_all_LowR1_LowQ[, -c(indexStart:indexEnd)]

MaxYear = 40
MaxTime = max(TimeData_LowR1_LowQ$Time)
MinTime = max(0, MaxTime - MaxYear * Season)

GetPopData(InPutData = PopData_LowR1_LowQ, 
           OutPutData = "PopList_LowR1_LowQ",
           CohNr = CohNr, 
           InitColNr = which(colnames(PopData_LowR1_LowQ) == 'LCoh_1'), 
           Var = Varnr, StartSize = SB,
           NoData = Missing_value)


GiveNames(TimeData_all_LowR1_HighQ, maxcohorts = CohNr)
indexStart = which(colnames(TimeData_all_LowR1_HighQ) == 'LCoh_1') #Get pop data
indexEnd = indexStart + CohNr * Varnr - 1
PopData_LowR1_HighQ <-  TimeData_all_LowR1_HighQ[, c(1, indexStart:indexEnd)]
TimeData_LowR1_HighQ <- TimeData_all_LowR1_HighQ[, -c(indexStart:indexEnd)]


GetPopData(InPutData = PopData_LowR1_HighQ, 
           OutPutData = "PopList_LowR1_HighQ",
           CohNr = CohNr, 
           InitColNr = which(colnames(PopData_LowR1_HighQ) == 'LCoh_1'), 
           Var = Varnr, StartSize = SB,
           NoData = Missing_value)


TimeData_LowR1_LowQ$Quality = "Low"
TimeData_LowR1_HighQ$Quality = "High"

###Plot biomass timeseries####
xlabtime = expression(atop("",atop("Time (years)","")))
SBMTimePlot_LowR1_LowQ = ggplot(data =  TimeData_LowR1_LowQ,
                     aes(x = Time/Season, y = Larv_bm * 1000)) +
  geom_point(size = 0.1, aes(colour = "bJuveniles")) +
  geom_point(size = 0.1, aes(colour = "aJuveniles", y = Est_bm * 1000)) +
  geom_point(size = 0.1, aes(colour = "cJuveniles", y = Juv_bm * 1000)) +
  geom_point(size = 0.1, aes(colour = "dAdults", y = Ad_bm * 1000)) + 
  layout + 
  scale_color_manual(values = c(Stage_col_MJ),
                     labels = c("Juveniles in other nurseries", "Juveniles in focal nursery", "Subadults", "Adults"))+
  ylab(expression(atop("", atop("Consumer biomass",  paste("(mg ", L^-1, ")"))))) +
  xlab(xlabtime) +
  guides(color = guide_legend(byrow = TRUE,  override.aes = list(size = 1))) + 
  ylim(c(0, 200)) +
  xlim(c(0, 10)) +
  theme(legend.position = c(0.3, 0.8)) +
  NULL
SBMTimePlot_LowR1_LowQ


SBMTimePlot_LowR1_HighQ = ggplot(data =  TimeData_LowR1_HighQ,
                                aes(x = Time/Season, y = Larv_bm * 1000)) +
  geom_point(size = 0.1, aes(colour = "bJuveniles")) +
  geom_point(size = 0.1, aes(colour = "aJuveniles", y = Est_bm * 1000)) +
  geom_point(size = 0.1, aes(colour = "cJuveniles", y = Juv_bm * 1000)) +
  geom_point(size = 0.1, aes(colour = "dAdults", y = Ad_bm * 1000)) + 
  layout + 
  scale_color_manual(values = c(Stage_col_MJ),
                     labels = c("Juveniles in other nurseries", "Juveniles in focal nursery", "Subadults", "Adults"))+
  ylab(expression(atop("", atop("Consumer biomass",  paste("(mg ", L^-1, ")"))))) +
  xlab(xlabtime) +
  guides(color = guide_legend(byrow = TRUE,  override.aes = list(size = 1))) + 
  ylim(c(0, 200)) +
  theme(legend.position = 'none') +
  NULL

SBMTimePlot_LowR1_HighQ
SBMTimePlot_LowR1_LowQ

###Plot growth curves####
Pop1_Set1 = PopList_LowR1_LowQ$Day_750
Pop1_Set2 = PopList_LowR1_HighQ$Day_750
MaxAge = 20 * 250
GrowthPlot_LowR1max = 
  ggplot(data = subset(Pop1_Set1, Age <= MaxAge), aes(x = Age / Season, y = Length)) +
  geom_path(size = 2, aes(colour = "Low quality other nurseries")) +
  geom_path(size = 2, data = subset(Pop1_Set2, Age <= MaxAge),
            aes(colour = "High quality other nurseries")) +
  xlab(expression(atop("",atop("Time (years)", "")))) +
  ylab(expression(atop("", atop("Individual mass",  paste("(g)"))))) +
  geom_hline(yintercept = 100, linetype = 'dashed') +
  layout + 
  theme(legend.position = c(0.7, 0.05), legend.background = element_blank()) +
  scale_color_manual(values = Qual_col) + 
  NULL
GrowthPlot_LowR1max


###Export plot####
plots1 <- list(SBMTimePlot_LowR1_LowQ,
               SBMTimePlot_LowR1_HighQ,
               GrowthPlot_LowR1max)
grobs1 <- lapply(plots1, ggplotGrob)
part1 <- do.call(cbind, c(grobs1, size = "last"))

g1 <- gtable::gtable_add_grob(part1, lapply(LETTERS[1:3],
                                            textGrob, vjust=1, y=1,
                                            gp=gpar(fontface=2)), 
                              t=c(6, 6, 6), l=c(3, 12, 21),
                              z=c(20, 20, 20), clip="off")




PlotName = "Timeseries_qualityotherNurseries_LowR1max"
Export_plot_func(g1, filename = PlotName,
                 location = FigMain_loc,
                 plotheigh = 5, plotwidth = 10)

####Get values####
#Average survival of juveniles#
mean(TimeData_LowR1_HighQ$Larv_surv, na.rm = T)
mean(TimeData_LowR1_LowQ$Larv_surv, na.rm = T)
#Average time until migration to NS#
mean(TimeData_LowR1_HighQ$Larv_period, na.rm = T) / 250
mean(TimeData_LowR1_LowQ$Larv_period, na.rm = T) / 250
#Fraction of recruitment originated in WS#
FractionWS_HighQ = mean(TimeData_LowR1_HighQ$MatLrate) / (mean(TimeData_LowR1_HighQ$MatLrate) + mean(TimeData_LowR1_HighQ$MatLestrate))
FractionWS_LowQ = mean(TimeData_LowR1_LowQ$MatLrate) / (mean(TimeData_LowR1_LowQ$MatLestrate) + mean(TimeData_LowR1_LowQ$MatLrate))
FractionWS_LowQ
FractionWS_HighQ
mean(TimeData_LowR1_LowQ$FracWS_nr)
mean(TimeData_LowR1_HighQ$FracWS_nr)

#Absolute density recruited originated in WS#
mean(TimeData_LowR1_HighQ$MatLrate) * 250 * 1000
mean(TimeData_LowR1_LowQ$MatLrate) * 250 * 1000


###__In case R1max high (bottleneck in adult nursery) __####
###Get Data High R1max####
HighR1_LowQ_Name = "HighR1max_LowQ"
HighR1_HighQ_Name = "HighR1max_HighQ"
TimeData_all_HighR1_LowQ = read.table(paste(Time_loc, HighR1_LowQ_Name,".out", sep =''),
                                      header = F)
TimeData_all_HighR1_HighQ = read.table(paste(Time_loc, HighR1_HighQ_Name,".out", sep =''),
                                       header = F)

GiveNames(TimeData_all_HighR1_LowQ, maxcohorts = CohNr)
indexStart = which(colnames(TimeData_all_HighR1_LowQ) == 'LCoh_1') #Get pop data
indexEnd = indexStart + CohNr * Varnr - 1
PopData_HighR1_LowQ <-  TimeData_all_HighR1_LowQ[, c(1, indexStart:indexEnd)]
TimeData_HighR1_LowQ <- TimeData_all_HighR1_LowQ[, -c(indexStart:indexEnd)]

MaxYear = 40
MaxTime = max(TimeData_HighR1_LowQ$Time)
MinTime = max(0, MaxTime - MaxYear * Season)

GetPopData(InPutData = PopData_HighR1_LowQ, 
           OutPutData = "PopList_HighR1_LowQ",
           CohNr = CohNr, 
           InitColNr = which(colnames(PopData_HighR1_LowQ) == 'LCoh_1'), 
           Var = Varnr, StartSize = SB,
           NoData = Missing_value)


GiveNames(TimeData_all_HighR1_HighQ, maxcohorts = CohNr)
indexStart = which(colnames(TimeData_all_HighR1_HighQ) == 'LCoh_1') #Get pop data
indexEnd = indexStart + CohNr * Varnr - 1
PopData_HighR1_HighQ <-  TimeData_all_HighR1_HighQ[, c(1, indexStart:indexEnd)]
TimeData_HighR1_HighQ <- TimeData_all_HighR1_HighQ[, -c(indexStart:indexEnd)]


GetPopData(InPutData = PopData_HighR1_HighQ, 
           OutPutData = "PopList_HighR1_HighQ",
           CohNr = CohNr, 
           InitColNr = which(colnames(PopData_HighR1_HighQ) == 'LCoh_1'), 
           Var = Varnr, StartSize = SB,
           NoData = Missing_value)


TimeData_HighR1_LowQ$Quality = "Low"
TimeData_HighR1_HighQ$Quality = "High"

###Plot biomass timeseries####
xlabtime = expression(atop("",atop("Time (years)","")))
SBMTimePlot_HighR1_LowQ = ggplot(data =  TimeData_HighR1_LowQ,
                                 aes(x = Time/Season, y = Larv_bm * 1000)) +
  geom_point(size = 0.1, aes(colour = "bJuveniles")) +
  geom_point(size = 0.1, aes(colour = "aJuveniles", y = Est_bm * 1000)) +
  geom_point(size = 0.1, aes(colour = "cJuveniles", y = Juv_bm * 1000)) +
  geom_point(size = 0.1, aes(colour = "dAdults", y = Ad_bm * 1000)) + 
  layout + 
  scale_color_manual(values = c(Stage_col_MJ),
                     labels = c("Juveniles in other nurseries", "Juveniles in focal nursery", "Subadults", "Adults"))+
  ylab(expression(atop("", atop("Consumer biomass",  paste("(mg ", L^-1, ")"))))) +
  xlab(xlabtime) +
  guides(color = guide_legend(byrow = TRUE,  override.aes = list(size = 1))) + 
  ylim(c(0, 250)) +
  theme(legend.position = c(0.3, 0.8)) +
  NULL
SBMTimePlot_HighR1_LowQ


SBMTimePlot_HighR1_HighQ = ggplot(data =  TimeData_HighR1_HighQ,
                                  aes(x = Time/Season, y = Larv_bm * 1000)) +
  geom_point(size = 0.1, aes(colour = "bJuveniles")) +
  geom_point(size = 0.1, aes(colour = "aJuveniles", y = Est_bm * 1000)) +
  geom_point(size = 0.1, aes(colour = "cJuveniles", y = Juv_bm * 1000)) +
  geom_point(size = 0.1, aes(colour = "dAdults", y = Ad_bm * 1000)) + 
  layout + 
  scale_color_manual(values = c(Stage_col_MJ),
                     labels = c("Juveniles in other nurseries", "Juveniles in focal nursery", "Subadults", "Adults"))+
  ylab(expression(atop("", atop("Consumer biomass",  paste("(mg ", L^-1, ")"))))) +
  xlab(xlabtime) +
  guides(color = guide_legend(byrow = TRUE,  override.aes = list(size = 1))) + 
  ylim(c(0, 250)) +
  theme(legend.position = 'none') +
  NULL

SBMTimePlot_HighR1_HighQ
SBMTimePlot_HighR1_LowQ

###Plot growth curves####
Pop1_Set1 = PopList_HighR1_LowQ$Day_1500
Pop1_Set2 = PopList_HighR1_HighQ$Day_1500
MaxAge = 20 * 250
GrowthPlot_HighR1max = 
  ggplot(data = subset(Pop1_Set1, Age <= MaxAge), aes(x = Age / Season, y = Length)) +
  geom_path(size = 2, aes(colour = "Low quality other nurseries")) +
  geom_path(size = 2, data = subset(Pop1_Set2, Age <= MaxAge),
            aes(colour = "High quality other nurseries"),
            alpha = 0.5) +
  xlab(expression(atop("",atop("Time (years)", "")))) +
  ylab(expression(atop("", atop("Individual mass",  paste("(g)"))))) +
  geom_hline(yintercept = 100, linetype = 'dashed') +
  layout + 
  theme(legend.position = c(0.7, 0.05), legend.background = element_blank()) +
  scale_color_manual(values = Qual_col) + 
  NULL
GrowthPlot_HighR1max



###Export plot####
plots1 <- list(SBMTimePlot_HighR1_LowQ,
               SBMTimePlot_HighR1_HighQ,
               GrowthPlot_HighR1max)
grobs1 <- lapply(plots1, ggplotGrob)
part1 <- do.call(cbind, c(grobs1, size = "last"))

g1 <- gtable::gtable_add_grob(part1, lapply(LETTERS[1:3],
                                            textGrob, vjust=1, y=1,
                                            gp=gpar(fontface=2)), 
                              t=c(6, 6, 6), l=c(3, 12, 21),
                              z=c(20, 20, 20), clip="off")




PlotName = "Timeseries_qualityotherNurseries_HighR1max"
Export_plot_func(g1, filename = PlotName,
                 location = FigMain_loc,
                 plotheigh = 5, plotwidth = 10)

####Get values####
#Average survival of juveniles#
mean(TimeData_HighR1_HighQ$Larv_surv, na.rm = T)
mean(TimeData_HighR1_LowQ$Larv_surv, na.rm = T)
#Average time until migration to NS#
mean(TimeData_HighR1_HighQ$Larv_period, na.rm = T) / 250
mean(TimeData_HighR1_LowQ$Larv_period, na.rm = T) / 250
#Fraction of recruitment originated in WS#
FractionWS_HighQ = mean(TimeData_HighR1_HighQ$MatLrate) / (mean(TimeData_HighR1_HighQ$MatLrate) + mean(TimeData_HighR1_HighQ$MatLestrate))
FractionWS_LowQ = mean(TimeData_HighR1_LowQ$MatLrate) / (mean(TimeData_HighR1_LowQ$MatLestrate) + mean(TimeData_HighR1_LowQ$MatLrate))
FractionWS_LowQ
FractionWS_HighQ
mean(TimeData_HighR1_LowQ$FracWS_nr)
mean(TimeData_HighR1_HighQ$FracWS_nr)

#Absolute density recruited originated in WS#
mean(TimeData_HighR1_HighQ$MatLrate) * 250 * 1000
mean(TimeData_HighR1_LowQ$MatLrate) * 250 * 1000


###________Result 2: Effect of mortality_________####
###Get Timeseries low quality other nurseries####
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
AllTimeData_LowQ = AllTimeData
###Make plot of mortality series Low Q#####
xlabtime = expression(atop("",atop("Time (years)","")))

SBMTimePlotLowQ = ggplot(data =  AllTimeData_LowQ,
                     aes(x = (Time - MinTime)/Season, y = Larv_bm * 1000)) +
  geom_point(data = subset(AllTimeData_LowQ, Larv_bm > 0), size = 0.1, aes(colour = "aJuveniles")) +
  geom_point(data = subset(AllTimeData_LowQ, Est_bm > 0), size = 0.1, aes(colour = "aAJuveniles", y = Est_bm * 1000)) +
  geom_point(data = subset(AllTimeData_LowQ, Juv_bm > 0), size = 0.1, aes(colour = "bJuveniles", y = Juv_bm * 1000)) +
  geom_point(data = subset(AllTimeData_LowQ, Ad_bm > 0), size = 0.1, aes(colour = "cAdults", y = Ad_bm * 1000)) + 
  layout + 
  scale_color_manual(values = c(Stage_col_MJ),
                     labels = c("Juveniles in other nurseries",
                                "Juveniles in focal nursery", "Subadults", "Adults"))+
  ylab(expression(atop("", atop("Consumer biomass",  paste("(mg ", L^-1, ")"))))) +
  xlab(xlabtime) +
  guides(color = guide_legend(byrow = TRUE,  override.aes = list(size = 1))) + 
  geom_vline(xintercept = (subset(minMaxTime, BifPar > 0)$MinTime - MinTime) / Season, 
             linetype = 'dashed') +
  theme(legend.position = c(0.65, 0.7),
        legend.background = element_blank()) +
  ggtitle("Low quality other nurseries") +
  annotate(geom = "text", x = 60, y = 300, label = "0.003 / Day", size = 2) + 
  annotate(geom = "text", x = 100, y = 300, label = "0.006 / Day", size = 2) +
  NULL
SBMTimePlotLowQ

###Get Timeseries high quality other nurseries#####
DataNames = c("HighQ_nomort",
              "HighQ_Intermort",
              "HighQ_Highmort")
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
###Make plot mortality series High Q#####
SBMTimePlotHighQ = ggplot(data =  AllTimeData_HighQ,
                         aes(x = (Time - MinTime)/Season, y = Larv_bm * 1000)) +
  geom_point(data = subset(AllTimeData_HighQ, Larv_bm > 0), size = 0.1, aes(colour = "aJuveniles")) +
  geom_point(data = subset(AllTimeData_HighQ, Est_bm > 0), size = 0.1, aes(colour = "aAJuveniles", y = Est_bm * 1000)) +
  geom_point(data = subset(AllTimeData_HighQ, Juv_bm > 0), size = 0.1, aes(colour = "bJuveniles", y = Juv_bm * 1000)) +
  geom_point(data = subset(AllTimeData_HighQ, Ad_bm > 0), size = 0.1, aes(colour = "cAdults", y = Ad_bm * 1000)) + 
  layout + 
  scale_color_manual(values = c(Stage_col_MJ),
                     labels = c("Juveniles in other nurseries",
                                "Juveniles in focal nursery", "Subadults", "Adults"))+
  ylab(expression(atop("", atop("Consumer biomass",  paste("(mg ", L^-1, ")"))))) +
  xlab(xlabtime) +
  guides(color = guide_legend(byrow = TRUE,  override.aes = list(size = 1))) + 
  geom_vline(xintercept = (subset(minMaxTime, BifPar > 0)$MinTime - MinTime) / Season, 
             linetype = 'dashed') +
  theme(legend.position = 'none') +
  ggtitle("High quality other nurseries") +
  annotate(geom = "text", x = 60, y = 300, label = "0.003 / Day", size = 2) + 
  annotate(geom = "text", x = 100, y = 300, label = "0.006 / Day", size = 2) +
  NULL

###Bifurcations over mortality####
bifname = "mu_size"
Name_data1 = "HighQ_MortBif"
Name_data2 = "LowQ_MortBif"
AVGName_data1 = paste(Bif_loc, Name_data1, ".avg.out", sep = "")
AVGName_data2 = paste(Bif_loc, Name_data2, ".avg.out", sep = "")
BifMortLowQ = read.table(AVGName_data2, header = F)
BifMortHighQ = read.table(AVGName_data1, header = F)
GiveNames(BifMortLowQ, bif = T, bifname = bifname, 
          MinMax = F) #Give names
GiveNames(BifMortHighQ, bif = T,  bifname = bifname, 
          MinMax = F) #Give names

MortBif_GrowthRate = ggplot(data = BifMortLowQ, aes(x = mu_size, y = Growth_WS))+
  geom_point(size = 2, aes(colour = "Low quality")) +
  geom_point(size = 2, data = BifMortHighQ, aes(colour = "High quality")) +
  layout +
  theme(legend.position = c(0.8, 0.2)) +
  xlab(expression(atop("", atop("Additional mortality in focal nursery", paste("(", day^-1, ")"))))) +
  ylab(expression(atop("", atop("Growth rate",  paste("(g ", g^-1, d^-1, ")"))))) +
  scale_color_manual(values = Qual_col) + 
  NULL
MortBif_GrowthRate  

MortBif_YearlySurv = ggplot(data = BifMortLowQ, aes(x = mu_size, y = exp(-(mort_L + mu_size) * Season)))+
  geom_point(size = 2, aes(colour = "Low quality"))+
  geom_point(size = 2, data = BifMortHighQ, aes(colour = "High quality"),
             alpha = 0.5) +
  layout +
  theme(legend.position = c(0.8, 0.8)) +
  xlab(expression(atop("", atop("Additional mortality in focal nursery", paste("(", day^-1, ")"))))) +
  ylab(expression(atop("", atop("Yearly survival probability in Wadden Sea", "")))) +
  scale_color_manual(values = c("#0072B2","#D55E00")) + 
  NULL
MortBif_YearlySurv

plots1 <- list(SBMTimePlotLowQ, SBMTimePlotHighQ)
grobs1 <- lapply(plots1, ggplotGrob)
part1 <- do.call(cbind, c(grobs1, size = "last"))
plots2 <- list(MortBif_GrowthRate, MortBif_YearlySurv)
grobs2 <- lapply(plots2, ggplotGrob)
part2 <- do.call(cbind, c(grobs2, size = "last"))
part3 <- do.call(rbind, c(list(part1, part2), size="first"))
part4 <- gtable_add_cols(part3, unit(0.1, "null"), 11)
g1 <- gtable::gtable_add_grob(part4, lapply(LETTERS[1:4],
                                            textGrob, vjust=1, y=1,
                                            gp=gpar(fontface=2)), 
                              t=c(6, 6, 18, 18), l=c(3, 13, 3, 13),
                              z=c(20, 20, 20, 20), clip="off")
PlotName = "HighR1max_effectmort"
Export_plot_func(g1, filename = PlotName,
                   location = FigMain_loc,
                 plotheigh = 8, plotwidth = 8)

###__Result 3: Effect of R1max___####
###Timeseries low quality###
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
AllTimeData_R1maxLowQ = AllTimeData
######Make plot of AllTimeData#####
label1 = expression(paste("7.5 mg ", L^-1, day^-1))
label2 = expression(paste("2.5 mg ", L^-1, day^-1))
label3 = expression(paste("0.5 mg ", L^-1, day^-1))

xlabtime = expression(atop("",atop("Time (years)","")))

# SBMTimePlotR1maxLowQ = ggplot(data =  AllTimeData_R1maxLowQ,
#                          aes(x = (Time - MinTime)/Season, y = Larv_bm * 1000)) +
#   geom_point(data = subset(AllTimeData_R1maxLowQ, Larv_bm > 0), size = 0.1, aes(colour = "aJuveniles")) +
#   geom_point(data = subset(AllTimeData_R1maxLowQ, Est_bm > 0), size = 0.1, aes(colour = "aAJuveniles", y = Est_bm * 1000)) +
#   geom_point(data = subset(AllTimeData_R1maxLowQ, Juv_bm > 0), size = 0.1, aes(colour = "bJuveniles", y = Juv_bm * 1000)) +
#   geom_point(data = subset(AllTimeData_R1maxLowQ, Ad_bm > 0), size = 0.1, aes(colour = "cAdults", y = Ad_bm * 1000)) + 
#   layout + 
#   scale_color_manual(values = c(Stage_col_MJ),
#                      labels = c("Juveniles in other nurseries",
#                                 "Juveniles in focal nursery", "Subadults", "Adults"))+
#   ylab(expression(atop("", atop("Consumer biomass",  paste("(mg ", L^-1, ")"))))) +
#   xlab(xlabtime) +
#   guides(color = guide_legend(byrow = TRUE,  override.aes = list(size = 1))) + 
#   geom_vline(xintercept = (subset(minMaxTime, BifPar < 0.075)$MinTime - MinTime) / Season, 
#              linetype = 'dashed') +
#   theme(legend.position = c(0.6, 0.85),
#         legend.background = element_blank()) +
#   ggtitle("Low quality other nurseries") +
#   annotate(geom = "text", x = 20, y = 300, label = label1, size = 2) + 
#   annotate(geom = "text", x = 60, y = 300, label = label2, size = 2) + 
#   annotate(geom = "text", x = 100, y = 300, label = label3, size = 2) +
#   NULL


SBMTimePlotR1maxLowQ = ggplot(data =  AllTimeData_R1maxLowQ,
                              aes(x = Time/Season, y = Larv_bm * 1000)) +
  geom_point(data = subset(AllTimeData_R1maxLowQ, Larv_bm > 0), size = 0.1, aes(colour = "aJuveniles")) +
  geom_point(data = subset(AllTimeData_R1maxLowQ, Est_bm > 0), size = 0.1, aes(colour = "aAJuveniles", y = Est_bm * 1000)) +
  geom_point(data = subset(AllTimeData_R1maxLowQ, Juv_bm > 0), size = 0.1, aes(colour = "bJuveniles", y = Juv_bm * 1000)) +
  geom_point(data = subset(AllTimeData_R1maxLowQ, Ad_bm > 0), size = 0.1, aes(colour = "cAdults", y = Ad_bm * 1000)) + 
  layout + 
  scale_color_manual(values = c(Stage_col_MJ),
                     labels = c("Juveniles in other nurseries",
                                "Juveniles in focal nursery", "Subadults", "Adults"))+
  ylab(expression(atop("", atop("Consumer biomass",  paste("(mg ", L^-1, ")"))))) +
  xlab(xlabtime) +
  guides(color = guide_legend(byrow = TRUE,  override.aes = list(size = 1))) + 
  geom_vline(xintercept = (subset(minMaxTime, BifPar < 0.075)$MinTime) / Season, 
             linetype = 'dashed') +
  theme(legend.position = c(0.6, 0.85),
        legend.background = element_blank()) +
  ggtitle("Low quality other nurseries") +
  annotate(geom = "text", x = 20, y = 300, label = label1, size = 2) + 
  annotate(geom = "text", x = 60, y = 300, label = label2, size = 2) + 
  annotate(geom = "text", x = 100, y = 300, label = label3, size = 2) +
  NULL
SBMTimePlotR1maxLowQ


###high q#####
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
  scale_color_manual(values = c(Stage_col_MJ),
                     labels = c("Juveniles in focal nursery", "Subadults + Adults"))+
  ylab(expression(atop("", atop("Consumer biomass",  paste("(mg ", L^-1, ")"))))) +
  xlab(xlabtime) +
  guides(color = guide_legend(byrow = TRUE,  override.aes = list(size = 1))) + 
  geom_vline(xintercept = (subset(minMaxTime, BifPar < 0.075)$MinTime) / Season, 
             linetype = 'dashed') +
  theme(legend.position = c(0.5, 0.85),
        legend.background = element_blank(),
        legend.text = element_text(size=10)) +
  annotate(geom = "text", x = 20, y = 300, label = label1, size = 5) + 
  annotate(geom = "text", x = 60, y = 300, label = label2, size = 5) + 
  annotate(geom = "text", x = 100, y = 300, label = label3, size = 5) +
  NULL
SBMTimePlotR1maxHighQ

###Bifurcations####
bifname = "R1max"
Name_data1 = "HighQ_R1maxbif"
Name_data2 = "LowQ_R1maxbif"
AVGName_data1 = paste(Bif_loc, Name_data1, ".avg.out", sep = "")
AVGName_data2 = paste(Bif_loc, Name_data2, ".avg.out", sep = "")
BifLowR1max = read.table(AVGName_data2, header = F)
BifHighR1max = read.table(AVGName_data1, header = F)
GiveNames(BifLowR1max, bif = T,  bifname = bifname, 
          MinMax = F) #Give names
GiveNames(BifHighR1max, bif = T, bifname = bifname, 
          MinMax = F) #Give names


GrowthRateR1max = ggplot(data = BifLowR1max, aes(x = R1max * 100, y = Growth_WS))+
  geom_point(size = 2, aes(colour = "Low quality"))+
  geom_point(size = 2, data = BifHighR1max, aes(colour = "High quality")) +
  layout +
  theme(legend.position = c(0.2, 0.2)) +
  xlab(expression(atop("", atop("Productivity in focal nursery", paste("(mg ", L^1, day^-1, ")"))))) +
  ylab(expression(atop("", atop("Growth rate in focal nursery",  paste("(g ", g^-1, d^-1, ")"))))) +
  scale_color_manual(values = Qual_col) + 
  scale_x_continuous(trans = "reverse", limits = c(8, 0)) +
  NULL
GrowthRateR1max

YearlySurvR1max = ggplot(data = BifLowR1max, aes(x = R1max * 100, y = exp(-(mort_L) * 250)))+
  geom_point(size = 2, aes(colour = "Low quality"))+
  geom_point(size = 2, data = BifHighR1max, aes(colour = "High quality")) +
  layout +
  theme(legend.position = c(0.2, 0.2)) +
  xlab(expression(atop("", atop("Productivity in focal nursery", paste("(mg ", L^1, day^-1, ")"))))) +
  ylab(expression(atop("", atop("Yearly survival probability in focal nursery", ""))))+
  scale_color_manual(values = Qual_col) +
  scale_x_continuous(trans = "reverse", limits = c(8, 0)) +
  NULL
YearlySurvR1max


plots1 <- list(SBMTimePlotR1maxLowQ, SBMTimePlotR1maxHighQ)
grobs1 <- lapply(plots1, ggplotGrob)
part1 <- do.call(cbind, c(grobs1, size = "last"))
plots2 <- list(GrowthRateR1max, YearlySurvR1max)
grobs2 <- lapply(plots2, ggplotGrob)
part2 <- do.call(cbind, c(grobs2, size = "last"))
part3 <- do.call(rbind, c(list(part1, part2), size="first"))
part4 <- gtable_add_cols(part3, unit(0.1, "null"), 11)
g1 <- gtable::gtable_add_grob(part4, lapply(LETTERS[1:4],
                                            textGrob, vjust=1, y=1,
                                            gp=gpar(fontface=2)), 
                              t=c(6, 6, 18, 18), l=c(3, 13, 3, 13),
                              z=c(20, 20, 20, 20), clip="off")


PlotName = "EffectR1max"
Export_plot_func(SBMTimePlotR1maxHighQ, filename = PlotName,
                 location = FigMain_loc,
                 plotheigh = 8, plotwidth = 8)


###___Result 4: Effect of mortality in North Sea__####
###Read data####
LowR1max_LowQName = paste(Bif_loc, "LowQLowR1max_NSbif.avg.out", sep ="")
HighR1max_LowQName = paste(Bif_loc, "LowQHighR1max_NSbif.avg.out", sep ="")
LowR1max_HighQName = paste(Bif_loc, "HighQLowR1max_NSbif.avg.out", sep ="")
HighR1max_HighQName = paste(Bif_loc, "HighQHighR1max_NSbif.avg.out", sep ="")

LowR1max_LowQ = read.table(LowR1max_LowQName, 
                          header = F)
LowR1max_HighQ = read.table(LowR1max_HighQName, 
                           header = F)
HighR1max_LowQ = read.table(HighR1max_LowQName, 
                       header = F)
HighR1max_HighQ = read.table(HighR1max_HighQName, 
                            header = F)

GiveNames(LowR1max_LowQ,  bif = T, bifname = "mu_size")
GiveNames(HighR1max_LowQ,  bif = T, bifname = "mu_size")
GiveNames(LowR1max_HighQ,  bif = T, bifname = "mu_size")
GiveNames(HighR1max_HighQ,  bif = T, bifname = "mu_size")
####plot of densities####
DensLMPlot = ggplot(data = LowR1max_LowQ,
                  aes(x = mu_size, y = Larv_bm * 1000)) +
  geom_point(aes(colour = "Low quality"),
             shape = 1, fill = 'white') +
  geom_point(data = LowR1max_HighQ,
             aes(colour = "High quality"),
             shape = 1, fill = 'white') +
  geom_point(data = HighR1max_LowQ, 
             aes(colour = "Low quality")) +
  geom_point(data = HighR1max_HighQ,
             aes(colour = "High quality")) +
  scale_color_manual(values = Qual_col) + 
  layout +
  ylim(c(0, 0.08 * 1000)) +
  theme(legend.position = c(0.8, 0.8)) +
  ylab(expression(atop("", atop("Density of juveniles",  paste("(mg ", L^-1, ")"))))) +
  xlab(expression(atop("", atop("Additional mortality in adult habitat", paste("(", day^-1, ")"))))) +
  NULL

DensLMPlot

DensNSPlot = ggplot(data = LowR1max_LowQ,
                    aes(x = mu_size, y = Large_bm * 1000)) +
  geom_point(aes(colour = "Low quality"),
             shape = 1, fill = 'white') +
  geom_point(data = LowR1max_HighQ,
             aes(colour = "High quality"),
             shape = 1, fill = 'white') +
  geom_point(data = HighR1max_LowQ, 
             aes(colour = "Low quality")) +
  geom_point(data = HighR1max_HighQ,
             aes(colour = "High quality")) +
  scale_color_manual(values = Qual_col) + 
  layout +
  theme(legend.position = c(0.8, 0.8)) +
  ylab(expression(atop("", atop("Density of (sub)adults",  paste("(mg ", L^-1, ")"))))) +
  xlab(expression(atop("", atop("Additional mortality in adult habitat", paste("(", day^-1, ")"))))) +
  NULL
DensNSPlot

BirthPlot = ggplot(data = LowR1max_LowQ,
       aes(x = mu_size, y = Birthrate * 1000 * 250)) +
  geom_point(aes(colour = "Low quality"),
             shape = 1, fill = 'white') +
  geom_point(data = LowR1max_HighQ,
             aes(colour = "High quality"),
             shape = 1, fill = 'white') +
  geom_point(data = HighR1max_LowQ, 
             aes(colour = "Low quality")) +
  geom_point(data = HighR1max_HighQ,
             aes(colour = "High quality")) +
  scale_color_manual(values = Qual_col) + 
  layout +
  theme(legend.position = c(0.8, 0.8)) +
  ylab(expression(atop("", atop("Population birthrate",  paste("(mg ", L^-1, year^-1, ")"))))) +
  xlab(expression(atop("", atop("Additional mortality in adult habitat", paste("(", day^-1, ")"))))) +
  NULL
BirthPlot

SurvPlot = ggplot(data = LowR1max_LowQ,
                   aes(x = mu_size, y = exp(250 * -(mu_size + mort_J)))) +
  geom_point(aes(colour = "Low quality"),
             shape = 1, fill = 'white') +
  geom_point(data = LowR1max_HighQ,
             aes(colour = "High quality"),
             shape = 1, fill = 'white') +
  geom_point(data = HighR1max_LowQ, 
             aes(colour = "Low quality")) +
  geom_point(data = HighR1max_HighQ,
             aes(colour = "High quality")) +
  scale_color_manual(values = Qual_col) + 
  layout +
  theme(legend.position = c(0.8, 0.8)) +
  ylab(expression(atop("", atop("Yearly survival probability in adult habitat", ""))))+
  xlab(expression(atop("", atop("Additional mortality in adult habitat", paste("(", day^-1, ")"))))) +
  NULL

SurvPlot


plots1 <- list(DensLMPlot, DensNSPlot)
grobs1 <- lapply(plots1, ggplotGrob)
part1 <- do.call(cbind, c(grobs1, size = "last"))
plots2 <- list(BirthPlot, SurvPlot)
grobs2 <- lapply(plots2, ggplotGrob)
part2 <- do.call(cbind, c(grobs2, size = "last"))
part3 <- do.call(rbind, c(list(part1, part2), size="first"))
part4 <- gtable_add_cols(part3, unit(0.1, "null"), 11)
g1 <- gtable::gtable_add_grob(part4, lapply(LETTERS[1:4],
                                            textGrob, vjust=1, y=1,
                                            gp=gpar(fontface=2)), 
                              t=c(6, 6, 18, 18), l=c(3, 13, 3, 13),
                              z=c(20, 20, 20, 20), clip="off")

PlotName = "EffectmuNS"
Export_plot_func(g1, filename = PlotName,
                 location = FigMain_loc,
                 plotheigh = 8, plotwidth = 8)

###___Result 5: Effect of loss of connectivity___####
###Read data#####
GoodCond_LowQ_connectName = paste(Bif_loc, "GoodCond_connectivitybif.avg.out",
                                  sep = "")
GoodCond_LowQ_connect = read.table(GoodCond_LowQ_connectName, 
                           header = F)
GiveNames(GoodCond_LowQ_connect,  bif = T, bifname = "connectivity")

HighNSMort_HighQ_connectName =
  paste(Bif_loc, "HighQ_highNSmort_connectivitybif.avg.out",
        sep = "")
HighNSMort_LowQ_connectName =
  paste(Bif_loc, "LowQ_highNSmort_connectivitybif.avg.out",
        sep = "")
HighWSMort_HighQ_connectName =
  paste(Bif_loc, "HighQ_highWSmort_connectivitybif.avg.out",
        sep = "")
HighWSMort_LowQ_connectName =
  paste(Bif_loc, "LowQ_highWSmort_connectivitybif.avg.out",
        sep = "")
HighNSMort_HighQ_connect = read.table(HighNSMort_HighQ_connectName, 
                                      header = F)
HighNSMort_LowQ_connect = read.table(HighNSMort_LowQ_connectName, 
                                      header = F)
HighWSMort_HighQ_connect = read.table(HighWSMort_HighQ_connectName, 
                                      header = F)
HighWSMort_LowQ_connect = read.table(HighWSMort_LowQ_connectName, 
                                     header = F)

GiveNames(HighWSMort_LowQ_connect,  bif = T, bifname = "connectivity")
GiveNames(HighNSMort_LowQ_connect,  bif = T, bifname = "connectivity")
GiveNames(HighWSMort_HighQ_connect,  bif = T, bifname = "connectivity")
GiveNames(HighNSMort_HighQ_connect,  bif = T, bifname = "connectivity")



####plot bif#####
DensityPlotLow = ggplot(data = GoodCond_LowQ_connect,
                        aes(x = connectivity,
                            y = Larv_bm * 1000)) +
  geom_point(size = 1, aes(colour = "bJuveniles")) +
  geom_point(size = 1, aes(colour = "aJuveniles", y = Est_bm * 1000)) +
  geom_point(size = 1, aes(colour = "cJuveniles", y = Juv_bm * 1000)) +
  geom_point(size = 1, aes(colour = "dAdults", y = Ad_bm * 1000)) + 
  layout +
  scale_color_manual(values = c(Stage_col_MJ),
                     labels = c("Juveniles in other nurseries", "Juveniles in focal nursery", "Subadults", "Adults"))+
  ylab(expression(atop("", atop("Consumer biomass",  paste("(mg ", L^-1, ")"))))) +
  xlab(expression(atop("", atop("Connectivity")))) +
  guides(color = guide_legend(byrow = TRUE,  override.aes = list(size = 1))) + 
  scale_x_continuous(trans = "reverse") +
  theme(legend.position = c(0.25, 0.9)) +
  NULL

DensityPlotLow

GrowthRate_connect = ggplot(data = GoodCond_LowQ_connect, aes(x = connectivity, y = Growth_WS))+
  geom_point(size = 2) +
  layout +
  theme(legend.position = c(0.8, 0.2)) +
  ylab(expression(atop("", atop("Growth rate",  paste("(g ", g^-1, d^-1, ")"))))) +
  scale_x_continuous(trans = "reverse") +
  xlab(expression(atop("", atop("Connectivity")))) +
  NULL
GrowthRate_connect

DensityMortNS = ggplot(data = HighNSMort_LowQ_connect, 
                       aes(x = connectivity, y = Tot_bm_nurs * 1000))+
  geom_path(size = 2, aes(colour = "Low quality"))+
  geom_path(size = 2, data = HighNSMort_HighQ_connect, 
            aes(colour = "High quality")) +
  layout +
  theme(legend.position = c(0.8, 0.8)) +
  scale_x_continuous(trans = "reverse") +
  xlab(expression(atop("", atop("Connectivity")))) +
  scale_color_manual(values = Qual_col) + 
  ylab(expression(atop("", atop("Total consumer biomass",  paste("(mg ", L^-1, ")"))))) +
  ggtitle("High mortality in the North Sea") +
  NULL

DensityMortWS = ggplot(data = HighWSMort_LowQ_connect, 
                       aes(x = connectivity, y = Tot_bm_nurs * 1000))+
  geom_path(size = 2, aes(colour = "Low quality"))+
  geom_path(size = 2, data = HighWSMort_HighQ_connect, 
            aes(colour = "High quality")) +
  layout +
  theme(legend.position = c(0.8, 0.2)) +
  scale_x_continuous(trans = "reverse") +
  xlab(expression(atop("", atop("Connectivity")))) +
  scale_color_manual(values = Qual_col) + 
  ylab(expression(atop("", atop("Total consumer biomass",  paste("(mg ", L^-1, ")"))))) +
  ggtitle("High mortality in the Wadden Sea") +
  NULL

DensityMortNS
DensityMortWS

GrowthRate_connect
DensityPlotLow

plots1 <- list(DensityPlotLow, GrowthRate_connect)
grobs1 <- lapply(plots1, ggplotGrob)
part1 <- do.call(cbind, c(grobs1, size = "last"))
plots2 <- list(DensityMortNS, DensityMortWS)
grobs2 <- lapply(plots2, ggplotGrob)
part2 <- do.call(cbind, c(grobs2, size = "last"))
part3 <- do.call(rbind, c(list(part1, part2), size="first"))
part4 <- gtable_add_cols(part3, unit(0.1, "null"), 11)
g1 <- gtable::gtable_add_grob(part4, lapply(LETTERS[1:4],
                                            textGrob, vjust=1, y=1,
                                            gp=gpar(fontface=2)), 
                              t=c(6, 6, 18, 18), l=c(3, 13, 3, 13),
                              z=c(20, 20, 20, 20), clip="off")


PlotName = "Effectconnect"
Export_plot_func(g1, filename = PlotName,
                 location = FigMain_loc,
                 plotheigh = 8, plotwidth = 8)


Export_plot_func(g1, filename = PlotName, location = "../../Presentations/Effectconnect",
                 plotheigh = 8, plotwidth = 8)
