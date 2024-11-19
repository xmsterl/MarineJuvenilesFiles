###Script with all functions####
######_______________SETTINGS_______________######
###LIBS#####
library(tidyverse)
library(viridis)
library(grid)
library(gtable)
library(ggpubr)
###PARAMETER VALUES####
CohNr = 10 
Varnr = 3
Missing_value = 1E308
delta = 0.1 
mu_b = 0.001
Season = 250 
SB = 0.351
Sad = 200
Sshift = 100
Egg_prod = 0.5 * (1 - 0.97)
sigma = 0.3
attack = 26.5
handling = 12.5
maintenance = 0.015
CohNr = 21
xlabtime = expression(atop("",atop("Time (years)","")))

######_______________FUNCTIONS_______________######
###Function to give names to output files(GiveNames)####
Env_names = c("Time", "R1", "R2")
Indiv_names = c("Density", "Age", "Size", "Buffer", "IDnbegin", 
                "Age_Dietshift", "Age_adult", "Age_repro", "Shifted", 
                "Matured","TotalBuffer", "IDndiet",
                "PopIncl", "EstSource")
Pop_names = c("Larv_nr", "Larv_bm", "Juv_nr", "Juv_bm",
              "Ad_nr", "Ad_bm", "Vuln_nr", "Vuln_bm", "Buffer_bm",
              "Larv_period", "Larv_surv", "MatLrate", "Juv_period",
              "Juv_surv", "MatJrate", "Birthrate", 
              "newborns_nr", "MeanFec", "YOYsurv", "YOYsize", 
              "nu_L", "nu_J", "nu_A", "mort_L", "mort_J", "mort_A",
              "Cohorts_nr", "LEvents", "JEvents", "Starve_events",
              "Prod_L", "Prod_J", "Prod_A",
              "Prod_Est", "Est_nr", "MatLestrate", 
              "Est_coh", "Est_bm",
              "FracWS_nr")

GiveNames = function(Dataset, 
                     SeasonLength = 250, 
                     Size_mat = 200, 
                     biftime = F, bif = F, 
                     maxcohorts = 0, cohvar = 3,
                     MinMax = F,
                     bifname = 'Rbmax') {
  Data <- Dataset
  Popnames = Pop_names
  if(bif) {
    colnames(Data) = c(Env_names, Popnames, bifname, "Period","ch1")
  }  else if (biftime){
    colnames(Data) = c(Env_names, Popnames, bifname)
  } else {
    if(maxcohorts == 0) {
      colnames(Data) = c(Env_names, Popnames)
    }
    else {
      if(cohvar == 3) {
        adnames = cbind(paste("LCoh_", 1, sep =""), paste("NCoh_", 1, sep =""), paste("BCoh_", 1, sep =""))
        for(i in 2:maxcohorts){
          adnames = cbind(adnames, paste("LCoh_", i, sep =""), paste("NCoh_", i, sep =""), paste("BCoh_", i, sep =""))
        }}
      if(cohvar == 4) {
        adnames = cbind(paste("LCoh_", 1, sep =""), paste("NCoh_", 1, sep =""), paste("BCoh_", 1, sep =""), paste("BcurCoh_", 1, sep =""))
        for(i in 2:maxcohorts){
          adnames = cbind(adnames, paste("LCoh_", i, sep =""), paste("NCoh_", i, sep =""), paste("BCoh_", i, sep =""), paste("BcurCoh_", i, sep =""))
        }}
      
      colnames(Data) = c(Env_names, Popnames, adnames)
    }
  }
  Data$Tot_nr <- Data$Larv_nr + Data$Juv_nr +
    Data$Ad_nr
  Data$Tot_bm_buf <- Data$Larv_bm + Data$Juv_bm +
    Data$Ad_bm + Data$Buffer_bm 
  Data$Tot_bm <- Data$Larv_bm + Data$Juv_bm +
    Data$Ad_bm
  Data$AdBuf_bm <- Data$Ad_bm + Data$Buffer_bm 
  Data$TotJuv_nr <- Data$Larv_nr + Data$Juv_nr
  Data$TotJuv_bm <- Data$Larv_bm + Data$Juv_bm
  Data$Large_nr <- Data$Juv_nr +  Data$Ad_nr
  Data$Large_bm <- Data$Juv_bm + Data$Ad_bm
  Data$Large_bm_buf <- Data$Juv_bm + Data$Ad_bm + Data$Buffer_bm 
  Data$Larv_period[Data$Larv_period > Missing_value] = NA
  Data$Juv_period[Data$Juv_period > Missing_value] = NA
  Data$Larv_surv[Data$Larv_surv > Missing_value] = NA
  Data$Juv_surv[Data$Juv_surv > Missing_value] = NA
  Data$Prod_L[Data$Prod_L > Missing_value] = NA
  Data$Prod_J[Data$Prod_J > Missing_value] = NA
  Data$Prod_A[Data$Prod_A > Missing_value] = NA
  Data$MatL_all = Data$MatLrate + Data$MatLestrate
  Data$Prod_Est[Data$Prod_Est > Missing_value] = NA
  Data$TotLarvbm = Data$Est_bm + Data$Larv_bm
  Data$Tot_bm_nurs = Data$Tot_bm + Data$Est_bm
  Data$Growth_WS = sigma * (Data$R1 * attack /(1 + Data$R1 * attack * handling)) - maintenance
  Data$Growth_NS = sigma * (Data$R2 * attack /(1 + Data$R2 * attack * handling)) - maintenance
  Data$Growth_WS[Data$Growth_WS < 0] = 0
  Data$Growth_NS[Data$Growth_NS < 0] = 0
  name = deparse(substitute(Dataset))
  assign(name, Data, .GlobalEnv)    
}




###Export plot (Export_plot_func#####
Export_plot_func <- function(plot, location, 
                             filename, 
                             plotheight = 4, 
                             plotwidth = 4) {
  
  plotheight = plotheight
  textwidth = plotwidth
  saveloc = paste(location, filename, ".pdf", sep = "")
  write = 'y'
  if (file.exists(saveloc)){
    message = paste('The file\n', saveloc, ' \nalready exist. Do you want to overwrite [y/n]? ')
    write <- readline(prompt = message)
  }
  if(write=='y'){
    pdf(saveloc, width = textwidth, 
        height = plotheight, pointsize = 8)
    grid.draw(plot)
    dev.off()
  } else{
    print("The plot has not been saved!")
  }
  
}

######_______________LAYOUT__________________######
###Layout plots (layout)#####
layout = theme_bw()+
  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank(),
        text = element_text(size = 7, colour="black"),
        axis.text = element_text(size = 7, colour='black'),
        axis.title = element_text(size = 10, colour='black'),
        legend.title = element_text(size = 7, colour='black'),
        legend.background = element_rect(fill = "white"),
        legend.text = element_text(size = 7, colour = 'black'),
        legend.key.size = unit(.1, 'cm'), #change legend key size
        legend.key.height = unit(.2, 'cm'), #change legend key height
        legend.key.width = unit(.1, 'cm'), #change legend key width
        legend.box.margin=margin(t = -10, r = -5, b = -10, l = -5), #Change margin around legend
        plot.title = element_text(hjust = 0.5, size = 7, colour='black'))+
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 8, b = 0, l = -3)),
        axis.title.x = element_text(margin = margin(t = -2, r = 0, b = 0, l = 0))) +
  theme(legend.title = element_blank()) +
  theme(legend.spacing.y = unit(.1, 'cm')) +
  NULL

Emptyplot = ggplot(data=NULL)+
  layout+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y=element_blank(),
        axis.ticks = element_blank(),
        panel.border = element_blank())+
  NULL

####functions for getting pop data#####
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