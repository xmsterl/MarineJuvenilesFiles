####Script to get growth curves#####
rm(list = ls())

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

CohNr_spec = 20 #Nr of cohorts in output #CHANGE IN .H FILE!!!!
SB = 0.351 #Size at birth
Varnr = 3 #3 output variables per cohort
minmortsize = 0.351 #Min size-specific mort
maxmortsize = 100 #Max size-specific mort
Missing_value = 1E308
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
  # assign(name, Data, .GlobalEnv)    
  Data
}


####Function to read pop data from .out output (GetPopData) ####
GetPopData <- function(InPutData = PopData,   
                       OutPutData = "PopList", #name of output
                       CohNr = 20,  #nr of cohorts that are followed
                       Var = 3, #nr of columns per cohort
                       InitColNr = 2, #index of first column
                       StartSize = 0.351, #Size at birth
                       NoData = 10E10, #Large value
                       eggmort = 0.97,  #mortality of eggs
                       eggeff = 0.5, #efficiency of egg production
                       background_mort = 0.001, #background mortality
                       sizemort = 0, #Size-specific mortality
                       mort_smin = 0.351, #Min size of size-specific mort
                       mort_smax = 100, #Max size of size-specific mort
                       Size_shift = 100, #Size at diet shift
                       Size_mat = 200 #Size at maturation
){ 
  if(Var < 3 || Var > 4) {
    print("The function only works for 3 or 4 variables in the output (Length, Density, TotBuffer, CurrentBuf)")
    stop()
  }
  #Create poplist
  PopList <- list()
  #Get column indices of length columns#
  LengthIn <- seq(InitColNr, ((CohNr - 1) * Var + (Var - 1)), Var)
  
  #for each set of multiple (=Var) columns (number of sets = length(LengthIn))
  j = 1 #iterate over each set
  ###First, find all starting & ending indices###
  for(j in 1:length(LengthIn)) {
    #Get for the first set the first input, where size = size at birth#
    StartCoh_first = which(InPutData[, LengthIn[j]] == StartSize) #all rows with the size at birth
    StartCoh = StartCoh_first[1] #this vector will contain all real starting indices
    if(is.na(StartCoh)) {next} #just a check to make sure it contains data 
    if(length(StartCoh_first) > 1) { #multiple cohorts in this column 
      for(k in (2:length(StartCoh_first))){
        if(StartCoh_first[k] != (StartCoh_first[k - 1] + 1)) { #Make sure it is really a new cohort, and not just no growth
          StartCoh = c(StartCoh, StartCoh_first[k]) #if so, add it to the starting index. 
        }
      }}
    EndCoh_start <- which(InPutData[, LengthIn[j]] > NoData) #Which rows do not contain data? 
    EndCoh = c() #This vector will contain all rows with the real ending indices
    if(length(EndCoh_start) > 0 ){
      for(k in 1:length(EndCoh_start)) {
        if(k == 1) {
          if(EndCoh_start[k] > StartCoh[1]){
            EndCoh = c(EndCoh, EndCoh_start[k]) #is it after a starting row? 
          }} else {
            if((EndCoh_start[k] > StartCoh[1]) & (EndCoh_start[k] > (EndCoh_start[k - 1] + 1))){
              EndCoh = c(EndCoh, EndCoh_start[k])
            }
          }
      }
    }
    
    ###Get data###
    BirthTimes = InPutData$Time[StartCoh]
    #for loop for each cohort#
    i = 1
    if(length(EndCoh) > 0) { 
      for(i in 1:length(EndCoh)) {
        Name = paste("Day_", BirthTimes[i], sep = "")
        NewData <- data.frame(
          BirthDay = BirthTimes[i],
          Time = InPutData$Time[StartCoh[i] : (EndCoh[i] - 1)],
          Length = InPutData[c(StartCoh[i] : (EndCoh[i] - 1)), LengthIn[j]],
          Density = InPutData[c(StartCoh[i] : (EndCoh[i] - 1)), LengthIn[j] + 1],
          Tot_Buffer = InPutData[c(StartCoh[i] : (EndCoh[i] - 1)), LengthIn[j] + 2])
        if (Var == 4) {
          NewData$CurrentBuf = InPutData[c(StartCoh[i] : (EndCoh[i] - 1)), LengthIn[j] + 3]
        }
        NewData$Age <- NewData$Time - BirthTimes[i]
        NewData$Age_dietshift = subset(NewData, Length >= Size_shift)$Age[1]
        NewData$Age_mat = subset(NewData, Length >= Size_mat)$Age[1]
        #Calculate expected survival probability in the absence of starvation#
        Mort_part = subset(NewData, Length >= mort_smin & Length <= mort_smax)
        if(nrow(Mort_part) < 1) {
          index_mort_start = index_mort_end = 0
        } else {
          index_mort_start = as.numeric(rownames(Mort_part)[1])
          index_mort_end = as.numeric(rownames(Mort_part)[nrow(Mort_part)])
        }
        age_dif = NewData$Age[2] - NewData$Age[1]
        NewData$Expected_surv = 1
        if(nrow(NewData) > 1) {
          for(k in 2:nrow(NewData)) {
            if (k >=index_mort_start & k <= index_mort_end) {
              mu = sizemort + background_mort
            } else {
              mu = background_mort
            }
            NewData$Expected_surv[k] = exp(-age_dif * mu) * NewData$Expected_surv[k - 1]
          }
        }
        #End of calculation#
        NewData$Density_scaled = NewData$Density/NewData$Density[1]
        #R0 = probability to survive (=NewData$Density_scaled) * PER CAPITA repro (=NewData$Tot_Buffer/NewData$Density * egg surv * egg eff)
        NewData$R0 <- (1 / StartSize) * (eggeff) * (1 - eggmort) * NewData$Density_scaled * NewData$Tot_Buffer/NewData$Density #times survival
        PopList[[Name]] <- NewData
      }
    }
    if ((length(EndCoh) == 0) & (length(StartCoh) == 1)) { #If a cohort starts, but doesn't die before end of simulation
      Name = paste("Day_", BirthTimes[i], sep = "")
      #Get last one
      NewData <- data.frame(
        BirthDay = BirthTimes[i],
        Time = InPutData$Time[StartCoh[i]: nrow(InPutData)],
        Length = InPutData[c(StartCoh[i] : nrow(InPutData)), LengthIn[j]],
        Density = InPutData[c(StartCoh[i] : nrow(InPutData)), LengthIn[j] + 1],
        Tot_Buffer = InPutData[c(StartCoh[i] : nrow(InPutData)), LengthIn[j] + 2])
      if (Var == 4) {
        NewData$CurrentBuf = InPutData[c(StartCoh[i] : nrow(InPutData)), LengthIn[j] + 3]
      }
      NewData$Age <- NewData$Time - BirthTimes[i]
      NewData$Age_dietshift = subset(NewData, Length >= Size_shift)$Age[1]
      NewData$Age_mat = subset(NewData, Length >= Size_mat)$Age[1]
      #Calculate expected survival probability in the absence of starvation#
      Mort_part = subset(NewData, Length >= mort_smin & Length <= mort_smax)
      if(nrow(Mort_part) < 1) {
        index_mort_start = index_mort_end = 0
      } else {
        index_mort_start = as.numeric(rownames(Mort_part)[1])
        index_mort_end = as.numeric(rownames(Mort_part)[nrow(Mort_part)])
      }
      age_dif = NewData$Age[2] - NewData$Age[1]
      NewData$Expected_surv = 1
      if(nrow(NewData) > 1) {
        for(k in 2:nrow(NewData)) {
          if (k >=index_mort_start & k <= index_mort_end) {
            mu = sizemort + background_mort
          } else {
            mu = background_mort
          }
          NewData$Expected_surv[k] = exp(-age_dif * mu) * NewData$Expected_surv[k - 1]
        }
      }
      #End of calculation#
      NewData$Density_scaled = NewData$Density/NewData$Density[1]
      NewData$R0 <- (1 / StartSize) * (eggeff) * (1 - eggmort) * NewData$Density_scaled * NewData$Tot_Buffer/NewData$Density
      PopList[[Name]] <- NewData }
    if((length(StartCoh) > length(EndCoh)) & (length(StartCoh) > 1)) { #The last cohort in the set/
      i = i + 1
      Name = paste("Day_", BirthTimes[i], sep = "")
      #Get last one
      NewData <- data.frame(
        BirthDay = BirthTimes[i],
        Time = InPutData$Time[StartCoh[i]: nrow(InPutData)],
        Length = InPutData[c(StartCoh[i] : nrow(InPutData)), LengthIn[j]],
        Density = InPutData[c(StartCoh[i] : nrow(InPutData)), LengthIn[j] + 1],
        Tot_Buffer = InPutData[c(StartCoh[i] : nrow(InPutData)), LengthIn[j] + 2])
      if( Var == 4) {
        NewData$CurrentBuf = InPutData[c(StartCoh[i] : nrow(InPutData)), LengthIn[j] + 3]
      }
      NewData$Age <- NewData$Time - BirthTimes[i]
      NewData$Age_dietshift = subset(NewData, Length >= Size_shift)$Age[1]
      NewData$Age_mat = subset(NewData, Length >= Size_mat)$Age[1]
      #Calculate expected survival probability in the absence of starvation#
      Mort_part = subset(NewData, Length >= mort_smin & Length <= mort_smax)
      if(nrow(Mort_part) < 1) {
        index_mort_start = index_mort_end = 0
      } else {
        index_mort_start = as.numeric(rownames(Mort_part)[1])
        index_mort_end = as.numeric(rownames(Mort_part)[nrow(Mort_part)])
      }
      age_dif = NewData$Age[2] - NewData$Age[1]
      NewData$Expected_surv = 1
      if(nrow(NewData) > 1) {
        for(k in 2:nrow(NewData)) {
          if (k >=index_mort_start & k <= index_mort_end) {
            mu = sizemort + background_mort
          } else {
            mu = background_mort
          }
          NewData$Expected_surv[k] = exp(-age_dif * mu) * NewData$Expected_surv[k - 1]
        }
      }
      #End of calculation#
      NewData$Density_scaled = NewData$Density/NewData$Density[1]
      NewData$R0 <- (1 / StartSize) * (eggeff) * (1 - eggmort) * NewData$Density_scaled * NewData$Tot_Buffer/NewData$Density
      PopList[[Name]] <- NewData }
  }
  PopList <- PopList[order(as.integer(gsub("[^0-9]","",names(PopList))))]
  
  assign(OutPutData, PopList, envir = .GlobalEnv) ##save data globally
}


####Function to plot growth curves ####
PlotGrowth <- function(filename){
  ###Settings and load data####
  CohNr_spec = 20 #Nr of cohorts in output #CHANGE IN .H FILE!!!!
  SB = 0.351 #Size at birth
  Varnr = 3 #3 output variables per cohort
  minmortsize = 0.351 #Min size-specific mort
  maxmortsize = 100 #Max size-specific mort
  Missing_value = 1E308
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
  
  setwd('U:\\Manuscript\\Timeseries') #CHANGE!!!
  # filename <- "HighQ_HighR1max.out" #CHANGE!!!!
  GetData <- read.table(filename, header = F)
  GetData <- GiveNames(GetData, maxcohorts = CohNr_spec)
  
  indexStart = which(colnames(GetData) == 'LCoh_1') #Get pop data
  indexEnd = indexStart + CohNr_spec * Varnr - 1
  
  PopData <-  GetData[, c(1, indexStart:indexEnd)]
  TimeData <- GetData[, -c(indexStart:indexEnd)]

  GetPopData(InPutData = PopData, OutPutData = "PopList",
             CohNr = CohNr_spec,
             InitColNr = which(colnames(PopData) == 'LCoh_1'),
             Var = Varnr, StartSize = SB,
             NoData = 1E10,
             mort_smin = minmortsize,
             mort_smax = maxmortsize,
             sizemort = 0)
  ######Calculate average growth rate per size-class#####
  TotPops = 17 #I want the data for 17 populations (THIS IS THE PERIOD!) #YOU MIGHT NEED TO CHANGE!!!
  AllPopData = data.frame()
  ###Get all growth rate data from the pop
  for(j in 1:TotPops) {
    Pop = PopList[[j]]
    #calculate mortality rate
    for(k in 1:(nrow(Pop) - 1)) {
      Pop$MortRate[k] = log(Pop$Density[k]/Pop$Density[k + 1])
    }
    Pop$MortRate[nrow(Pop)] = Pop$MortRate[nrow(Pop) - 1]

    #Calculate density at diet shift#
    if(is.na(Pop$Age_dietshift[1])) {
      Pop$Density_diet = NA
    } else {
      Pop$Density_diet = Pop[Pop$Age == Pop$Age_dietshift,]$Density
    }

    #Get the index at maturation#
    if(is.na(Pop$Age_mat[1])) {
      matindex = nrow(Pop)
    } else {
      matindex = which.min(abs(Pop$Age - Pop$Age_mat))
    }
    Pop$GrowthRate = NA
    #Get growth rate before maturation (no growth after maturation!)
    for(k in 1:(matindex - 1)) {
      Pop$GrowthRate[k] = log(Pop$Length[k+1]/Pop$Length[k])
    }
    Pop$GrowthRate[matindex] = Pop$GrowthRate[matindex - 1]
    ##if cohortdata available before diet shift only:
    if(is.na(Pop$Age_dietshift[1])) {
      Pop$MeanLMort = mean(Pop$MortRate)
      Pop$MeanJMort = NA
      Pop$MeanLGrowth = mean(Pop$GrowthRate, na.rm = T)
      Pop$MeanJGrowth = NA
    } else if(is.na(Pop$Age_mat[1])) { #If cohortdata is available after diet shift but before maturation
      Pop$MeanLMort = mean(subset(Pop, Age < Age_dietshift)$MortRate)
      Pop$MeanJMort = mean(subset(Pop, Age >= Age_dietshift)$MortRate)
      Pop$MeanLGrowth = mean(subset(Pop, Age < Age_dietshift)$GrowthRate, na.rm = T)
      Pop$MeanJGrowth = mean(subset(Pop, Age >= Age_dietshift)$GrowthRate, na.rm = T)
    } else {
      Pop$MeanLMort = mean(subset(Pop, Age < Age_dietshift)$MortRate)
      Pop$MeanJMort = mean(subset(Pop, Age >= Age_dietshift)$MortRate)
      Pop$MeanLGrowth = mean(subset(Pop, Age < Age_dietshift)$GrowthRate, na.rm = T)
      Pop$MeanJGrowth = mean(subset(Pop, Age >= Age_dietshift & Age <= Age_mat)$GrowthRate, na.rm = T)
    }

    AllPopData = rbind(AllPopData, Pop)
  }

  ####Get curves of interest#####
  UniquePop = subset(AllPopData, Age == 0) ##Get one entry per pop
  UniquePop$MeanLGrowth_scaled = UniquePop$MeanLGrowth * UniquePop$Density #Scale growth with density#
  UniquePop$MeanJGrowth_scaled = UniquePop$MeanJGrowth * UniquePop$Density_diet #Scale growth with density#

  MeanGrowthL_index = UniquePop  %>%
    summarize(MeanGrowth_scaled2 = sum(MeanLGrowth_scaled) / sum(Density),
              IndexAVG = which.min(abs(MeanLGrowth - MeanGrowth_scaled2)),
              AVGGrowth = MeanLGrowth[IndexAVG],
              Birthday_AVG = BirthDay[IndexAVG],
              IndexMin = which.min(MeanLGrowth),
              MinGrowth = MeanLGrowth[IndexMin],
              Birthday_min = BirthDay[IndexMin],
              IndexMax = which.max(MeanLGrowth),
              MaxGrowth = MeanLGrowth[IndexMax],
              Birthday_max = BirthDay[IndexMax])
  MeanGrowthL_index


  MeanGrowthJ_index = UniquePop  %>%
    summarize(MeanGrowth_scaled2 = sum(MeanJGrowth_scaled) / sum(Density_diet),
              IndexAVG = which.min(abs(MeanJGrowth - MeanGrowth_scaled2)),
              AVGGrowth = MeanJGrowth[IndexAVG],
              Birthday_AVG = BirthDay[IndexAVG],
              IndexMin = which.min(MeanLGrowth),
              MinGrowth = MeanJGrowth[IndexMin],
              Birthday_min = BirthDay[IndexMin],
              IndexMax = which.max(MeanLGrowth),
              MaxGrowth = MeanJGrowth[IndexMax],
              Birthday_max = BirthDay[IndexMax])
  MeanGrowthJ_index

  ###Plot####

  subset_AllPopDataL_minmax <- AllPopData %>%
    filter(BirthDay == MeanGrowthL_index$Birthday_max | BirthDay == MeanGrowthL_index$Birthday_min)

  subset_AllPopDataL <- AllPopData %>%
    filter(BirthDay == MeanGrowthL_index$Birthday_AVG)

  GrowthCurves_Juv = ggplot(data = subset(subset_AllPopDataL,
                                          Length < (100-1E-9)),
                            aes(x = (Age) / 250, y = Length,
                                group = BirthDay)) +
    geom_path(alpha = .5, size = 2) +
    geom_path(data = subset(subset_AllPopDataL_minmax,
                            Length < (100-1E-9)), alpha = 0.3, size = 1) +
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

  GrowthCurves_Juv
}

# Plot growth curves for different par values in Result 1
HighR1max_growth <- PlotGrowth("HighQ_HighR1max.out")
HighR1max_growth
InterR1max_growth <- PlotGrowth("HighQ_InterR1max.out")
InterR1max_growth
# LowR1max_growth <- PlotGrowth("HighQ_LowR1max.out") # This one is not very relevant since the juveniles are extinct at that point

ggarrange(HighR1max_growth + theme(aspect.ratio = 0.5), 
          InterR1max_growth + theme(aspect.ratio = 0.5), 
          nrow = 2, ncol = 1, labels = c("A", "B"))

# Plot growth curves for different par values in Result 2
NoMortFocal_growth <- PlotGrowth("HighQ_noMort.out")
InterMortFocal_growth <- PlotGrowth("HighQ_InterMort.out")

ggarrange(NoMortFocal_growth + theme(aspect.ratio = 0.5), 
          InterMortFocal_growth + theme(aspect.ratio = 0.5), 
          nrow = 2, ncol = 1, labels = c("A", "B"))
