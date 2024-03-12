# library(ggplot2)
# library(ggpubr)
# 
# # Working directory for different R1 values
# setwd('C:\\Users\\xmste\\OneDrive\\Documents\\Systems Biology\\Internship\\Models\\MarJuv_R1')


#----------------------------------------------------------------------------------------#
# Function for adding adult bm with buffer and total consumer bm to data, change colnames
#----------------------------------------------------------------------------------------#
process_data <- function(bif_data, flip_bif_par){
  bif_names = c('Time', 'R1', 'R2', 'Larv_nr', 'Larv_bio', 'Juv_nr', 'Juv_bio', 
                'Adu_nr', 'Adu_bio', 'Vuln_nr', 'Vuln_bm', 'Buf_bm', 'LarvPeriod',
                'LarvSurv', 'MatLRate', 'JuvPeriod', 'JuvSurv', 'MatJRate', 'birthrate',
                'newborns', 'MeanFecundity', 'YOYsurvival', 'SizeOYO', 'nu_L', 'nu_J',
                'nu_A', 'mort_L', 'mort_J', 'mort_A', 'cohort_no', 'Frac_L', 'Frac_J',
                'LarvStarv', 'Est_nr', 'MatLRate_Est', 'WS_nr', 'WS_bm', 'WS_event_nr',
                'Est_event_nr', 'bif_par', 'period', 'num_cohorts')
  
  colnames(bif_data) = bif_names
  
  # Add new column with buffer bm added to adult bm
  bif_data$Adu_bio_tot = bif_data$Adu_bio + bif_data$Buf_bm
  
  # Add column with consumer bm, also including reproductive buffer and Juveniles
  bif_data$Bio_tot_larv_buf = bif_data$Adu_bio + bif_data$Juv_bio + 
    bif_data$Larv_bio + bif_data$Buf_bm
  
  # Add column with consumer bm excluding reproductive buffer
  bif_data$Bio_tot_larv = bif_data$Adu_bio + bif_data$Juv_bio + 
    bif_data$Larv_bio
  
  # Add column with consumer bm excluding buffer and larval biomass
  bif_data$Bio_tot = bif_data$Adu_bio + bif_data$Juv_bio
  
  # Add column with consumer bm excluding Juveniles but including reproductive buffer
  bif_data$Bio_tot_JA = bif_data$Adu_bio + bif_data$Juv_bio + bif_data$Buf_bm
  
  # Add column with total number of Juveniles maturating
  bif_data$MatLRate_tot = bif_data$MatLRate + bif_data$MatLRate_Est
  
  # Fix values for maturation period. Only for avg files!!!
  # TODO: fix this for files not avg (so if/else with extra input argument). Or extra columns?
  # TODO: correct to divide by mean of fracs?
  bif_data$LarvPeriod = bif_data$LarvPeriod / bif_data$Frac_L
  bif_data$JuvPeriod = bif_data$JuvPeriod / bif_data$Frac_J
  
  # # Add column with number in NS from other ests
  # bif_data$Est_nr_NS = (bif_data$Adu_nr + bif_data$Juv_nr) - bif_data$WS_nr
  # 
  # # Add column with bm in NS from other ests
  # bif_data$Est_bm_NS = (bif_data$Adu_bio + bif_data$Juv_bio) - bif_data$WS_bm
  
  # Flip the bifurcation parameter around (so 1-bif_par) if that was given in input
  if (flip_bif_par){
    bif_data$bif_par = 1 - bif_data$bif_par
  }
  
  bif_data
}


#-----------------------------------------------------------------------------#
# Function for plotting bifurcations for adults, Subadults, Juveniles, total
#-----------------------------------------------------------------------------#
plot_bif <- function(bif_data, xlabel){
  # Input data should be processed
  
  # Plot adults including buffer biomass
  Adu_plot <- ggplot(data=bif_data, aes(bif_par, y=Adu_bio_tot)) +
    geom_point() + labs(y= "Adult biomass (g/L)", x = xlabel) +
    theme(axis.text.x  = element_blank(), axis.title.x = element_blank(),
          axis.ticks.x = element_blank())
  
  # Plot Subadults
  Juv_plot <- ggplot(data=bif_data, aes(bif_par, y=Juv_bio)) + 
    geom_point() + labs(y= "Juvenile biomass (g/L)", x = xlabel) +
    theme(axis.text.x  = element_blank(), axis.title.x = element_blank(),
          axis.ticks.x = element_blank())
  
  # Plot Juveniles
  Larv_plot <- ggplot(data=bif_data, aes(bif_par, y=Larv_bio)) + 
    geom_point() + labs(y= "Larval biomass (g/L)", x = xlabel)
  
  # Plot bifurcation of total consumer biomass
  Con_plot <- ggplot(data=bif_data, aes(bif_par, y=Bio_tot_larv_buf)) + 
    geom_point() + labs(y= "Consumer biomass (g/L)", x = xlabel)
  
  # Make subplots with ggarrange
  plot_tot <- ggarrange(Adu_plot, Juv_plot, Larv_plot, Con_plot, 
                        labels = c("A", "B", "C", "D"),
                        ncol = 2, nrow = 2)
  
  plot_tot
}


#-----------------------------------------------------------------------------#
# Function for plotting larval and juvenile periods separately
#-----------------------------------------------------------------------------#
plot_period_sep <- function(bif_data, xlabel){
  periodLplot <- ggplot(data=bif_data, aes(bif_par, y=LarvPeriod)) + geom_point() +
    labs(y= "Larval period (days)", x = xlabel)
  
  periodJplot <- ggplot(data=bif_data, aes(bif_par, y=JuvPeriod)) + geom_point() +
    labs(y= "Juvenile period (days)", x = xlabel)
  
  plot_tot <- ggarrange(periodLplot, periodJplot,
                        labels = c("A", "B"),
                        ncol = 2, nrow = 1)
}


#-----------------------------------------------------------------------------#
# Function for plotting larval and juvenile periods separately
#-----------------------------------------------------------------------------#
plot_prod_sep <- function(bif_data, xlabel){
  prodLplot <- ggplot(data=bif_data, aes(bif_par, y=nu_L)) + geom_point() +
    labs(y= "Biomass production Juveniles (g/L)", x = xlabel)
  
  prodJplot <- ggplot(data=bif_data, aes(bif_par, y=nu_J)) + geom_point() +
    labs(y= "Biomass production subadults/adults (g/L)", x = xlabel)
  
  plot_tot <- ggarrange(prodLplot, prodJplot,
                        labels = c("A", "B"),
                        ncol = 2, nrow = 1)
}


#-----------------------------------------------------------------------------#
# Function for plotting larval and juvenile periods separately
#-----------------------------------------------------------------------------#
plot_R_sep <- function(bif_data, xlabel){
  R1_plot <- ggplot(data=bif_data, aes(bif_par, y=R1)) + geom_point() +
    labs(y= "R1 density (g/L)", x = xlabel)
  
  R2_plot <- ggplot(data=bif_data, aes(bif_par, y=R2)) + geom_point() +
    labs(y= "R2 density (g/L)", x = xlabel)
  
  plot_tot <- ggarrange(R1_plot, R2_plot,
                        labels = c("A", "B"),
                        ncol = 2, nrow = 1)
}


#-----------------------------------------------------------------------------#
# Function for plotting maturation rates separately
#-----------------------------------------------------------------------------#
plot_matrate_sep <- function(bif_data, xlabel){
  mat_larv_plot <- ggplot(data=bif_data, aes(bif_par, y=MatLRate)) + geom_point() +
    labs(y= "Maturation rate Juveniles", x = xlabel)
  
  mat_juv_plot <- ggplot(data=bif_data, aes(bif_par, y=MatJRate)) + geom_point() +
    labs(y= "Maturation rate Subadults", x = xlabel)
  
  mat_larv_E_plot <- ggplot(data=bif_data, aes(bif_par, y=MatLRate_Est)) + geom_point() +
    labs(y= "Maturation rate Juveniles E", x = xlabel) 
  
  plot_tot <- ggarrange(mat_larv_plot, mat_juv_plot, mat_larv_E_plot,
                        labels = c("A", "B"),
                        ncol = 3, nrow = 1)
}


#-----------------------------------------------------------------------------#
# Function for plotting all stuffs
#-----------------------------------------------------------------------------#
plot_all <- function(bif_data, xlabel){
  matRate_plot <- ggplot(data=bif_data, aes(bif_par, y=MatLRate*1000, color='Juvenile->Subadult')) + 
    geom_point(aes(y=MatJRate*1000, color='Subadult->Adult')) + 
    geom_point(aes(y=MatLRate_Est*1000, color='Juvenile->Subadult other estuaries')) + 
    geom_point() + labs(y = 'Maturation rate (mg/L/day)', x = xlabel) + 
    theme(legend.position="bottom")
    #theme(axis.text=element_text(size=20), axis.title=element_text(size=22)) +
    #theme(legend.text = element_text(size=22)) + scale_color_discrete(name=NULL)
  
  # Try to plot birth rate
  birthrate_plot <- ggplot(data=bif_data, aes(bif_par, y=birthrate)) + 
    geom_point() + labs(y = "Birth rate", x = xlabel) + theme(legend.position="bottom") 
    # + theme(axis.text=element_text(size=20), axis.title=element_text(size=22)) +
    # theme(legend.text = element_text(size=22))
  
  # Plot all size classes in 1 plot
  sizeclass_plot <- ggplot(data=bif_data, aes(bif_par, y=Larv_bio*1000, color='Juveniles')) +
    geom_point(aes(y=Juv_bio*1000, color='Subadults')) + geom_point(aes(y=Adu_bio*1000, color='Adults')) +
    geom_point(aes(y=Bio_tot*1000, color='Adults + Subadults')) + 
    geom_point() + labs(y = expression(Biomass ~ (mg ~ L^-1)), x = xlabel) + #theme(legend.position="bottom") + 
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black")) +
    # theme(legend.position = c(0.9, 0.9)) +
    theme(legend.key=element_blank()) + 
    scale_color_manual(breaks = c('Juveniles', 'Subadults', 'Adults', 'Adults + Subadults'),
                       values = c("Juveniles" = "#4B0055", "Subadults" = "#0499BE", 
                                  "Adults" = "#36E9A7", "Adults + Subadults" = "#FDE333"), 
                       name=NULL) + 
    guides(color = guide_legend(override.aes = list(size = 3))) +
    theme(legend.text = element_text(size=15)) + 
    theme(axis.text=element_text(size=12),
          axis.title=element_text(size=14),
          legend.text=element_text(size=14))
  
  sizeOYO_plot <- ggplot(data=bif_data, aes(bif_par, y=SizeOYO)) + 
    geom_point() + labs(y = "Size one year olds", x = xlabel) + theme(legend.position="bottom") 
    # + theme(axis.text=element_text(size=20), axis.title=element_text(size=22)) +
    # theme(legend.text = element_text(size=22)) 
  
  period_plot <- ggplot(data=bif_data, aes(bif_par, y=LarvPeriod, color='Larval period')) + 
    geom_point(aes(y=JuvPeriod-LarvPeriod, color='Juvenile period')) +
    geom_point() + labs(y = "Time (days)", x = xlabel) + theme(legend.position="bottom") +
    scale_color_discrete(name=NULL, breaks=c('Larval period', 'Juvenile period'))
    # + theme(axis.text=element_text(size=20), axis.title=element_text(size=22)) +
    # theme(legend.text = element_text(size=22)) 
  
  con_plot <- ggplot(data=bif_data, aes(bif_par, y=Bio_tot_larv_buf)) + 
    geom_point() + labs(y = "Consumer biomass (g/L)", x = xlabel) + theme(legend.position="bottom") 
    # + theme(axis.text=element_text(size=20), axis.title=element_text(size=22)) +
    # theme(legend.text = element_text(size=22))
  
  bm_prod_plot <- ggplot(data=bif_data, aes(bif_par, y=nu_L, color='Juveniles')) + 
    geom_point(aes(y=nu_J, color='Subadults/Adults')) +
    geom_point() + labs(y = "Biomass production (g/g/L)", x = xlabel) + theme(legend.position="bottom") 
    # + theme(axis.text=element_text(size=20), axis.title=element_text(size=22)) +
    # theme(legend.text = element_text(size=22)) 
    # + scale_color_discrete(name=NULL, breaks=c('Juveniles', 'Subadults/Adults'))
  
  res_plot <- ggplot(data=bif_data, aes(bif_par, y=R1, color='R1')) + 
    geom_point(aes(y=R2, color='R2')) + geom_point() + 
    labs(y='Density (g/L)', x=xlabel) + theme(legend.position="bottom") + 
    # + theme(axis.text=element_text(size=20), axis.title=element_text(size=22)) +
    # theme(legend.text = element_text(size=22)) + scale_color_discrete(name=NULL) + 
    scale_color_manual(values = c("R1" = "#00BFC4", "R2" = "#F8766D"), name=NULL)
  
  larvsurv_plot <- ggplot(data=bif_data, aes(bif_par, y=LarvSurv)) + 
    geom_point() + labs(y = "Larval survival", x = xlabel) + theme(legend.position="bottom") 
  # + theme(axis.text=element_text(size=20), axis.title=element_text(size=22)) +
  # theme(legend.text = element_text(size=22))
  
  plot_tot <- ggarrange(matRate_plot, birthrate_plot, sizeclass_plot, 
                        sizeOYO_plot, period_plot, con_plot, bm_prod_plot, res_plot,
                        ncol = 2, nrow = 4)
  
  # Plots of biomasses per sizeclass
  sizeclass_sep <- plot_bif(bif_data, xlabel)
  period_sep <- plot_period_sep(bif_data, xlabel)
  bm_prod_sep <- plot_prod_sep(bif_data, xlabel)
  R_sep <- plot_R_sep(bif_data, xlabel)
  matrate_sep <- plot_matrate_sep(bif_data, xlabel)
  
  Est_nr_plot <- ggplot(data=bif_data, aes(bif_par, y=Est_nr)) + geom_point() + 
    labs(y='Number in estuaries', x=xlabel)
  
  out <- list(matRate_plot, birthrate_plot, sizeclass_plot, sizeOYO_plot, period_plot, 
              con_plot, bm_prod_plot, res_plot, larvsurv_plot, plot_tot, sizeclass_sep, 
              period_sep, bm_prod_sep, R_sep, Est_nr_plot, matrate_sep)
  return(out)
  
}


#-----------------------------------------------------------------------------#
# Function for processing data for time series
#-----------------------------------------------------------------------------#
process_data_TS <- function(TS_data){
  
  # Define column names for time series
  TS_names = c('Time', 'R1', 'R2', 'Larv_nr', 'Larv_bio', 'Juv_nr', 'Juv_bio', 
               'Adu_nr', 'Adu_bio', 'Vuln_nr', 'Vuln_bm', 'Buf_bm', 'LarvPeriod',
               'LarvSurv', 'MatLRate', 'JuvPeriod', 'JuvSurv', 'MatJRate', 'birthrate',
               'newborns', 'MeanFecundity', 'YOYsurvival', 'SizeOYO', 'nu_L', 'nu_J',
               'nu_A', 'mort_L', 'mort_J', 'mort_A', 'cohort_no', 'Frac_L', 'Frac_J',
               'LarvStarv', 'Est_nr', 'MatLRate_Est', 'WS_nr', 'WS_bm', 'WS_event_nr',
               'Est_event_nr')
  
  # Also define last 30 names of output, which are dependent on number of cohorts
  n_cohorts = 10
  for(i in 1:n_cohorts){
    TS_names = c(TS_names, sprintf('length_%s', i), sprintf('density_%s', i),
                 sprintf('buffer_%s', i))
  }
  
  # Apply column names to data
  colnames(TS_data) = TS_names
  
  # Add new column with buffer bm added to adult bm
  TS_data$Adu_bio_tot = TS_data$Adu_bio + TS_data$Buf_bm
  
  # Add column with consumer bm, also including reproductive buffer and Juveniles
  TS_data$Bio_tot_larv_buf = TS_data$Adu_bio + TS_data$Juv_bio + 
    TS_data$Larv_bio + TS_data$Buf_bm
  
  # Add column with consumer bm excluding reproductive buffer
  TS_data$Bio_tot_larv = TS_data$Adu_bio + TS_data$Juv_bio + 
    TS_data$Larv_bio
  
  # Add column with consumer bm excluding buffer and larval biomass
  TS_data$Bio_tot = TS_data$Adu_bio + TS_data$Juv_bio
  
  # Add column with time in years
  TS_data$Time_y = TS_data$Time / 250
  
  # Add column with number in NS from other ests
  TS_data$Est_nr_NS = (TS_data$Adu_nr + TS_data$Juv_nr) - TS_data$WS_nr
  
  # Add column with bm in NS from other ests
  TS_data$Est_bm_NS = (TS_data$Adu_bio + TS_data$Juv_bio) - TS_data$WS_bm
  
  TS_data
}


#-----------------------------------------------------------------------------#
# Function for plotting birth rates of two datasets next to each other
#-----------------------------------------------------------------------------#
plot_birth_comp <- function(bif_data, bif_data2, xlabel){
  max_y = max(rbind(bif_data$birthrate, bif_data2$birthrate))
  min_y = min(rbind(bif_data$birthrate, bif_data2$birthrate))
  
  max_y = max_y + 0.01*max_y
  min_y = min_y - 0.01*min_y
  
  plot1 = ggplot(data=bif_data, aes(bif_par, y=birthrate)) +
    geom_point() + labs(y = "Birth rate", x = xlabel) + ylim(min_y, max_y)
  
  plot2 = ggplot(data=bif_data2, aes(bif_par, y=birthrate)) +
    geom_point() + labs(y = "Birth rate", x = xlabel) + ylim(min_y, max_y)
  
  plot_tot <- ggarrange(plot1, plot2, 
                        ncol = 2, nrow = 1)
  
  plot_tot
}


#-----------------------------------------------------------------------------#
# Function for plotting mat rates of two datasets next to each other
#-----------------------------------------------------------------------------#
plot_mat_comp <- function(bif_data, bif_data2, xlabel){
  max_mat = max(rbind(bif_data$MatLRate, bif_data$MatJRate, bif_data$MatLRate_Est, bif_data$MatLRate_tot,
                      bif_data2$MatLRate, bif_data2$MatJRate, bif_data2$MatLRate_Est, bif_data2$MatLRate_tot))
  
  min_mat = min(rbind(bif_data$MatLRate, bif_data$MatJRate, bif_data$MatLRate_Est, bif_data$MatLRate_tot, 
                      bif_data2$MatLRate, bif_data2$MatJRate, bif_data2$MatLRate_Est, bif_data2$MatLRate_tot))
  
  max_mat = max_mat + 0.01*max_mat
  min_mat = min_mat - 0.01*min_mat
  
  plot1 = ggplot(data=bif_data, aes(bif_par, y=MatLRate, color='L->J')) + 
    geom_point(aes(y=MatJRate, color='J->A')) + geom_point(aes(y=MatLRate_Est, color='L->J E')) + 
    geom_point(aes(y=MatLRate_tot, color='L->J total')) +
    geom_point() + labs(y = "Maturation rate", x = xlabel) + ylim(min_mat, max_mat)
  
  plot2 = ggplot(data=bif_data2, aes(bif_par, y=MatLRate, color='L->J')) + 
    geom_point(aes(y=MatJRate, color='J->A')) + geom_point(aes(y=MatLRate_Est, color='L->J E')) + 
    geom_point(aes(y=MatLRate_tot, color='L->J total')) +
    geom_point() + labs(y = "Maturation rate", x = xlabel) + ylim(min_mat, max_mat)
  
  
  plot_tot <- ggarrange(plot1, plot2, common.legend = TRUE, legend="bottom",
                        ncol = 2, nrow = 1)
  
  plot_tot
}


#----------------------------------------------------------------------------------#
# Function for plotting biomasses per sizeclass of two datasets next to each other
#----------------------------------------------------------------------------------#
plot_bm_comp <- function(bif_data, bif_data2, xlabel){
  # larv_max = max(rbind(bif_data$Larv_bio, bif_data2$Larv_bio))
  # larv_max = larv_max + 0.01*larv_max
  # 
  # larv_min = min(rbind(bif_data$Larv_bio, bif_data2$Larv_bio))
  # larv_min = larv_min - 0.01*larv_min
  # 
  # juv_max = max(rbind(bif_data$Juv_bio, bif_data2$Juv_bio))
  # juv_max = juv_max + 0.01*juv_max
  # 
  # juv_min = min(rbind(bif_data$Juv_bio, bif_data2$Juv_bio))
  # juv_min = juv_min - 0.01*juv_min
  # 
  # adu_max = max(rbind(bif_data$Adu_bio_tot, bif_data2$Adu_bio_tot))
  # adu_max = adu_max + 0.01*adu_max
  # 
  # adu_min = min(rbind(bif_data$Adu_bio_tot, bif_data2$Adu_bio_tot))
  # adu_min = adu_min - 0.01*adu_min
  # 
  # Adu_plot1 <- ggplot(data=bif_data, aes(bif_par, y=Adu_bio_tot)) +
  #   geom_point() + labs(y= "Adult biomass (g/L)", x = xlabel) + ylim(adu_min, adu_max)
  # 
  # Adu_plot2 <- ggplot(data=bif_data2, aes(bif_par, y=Adu_bio_tot)) +
  #   geom_point() + labs(y= "Adult biomass (g/L)", x = xlabel) + ylim(adu_min, adu_max)
  # 
  # # Plot Subadults
  # Juv_plot1 <- ggplot(data=bif_data, aes(bif_par, y=Juv_bio)) + 
  #   geom_point() + labs(y= "Juvenile biomass (g/L)", x = xlabel) + ylim(juv_min, juv_max)
  # 
  # Juv_plot2 <- ggplot(data=bif_data2, aes(bif_par, y=Juv_bio)) + 
  #   geom_point() + labs(y= "Juvenile biomass (g/L)", x = xlabel) + ylim(juv_min, juv_max)
  # 
  # # Plot Juveniles
  # Larv_plot1 <- ggplot(data=bif_data, aes(bif_par, y=Larv_bio)) + 
  #   geom_point() + labs(y= "Larval biomass (g/L)", x = xlabel) + ylim(larv_min, larv_max)
  # 
  # Larv_plot2 <- ggplot(data=bif_data2, aes(bif_par, y=Larv_bio)) + 
  #   geom_point() + labs(y= "Larval biomass (g/L)", x = xlabel) + ylim(larv_min, larv_max)
  # 
  # plot_tot = ggarrange(Larv_plot1, Larv_plot2, Juv_plot1, Juv_plot2, Adu_plot1, Adu_plot2,
  #                      ncol = 2, nrow = 3)
  # 
  # plot_tot
  
  max_y = max(rbind(bif_data$Larv_bio, bif_data$Juv_bio, bif_data$Adu_bio, bif_data$Bio_tot_JA, 
                    bif_data2$Larv_bio, bif_data2$Juv_bio, bif_data2$Adu_bio, bif_data2$Bio_tot_JA))
  
  min_y = min(rbind(bif_data$Larv_bio, bif_data$Juv_bio, bif_data$Adu_bio, bif_data$Bio_tot_JA,
                    bif_data2$Larv_bio, bif_data2$Juv_bio, bif_data2$Adu_bio, bif_data2$Bio_tot_JA))
  
  max_y = max_y + 0.01*max_y
  min_y = min_y - 0.01*min_y
  
  plot1 = ggplot(data=bif_data, aes(bif_par, y=Larv_bio, color='Juveniles')) + 
    geom_point(aes(y=Juv_bio, color='Subadults')) + geom_point(aes(y=Adu_bio, color='Adults')) + 
    geom_point(aes(y=Bio_tot_JA, color='Adults + Subadults')) + geom_point() + 
    labs(y = "Biomass (g/L)", x = xlabel) + ylim(min_y, max_y)
  
  plot2 = ggplot(data=bif_data2, aes(bif_par, y=Larv_bio, color='Juveniles')) + 
    geom_point(aes(y=Juv_bio, color='Subadults')) + geom_point(aes(y=Adu_bio, color='Adults')) + 
    geom_point(aes(y=Bio_tot_JA, color='Adults + Subadults')) + geom_point() + 
    labs(y = "Biomass (g/L)", x = xlabel) + ylim(min_y, max_y)
  
  
  plot_tot <- ggarrange(plot1, plot2, common.legend = TRUE, legend="bottom",
                        ncol = 2, nrow = 1)
  
  plot_tot
}


#----------------------------------------------------------------------------------#
# Function for plotting periods of two datasets next to each other
#----------------------------------------------------------------------------------#
plot_period_comp <- function(bif_data, bif_data2, xlabel){
  # max_larv = max(rbind(bif_data$LarvPeriod, bif_data2$LarvPeriod))
  # max_larv = max_larv + 0.01*max_larv
  # 
  # min_larv = min(rbind(bif_data$LarvPeriod, bif_data2$LarvPeriod))
  # min_larv = min_larv - 0.01*min_larv
  # 
  # max_juv = max(rbind(bif_data$JuvPeriod-bif_data$LarvPeriod, 
  #                     bif_data2$JuvPeriod-bif_data2$LarvPeriod))
  # max_juv = max_juv + 0.01*max_juv
  # 
  # min_juv = min(rbind(bif_data$JuvPeriod-bif_data$LarvPeriod, 
  #                      bif_data2$JuvPeriod-bif_data2$LarvPeriod))
  # min_juv = min_juv - 0.01*min_juv
  # 
  # larv_plot1 <- ggplot(data=bif_data, aes(bif_par, y=LarvPeriod)) + 
  #   geom_point() + labs(y = "Time (days)", x = xlabel) + ylim(min_larv, max_larv)
  # 
  # larv_plot2 <- ggplot(data=bif_data2, aes(bif_par, y=LarvPeriod)) + 
  #   geom_point() + labs(y = "Time (days)", x = xlabel) + ylim(min_larv, max_larv)
  # 
  # juv_plot1 <- ggplot(data=bif_data, aes(bif_par, y=JuvPeriod-LarvPeriod)) + 
  #   geom_point() + labs(y = "Time (days)", x = xlabel) + ylim(min_juv, max_juv)
  # 
  # juv_plot2 <- ggplot(data=bif_data2, aes(bif_par, y=JuvPeriod-LarvPeriod)) + 
  #   geom_point() + labs(y = "Time (days)", x = xlabel) + ylim(min_juv, max_juv)
  # 
  # plot_tot = ggarrange(larv_plot1, larv_plot2, juv_plot1, juv_plot2,
  #                      ncol = 2, nrow = 2)
  # 
  # plot_tot
  
  max_y = max(rbind(bif_data$LarvPeriod, bif_data$JuvPeriod-bif_data$LarvPeriod,
                    bif_data2$LarvPeriod, bif_data2$JuvPeriod-bif_data2$LarvPeriod))
  
  min_y = min(rbind(bif_data$LarvPeriod, bif_data$JuvPeriod-bif_data$LarvPeriod,
                    bif_data2$LarvPeriod, bif_data2$JuvPeriod-bif_data2$LarvPeriod))
  
  max_y = max_y + 0.01*max_y
  min_y = min_y - abs(0.01*min_y)
  
  plot1 = ggplot(data=bif_data, aes(bif_par, y=LarvPeriod, color='Larval period')) + 
    geom_point(aes(y=JuvPeriod-LarvPeriod, color='Juvenile period')) + 
    geom_point() + labs(y = "Time (days)", x = xlabel) + ylim(min_y, max_y)
  
  plot2 = ggplot(data=bif_data2, aes(bif_par, y=LarvPeriod, color='Larval period')) + 
    geom_point(aes(y=JuvPeriod-LarvPeriod, color='Juvenile period')) + 
    geom_point() + labs(y = "Time (days)", x = xlabel) + ylim(min_y, max_y)
  
  
  plot_tot <- ggarrange(plot1, plot2, common.legend = TRUE, legend="bottom",
                        ncol = 2, nrow = 1)
  
  plot_tot

}


#----------------------------------------------------------------------------------#
# Function for plotting biomass productions of two datasets next to each other
#----------------------------------------------------------------------------------#
plot_bmprod_comp <- function(bif_data, bif_data2, xlabel){
  
  max_y = max(rbind(bif_data$nu_L, bif_data$nu_J,
                    bif_data2$nu_L, bif_data2$nu_J))
  
  min_y = min(rbind(bif_data$nu_L, bif_data$nu_J,
                    bif_data2$nu_L, bif_data2$nu_J))
  
  max_y = max_y + 0.01*max_y
  min_y = min_y - 0.01*min_y
  
  plot1 = ggplot(data=bif_data, aes(bif_par, y=nu_L, color='Juveniles')) + 
    geom_point(aes(y=nu_J, color='Subadults')) + 
    geom_point() + labs(y = "Biomass production (g/L)", x = xlabel) + ylim(min_y, max_y)
  
  plot2 = ggplot(data=bif_data2, aes(bif_par, y=nu_L, color='Juveniles')) + 
    geom_point(aes(y=nu_J, color='Subadults')) + 
    geom_point() + labs(y = "Biomass production (g/L)", x = xlabel) + ylim(min_y, max_y)
  
  plot_tot <- ggarrange(plot1, plot2, common.legend = TRUE, legend="bottom",
                        ncol = 2, nrow = 1)
  
  plot_tot
  
}


#----------------------------------------------------------------------------------#
# Function for plotting biomass productions of two datasets next to each other
#----------------------------------------------------------------------------------#
plot_R_comp <- function(bif_data, bif_data2, xlabel){
  
  max_y = max(rbind(bif_data$R1, bif_data$R2,
                    bif_data2$R1, bif_data2$R1))
  
  min_y = min(rbind(bif_data$R1, bif_data$R2,
                    bif_data2$R1, bif_data2$R2))
  
  max_y = max_y + 0.01*max_y
  min_y = min_y - 0.01*min_y
  
  plot1 = ggplot(data=bif_data, aes(bif_par, y=R1, color='R1')) + 
    geom_point(aes(y=R2, color='R2')) + 
    geom_point() + labs(y = "Resource density (g/L)", x = xlabel) + ylim(min_y, max_y)
  
  plot2 = ggplot(data=bif_data2, aes(bif_par, y=R1, color='R1')) + 
    geom_point(aes(y=R2, color='R2')) + 
    geom_point() + labs(y = "Resource density (g/L)", x = xlabel) + ylim(min_y, max_y)
  
  plot_tot <- ggarrange(plot1, plot2, common.legend = TRUE, legend="bottom",
                        ncol = 2, nrow = 1)
  
  plot_tot
  
}


#-----------------------------------------------------------------------------#
# Function for plotting size OYOs of two datasets next to each other
#-----------------------------------------------------------------------------#
plot_OYO_comp <- function(bif_data, bif_data2, xlabel){
  max_y = max(rbind(bif_data$SizeOYO, bif_data2$SizeOYO))
  min_y = min(rbind(bif_data$SizeOYO, bif_data2$SizeOYO))
  
  max_y = max_y + 0.01*max_y
  min_y = min_y - 0.01*min_y
  
  plot1 = ggplot(data=bif_data, aes(bif_par, y=SizeOYO)) +
    geom_point() + labs(y = "Size OYO", x = xlabel) + ylim(min_y, max_y)
  
  plot2 = ggplot(data=bif_data2, aes(bif_par, y=SizeOYO)) +
    geom_point() + labs(y = "SizeOYO", x = xlabel) + ylim(min_y, max_y)
  
  plot_tot <- ggarrange(plot1, plot2, 
                        ncol = 2, nrow = 1)
  
  plot_tot
}


#-------------------------------------------------------------------------------#
# Function for plotting total consumer biomass of two datasets next to each other
#-------------------------------------------------------------------------------#
plot_conbm_comp <- function(bif_data, bif_data2, xlabel){
  max_y = max(rbind(bif_data$Bio_tot_JA, bif_data2$Bio_tot_JA))
  min_y = min(rbind(bif_data$Bio_tot_JA, bif_data2$Bio_tot_JA))
  
  max_y = max_y + 0.01*max_y
  min_y = min_y - 0.01*min_y
  
  plot1 = ggplot(data=bif_data, aes(bif_par, y=Bio_tot_JA)) +
    geom_point() + labs(y = "Total consumer biomass", x = xlabel) + ylim(min_y, max_y)
  
  plot2 = ggplot(data=bif_data2, aes(bif_par, y=Bio_tot_JA)) +
    geom_point() + labs(y = "Total consumer biomass", x = xlabel) + ylim(min_y, max_y)
  
  plot_tot <- ggarrange(plot1, plot2, 
                        ncol = 2, nrow = 1)
  
  plot_tot
}



#-------------------------------------------------------------------------------#
# Function that gives all separate plots for comparison as output
#-------------------------------------------------------------------------------#
plot_comp_tot <- function(bif_data, bif_data2, xlabel){
  mat = plot_mat_comp(bif_data, bif_data2, xlabel)
  birth = plot_birth_comp(bif_data, bif_data2, xlabel)
  bm = plot_bm_comp(bif_data, bif_data2, xlabel)
  period = plot_period_comp(bif_data, bif_data2, xlabel)
  bmprod = plot_bmprod_comp(bif_data, bif_data2, xlabel)
  OYO = plot_OYO_comp(bif_data, bif_data2, xlabel)
  Rdensity = plot_R_comp(bif_data, bif_data2, xlabel)
  conbm = plot_conbm_comp(bif_data, bif_data2, xlabel)
  
  out = list(mat, birth, bm, period, bmprod, OYO, Rdensity, conbm)
  return(out)
}


#-------------------------------------------------------------------------------#
# Function to combine data of forward and backward runs
#-------------------------------------------------------------------------------#
combine_data <- function(df1, df2, flip_bif_par){
  df1_pr = process_data(read.table(df1, header=F), flip_bif_par)
  df2_pr = process_data(read.table(df2, header=F), flip_bif_par)
  df_tot = rbind(df1_pr, df2_pr)
  df_tot
}


#-------------------------------------------------------------------------------#
# Function to make time series plots
#-------------------------------------------------------------------------------#
plot_TS <- function(TS_data){
  TS_bm_plot = ggplot(data=TS_data, aes(Time_y, y=Larv_bio * 1000, color='Juveniles')) + 
    geom_point(aes(y=Juv_bio * 1000, color='Subadults')) + 
    geom_point(aes(y=Adu_bio * 1000, color='Adults')) + geom_point() +
    guides(color = guide_legend(override.aes = list(size = 3))) +
    labs(y = expression(Biomass ~ (mg ~ L^-1)), x = "Time (years)") + 
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black")) +
    theme(legend.position = c(0.9, 0.9)) +
    theme(legend.key=element_blank()) + #theme(legend.background=element_blank()) + 
    scale_color_manual(breaks=c("Juveniles", "Subadults", "Adults"),
                       values=c("Juveniles"="#4B0055","Subadults"="#0499BE",
                                "Adults"="#36E9A7"), name=NULL) +
    theme(axis.text=element_text(size=12),
          axis.title=element_text(size=14),
          legend.text=element_text(size=12))
          
    # scale_color_manual(values = c("Juveniles" = "#4B0055", "Subadults" = "#0499BE",
    #                               "Adults" = "#36E9A7"), name=NULL)
  
  TS_data$birthrate[TS_data$birthrate == 0] = NA # Set points where birth rate is 0 to NA to not include them in plot
  TS_data_br = na.omit(TS_data)
  TS_br_plot = ggplot(data=TS_data_br, aes(Time_y, y=birthrate*1000)) + 
    geom_line(linewidth=1.5) + geom_point(size=3) +
    labs(y = expression(Birth ~ rate ~ (mg ~ L^-1 ~ year^-1)), 
         x = "Time (years)") +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black")) +
    scale_color_discrete(name=NULL) + 
    theme(axis.text=element_text(size=12),
          axis.title=element_text(size=14),
          legend.text=element_text(size=12))
  
  TS_R_plot = ggplot(data=TS_data, aes(Time_y, y=R1*1000, color='Wadden Sea')) + 
    geom_point(aes(y=R2*1000, color='North Sea')) + 
    geom_point() + labs(y = expression(Resource ~ density ~ (mg ~ L^-1)), x = "Time (years)") + 
    guides(color = guide_legend(override.aes = list(size = 3))) +
    theme(legend.position="bottom") + geom_hline(yintercept = 5, linetype = "dashed") +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black")) +
    theme(legend.position = c(0.9, 0.9)) + theme(legend.key=element_blank()) + 
    #theme(legend.background=element_blank()) + 
    scale_color_manual(breaks=c("Wadden Sea", "North Sea"),
                       values=c("Wadden Sea"="#4B0055","North Sea"="#00CFBF"), name=NULL) +
    theme(axis.text=element_text(size=12),
          axis.title=element_text(size=14),
          legend.text=element_text(size=12))
    
    # scale_color_manual(values = c("Wadden Sea" = "#4B0055", "North Sea" = "#00CFBF", 
    #                               name=NULL)) +
    # scale_fill_discrete(breaks = c("Wadden Sea", "North Sea"))
  
  out = list(TS_bm_plot, TS_br_plot, TS_R_plot)
  return(out)
  
}


#-------------------------------------------------------------------------------#
# Function to subset cohort data
#-------------------------------------------------------------------------------#
subset_coh <- function(TS_data, n_cohorts = 10){
  coh_names = c('Time')
  for(i in 1:n_cohorts){
    coh_names = c(coh_names, sprintf('length_%s', i), sprintf('density_%s', i),
                  sprintf('buffer_%s', i))
  }
  subset(TS_data, select = coh_names)
}  


#-------------------------------------------------------------------------------#
# Function to plot growth curves
#-------------------------------------------------------------------------------#
plot_growth <- function(TS_data, num_coh){
  # Get cohort data
  GetPopData(subset_coh(TS_data))
  
  # Create empty vectors to fill in loop
  timecols = c()
  lengthcols = c()
  
  # Loop over number of cohorts to fill vectors with times and lengths
  for(i in 1:length(PopList)){
    timecols = c(timecols, PopList[[i]]$Time/250)
    lengthcols = c(lengthcols, PopList[[i]]$Length)
  }
  
  # Combine time and length data into one dataframe
  timelengthcols = as.data.frame(cbind(timecols, lengthcols))
  
  ggplot(data = timelengthcols, aes(timecols, y=lengthcols)) + geom_point() + 
    labs(y= "Body mass (g)", x = "Time (years)") + 
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black"))
}


#-------------------------------------------------------------------------------#
# Function to subset cohort data
#-------------------------------------------------------------------------------#
GetPopData <- function(InPutData = PopData,   
                       OutPutData = "PopList", #name of output
                       CohNr = 10,  #nr of cohorts that are followed
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


#-------------------------------------------------------------------------------#
# Function to make sink, source plot
#-------------------------------------------------------------------------------#
sink_source <- function(bif_data1, bif_data2, xlabel){
  bif_data = combine_data(bif_data1, bif_data2, flip_bif_par = T)
  bif_data$frac_WS = bif_data$WS_event_nr / (bif_data$WS_event_nr + bif_data$Est_event_nr)
  ss_plot = ggplot(data=bif_data, aes(bif_par, y=frac_WS)) + geom_point() + 
    labs(y = "Fraction of outflow from WS", x = xlabel) + geom_abline(slope=-1, intercept=1) +
    ylim(0,1) + xlim(0,1)
  
  ss_plot
}