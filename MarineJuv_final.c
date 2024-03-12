/*
NAME
MarineJuv_final.c


DESCRIPTION
EBT implementation of basic size-structured consumer model with 3 size-class (small & large juveniles & adults)
Ontogenetic habitat shift at size s_shift.
Only fraction 'egg_frac' of eggs migrate to the estuary
Habitat 2 gets inflow of other estuaries.
Seasonal reproduction
Size-dependent mortality possible
Duration of larval stage in the other estuaries is explicitely modelled as a function of resource density.

Original implementation: HtB
Last modification: HtB - 7 Aug 2023
--3 Nov: add cohorts in the output
--12 Jan 23: Remove scaling of parameters with adult body mass, include conversion efficiency eggs
--9 Juv 23: Period and survival for juvenile phase only
-------------Maturatation rates corrected
-------------Production rates included
-------------Including biomassa and real number of the other estuaries. Note that number does change over time
______________________Only in biomass and Est_nr we take mortality into account
--27 Jun 23: In case of too low food availability in other estuaries, no outflow
--7 Aug 23: Include connectivity to the focal juveniles habitat
------------Calculate fraction of the adult habitat coming for the focal juvenile habitat. 

***/

#include  "escbox.h"


/*
*==========================================================================
*
*        LABELLING ENVIRONMENT AND I-STATE VARIABLES
*
*==========================================================================
*/
#define time                      env[0]
#define R1                        env[1]
#define R2                        env[2]

#define Age                       i_state(0)
#define Mass                      i_state(1)
#define Buffer                    i_state(2)

#define    IDnbegin             i_const(0) //initial cohort density
#define    AgeShift_Diet        i_const(1) //Age at the shift to large juveniles
#define    AgeShift_Adult       i_const(2) //Age at the shift to subadults
#define    AgeFirstRepro        i_const(3) //Age at first repro
#define    shift	              i_const(4) //Shift status
#define    maturity	            i_const(5) //Maturation status
#define    TotalReproBuf        i_const(6) //Total repro buffer
#define    IDndiet              i_const(7) //Number of individuals at diet shift
#define    pop_incl	            i_const(8) //Is it already part of the population?
#define    Source_L             i_const(9) //Where did the individual grow up? 


/*
*==========================================================================
*
*        DEFINING AND LABELLING CONSTANTS AND PARAMETERS
*
*==========================================================================
*/

#define RHO                             parameter[0] // Turn-over rate resources
#define R1MAX                           parameter[1] // Carrying capacity R1
#define R2MAX                           parameter[2] // Carrying capacity R2

#define SB                              parameter[3] //Body Mass at birth
#define S_shift                         parameter[4] //Body Mass at shift
#define SM                              parameter[5] //Body Mass at maturation

#define Attack1_Const                   parameter[6] //Attack rate  on R1 (26.5 is the default)
#define Attack2_Const                   parameter[7] //Attack rate on R2 (26.5 is the default)
#define Handling1_Const                 parameter[8] //Handling  R1 (12.5 is the default)
#define Handling2_Const                 parameter[9]  //Handling  R2 (12.5 is the default)
#define T_Const                         parameter[10] //Maintenance costs constant
#define sigma1                          parameter[11] //conversion efficiency R1
#define sigma2                          parameter[12] //conversion efficiency R2

#define qL                              parameter[13] //relative efficiency L
#define qJ                              parameter[14] //relative efficiency J
#define qA                              parameter[15] //relative efficiency A

#define theta                           parameter[16] //Gonad - tissue conversion
#define mu_egg                          parameter[17] //Fraction of eggs that survive
#define mu_b_Const                      parameter[18] //Background mortality
#define mu_size                         parameter[19] //Additional size_specific mortality
#define S_min                           parameter[20] //Min size mortality
#define S_max                           parameter[21] //Max size mortality

#define SEASON                          parameter[22] // length of the season
/*___additional parameter MJ___*/
#define egg_frac                        parameter[23] // fraction of eggs to the estuary
#define RL_other                        parameter[24] // Resource density in the other estuaries
#define vol_scale                       parameter[25] // Volume scaling
#define mu_other                        parameter[26] // Additional mortality in the other estuaries
#define connect                         parameter[27] // Connectivity to the focal nursery


#define MIN_SURVIVAL              1.0E-9
#define ZERODENSITY	              1.0E-9
#define ZERODENSITY2              1.0E-12
#define	SMALL		                  0.001


#define ismature(n)	((popIDcard[0][n][maturity] > 0.5))
#define isshifted(n)	((popIDcard[0][n][shift] > 0.5))



/*
*==========================================================================
*
*        OTHER DEFINITIONS
*
*==========================================================================
*/


static double   Scaling, a1, a2, h1, h2, T, mu_b, MAX_AGE;
static double		Adu_bio, Juv_bio, Larv_bio, Adu_nr, Juv_nr, Larv_nr, Vuln_bm, Vuln_nr, Buf_bm, FocalFrac_nr, Est_nr = 0;
static double		nu_L, nu_J, nu_A, nuplus_L, nuplus_J, nuplus_A;
static double		Mort_L, Mort_J, Mort_A;
static double	  birthrate = 0, MatLRate = 0, MatJRate = 0, MatLRate_Est = 0;
static double   MatJRate_O = 0, MatLRate_O = 0, MatLRate_Est_O = 0; 
static double		logsm, logshift,  TotalRepro, newborns, MeanFecundity;
static double   IntakeTot1 = 0, IntakeTot2 = 0;
static double		JuvPeriod = MISSING_VALUE, JuvSurv = MISSING_VALUE, LarvPeriod = MISSING_VALUE, LarvSurv = MISSING_VALUE;
static double   YOYsurvival = 1.0, SizeOYO;
static int      i, j, ReproCohorts, LarvStarv;
static double   Adshift, JuvShift;
static double   nu_E, Duration_E, Survival_E;
//static int      LarvStarv;
static double   Production_L, Production_J, Production_A, Production_E;
static int      Est_coh = 0; //Cohorts in other estuaries
static double   Est_bm = 0; //Biomass in other estuaries

#if (LENGTHCURVES > 0)
static double           BirthTimes[LENGTHCURVES]; //Birthtimes of the cohorts, necessary to find the right cohort for the output.
#endif
/*
*==========================================================================
*
* USER INITIALIZATION ROUTINE ALLOWS OPERATIONS ON INITIAL POPULATIONS
*
*==========================================================================
*/

void    UserInit(int argc, char **argv, double *env, population *pop)

{
  //Set parameters//
  Scaling = 1;//pow(SM, -0.25);
  a1 = Attack1_Const * Scaling;
  a2 = Attack2_Const * Scaling;
  h1 = Handling1_Const * 1;//pow(SM, 0.25);
  h2 = Handling2_Const * 1;//pow(SM, 0.25);
  T = T_Const * Scaling;
  mu_b = mu_b_Const * Scaling;
  MAX_AGE = - 1.0 * log(MIN_SURVIVAL) / mu_b;
  

  nu_E = sigma1 * a1 * RL_other / (1 + a1 * h1 * RL_other) - T; //Energy-intake in the other estuaries.
  Duration_E = log(S_shift / SB) / (nu_E); //Duration of the stay
  Survival_E = exp(-Duration_E * (mu_b + mu_other)); //Survival
  if(nu_E <= ZERODENSITY) {
    printf("Warning: No growth in the other estuaries!\n");
    Survival_E = 0;
    Duration_E = 1E9;
  }

    ReportNote("\n    %s%f%s%f%s%f%s",
    "Net-biomass production in the other estuaries: ", nu_E, ". Duration: ", Duration_E, " days, survival is: ", Survival_E ,". \n");
  //printf("Net-biomass production in the other estuaries: %f. Duration: %f days, Survival: %f\n", nu_E, Duration_E, Survival_E);

  logsm = log(SM);
  logshift = log(S_shift);


  //set i states//
  for(int i = 0, Adu_bio = 0.0, Juv_bio = 0.0, Larv_bio = 0.0, Est_bm = 0.0, Est_nr = 0.0; i < cohort_no[0]; i++) {

    if (pop[0][i][Buffer] == MISSING_VALUE) {
      pop[0][i][Buffer] = 0;
    }

    if (popIDcard[0][i][pop_incl] == MISSING_VALUE) {
      popIDcard[0][i][pop_incl] = 1;
    }

    if ((popIDcard[0][i][IDnbegin] == MISSING_VALUE) || (popIDcard[0][i][IDnbegin] < pop[0][i][number]) || iszero(popIDcard[0][i][IDnbegin])) {
      popIDcard[0][i][IDnbegin] = pop[0][i][number];
    }

    if ((popIDcard[0][i][AgeShift_Diet] == MISSING_VALUE) || iszero(popIDcard[0][i][AgeShift_Diet])) {
      popIDcard[0][i][AgeShift_Diet] = 0;
    }

    if ((popIDcard[0][i][AgeShift_Adult] == MISSING_VALUE) || iszero(popIDcard[0][i][AgeShift_Adult])) {
      popIDcard[0][i][AgeShift_Adult ] = 0;
    }

    if ((popIDcard[0][i][AgeFirstRepro] == MISSING_VALUE) || iszero(popIDcard[0][i][AgeFirstRepro])) {
      popIDcard[0][i][AgeFirstRepro] = 0;
    }

    if ((popIDcard[0][i][TotalReproBuf] == MISSING_VALUE) || iszero(popIDcard[0][i][TotalReproBuf])) {
      popIDcard[0][i][TotalReproBuf] = 0;
    }

    if (popIDcard[0][i][Source_L] == MISSING_VALUE) {
      popIDcard[0][i][Source_L] = 1; //1 = Wadden Sea
    }

    if (iszero(popIDcard[0][i][IDndiet])) {
      popIDcard[0][i][IDndiet] = MISSING_VALUE;
    }

    

    // Set the shift and maturity flags
    popIDcard[0][i][shift] = 0.0;
    popIDcard[0][i][maturity] = 0.0;


    if (popIDcard[0][i][pop_incl] == 0) {
      Est_coh += 1;
      popIDcard[0][i][shift] = 1.0;
      Est_nr += pop[0][i][number];
      if(nu_E > 0) {
      Est_bm += exp(nu_E * pop[0][i][Age]) * SB * pop[0][i][number];
      } else {
        Est_bm += SB * pop[0][i][number];
      }
      if(popIDcard[0][i][AgeShift_Diet] == 0) {
        popIDcard[0][i][AgeShift_Diet] = Duration_E; 
      }
    } else {
    if ((pop[0][i][Mass] >= SM) || isequal(log(pop[0][i][Mass]), logsm)) {
      popIDcard[0][i][maturity] = 1.0;
      Adu_bio += pop[0][i][number] * pop[0][i][Mass];
      if (SM >= S_shift) {
        popIDcard[0][i][shift] = 1.0;
      }
    } else if ((pop[0][i][Mass] >= S_shift) || isequal(log(pop[0][i][Mass]), logshift)) {
      popIDcard[0][i][shift] = 1.0;
      Juv_bio += pop[0][i][number] * pop[0][i][Mass];
    } else {
      Larv_bio += pop[0][i][number] * pop[0][i][Mass];
    }

  }
  }

  #if (LENGTHCURVES > 0)
    //printf("At initialization....\n");
    for (i = 0; i < LENGTHCURVES; i++) {
      BirthTimes[i] = -1.0; //initialize the birthtimes
      // printf("Birthtime %d is %f\n", i, BirthTimes[i]);
    }
  #endif

  return;
}


/*
*==========================================================================
*
*    SPECIFICATION OF THE NUMBER AND VALUES OF BOUNDARY POINTS
*
*==========================================================================
*/

void    SetBpointNo(double *env, population *pop, int *bpoint_no)

{
  bpoint_no[0] = 0;
  return;
}



/*==========================================================================*/

void    SetBpoints(double *env, population *pop, population *bpoints)

{
  return; //Pulsed repro
}

/*==========================================================================*/
//Function to calculate biomassa and intake

void	SetStructVars(double *env, population *pop, population *ofs)

{
  double Ingestion_A, Ingestion_J, Ingestion_L;
  Adu_bio = Adu_nr = Juv_bio = Juv_nr = Larv_bio = Larv_nr = Vuln_bm = Vuln_nr = Buf_bm = Est_nr = Est_coh = Est_bm = FocalFrac_nr = 0;
  LarvStarv = 0;

  //Set parameters//
  Scaling = 1;//pow(SM, -0.25);
  a1 = Attack1_Const * Scaling;
  a2 = Attack2_Const * Scaling;
  h1 = Handling1_Const * 1;//pow(SM, 0.25);
  h2 = Handling2_Const * 1;//pow(SM, 0.25);
  T = T_Const * Scaling;
  mu_b = mu_b_Const * Scaling;
  MAX_AGE = - 1.0 * log(MIN_SURVIVAL) / mu_b;

  nu_E = sigma1 * a1 * RL_other / (1 + a1 * h1 * RL_other) - T; //Energy-intake in the other estuaries.
  Duration_E = log(S_shift / SB) / (nu_E); //Period in the other estuaries
  Survival_E = exp(-Duration_E * (mu_b + mu_other)); //Survival in the other estuaries.

  if(nu_E <= ZERODENSITY) {
    Survival_E = 0;
    Duration_E = 1E9;
  }

  logsm = log(SM);
  logshift = log(S_shift);


  for (int i = 0; i < cohort_no[0]; i++)
  {
    if (pop[0][i][number] <= ZERODENSITY * popIDcard[0][i][IDnbegin]) continue; //Too few
    if (pop[0][i][Age] >= MAX_AGE) continue; //Too old
    if (popIDcard[0][i][pop_incl] == 0) continue; //Not yet part of the main population (in other nursery)

    if (ismature(i)) {
      Adu_bio += pop[0][i][number] * pop[0][i][Mass];
      Buf_bm += pop[0][i][number] * pop[0][i][Buffer];
      Adu_nr += pop[0][i][number];
      if(popIDcard[0][i][Source_L] == 1) {
        FocalFrac_nr += pop[0][i][number];
      }
    } else if (isshifted(i)) {
      Juv_bio += pop[0][i][number] * pop[0][i][Mass];
      Juv_nr += pop[0][i][number];
      if(popIDcard[0][i][Source_L] == 1) {
        FocalFrac_nr += pop[0][i][number];
      }
    } else {
      Larv_bio += pop[0][i][number] * pop[0][i][Mass];
      Larv_nr += pop[0][i][number];
    }

    if ((pop[0][i][Mass] >= S_min) && (pop[0][i][Mass] < S_max)) {
      Vuln_bm += pop[0][i][number] * pop[0][i][Mass];
      Vuln_nr += pop[0][i][number];
    }
  }

  //Get the pop in the alterative nursery//
  for (int i = 0; i < cohort_no[0]; i++)
  {
    if (popIDcard[0][i][pop_incl] == 0) {
      Est_nr += pop[0][i][number];
      Est_coh += 1;
      if(nu_E > 0) {
      Est_bm += exp(nu_E * pop[0][i][Age]) * SB * pop[0][i][number];
      } else {
        Est_bm += SB * pop[0][i][number];
      }
    }
  }

  IntakeTot1 = IntakeTot2 = Ingestion_A = Ingestion_J = Ingestion_L = nu_A = nu_J = nu_L = 0;
  if (SM >= S_shift) { //with diet shift
    Ingestion_A = qA * a2 * R2 / (1 + h2 * a2 * R2); //After shift
    Ingestion_J = qJ * a2 * R2 / (1 + h2 * a2 * R2); //After shift
    Ingestion_L = qL * a1 * R1 / (1 + h1 * a1 * R1); //Before shift
    nu_A = (sigma2 * Ingestion_A - T);
    nu_J = (sigma2 * Ingestion_J - T);
    nu_L = (sigma1 * Ingestion_L - T);
    IntakeTot1 = Ingestion_L * Larv_bio;
    IntakeTot2 = Ingestion_A * Adu_bio + Ingestion_J * Juv_bio;
  } else { //No diet shift
    Ingestion_A = qA * a1 * R1 / (1 + h1 * a1 * R1);
    Ingestion_L = qL * a1 * R1 / (1 + h1 * a1 * R1);
    IntakeTot1 = Ingestion_A * Adu_bio + Ingestion_L * Larv_bio;
    nu_A = (sigma1 * Ingestion_A - T);
    nu_L = (sigma1 * Ingestion_L - T);
  }
  
  nuplus_L  = max(nu_L, 0.0);
  nuplus_J  = max(nu_J, 0.0);
  nuplus_A  = max(nu_A, 0.0);

  Mort_L = Mort_J = Mort_A = mu_b;

  Mort_L -= min(nu_L, 0.0);
  Mort_J -= min(nu_J, 0.0);
  Mort_A -= min(nu_A, 0.0);

  if(nu_L < 0){
    LarvStarv = 1;
  }

  //Get biomass production (per gram) in the presence of the individuals only, NA otherwise 
  //Note that I assume size-specific mortality is affecting all individuals in a certain stage//
  //This is different from what I can get out of the data, since I assume it is NA in the absence of consumers of that stage
  //Note, that for average values, the bif DOES take the zero's into account! So, we need to account for that by either adding missing values or calculate back based on fraction of time it was not zero//
  //Adult production is not completely true, since individuals (and thereby buffer!) die!
  if(Est_nr > 0) {
    Production_E = nu_E - mu_b - mu_other;
  }

  if(Larv_bio > 0) {
    Production_L = nu_L - mu_b; 
    if (S_min < S_shift) {
      Production_L -= mu_size; //assuming that whole life stage is affected
    }
  } else {
    Production_L = MISSING_VALUE;
  }

  if(Juv_bio > 0) {
    Production_J = nu_J - mu_b;
    if ((S_min <= S_shift) && (S_max >= SM)) { //assuming that whole life stage is affected, otherwise no add mort in calc
      Production_J -= mu_size;
    }
  } else {
    Production_J = MISSING_VALUE;
  }

  if(Adu_bio > 0) {
    Production_A = (1 - mu_egg) * theta * nu_A - mu_b;
    if (S_max > SM) {
      Production_A -= mu_size;
    }
  } else {
    Production_A = MISSING_VALUE; 
  }
  

  return;
}


/*
*==========================================================================
*
*            SPECIFICATION OF DERIVATIVES
*
*==========================================================================
*/

void    Gradient(double *env, population *pop, population *ofs, double *envgrad, population *popgrad, population *ofsgrad, population *bpoints)

{
  SetStructVars(env, pop, NULL);

  double Mort;
  double size;

  for (int i = 0; i < cohort_no[0]; i++) {
    popgrad[0][i][number] = 0;
    popgrad[0][i][Mass] = 0;
    popgrad[0][i][Buffer] = 0;
    popgrad[0][i][Age] = 1; //All need to increase in age, also the ones in the other estuaries
    if (pop[0][i][Age] > MAX_AGE) continue; //don't do this for cohorts that are extinct
    if (pop[0][i][number] <= ZERODENSITY * popIDcard[0][i][IDnbegin]) continue; //don't do this for cohorts with small density
    
    ///For individuals in alternative nurseries/// not necessary to get ode for mass, since we can calculate it directly based on age. 
    if (popIDcard[0][i][pop_incl] == 0) { //Not yet part of the pop
      if(nu_E > 0) {
        popgrad[0][i][number]   = - (mu_b + mu_other) * pop[0][i][number];
      } else {
        popgrad[0][i][number]   = - (mu_b + mu_other - nu_E) * pop[0][i][number];
      }
    }  else { //For individuals in main nursery and adult habitat/ 
    size = (pop[0][i][Mass]);
    Mort = 0;
    if ((size >= S_min) & (size < S_max)) {
      Mort = mu_size;
    }

    if (ismature(i)) {
      popgrad[0][i][Buffer] = nuplus_A * size; //energy in buffer
      popgrad[0][i][number]   = -(Mort_A + Mort) * pop[0][i][number];
    } else if (isshifted(i)) {
      popgrad[0][i][Mass] = nuplus_J * size;
      popgrad[0][i][number]   = -(Mort_J + Mort) * pop[0][i][number];
    } else {
      popgrad[0][i][Mass] = nuplus_L * size;
      popgrad[0][i][number]   = -(Mort_L + Mort) * pop[0][i][number];
    }
  }
  }

  ///Change in environment///
  IntakeTot1 = max(0.0, IntakeTot1);
  IntakeTot2 = max(0.0, IntakeTot2);

  envgrad[0] = 1.0;
  envgrad[1] = (RHO * (R1MAX - R1) - IntakeTot1);
  envgrad[2] = (RHO * (R2MAX - R2) - vol_scale * IntakeTot2);

  return;
}

/*
*==========================================================================
*
*	SPECIFICATION OF EVENT LOCATION AND DYNAMIC COHORT CLOSURE
*
*==========================================================================
*/

void	EventLocation(double *env, population *pop, population *ofs,
  population *bpoints, double *events) //Find mature cohorts

  {

    events[0] = -1.0; //maturating
    events[1] = -1.0; //habitat shift
    events[2] = -1.0; //habitat shift other E

    for (int i = 0; i < cohort_no[0]; i++)
    {
      if (popIDcard[0][i][pop_incl] == 1) {
      if(SM <= S_shift) { //not necessary to have the second event checked: no diet shift or at same moment as maturation
        if (popIDcard[0][i][maturity] < 0.5)  {  // if juvenile
          events[0] = max(events[0], log(pop[0][i][Mass]) - logsm);
        }
      } else {
        if (popIDcard[0][i][shift] < 0.5) { //before diet shift
          events[1] = max(events[1], log(pop[0][i][Mass]) - logshift);
        } else {
          if (popIDcard[0][i][maturity] < 0.5)  {  // if juvenile
            events[0] = max(events[0], log(pop[0][i][Mass]) - logsm);
          }
        }
      }
    } else { //Recruitment from alternative nursery to adult habitat
      events[2] = max(events[2], (pop[0][i][Age]) - (popIDcard[0][i][AgeShift_Diet]));
    }
  }
    //printf("Event location at time %f, event0: %f event1: %f\n", time, events[0], events[1]);
    return;
  }

/*==============================================================================*/
int	ForceCohortEnd(double *env, population *pop, population *ofs, population *bpoints)
{
  int			i;

//For the small and large juvenile periods/survival, I want to calculate the average juvenile period & survival of the switching cohorts.
//In theory, it is possible that two cohorts switch at the same time, even though they were born in a different year.
//Therefore, to get the average in the population , I multiply with the number of individuals in each cohort.
// JuvPeriod JuvSurv LarvPeriod LarvSurv
//The moment I write output, I divide by the total number of switching individuals to get the average.

//-> by changing it to MISSING_VALUE, it is possible to get the min max, and by changin underlying c-code, it will calculate average correctly 


  if(SM > S_shift) { //First diet shift, than maturation.
    for(i = 0; i < cohort_no[0]; i++) {
      if (ismature(i)) continue;
      if((pop[0][i][Mass] >= SM) || isequal(log(pop[0][i][Mass]), logsm)) { 
        popIDcard[0][i][maturity] = 1.0; //change status
        pop[0][i][Mass] = SM;
        popIDcard[0][i][AgeShift_Adult] = pop[0][i][Age];
        if(JuvPeriod > 0.9 * MISSING_VALUE) {
          JuvPeriod =  (pop[0][i][Age] - popIDcard[0][i][AgeShift_Diet]) * pop[0][i][number];
          //printf("At time %f, start of juvperiod, juvperiod is %5e\n", env[0], JuvPeriod);
          if((popIDcard[0][i][IDndiet] < 0.9 * MISSING_VALUE) && (popIDcard[0][i][IDndiet] > 0)) {
          JuvSurv   = (pop[0][i][number] / popIDcard[0][i][IDndiet]) * pop[0][i][number]; 
        } else {
          JuvSurv = 0; 
        }
        } else {
        JuvPeriod +=  (pop[0][i][Age] - popIDcard[0][i][AgeShift_Diet]) * pop[0][i][number];
        //printf("At time %f, later juvperiod, juvperiod is %5e\n", env[0], JuvPeriod);
        if((popIDcard[0][i][IDndiet] < 0.9 * MISSING_VALUE) && (popIDcard[0][i][IDndiet] > 0)) {
          JuvSurv   += (pop[0][i][number] / popIDcard[0][i][IDndiet]) * pop[0][i][number]; 
        } else {
          JuvSurv += 0; 
        }
         }
        MatJRate   += (pop[0][i][number] * SM) / cohort_limit;
        Adshift += pop[0][i][number];
      }
      if (isshifted(i)) continue;
      if((pop[0][i][Mass] >= S_shift) || isequal(log(pop[0][i][Mass]), logshift)) {
        popIDcard[0][i][shift] = 1.0; //change status
        popIDcard[0][i][AgeShift_Diet] = pop[0][i][Age];
        popIDcard[0][i][IDndiet] = pop[0][i][number];
        if(LarvPeriod > 0.9 * MISSING_VALUE) {
          LarvPeriod = pop[0][i][Age] * pop[0][i][number];
          LarvSurv   = (pop[0][i][number]/popIDcard[0][i][IDnbegin]) * pop[0][i][number];
        } else {
          LarvPeriod += pop[0][i][Age] * pop[0][i][number];
          LarvSurv   += (pop[0][i][number]/popIDcard[0][i][IDnbegin]) * pop[0][i][number];
        }
        MatLRate   += pop[0][i][number] * S_shift / cohort_limit;
        JuvShift  += pop[0][i][number];
      }
    }
  } else if (SM == S_shift) { //Diet shift and maturation happens at the same size
    for(i = 0; i < cohort_no[0]; i++) {
      if (ismature(i)) continue;
      if((pop[0][i][Mass] >= SM) || isequal(log(pop[0][i][Mass]), logsm)) {
        popIDcard[0][i][maturity] = 1.0; //change status
        popIDcard[0][i][shift] = 1.0; //change status
        pop[0][i][Mass] = SM;
        popIDcard[0][i][AgeShift_Adult] = popIDcard[0][i][AgeShift_Diet] = pop[0][i][Age];
        popIDcard[0][i][IDndiet] = pop[0][i][number];
        if(LarvPeriod > 0.9 * MISSING_VALUE) {
          LarvPeriod = pop[0][i][Age] * pop[0][i][number];
          LarvSurv   = (pop[0][i][number]/popIDcard[0][i][IDnbegin]) * pop[0][i][number];
        } else {
          LarvPeriod += pop[0][i][Age] * pop[0][i][number];
          LarvSurv   += (pop[0][i][number]/popIDcard[0][i][IDnbegin]) * pop[0][i][number];
        }
        MatLRate   += pop[0][i][number] * S_shift / cohort_limit;
        JuvShift  += pop[0][i][number];
      }}
    } else { //No diet shift.
      for(i = 0; i < cohort_no[0]; i++) {
        if (ismature(i)) continue;
        if((pop[0][i][Mass] >= SM) || isequal(log(pop[0][i][Mass]), logsm)) {
          popIDcard[0][i][maturity] = 1.0;
          pop[0][i][Mass] = SM;
          popIDcard[0][i][AgeShift_Adult] = pop[0][i][Age];
          popIDcard[0][i][IDndiet] = pop[0][i][number];
          if(LarvPeriod > 0.9 * MISSING_VALUE) {
          LarvPeriod = pop[0][i][Age] * pop[0][i][number];
          LarvSurv   = (pop[0][i][number]/popIDcard[0][i][IDnbegin]) * pop[0][i][number];
        } else {
          LarvPeriod += pop[0][i][Age] * pop[0][i][number];
          LarvSurv   += (pop[0][i][number]/popIDcard[0][i][IDnbegin]) * pop[0][i][number];
        }
          MatLRate   += pop[0][i][number] * S_shift / cohort_limit;
          JuvShift  += pop[0][i][number];
        }}
      }
      
    ///Migration from other estuaries//
      for(i = 0; i < cohort_no[0]; i++) {
        if ((popIDcard[0][i][pop_incl] == 1) || (nu_E <= 0)) continue;
        if ((isequal(popIDcard[0][i][AgeShift_Diet], pop[0][i][Age])) ||  (pop[0][i][Age] > popIDcard[0][i][AgeShift_Diet])) {
          popIDcard[0][i][pop_incl] = 1;
          if(MatLRate_Est > 0.9 * MISSING_VALUE) {
            MatLRate_Est = (pop[0][i][number] * S_shift) / cohort_limit;
        } else {
            MatLRate_Est += (pop[0][i][number] * S_shift) / cohort_limit;
        }
          //printf("inflow event at time %f\n", time);
        }
        }
  

    return NO_COHORT_END;


    }


    /*
    *==========================================================================
    *
    *        SPECIFICATION OF BETWEEN COHORT CYCLE DYNAMICS
    *
    *==========================================================================
    */

void    InstantDynamics(double *env, population *pop, population *ofs)
{
  SetStructVars(env, pop, NULL);
   
  MatLRate_O = MatLRate;
  MatJRate_O = MatJRate;
  MatLRate_Est_O = MatLRate_Est; 
  MatLRate = 0;
  MatJRate = 0; 
  MatLRate_Est = 0;


  if (R1 < ZERODENSITY) R1 = ZERODENSITY; //Set food to minimum level//
  if (R2 < ZERODENSITY) R2 = ZERODENSITY; //Set food to minimum level//

  int nyoy = 0, i = 0, newcoh;
  double dummy;

  for (i = 0; i < cohort_no[0]; i++)
  {
    if ((pop[0][i][Age] > MAX_AGE) || (pop[0][i][number] <= ZERODENSITY * popIDcard[0][i][IDnbegin]) || (pop[0][i][number] <= ZERODENSITY)) { //remove individuals that are too old
      pop[0][i][number] = 0.0;

      #if (LENGTHCURVES > 0)
                    for (j = 0; j < LENGTHCURVES; j++) //reset Birthtimes if the cohort is gone. Set it to -2 so it will not be filled at this time, but only next time.
                      {
                        if (fabs(time - BirthTimes[j] - pop[0][i][Age]) < 0.5) // cohort_limit is 1.0!
                          {
                            //printf("At instant dynamics, cohort %d goes extinct, it birthtime used to be %f\n", i, BirthTimes[j]);
                            BirthTimes[j] = -2.0;
                            break;
                          }
                      }
      #endif
    } else {


    // Store YOY survival for output
    if ((pop[0][i][Age] < SEASON + SMALL) && (popIDcard[0][i][IDnbegin] > ZERODENSITY)) {
      YOYsurvival = (nyoy * YOYsurvival + pop[0][i][number]/popIDcard[0][i][IDnbegin])/(nyoy + 1);
      SizeOYO = (nyoy * SizeOYO + pop[0][i][Mass])/(nyoy + 1);
      nyoy++;
    }

  }}


  //Pulsed reproduction//
  dummy = fmod(time, SEASON); //Day of the year
  birthrate = 0.0;
  ReproCohorts = 0;
  TotalRepro   = 0.0;
  newborns    = 0.0;
  MeanFecundity = 0.0;

  if ((dummy < SMALL) || (dummy > (SEASON - SMALL)))
  {
    for (i = 0; i < cohort_no[0]; i++)
    {
      if((ismature(i)) && (popIDcard[0][newcoh][pop_incl] == 1)) {
        if (pop[0][i][Buffer] > 0)
        {
          newborns += theta * (1 - mu_egg) * pop[0][i][Buffer] * pop[0][i][number] / SB; //number of newborns
          birthrate += pop[0][i][Buffer] * pop[0][i][number]; //repro rate in bioMass
          popIDcard[0][i][TotalReproBuf] += pop[0][i][Buffer] * pop[0][i][number]; //total output from this cohort.
          pop[0][i][Buffer] = 0; //reset buffer

          ReproCohorts++; //how many cohorts reproduce
          TotalRepro += pop[0][i][number]; //Total number of individuals that reproduce

          if (popIDcard[0][i][AgeFirstRepro] == 0) {
            popIDcard[0][i][AgeFirstRepro] = pop[0][i][Age];
          }
        }
      }
    }
    //printf("At time %f, there are %.12f newborns\n", time, newborns);

    if (newborns)
    {
      if(egg_frac < 1 ) { //part that is going to the other est.
      newcoh = AddCohorts(pop, 0, 2);
      //printf("And %d \n", newcoh);
      //printf("At time %f newcohort is: %d and %d\n", time, newcoh, newcoh + 1);
      pop[0][newcoh + 1][number] = popIDcard[0][newcoh + 1][IDnbegin] = (1 - egg_frac) * newborns;
      pop[0][newcoh + 1][Age]    = 0.0;
      pop[0][newcoh + 1][Mass]   = S_shift;
      pop[0][newcoh + 1][Buffer]  = 0.0;
      popIDcard[0][newcoh + 1][AgeShift_Diet] = Duration_E;
      popIDcard[0][newcoh + 1][shift] = 1;
      popIDcard[0][newcoh + 1][AgeFirstRepro] = 0;
      popIDcard[0][newcoh + 1][AgeShift_Adult] = 0;
      popIDcard[0][newcoh + 1][maturity] = 0;
      popIDcard[0][newcoh + 1][pop_incl] = 0;
      popIDcard[0][newcoh + 1][Source_L] = 0; //Grow up in other E
      if(SM == S_shift) {
        popIDcard[0][newcoh + 1][AgeShift_Adult] = Duration_E;
        popIDcard[0][newcoh + 1][maturity] = 1;
      }
    }
      else {
      newcoh = AddCohorts(pop, 0, 1);
    }
      //printf("At time %f newcohort is: %d and %d\n", time, newcoh, newcoh + 1);
      //printf("At time %f newcohort is: %d \t", time, newcoh);
      pop[0][newcoh][number] = popIDcard[0][newcoh][IDnbegin] = connect * egg_frac * newborns;
      pop[0][newcoh][Age]    = 0.0;
      pop[0][newcoh][Mass]   = SB;
      pop[0][newcoh][Buffer]  = 0.0;
      popIDcard[0][newcoh][AgeShift_Diet] = 0;
      popIDcard[0][newcoh][AgeShift_Adult] = 0;
      popIDcard[0][newcoh][AgeFirstRepro] = 0;
      popIDcard[0][newcoh][maturity] = 0;
      popIDcard[0][newcoh][shift] = 0;
      popIDcard[0][newcoh][pop_incl] = 1;
      popIDcard[0][newcoh][Source_L] = 1; //Grow up in Wadden Sea
      //Add cohorts of other estuaries//
      #if (LENGTHCURVES > 0)
    for (i = 0; i < LENGTHCURVES; i++) //Follow a new cohort
      {
        if (iszero(BirthTimes[i] + 1.0)) //if birthtimes = -1, fill it again.
          {
            //printf("at time %f, birthtime %d is initialized with time\n", time, i);
            BirthTimes[i] = time;
            break;
          }
      }
      #endif
      SetStepSize(SMALL);
    }
  }

  if (TotalRepro) MeanFecundity = newborns / TotalRepro;
  else MeanFecundity = 0.0;



  return;
}



/*
*==========================================================================
*
*            SPECIFICATION OF OUTPUT VARIABLES
*
*==========================================================================
*/

void    DefineOutput(double *env, population *pop, double *output)

/*
* OUTPUT VARIABLES:
* [ 1]     2: Resource R1
* [ 2]     3: Resource R2
* [ 3]     4: Total larvae number pop
* [ 4]     5: Total larvae bioMass pop
* [ 5]     6: Total juveniles number pop
* [ 6]     7: Total juveniles biomass pop
* [ 7]     8: Total adults number pop
* [ 8]     9: Total adults biobass pop
* [ 9]     10: Total vulnerable number pop
* [10]     11: Total vulnerable biomass pop
* [11]     12: Total reproductive buffer
* [12]     13: Larval JuvPeriod
* [13]     14: Larval survival
* [14]     15: Maturation rate from larvae to juveniles
* [15]     16: juv JuvPeriod
* [16]     17: juv survival
* [17]     18: Maturation rate from juveniles to subadults
* [18]     19: Total reproductive output (bioMass)
* [19]     20: Total reproductive output (numbers)
* [20]     21: Mean Fecundity
* [21]     22: yoy survival
* [22]     23: Size one year olds
* [23]     24: nu_L
* [24]     25: nu_J
* [25]     26: nu_A
* [26]     27: mu_L (without size-specific mort!)
* [27]     28: mu_J (without size-specific mort!)
* [28]     29: mu_A (without size-specific mort!)
* [29]     30: Number of cohorts
* [30]     31: Diet/habitat shift events
* [31]     32: Maturation events
* [32]     33: Number of individuals in the other estuaries
*/

{

  int             outnr = 0;
  double FracL = 0, FracJ = 0;
  SetStructVars(env, pop, NULL);

  if(LarvPeriod <= ZERODENSITY) {
    LarvPeriod = MISSING_VALUE;
    LarvSurv = MISSING_VALUE;
  }

  if(JuvPeriod <= ZERODENSITY) {
    JuvPeriod = MISSING_VALUE;
    JuvSurv = MISSING_VALUE;
  }

  if(Adshift) { //calculate average values
  JuvPeriod /= Adshift;
  JuvSurv /= Adshift;
  FracJ = 1;
  }
  if(JuvShift) {
    LarvPeriod /= JuvShift;
    LarvSurv /= JuvShift;
    FracL = 1;
  }

  #if (LENGTHCURVES > 0)
    double                Lengths[LENGTHCURVES]; //create array
    double                Numbers[LENGTHCURVES]; //create array
    double                TotalBif[LENGTHCURVES];
    for (j = 0; j < LENGTHCURVES; j++) {
      Lengths[j] = DBL_MAX;
      Numbers[j] = DBL_MAX;
      TotalBif[j] = DBL_MAX;
    } //initialize with maximum possible floating-point number
  #endif

  #if (LENGTHCURVES > 0) //Check if the cohort belongs to the time at birth and than get length.
  for(i = 0; i < cohort_no[0]; i++) {
        for (j = 0; j < LENGTHCURVES; j++)
          {
            if ((fabs(time - BirthTimes[j] - pop[0][i][Age]) < 0.5) & (BirthTimes[j] >= 0) & (popIDcard[0][i][Source_L] == 1))  // cohort_limit is 1.0!!!
              {
                Lengths[j] = pop[0][i][Mass];
                Numbers[j] = pop[0][i][number];
                TotalBif[j] = popIDcard[0][i][TotalReproBuf];
                break;
              }
          }
        }
  #endif

  if((Adu_nr + Juv_nr) > 0) {
    FocalFrac_nr = FocalFrac_nr/(Adu_nr + Juv_nr);
  } else {
    FocalFrac_nr = MISSING_VALUE; 
  }      

  output[outnr++] = R1; //1
  output[outnr++] = R2; //2

  output[outnr++] = Larv_nr; //3
  output[outnr++] = Larv_bio;
  output[outnr++] = Juv_nr; //5
  output[outnr++] = Juv_bio;
  output[outnr++] = Adu_nr;
  output[outnr++] = Adu_bio; //8
  output[outnr++] = Vuln_nr;
  output[outnr++] = Vuln_bm; //10
  output[outnr++] = Buf_bm;

  output[outnr++] = LarvPeriod; //12
  output[outnr++] = LarvSurv;
  output[outnr++] = MatLRate_O;
  output[outnr++] = JuvPeriod;
  output[outnr++] = JuvSurv; //16
  output[outnr++] = MatJRate_O; //17

  output[outnr++] = birthrate; //18 birthrate in bioMass!
  output[outnr++] = newborns; //number of offspring
  output[outnr++] = MeanFecundity; //mean number of individuals per adult
  output[outnr++] = YOYsurvival; // 21 YOY survival
  output[outnr++] = SizeOYO;  // 22 YOY growth

  output[outnr++] = nu_L; //23
  output[outnr++] = nu_J;
  output[outnr++] = nu_A; //25
  output[outnr++] = Mort_L; //THIS IS EXCLUSIVE size-specific mort!
  output[outnr++] = Mort_J;
  output[outnr++] = Mort_A;
  output[outnr++] = cohort_no[0]; //29 nr of cohorts

  output[outnr++] = FracL; //30 Diet/habitat shift took place
  output[outnr++] = FracJ; //31 maturation event took place
  output[outnr++] = LarvStarv; // 32
 

  output[outnr++] = Production_L; //33
  output[outnr++] = Production_J;
  output[outnr++] = Production_A;  //35
  output[outnr++] = Production_E;

  output[outnr++] = Est_nr; //37
  output[outnr++] = MatLRate_Est_O; //38
  output[outnr++] = Est_coh; //39
  output[outnr++] = Est_bm; //40
  output[outnr++] = FocalFrac_nr; // Fraction of individuals (nr) that grew up in focal nursery. 
  


  #if (LENGTHCURVES > 0) //output lengths
    for (j = 0; j < LENGTHCURVES; j++)
      {
        output[outnr++]  = Lengths[j];
        output[outnr++]  = Numbers[j];
        output[outnr++]  = TotalBif[j];
        //output[outnr++] = BirthTimes[j];
        BirthTimes[j]    = max(BirthTimes[j], -1.0); //reset birthtimes if necessary
      }
  #endif

  //reset some variables
  MatJRate_O = 0; 
  JuvPeriod = MISSING_VALUE;
  JuvSurv = MISSING_VALUE;
  Adshift = 0.0;

  MatLRate_O = 0.0; 
  LarvPeriod = MISSING_VALUE;
  LarvSurv = MISSING_VALUE;
  JuvShift = 0.0;

  MatLRate_Est_O = 0.0; 
 

  return;
}
