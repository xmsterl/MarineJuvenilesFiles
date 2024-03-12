// - header file - Sep 2022

#define POPULATION_NR             1
#define I_STATE_DIM               3
#define I_CONST_DIM               10
#define ENVIRON_DIM               3
#define OUTPUT_VAR_NR             41 + 3 * LENGTHCURVES
#define PARAMETER_NR              28
#define EVENT_NR                  3
#define TIME_METHOD               DOPRI5
#define DYNAMIC_COHORTS           0 // Exclusively dynamic (0/1)
#define BIFURCATION               0

#if (BIFURCATION == 1)
#define LENGTHCURVES		0
#else
#define LENGTHCURVES		20
#endif
/*===========================================================================*/
