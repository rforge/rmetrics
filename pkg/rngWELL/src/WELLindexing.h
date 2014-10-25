/* ***************************************************************************** */
/* Copyright:      Francois Panneton and Pierre L'Ecuyer, University of Montreal */
/*                 Makoto Matsumoto, Hiroshima University                        */
/* Notice:         This code can be used freely for personal, academic,          */
/*                 or non-commercial purposes. For commercial purposes,          */
/*                 please contact P. L'Ecuyer at: lecuyer@iro.UMontreal.ca       */
/* ***************************************************************************** */

// Details of the algorithm figure 1 of Panneton et al. (2006).
// Array STATE[] represents v_{i,0}, ..., v_{i,r-1}, however, for efficiency,
// it is cyclically shifted so that v_{i,0} is STATE[state_i].

// The following macros are used for orders, which are not a power of two,
// as a more efficient replacement of STATE[(state_i + Mj) % R]. If R is
// a power of two, then mod R is computed using "&" with a bit mask.

// v_{i,0}
#define V0            STATE[state_i]
// v_{i,m_1}, first when state_i + M1 >= R
#define VM1Over       STATE[state_i+M1-R]
#define VM1           STATE[state_i+M1]
// v_{i,m_2}, first when state_i + M2 >= R
#define VM2Over       STATE[state_i+M2-R]
#define VM2           STATE[state_i+M2]
// v_{i,m_3}, first when state_i + M3 >= R
#define VM3Over       STATE[state_i+M3-R]
#define VM3           STATE[state_i+M3]
// v_{i,r-1}, second when state_i - 1 < 0
#define Vrm1          STATE[state_i-1]
#define Vrm1Under     STATE[state_i+R-1]
// v_{i,r-2}, second when state_i - 2 < 0
#define Vrm2          STATE[state_i-2]
#define Vrm2Under     STATE[state_i+R-2]
// v_{i+1,0}, second when state_i - 1 < 0
#define newV0         STATE[state_i-1]
#define newV0Under    STATE[state_i-1+R]
// v_{i+1,1}
#define newV1         STATE[state_i]
// v_{i+1,r-1}, second when state_i - 2 < 0
#define newVRm1       STATE[state_i-2]
#define newVRm1Under  STATE[state_i-2+R]

