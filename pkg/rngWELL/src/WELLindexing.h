/* ***************************************************************************** */
/* Copyright:      Francois Panneton and Pierre L'Ecuyer, University of Montreal */
/*                 Makoto Matsumoto, Hiroshima University                        */
/* Notice:         This code can be used freely for personal, academic,          */
/*                 or non-commercial purposes. For commercial purposes,          */
/*                 please contact P. L'Ecuyer at: lecuyer@iro.UMontreal.ca       */
/* ***************************************************************************** */

//details of the algorithm figure 1 of Panneton et al. (2006)
// state_i is       i mod R
//v_i,0
#define V0            STATE[state_i]
//v_i,m1, first when i > r-m1
#define VM1Over       STATE[state_i+M1-R]
#define VM1           STATE[state_i+M1]
//v_i,m2, first when i > r-m2
#define VM2Over       STATE[state_i+M2-R]
#define VM2           STATE[state_i+M2]
//v_i,m3, first when i > r-m3
#define VM3Over       STATE[state_i+M3-R]
#define VM3           STATE[state_i+M3]
//v_i,r-1, second when i < R
#define VRm1          STATE[state_i-1]
#define VRm1Under     STATE[state_i+R-1]
//v_i,r-2, second when i < R
#define VRm2          STATE[state_i-2]
#define VRm2Under     STATE[state_i+R-2]
//v_i+1,0, second when i < R
#define newV0         STATE[state_i-1]
#define newV0Under    STATE[state_i-1+R]
//v_i+1,1
#define newV1         STATE[state_i]
//v_i+1,r-1
#define newVRm1       STATE[state_i-2]
#define newVRm1Under  STATE[state_i-2+R]

