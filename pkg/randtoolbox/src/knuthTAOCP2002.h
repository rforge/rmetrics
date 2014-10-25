/*    This program by D E Knuth is in the public domain and freely copyable.
 *    It is explained in Seminumerical Algorithms, 3rd edition, Section 3.6
 *    (or in the errata to the 2nd edition --- see
 *        http://www-cs-faculty.stanford.edu/~knuth/taocp.html
 *    in the changes to Volume 2 on pages 171 and following).              */

/*    N.B. The MODIFICATIONS introduced in the 9th printing (2002) are
      included here; there's no backwards compatibility with the original. */

/*    This version also adopts Brendan McKay's suggestion to
      accommodate naive users who forget to call ranf_start(seed).         */

/*    If you find any bugs, please report them immediately to
 *                 taocp@cs.stanford.edu
 *    (and you will be rewarded if the bug is genuine). Thanks!            */

/************ see the book for explanations and caveats! *******************/
/************ in particular, you need two's complement arithmetic **********/

/* function definitions, the first in ANSI syntax and the second in K&R syntax
    K&R stands for Kernighan and Ritchie ;-) */

#ifndef knuthTAOCP2002_H
#define knuthTAOCP2002_H

#ifdef __STDC__
void ranf_array(double aa[], int n);
#else
void ranf_array(aa,n)    /* put n new random fractions in aa */
double *aa;   /* destination */
int n;      /* array length (must be at least KK) */
#endif


#ifdef __STDC__
void ranf_start(long seed);
#else
void ranf_start(seed)    /* do this before using ranf_array */
long seed;            /* selector for different streams */
#endif

#endif

