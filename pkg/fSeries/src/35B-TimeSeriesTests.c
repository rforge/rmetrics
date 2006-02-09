/*  

    Blake LeBaron
    Dept. Of Economics
    University Of Wisconsin-Madison
    July 1988 
    March 1990

    This software is distributed with the understanding that it is free.
    Users may distribute it to anyone as long as they don't charge anything.
    Also, the author gives this out without any support or 
    responsibility for errors of any kind.   I hope that the 
    distribution of this software will further enhance are understanding
    of these new measures of dependence.


*/


/* Changes for loading into R, A. Trapletti, 20.12.2000 */


#include <stdio.h>
#include <math.h>
/*
#include <R.h>
*/
      
/* NBITS is the number of useable bits per word entry.  Technically
   on the sun this should be 32, as the sun uses 4 byte integers.
   Since the counting algorithm uses a table lookup method we must
   keep that table reasonable, so only 15 bits are used.  This may be
   changed if space is a problem.
*/

#define NBITS 15 
#define ALLBITS 0xffff
#define PREC	double	
#define TABLEN 32767


/* ----------- grid macro: turn bits on --------------------------- */

#define GRIDON(x,y) \
	if(x!=y) {    \
		if(x>y) {  \
			ix = y; \
			iy = x; \
		}              \
		else {      \
			ix = x; \
			iy = y; \
		} \
		iy = iy-ix-1;  \
		ipos = iy / NBITS;  \
		ibit = NBITS - 1 - (iy % NBITS); \
		*(*(start+ix)+ipos) |= bits[ibit];\
	}

/* define struct */

struct position {
	PREC value;
	int pos;
};

/* globals */

static  int		bits[NBITS],
		*mask;

static short int    *grid,
                    **start;

static int	*lookup,first=1;

static	struct position *postab,*postlast;

/*
	free all memory allocations
*/


static void
freeall()
{
	free(grid);
	free(mask);
	free(postab);
	free(start);
	free(lookup);
}


/* module function definitions */

/*
	generate mask 
	mask pattern for row l, nbits: number of bits used
				omit:  number of bits omitted
				mask:  mask[0],mask[1] two word mask

*/

static void
genmask(l,n,nbits,omit,mask)
int l,n,nbits,omit,mask[];
{
	int i,k,j,last,itrue;

	mask[0] = mask[1] = ALLBITS;
	last = (n-l-1)/nbits;
	for(i=n-omit;i<n;i++) {
		itrue = i - l -1;
		j = itrue/nbits;
		k = nbits-1-(itrue % nbits);
		j = last-j ;
		mask[j] = mask[j] ^ bits[k];
	}
}

/*
	embed bitmap grid to next higher dimension 

	g(i,j) = g(i,j) & g(i+1,j+1)


*/

static void
embed(n,dim)
int n,dim;
{
	int j;
	register short int *i,*i2;

	for(j=0;j<n-dim;j++) {
		i = *(start+j);
		for(i2= *(start+j+1);i2<= *(start+j+2)-1;i2++) {
			*i = (*i) & (*i2);
			i++;
		}
		if(i!= *(start+j+1))
			*i = 0;
	}
}

/*
	count c stats for grid - zero out parts not counted using mask
	returns c
*/

static
double 
evalc(n)
int n;
{

	register long int count;
	int j;
	register short int *i;
	double nd;

	count = 0;
	nd = (double)n;

	for(j=0;j<n;j++) {

		if( (*(start+j+1)-*(start+j)) > 2 ) {

			for (i = *(start+j);i< *(start+j+1)-2;i++) {
				count += lookup[*i];
				/*
				if(lookup[*i]>15)
					Rprintf("%d %d %d\n", (int)(i-grid),*i,lookup[*i]);
				*/
			}
			for(i = *(start+j+1)-2;i< *(start+j+1);i++) {
				count += lookup[ (*i) & mask[j*2+ *(start+j+1)-i-1]];
			}
		}
		else {
			for(i = *(start+j);i<*(start+j+1);i++) {
				count += lookup[ (*i) & mask[j*2+ *(start+j+1)-i-1]];
			}
		}
	}

	return ( 2*((double)count)/ (nd*(nd-1)));
}

static
double
ipow(x,m)
double x;
int m;
{
	int j;
	double y;

	y = 1;

	for(j=0;j<m;j++)
		y *= x;

	return(y);
}

/*

This function calculates the asymptotic standard error from c
and k.  It then returns the test statistic which is asymptotically
distributed normal(0,1).  These formulas can be found
in Brock, Hsieh, LeBaron, page 43.


*/

static 
double
cstat(c,cm,k,m,n)
double c,cm,k;
int m,n;
{

	double sigma,
		stat,
		std,
		sqrt();
	int j;

	sigma = 0;
	for(j=1;j<=m-1;j++) {
		sigma += 2.*ipow(k,m-j)*ipow(c,2*j);
	}
	sigma += ipow(k,m) + (m-1)*(m-1)*ipow(c,2*m)
                 -m*m*k*ipow(c,2*m-2);
	sigma *= (double)4;

	std = sqrt(sigma/((double)n));

	stat = (cm - ipow(c,m))/std;
	return(stat);
}

static int
comp(a,b)
struct position *a,*b;
{
	if(a->value>b->value)
		return(1);
	else if(a->value<b->value)
		return(-1);
	else
		return(0);
}

static void
fkc(x,n,k,c,m,remove,eps) 
PREC x[],eps;
int n,m,remove;
double *k,c[];
{


	/* junk integers */
	int i,j;
	short int *ip;
	int memsize;

	int nobs;

	/* pointers */
	register struct position *pt;
	struct position *p;

	/* long counts */
	long 	count,tcount;
	/* double length */
	double dlength;
	double phi;

	register int ix,iy,ibit,ipos;


	nobs = n-remove;
	dlength = (double)nobs;

	/* allocate memory */
	if(first ) {
		/* mask = Calloc(2*n,int); */
		mask = (int *)calloc(2*n,sizeof(int));
		
		/* lookup = Calloc(TABLEN+1,int); */
		lookup = (int *)calloc(TABLEN+1,sizeof(int));

		/* postab = Calloc(n,struct position); */
		postab = (struct position *)calloc(n,sizeof(struct position));


		/* build start : grid pointers */
		/*start = Calloc(n+1,short int *);*/
		start = (short int **)calloc(n+1,sizeof(short int *));
		
		/* find out how big grid has to be */
		memsize = 0;
		for(i=0;i<=n;i++) 
			memsize += (n-i)/NBITS + 1;

		/* grid is defined as short (2 byte integers) */
		/* grid =  Calloc(memsize,short); */
		grid =  (short int *)calloc(memsize,sizeof(short));
		
	
		if(grid==NULL) {
			/* error("Out of memory\n"); */
			exit(-1);
		}
	

		start[0] = grid;
		for(i=1;i<=n;i++) 
			start[i] = start[i-1] + (n-i)/NBITS + 1;

		/* bit vector */
		bits[0] = 1;
		for(i=1;i<15;i++)
			bits[i] = (bits[i-1] << 1);

		/* table for bit countining */

		for(i=0;i<=TABLEN;i++){
			*(lookup+i) = 0;
			for(j=0;j<NBITS;j++)
				if( (i & bits[j])!=0)
					(*(lookup+i))++;
		}
	}
	/* end initialization */

	/* clear grid */
	for(ip=grid;ip<=start[n];ip++)
		*ip = 0;



	/* perform thieler sort */
	for(i=0;i<n;i++){
		(postab+i)->value = x[i];
		(postab+i)->pos   = i;
	}


	qsort((char *)postab,n,sizeof(struct position),comp);
	postlast = postab+n-1;

	/* start row by row construction */
	/* use theiler method */


	count = 0;
	phi   = 0;
	for(p=postab;p<=postlast;p++) {
		tcount = 0;
		pt = p ;
		/* count to right */
		while( (pt->value - p->value)<=eps) {
			GRIDON(p->pos,pt->pos);
			if( (p->pos<nobs)&&(pt->pos<nobs) )
				tcount++;
			if(pt==postlast)
				break;
			else
				pt++;
		}
		if(p!=postab){
			/* count to left : note - This is not
			necessary for building the grid, but
			it is needed to get the Dechert k */
			pt = p-1;
			while( (p->value - pt->value)<=eps) {
				if( (p->pos<nobs)&&(pt->pos<nobs) )
					tcount++;
				if(pt==postab)
					break;
				else
					pt--;
			}
		}
		count += tcount;
		/* Dechert speed up K */
		phi   += tcount*tcount;
	}
	/* adjust k and c to u statistic */
	count = count - nobs;
	phi   = phi - nobs - 3*count;

	*k    = ((double)phi)/(dlength*(dlength-1)*(dlength-2));
	c[1]  = ((double)count)/(dlength*(dlength-1));

	/* build mask */
	for(i=0;i<nobs;i++)
		genmask(i,n,NBITS,remove,mask+2*i);

	for(i=2;i<=m;i++) {
		embed(n,i);
		c[i] = evalc(nobs);
	}
	/*
	free(grid);
	free(mask);
	free(postab);
	free(start);
	*/
}

/*
	function version of gridon - this has been replaced
	with a macro for enhanced speed
*/

/*
void
gridon(ix,iy)
int ix,iy;
{
	int temp;
	int ipos,ibit;

	if(ix==iy)
		return;
	if(ix>iy){
		temp = ix;
		ix = iy;
		iy = temp;
	}
	iy = iy-ix-1;
	ipos = iy / NBITS;
	ibit = NBITS - 1 - (iy % NBITS);
	*(*(start+ix)+ipos) |= bits[ibit];
	if( *(*(start+ix)+ipos)<0)
		Rprintf("%d %d %d %d\n",ipos,ibit,ix,iy);
}

*/

/* 


friendly front end -  This main program is a friendly
front end program that calls the routines to calculate the
bds statistic.  It allows unix user to:
1.) have an easy to use command imediately
2.) see how to use the calling routines for calculations

Users doing montecarlo work will probably want to use
the subroutines directly.  

These routines are:

fkc(x,n,k,c,m,n,eps)
cstat(c,cm,k,m,n)
freeall()


fkc(x,n,k,c,m,mask,eps)

x = vector of series to test (double *), but it can be modified
    using the PREC definition.  Setting PREC to float or int, will
    allow the use of other types of series.  
n = length of series (int)
k = returned value of k (double *)
c = raw c values c[1],c[2],c[3].... (Note: the correct subscripts are used.)
    (double *)
m = maximum embedding - cstats will calculated for i=1 to m (int)
mask = number of points to ignore at the end of the series.
    Since the calculation of c(2) can effectively use more
    points then c(3), c(4) ..., often the last several points 
    are ignored so that all statistics are calculated on the
    same set of points.  ie. for m=3 we might only use x(1) through
    x(n-2) for the calculations of c(2) and c(3).   This is generally
    set to m-1 to allow all c to be estimated on a point set of
    n-m+1. (int)
eps = epsilon value for close points (double) or set to (PREC). 


cstat(c,cm,k,m,n)
This simple routine calculates the standard error and the normalized
bds stat.  It closely follows formulas in Brock Hsieh and LeBaron
on page 43.

c = c[1]    c for embedding 1 
cm = c[m]   c for embedding m
k  =  k stat
m  = embedding
n  = length of series


freeall()

The fkc algorithm allocates large amounts of memory.  This is
time consuming and for montecarlo simulations it is not desirable
to reallocate every time.  The routine can tell whether it needs
to reallocate.  For simulations fkc should be called repeatedly.
When the program is finally done freeall() should be called
to free all the allocated space.


This front end module can be removed from the begin front
end comment to the end front end comment.  The remaining
routines can be compiled as a stand alone library to be
called by other programs.
 

fkc_slow()

This extra routine is also included.  It is a slower algorithm
which performs exactly the same function as fkc.  Its only advantage
is that it is simpler and requires much less memory than the fast
algorithm.
To implement it just replace the call to fkc with fkc_slow() the
arguments are exactly the same.




*/

/* begin front end ---------------------------------- */

void bdstest_main (int *N, int *M, double *x, double *c, double *cstan, double *EPS, int *TRACE)
{
	int i;
	double k;
	
	int n, m;
	double eps;

	n = (*N);  
	m = (*M);
	eps = (*EPS);
	
	/* calculate raw c and k statistics : This is the hard part */
	fkc(x,n,&k,c,m,m-1,eps);

	/* calculate normalized stats:  This is the easy part */
	for(i=2; i<=m;i++) { 
		cstan[i] = cstat(c[1],c[i],k,i,n-m+1);
	}
	
	/* free allocated memory: This must be done when finished */
	freeall();
}

/* end front end ------------------------------------------*/

