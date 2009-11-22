#include <S.h>
#include <math.h>
#include <R_ext/Applic.h> /* for dgemm */

#define RANDIN  seed_in((long *)NULL)
#define RANDOUT seed_out((long *)NULL)
#define UNIF unif_rand()
#define S_EVALUATOR

void bootstrap(double *x, double *y, int *n, int *xlen, int *nboot, double *pboot, double *bootcor, int *rarray, int *rmat, double *xdif, double *ydif)

{

int i, j, k, l;
double r;
double nsamp;
double xmean, ymean;
double xsum;
double xxsum, yysum;

S_EVALUATOR

/* Set random seed using Splus function */

RANDIN;


for(i = 0; i < *nboot; i++) {

/* Set up rarray. */

   for(j = 0; j < *n; j++) {
      r = UNIF;
      if(r > *pboot)
         rarray[j] = 0;
      else rarray[j] = 1;
   }

/* Turn rarray into a lower-triangular sampling matrix. */
/* 1 means include, 0 means omit. */

   l = 0;
   for(j = 1; j < *n; j++) {
      for(k = 0; k < j; k++) {
         if(rarray[j] == 0 || rarray[k] == 0)
            rmat[l] = 0;
         else rmat[l] = 1;
         l++;
      }
   }


   nsamp = 0;
   for(j = 0; j < *xlen; j++) {
      nsamp += rmat[j];
   }


/* Calculate means for x and y. */

   xmean = 0;
   ymean = 0;
   for(j = 0; j < *xlen; j++) {
      if(rmat[j] == 1) {
         xmean += x[j];
         ymean += y[j];
      }
   }
   xmean = xmean/nsamp;
   ymean = ymean/nsamp;

/* Calculate deviations for x and y. */

   for(j = 0; j < *xlen; j++) {
      if(rmat[j] == 1) {
         xdif[j] = x[j] - xmean;
         ydif[j] = y[j] - ymean;
      }
      else {
         xdif[j] = 0;
         ydif[j] = 0;
      }
   }

   xsum = 0;
   xxsum = 0; 
   yysum = 0;

   for(j = 0; j < *xlen; j++) {
      if(rmat[j] == 1) {
         xsum += (xdif[j] * ydif[j]);
         xxsum += (xdif[j] * xdif[j]);
         yysum += (ydif[j] * ydif[j]);
      }
   }

   bootcor[i] = (xsum) / sqrt(xxsum * yysum);

}

/* Reset random seed using an Splus function. */

RANDOUT;

}


void permute(double *x, double *y, int *n, int *xlen, int *nperm, double *zstats, double *tmat, int *rarray)

{

int i, k, l, m;
double cumsum;
int temp;

S_EVALUATOR

/* Set random seed using Splus function */

RANDIN;

/* Calculate first z-statistic (unpermuted data). */

cumsum = 0;

for(k = 0; k < *xlen; k++) {
   cumsum += x[k] * y[k];
}

zstats[0] = cumsum / *xlen;

/* Start permutation routine */

for(i = 1; i < *nperm; i++) {

/* Set up rarray. */

   for(k = 0; k < *n; k++) {
      rarray[k] = k;
   }

/* Convert x to a full matrix. */

   m = 0;
   for(k = 1; k < *n; k++) {
      for(l = 0; l < k; l++) {
         tmat[k * *n + l] = x[m];
         tmat[l * *n + k] = x[m];
         m++;
      }
   }

/* Randomize rarray using an Splus function. */

   for(k = 0; k < (*n - 1); k++) {
      l = *n - k - 1;
      m = (int)((float)l * UNIF);
      if(m > l) m = l;
      temp = rarray[l];
      rarray[l] = rarray[m];
      rarray[m] = temp;
   }

/* Reorder x. */

   m = 0;
   for(k = 1; k < *n; k++) {
      for(l = 0; l < k; l++) {
         x[m] = tmat[rarray[k] * *n + rarray[l]];
         m++;
      }
   }


/* Calculate new sum of products. */

   cumsum = 0;

   for(k = 0; k < *xlen; k++) {
         cumsum += x[k] * y[k];
   
   }

   zstats[i] = cumsum / *xlen;

}

/* Reset random seed using an Splus function. */

RANDOUT;

}



void permpart(double *hmat, double *bmat, double *omat, double *y, double *xcor, double *ycor, int *n, int *ncol, int *xlen, int *nperm, double *zstats, double *tmat, int *rarray)

{

int i, k, l, m;
double cumsum;
double bsum;
double w1, w2;
int temp;

S_EVALUATOR

/* Set random seed using Splus function */

RANDIN;

/* Calculate first z-statistic (unpermuted data). */

cumsum = 0;

for(k = 0; k < *xlen; k++) {
   cumsum += xcor[k] * ycor[k];
}

zstats[0] = cumsum / *xlen;


/* Start permutation routine */

for(i = 1; i < *nperm; i++) {

/* Set up rarray. */

   for(k = 0; k < *n; k++) {
      rarray[k] = k;
   }


/* Convert y to a full matrix. */

   m = 0;
   for(k = 1; k < *n; k++) {
      for(l = 0; l < k; l++) {
         tmat[k * *n + l] = y[m];
         tmat[l * *n + k] = y[m];
         m++;
      }
   }

/* Randomize rarray using an Splus function. */

   for(k = 0; k < (*n - 1); k++) {
      l = *n - k - 1;
      m = (int)((float)l * UNIF);
      if(m > l) m = l;
      temp = rarray[l];
      rarray[l] = rarray[m];
      rarray[m] = temp;
   }


/* Reorder y. */

   m = 0;
   for(k = 1; k < *n; k++) {
      for(l = 0; l < k; l++) {
         y[m] = tmat[rarray[k] * *n + rarray[l]];
         m++;
      }
   }


/* Calculate residuals for y */

/* Calculate bmat */

for(k = 0; k < *ncol; k++) {
   bmat[k] = 0;
}

for(k = 0; k < *ncol; k++) {
   for(l = 0; l < *xlen; l++) {
      bmat[k] = bmat[k] + hmat[l * *ncol + k] * y[l];
   }
}

/* Calculate ycor (residuals) */

for(k = 0; k < *xlen; k++) {
   ycor[k] = 0;
}

for(k = 0; k < *xlen; k++) {
   bsum = 0;
   for(l = 0; l < *ncol; l++) {
      bsum = bsum + bmat[l] * omat[l * *xlen + k];
   }
   ycor[k] = y[k] - bsum;
}


/* Standardize residuals so z = r */

w1 = 0;
w2 = 0;

for(k = 0; k < *xlen; k++) {
   w1 = w1 + ycor[k];
   w2 = w2 + ycor[k] * ycor[k];
}
w1 = w1 / *xlen;
w2 = sqrt(w2 / *xlen - w1 * w1);
for(k = 0; k < *xlen; k++) {
   ycor[k] = (ycor[k] - w1) / w2;
}

/* Calculate new sum of products. */

   cumsum = 0;

   for(k = 0; k < *xlen; k++) {
      cumsum += xcor[k] * ycor[k];
   }

   zstats[i] = cumsum / *xlen;

}

/* Reset random seed using an Splus function. */

RANDOUT;

}


void xbootstrap(double *x, double *y, int *n, int *xlen, int *nboot, double *pboot, double *bootcor, int *rarray, int *rmat, double *xdif, double *ydif)

{

int i, j, k;
double r;
double nsamp;
double xmean, ymean;
double xsum;
double xxsum, yysum;

S_EVALUATOR

/* Set random seed using Splus function */

RANDIN;


for(i = 0; i < *nboot; i++) {

/* Set up rarray. */

   for(j = 0; j < *n; j++) {
      r = UNIF;
      if(r > *pboot)
         rarray[j] = 0;
      else rarray[j] = 1;
   }

/* Turn rarray into a square sampling matrix. */
/* 1 means include, 0 means omit. */

   for(j = 0; j < *xlen; j++) {
      rmat[j] = 1;
   }

   for(j = 0; j < *n; j++) {
      for(k = 0; k <= j; k++) {
         if(rarray[j] == 0 || rarray[k] == 0) {
            rmat[j * *n + k] = 0;
            rmat[k * *n + j] = 0;
         }
      }
   }

   nsamp = 0;
   for(j = 0; j < *xlen; j++) {
      nsamp += rmat[j];
   }


/* Calculate means for x and y. */

   xmean = 0;
   ymean = 0;
   for(j = 0; j < *xlen; j++) {
      if(rmat[j] == 1) {
         xmean += x[j];
         ymean += y[j];
      }
   }
   xmean = xmean/nsamp;
   ymean = ymean/nsamp;

/* Calculate deviations for x and y. */

   for(j = 0; j < *xlen; j++) {
      if(rmat[j] == 1) {
         xdif[j] = x[j] - xmean;
         ydif[j] = y[j] - ymean;
      }
      else {
         xdif[j] = 0;
         ydif[j] = 0;
      }
   }


   xsum = 0;
   xxsum = 0; 
   yysum = 0;

   for(j = 0; j < *xlen; j++) {
      if(rmat[j] == 1) {
         xsum += (xdif[j] * ydif[j]);
         xxsum += (xdif[j] * xdif[j]);
         yysum += (ydif[j] * ydif[j]);
      }
   }

   bootcor[i] = (xsum) / sqrt(xxsum * yysum);

}

/* Reset random seed using an Splus function. */

RANDOUT;

}



void xpermute(double *x, double *y, int *n, int *xlen, int *nperm, double *zstats, double *tmat, int *rarray)

{

int i, k, l, m;
double cumsum;
int temp;

S_EVALUATOR

/* Set random seed using Splus function */

RANDIN;

/* Calculate first z-statistic (unpermuted data). */

cumsum = 0;

for(k = 0; k < *xlen; k++) {
   cumsum += x[k] * y[k];
}

zstats[0] = cumsum;


/* Start permutation routine */

for(i = 1; i < *nperm; i++) {

/* Set up rarray. */

   for(k = 0; k < *n; k++) {
      rarray[k] = k;
   }

/* Randomize rarray using an Splus function. */

   for(k = 0; k < (*n - 1); k++) {
      l = *n - k - 1;
      m = (int)((float)l * UNIF);
      if(m > l) m = l;
      temp = rarray[l];
      rarray[l] = rarray[m];
      rarray[m] = temp;
   }

/* Reorder x. */

   for(k = 0; k < *n; k++) {
      for(l = 0; l <= k; l++) {
         x[k * *n + l] = tmat[rarray[k] * *n + rarray[l]];
         x[l * *n + k] = tmat[rarray[l] * *n + rarray[k]];
      }
   }


/* Calculate new sum of products. */

   cumsum = 0;

   for(k = 0; k < *xlen; k++) {
         cumsum += x[k] * y[k];
   
   }

   zstats[i] = cumsum;

}

/* Reset random seed using an Splus function. */

RANDOUT;

}




void xpermpart(double *hmat, double *y, double *xcor, double *ycor, int *n, int *xlen, int *nperm, double *zstats, double *tmat, int *rarray)

{

int i, k, l, m;
double cumsum;
int temp;

S_EVALUATOR

/* Set random seed using Splus function */

RANDIN;

/* Calculate residuals for y */

for(k = 0; k < *xlen; k++) {
   ycor[k] = 0;
}


for(k = 0; k < *xlen; k++) {
   for(l = 0; l < *xlen; l++) {
      ycor[k] = ycor[k] + hmat[k * *xlen + l] * y[l];
   }
}


/* Calculate first z-statistic (unpermuted data). */

cumsum = 0;

for(k = 0; k < *xlen; k++) {
   cumsum += xcor[k] * ycor[k];
}

zstats[0] = cumsum;


/* Start permutation routine */

for(i = 1; i < *nperm; i++) {

/* Set up rarray. */

   for(k = 0; k < *n; k++) {
      rarray[k] = k;
   }

/* Randomize rarray using an Splus function. */

   for(k = 0; k < (*n - 1); k++) {
      l = *n - k - 1;
      m = (int)((float)l * UNIF);
      if(m > l) m = l;
      temp = rarray[l];
      rarray[l] = rarray[m];
      rarray[m] = temp;
   }

/* Reorder y. */

   for(k = 0; k < *n; k++) {
      for(l = 0; l <= k; l++) {
         y[k * *n + l] = tmat[rarray[k] * *n + rarray[l]];
         y[l * *n + k] = tmat[rarray[l] * *n + rarray[k]];
      }
   }


/* Calculate residuals for y */

for(k = 0; k < *xlen; k++) {
   ycor[k] = 0;
}

for(k = 0; k < *xlen; k++) {
   for(l = 0; l < *xlen; l++) {
      ycor[k] = ycor[k] + hmat[k * *xlen + l] * y[l];
   }
}


/* Calculate new sum of products. */

   cumsum = 0;

   for(k = 0; k < *xlen; k++) {
         cumsum += xcor[k] * ycor[k];
   
   }

   zstats[i] = cumsum;

}

/* Reset random seed using an Splus function. */

RANDOUT;

}



void bcdist(double *x, int *pnrow, int *pncol, double *dist)
{
int i, j, k, l;
int nrow, ncol;
double sumi, sumj;
double minsum;


l = 0;
nrow = *pnrow;
ncol = *pncol;

for(i = 0; i < (nrow - 1); i++) {
   for(j = (i + 1); j < (nrow); j++) {
      minsum = 0;
      sumi = 0;
      sumj = 0;
      for(k = 0; k < ncol; k++) {
         if(x[i * ncol + k] < x[j * ncol + k]) 
            minsum += x[i * ncol + k];
         else 
            minsum += x[j * ncol + k];
         sumi += x[i * ncol + k];
         sumj += x[j * ncol + k]; 
      }
   if((sumi + sumj) == 0) 
      dist[l] = 0;
   else
      dist[l] = (1 - (2 * minsum) / (sumi + sumj));
   l++;
   }
}
}


void weight(int *n, double *datadist, double *d1, double *d2, double *w)

{
int i;
double m1, m2;
double w1, w2;
double pi;

pi = 2 * acos(0);

for(i = 0; i < *n * *n; i++) {
   if(datadist[i] != 0) {
      if(d1[i] < datadist[i])
         m1 = d1[i] / datadist[i];
      else m1 = 1;

      if(d2[i] < datadist[i])
         m2 = d2[i] / datadist[i];
      else m2 = 1;
   }
   else {
      m1 = 0;
      m2 = 0;
   }

   w1 = 1 - (acos(m1) + acos(m2)) / pi;

   if(datadist[i] != 0) {
      m1 = d1[i] / datadist[i];
      if(m1 > 1)
         m1 = 1;

      m2 = d2[i] / datadist[i];
      if(m2 > 1)
         m2 = 1;
   }
   else {
      m1 = 0;
      m2 = 0;
   }

   w2 = 0.75 - (acos(m1) + acos(m2)) / (2 * pi);

   if((datadist[i] * datadist[i]) >= (d1[i] * d1[i] + d2[i] * d2[i]))
      w1 = 0;
   if((datadist[i] * datadist[i]) < (d1[i] * d1[i] + d2[i] * d2[i]))
      w2 = 0;

   w[i] = w1 + w2;
}
}

void newpermone(double *x, int *dclass, int *n, int *xlen, int *nperm, double *zstats, double *tmat, int *rarray)

{

int i, k, l, m;
double cumsum;
int temp;

S_EVALUATOR

/* Set random seed using Splus function */

RANDIN;

/* Calculate first z-statistic (unpermuted data). */

cumsum = 0;

for(k = 0; k < *xlen; k++) {
	if(dclass[k] == 0) {
   	cumsum += x[k];
	}
}

zstats[0] = cumsum;

/* Start permutation routine */

for(i = 1; i < *nperm; i++) {

/* Set up rarray. */

   for(k = 0; k < *n; k++) {
      rarray[k] = k;
   }

/* Convert x to a full matrix. */

   m = 0;
   for(k = 1; k < *n; k++) {
      for(l = 0; l < k; l++) {
         tmat[k * *n + l] = x[m];
         tmat[l * *n + k] = x[m];
         m++;
      }
   }

/* Randomize rarray using an Splus function. */

   for(k = 0; k < (*n - 1); k++) {
      l = *n - k - 1;
      m = (int)((float)l * UNIF);
      if(m > l) m = l;
      temp = rarray[l];
      rarray[l] = rarray[m];
      rarray[m] = temp;
   }

/* Reorder x. */

   m = 0;
   for(k = 1; k < *n; k++) {
      for(l = 0; l < k; l++) {
         x[m] = tmat[rarray[k] * *n + rarray[l]];
         m++;
      }
   }


/* Calculate new sum of products. */

   cumsum = 0;

   for(k = 0; k < *xlen; k++) {
      if(dclass[k] == 0) {
          cumsum += x[k];
      }
   }

   zstats[i] = cumsum;

}

/* Reset random seed using an Splus function. */

RANDOUT;

}




void newpermtwo(double *x, double *y, int *n, int *xlen, int *nperm, double *zstats, double *tmat, int *rarray)

{

int i, k, l, m;
double cumsum;
int temp;
float naval = -9999;

S_EVALUATOR

/* Set random seed using Splus function */

RANDIN;

/* Calculate first z-statistic (unpermuted data). */

cumsum = 0;

for(k = 0; k < *xlen; k++) {
	if(x[k] != naval) {
	   cumsum += x[k] * y[k];
	}
}

zstats[0] = cumsum;

/* Start permutation routine */

for(i = 1; i < *nperm; i++) {

/* Set up rarray. */

   for(k = 0; k < *n; k++) {
      rarray[k] = k;
   }

/* Convert x to a full matrix. */

   m = 0;
   for(k = 1; k < *n; k++) {
      for(l = 0; l < k; l++) {
         tmat[k * *n + l] = x[m];
         tmat[l * *n + k] = x[m];
         m++;
      }
   }

/* Randomize rarray using an Splus function. */

   for(k = 0; k < (*n - 1); k++) {
      l = *n - k - 1;
      m = (int)((float)l * UNIF);
      if(m > l) m = l;
      temp = rarray[l];
      rarray[l] = rarray[m];
      rarray[m] = temp;
   }

/* Reorder x. */

   m = 0;
   for(k = 1; k < *n; k++) {
      for(l = 0; l < k; l++) {
         x[m] = tmat[rarray[k] * *n + rarray[l]];
         m++;
      }
   }


/* Calculate new sum of products. */

   cumsum = 0;

   for(k = 0; k < *xlen; k++) {
      if(x[k] != naval) {
          cumsum += x[k] * y[k];
      }
   }
	
   zstats[i] = cumsum;

}

/* Reset random seed using an Splus function. */

RANDOUT;

}




void psum(double *x, int *pnrow, int *pncol, double *dist)
{
int row1, row2, col1;
int nrow, ncol;
int l;
double thisval, thatval;

l = 0;
nrow = *pnrow;
ncol = *pncol;

  for(col1 = 0; col1 < ncol; col1++) {
	for(row1 = 0; row1 < nrow; row1++) {
		thatval = x[row1 * ncol + col1];
		for(row2 = 0; row2 < nrow; row2++) {
			thisval = x[row2 * ncol + col1];
			dist[l] = thisval + thatval;
			l++;
		}
	}
}

}
    

void pdiff(double *x, int *pnrow, int *pncol, double *dist)
{
int row1, row2, col1;
int nrow, ncol;
int l;
double thisval, thatval;

l = 0;
nrow = *pnrow;
ncol = *pncol;

  for(col1 = 0; col1 < ncol; col1++) {
	for(row1 = 0; row1 < nrow; row1++) {
		thatval = x[row1 * ncol + col1];
		for(row2 = 0; row2 < nrow; row2++) {
			thisval = x[row2 * ncol + col1];
			dist[l] = thisval - thatval;
			l++;
		}
	}
}

}
    

void jpres(double *x, int *pnrow, int *pncol, double *dist)
{
int row1, row2, col1;
int nrow, ncol;
int l;
double thisval, thatval;

l = 0;
nrow = *pnrow;
ncol = *pncol;

  for(col1 = 0; col1 < ncol; col1++) {
	for(row1 = 0; row1 < nrow; row1++) {
		thatval = x[row1 * ncol + col1];
		for(row2 = 0; row2 < nrow; row2++) {
			thisval = x[row2 * ncol + col1];
			if((thisval > 0) & (thatval > 0)) {
				dist[l] = 1;
			}
			else {
				dist[l] = 0;
			}
			l++;
		}
	}
}

}
    

void jabs(double *x, int *pnrow, int *pncol, double *dist)
{
int row1, row2, col1;
int nrow, ncol;
int l;
double thisval, thatval;

l = 0;
nrow = *pnrow;
ncol = *pncol;

  for(col1 = 0; col1 < ncol; col1++) {
	for(row1 = 0; row1 < nrow; row1++) {
		thatval = x[row1 * ncol + col1];
		for(row2 = 0; row2 < nrow; row2++) {
			thisval = x[row2 * ncol + col1];
			if((thisval == 0) & (thatval == 0)) {
				dist[l] = 1;
			}
			else {
				dist[l] = 0;
			}
			l++;
		}
	}
}

}
    

void jfirst(double *x, int *pnrow, int *pncol, double *dist)
{
int row1, row2, col1;
int nrow, ncol;
int l;
double thisval, thatval;

l = 0;
nrow = *pnrow;
ncol = *pncol;

  for(col1 = 0; col1 < ncol; col1++) {
	for(row1 = 0; row1 < nrow; row1++) {
		thatval = x[row1 * ncol + col1];
		for(row2 = 0; row2 < nrow; row2++) {
			thisval = x[row2 * ncol + col1];
			if((thisval > 0) & (thatval == 0)) {
				dist[l] = 1;
			}
			else {
				dist[l] = 0;
			}
			l++;
		}
	}
}

}
    

void jsec(double *x, int *pnrow, int *pncol, double *dist)
{
int row1, row2, col1;
int nrow, ncol;
int l;
double thisval, thatval;

l = 0;
nrow = *pnrow;
ncol = *pncol;

  for(col1 = 0; col1 < ncol; col1++) {
	for(row1 = 0; row1 < nrow; row1++) {
		thatval = x[row1 * ncol + col1];
		for(row2 = 0; row2 < nrow; row2++) {
			thisval = x[row2 * ncol + col1];
			if((thisval == 0) & (thatval > 0)) {
				dist[l] = 1;
			}
			else {
				dist[l] = 0;
			}
			l++;
		}
	}
}

}


void mrmperm(double *x, double *y, int *p, int *nd, int *n, int *nperm, double *r2all, double *ball, double *fall, double *tmat, int *rarray, double *XX, double *XY, double *YY, double *b)

{

int i, k, l;
int m;
int temp;
double SSE=0.0, SSTO=0.0, SSR=0.0;
double r2=0, f=0;
double btemp=0.0;
int bcount = 0;
char *transt = "T", *transn = "N";
double one = 1.0, zero = 0.0;
int onei = 1;

S_EVALUATOR

/* Set random seed using Splus function */

RANDIN;

/* Start permutation routine */
for(i = 0; i < *nperm; i++) {

/* first do the unpermuted values */

/*	F77_CALL(dgemm)(transa, transb, &ncx, &ncy, &nrx, &one,
			x, &nrx, y, &nry, &zero, z, &ncx); */

/* take crossproduct t(X) %*% Y - WORKS */
    F77_CALL(dgemm)(transt, transn, 
            p, &onei, nd, 
            &one, x, nd, y, nd, 
            &zero, XY, p);

/* take crossproduct t(Y) %*% (Y) - WORKS */
    F77_CALL(dgemm)(transt, transn, 
            &onei, &onei, nd, 
            &one, y, nd, y, nd, 
            &zero, YY, &onei);

/* calculate regression coefficients XX %*% XY - WORKS */
    F77_CALL(dgemm)(transn, transn, 
            p, &onei, p, 
            &one, XX, p, XY, p, 
            &zero, b, p);

/* calculate regression components - WORKS */
    F77_CALL(dgemm)(transt, transn, 
            &onei, &onei, p, 
            &one, b, p, XY, p, 
            &zero, &btemp, &onei);

/* SSE - WORKS */    
    SSE = YY[0] - btemp;

/* SSTO - WORKS */  
    SSTO = 0;
    for(k = 0; k < *nd; k++) {
        SSTO = SSTO + y[k];
    }
    SSTO = YY[0] - (SSTO * SSTO) / *nd;

    SSR = SSTO - SSE;

    /* calculate R2 - WORKS */
    r2 = 1 - SSE / SSTO;

    /* calculate F* - WORKS */
    f = (SSR / (*p - 1)) / (SSE / (*nd - *p));

    r2all[i] = r2;
    fall[i] = f;

    /* calculate pseudo-t for regression coefficients - WORKS*/
    /* b / sqrt(1 - R2) */
    for(k=0; k<*p; k++) {
        ball[bcount] = b[k] / sqrt(1 - r2);
        bcount++;
    }


/* permute Y */
/* Set up rarray. */

   for(k = 0; k < *n; k++) {
      rarray[k] = k;
   }

/* Convert y to a full matrix. */

   m = 0;
   for(k = 1; k < *n; k++) {
      for(l = 0; l < k; l++) {
         tmat[k * *n + l] = y[m];
         tmat[l * *n + k] = y[m];
         m++;
      }
   }

/* Randomize rarray using an Splus function. */

   for(k = 0; k < (*n - 1); k++) {
      l = *n - k - 1;
      m = (int)((float)l * UNIF);
      if(m > l) m = l;
      temp = rarray[l];
      rarray[l] = rarray[m];
      rarray[m] = temp;
   }

/* Reorder y. */

   m = 0;
   for(k = 1; k < *n; k++) {
      for(l = 0; l < k; l++) {
         y[m] = tmat[rarray[k] * *n + rarray[l]];
         m++;
      }
   }

}

/* Reset random seed using an Splus function. */

RANDOUT;

}


