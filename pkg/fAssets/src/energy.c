/*
   energy.c: energy package

   Author:  Maria Rizzo <mrizzo @ bgnet.bgsu.edu>
   Created: 4 Jan 2004 for R-1.8.1
   Revised: 20 March 2004 (E2, twosampleIEtest added)
   Revised: 13 June 2004 (distance() changed, some utilities added)

   mvnEstat()     computes the E-test of multivariate normality
   ksampleEtest() performs the multivariate E-test for equal distributions,
                  complete version, from data matrix
   twosampleIEtest()  incomplete version
   E2sample()     computes the 2-sample E-statistic without creating distance
   poisMstat()    computes the mean distance test of Poissonity
   sumdist()      sums the distance matrix without creating the matrix
*/

#include <R.h>
#include <Rmath.h>

void   mvnEstat(double *y, int *byrow, int *nobs, int *dim, double *stat);
void   poisMstat(int *x, int *nx, double *stat);
void   ksampleEtest(double *x, int *byrow, int *nsamples, int *sizes, int *dim,
            int *R, double *e0, double *e, double *pval);
void   twosampleIEtest(double *x, int *byrow, int *sizes, int *dim, int *iN,
                     int *R, double *e0, double *e, double *pval);
void   E2sample(double *x, int *sizes, int *dim, double *stat);

double edist(double **D, int m, int n);
double multisampleE(double **D, int nsamples, int *sizes, int *perm);
double twosampleE(double **D, int m, int n, int *xrows, int *yrows);
double E2(double **x, int *sizes, int *start, int ncol, int *perm);
double Eksample(double *x, int *byrow, int r, int d, int K, int *sizes, int *ix);
void   distance(double **bxy, double **D, int N, int d);
void   sumdist(double *x, int *byrow, int *nrow, int *ncol, double *lowersum);

double **alloc_matrix(int r, int c);
int    **alloc_int_matrix(int r, int c);
void   free_matrix(double **matrix, int r, int c);
void   free_int_matrix(int **matrix, int r, int c);
void   permute(int *J, int n);
void   roworder(double *x, int *byrow, int r, int c);
void   vector2matrix(double *x, double **y, int N, int d, int isroworder);



void mvnEstat(double *y, int *byrow, int *nobs, int *dim, double *stat)
{
    /*
      compute E test statistic for multivariate normality
      y is *standardized* multivariate sample
      best to have y in row order:  e.g. y=as.double(t(y))
    */

    int    d=(*dim), n=(*nobs);
    int    i, j, k, p, maxterms=2000;
    double D=(double)(*dim);
    double meanyy, meanyz, meanzz;
    double delta, eps=1.0e-7;
    double normy, yy, dif, sum, sum0, term;
    double lg0, lg1,logak, loggk;

    if (*byrow == FALSE)
        roworder(y, byrow, n, d);

    lg0 = lgammafn(D/2.0);
    lg1 = lgammafn((D+1.0)/2.0);
    meanzz = 2.0 * exp(lg1 - lg0);  /* second mean */

    meanyz = 0.0;    /* computing the first mean as series */
    for (i=0; i<n; i++) {
        yy = 0.0;
        p = i * d;
        for (j=0; j<d; j++) {
            dif = (*(y+p+j)) * (*(y+p+j));
            yy += dif;
        }
        normy = sqrt(yy);
        delta = 1.0;
        sum = 0.0;
        k = 0;
        while (delta > eps && k < maxterms) {
            sum0 = sum;
            logak = (k+1)*log(yy) - lgammafn(k+1) - k*M_LN2 -
                    log(2*k+1) - log(2*k+2);
            loggk = lg1 + lgammafn(k+1.5) - lgammafn(k+D/2+1);
            term = exp(logak + loggk);
            if (k % 2 == 0)
                sum += term;
                else
                    sum -= term;
            delta = fabs(sum - sum0);
            k++;
        }
        if (delta < eps)
            meanyz += meanzz/M_SQRT2 + M_SQRT_2dPI * sum;
            else {
                meanyz += normy;
                Rf_warning("E|y-Z| did not converge, replaced by %f", normy);
            }
    }
    meanyz /= (double) n;

    sumdist(y, byrow, nobs, dim, &meanyy);  /* computing third mean */
    meanyy *= (2.0/(double)(n*n));

    *stat = ((double) n)*(2.0*meanyz - meanzz - meanyy);
    return;
}

void poisMstat(int *x, int *nx, double *stat)
{
    /* computes the Poisson mean distance statistic */
    int i, j, k, n=(*nx);
    double eps=1.0e-10;
    double cvm, d, lambda, m, q;
    double Mcdf1, Mcdf0, Mpdf1, cdf1, cdf0;

    lambda = 0;
    for (i=0; i<n; i++)
        lambda += x[i];
    lambda /= ((double) n);
    q = qpois(1.0-eps, lambda, TRUE, FALSE) + 1;

    m = 0.0;
    for (j=0; j<n; j++) m += abs(x[j] - 1);
    m /= ((double) n);                   /* est of m_1 = E|1 - X| */
    Mcdf0 = (m + 1.0 - lambda) / 2.0;    /* M-est of F(0) */

    cdf0 = exp(-lambda);                 /* MLE of F(0) */
    d = Mcdf0 - cdf0;
    cvm = d * d * cdf0;   /* von Mises type of distance */

    for (i=1; i<q; i++) {
        m = 0;
        k = i + 1;
        for (j=0; j<n; j++) m += abs(x[j]-k);
        m /= ((double) n);  /* est of m_{i+1} = E|i+1 - X| */

        /* compute M-estimate of f(i) and F(i) */
        Mpdf1 = (m-(k-lambda)*(2.0*Mcdf0-1.0))/((double) 2.0*k);
        if (Mpdf1 < 0.0) Mpdf1 = 0.0;
        Mcdf1 = Mcdf0 + Mpdf1;
        if (Mcdf1 > 1) Mcdf1 = 1.0;

        cdf1 = ppois(i, lambda, TRUE, FALSE); /* MLE of F(i) */
        d = Mcdf1 - cdf1;
        cvm += d * d * (cdf1 - cdf0);

        cdf0 = cdf1;
        Mcdf0 = Mcdf1;
    }
    cvm *= n;
    *stat = cvm;
}


void E2sample(double *x, int *sizes, int *dim, double *stat) {
    /*
      compute test statistic *stat for testing H:F=G
      does not store distance matrix
      x must be in row order: x=as.double(t(x)) where
      x is pooled sample in matrix sum(en) by dim
    */
    int    m=sizes[0], n=sizes[1], d=(*dim);
    int    i, j, k, p, q;
    double dif, dsum, sumxx, sumxy, sumyy, w;

    sumxy = 0.0;
    for (i=0; i<m; i++) {
        p = i*d;
        for (j=m; j<m+n; j++) {
            dsum = 0.0;
            q = j*d;
            for (k=0; k<d; k++) {
                dif = *(x+p+k) - *(x+q+k);
                dsum += dif*dif;
            }
            sumxy += sqrt(dsum);
        }
    }
    sumxy /= (double)(m*n);
    sumxx = 0.0;
    for (i=1; i<m; i++) {
        p = i*d;
        for (j=0; j<i; j++) {
            dsum = 0.0;
            q = j*d;
            for (k=0; k<d; k++) {
                dif = *(x+p+k) - *(x+q+k);
                dsum += dif*dif;
            }
            sumxx += sqrt(dsum);
        }
    }
    sumxx /= (double)(m*m);  /* half the sum */
    sumyy = 0.0;
    for (i=m+1; i<m+n; i++) {
        p = i*d;
        for (j=m; j<i; j++) {
            dsum = 0.0;
            q = j*d;
            for (k=0; k<d; k++) {
                dif = *(x+p+k) - *(x+q+k);
                dsum += dif*dif;
            }
            sumyy += sqrt(dsum);
        }
    }
    sumyy /= (double)(n*n);  /* half the sum */
    w = (double)(m*n)/(double)(m+n);
    *stat = 2.0*w*(sumxy - sumxx - sumyy);
}

void ksampleEtest(double *x, int *byrow,
                  int *nsamples, int *sizes, int *dim,
                  int *R, double *e0, double *e, double *pval)
{
    /*
      exported for R energy package: E test for equal distributions
      x         the pooled sample (or distances)
      *byrow    logical, TRUE if x is stored by row
                pass x=as.double(t(x)) if *byrow==TRUE
      *nsamples number of samples
      *sizes    vector of sample sizes
      *dim      dimension of data in x (0 if x is distance matrix)
      *R        number of replicates for permutation test
      *e0       observed E test statistic
      e         vector of replicates of E statistic
      *pval     approximate p-value
    */

    int    b, ek, i, k;
    int    B = (*R), K = (*nsamples), d=(*dim), N;
    int    *n, *perm;
    double **data, **D;

    N = 0;
    for (k=0; k<K; k++)
        N += sizes[k];
    n = Calloc(K, int);
    perm = Calloc(N, int);
    for (i=0; i<N; i++)
        perm[i] = i;
    D   = alloc_matrix(N, N);      /* distance matrix */
    if (d > 0) {
        data = alloc_matrix(N, d); /* sample matrix */
        vector2matrix(x, data, N, d, *byrow);
        distance(data, D, N, d);
        free_matrix(data, N, d);
    }
    else
        vector2matrix(x, D, N, N, *byrow);

    *e0 = multisampleE(D, K, sizes, perm);

    /* bootstrap */
    if (B > 0) {
        ek = 0;
        GetRNGstate();
        for (b=0; b<B; b++) {
            permute(perm, N);
            e[b] = multisampleE(D, K, sizes, perm);
            if ((*e0) < e[b]) ek++;
        }
        PutRNGstate();
        (*pval) = ((double) ek) / ((double) B);
    }

    free_matrix(D, N, N);
    Free(perm);
    Free(n);

}

void twosampleIEtest(double *x, int *byrow, int *sizes, int *dim, int *iN,
                   int *R, double *e0, double *e, double *pval)
{
    /*
      exported for R energy package: incomplete E test for equal distributions
      x         the pooled sample
      *byrow    logical, TRUE if x is stored by row
                pass x=as.double(t(x)) if *byrow==TRUE
      sizes     vector of original sample sizes
      *dim      dimension of data in x
      *iN       incomplete sample size
      *R        number of replicates for permutation test
      *e0       observed E test statistic
      e         vector of replicates of E statistic
      *pval     approximate p-value
    */
    int    I[2], start[2];
    int    i, b, B = (*R), ek, nrow, ncol = (*dim), N = (*iN);
    int    *perm;
    double **data;

    nrow = sizes[0] + sizes[1];
    I[0] = sizes[0] > N ? N : sizes[0];
    I[1] = sizes[1] > N ? N : sizes[1];

    if (*byrow == FALSE)
        roworder(x, byrow, nrow, ncol);
    data = alloc_matrix(nrow, ncol);
    vector2matrix(x, data, nrow, ncol, *byrow);
    perm = Calloc(nrow, int);
    for (i=0; i<nrow; i++) perm[i] = i;
    start[0] = 0;
    start[1] = sizes[0];
    permute(perm + start[0], sizes[0]);
    permute(perm + start[1], sizes[1]);

    *e0 = E2(data, I, start, ncol, perm);
    if (B > 0) {
        ek = 0;
        for (b = 0; b < B; b++) {
            permute(perm, nrow);
            e[b] = E2(data, I, start, ncol, perm);
            if ((*e0) < e[b]) ek++;
        }
        *pval = (double) ek / (double) B;
    }
    Free(data);
    Free(perm);
    return;
}

void sumdist(double *x, int *byrow, int *nrow, int *ncol, double *lowersum)
{
    /*
       sum all pairwise distances between rows of x
       equivalent to this in R:  h <- sum(dist(x))
       x must be in row order: x=as.double(t(x))
    */

    int i, j, k, p, q, n=(*nrow), d=(*ncol);
    double sum, dsum, dif;
    if (*byrow == FALSE)
        roworder(x, byrow, n, d);
    sum = 0.0;
    for (i=1; i<n; i++) {
        p = i*d;
        for (j=0; j<i; j++) {
            dsum = 0.0;
            q = j*d;
            for (k=0; k<d; k++) {
                dif = *(x+p+k) - *(x+q+k);
                dsum += dif*dif;
            }
            sum += sqrt(dsum);
        }
    }
    (*lowersum) = sum;
}

double E2(double **x, int *sizes, int *start, int ncol, int *perm)
{
    int    m=sizes[0], n=sizes[1];
    int    row1=start[0], row2=start[1];
    int    i, j, k, p, q;
    double dif, dsum, sumxx, sumxy, sumyy, w;

    sumxy = 0.0;
    for (i=0; i<m; i++) {
        p = perm[row1 + i];
        for (j=0; j<n; j++) {
            dsum = 0.0;
            q = perm[row2 + j];
            for (k=0; k<ncol; k++) {
                dif = x[p][k] - x[q][k];
                dsum += dif * dif;
            }
            sumxy += sqrt(dsum);
        }
    }
    sumxy /= (double)(m * n);
    sumxx = 0.0;
    for (i=1; i<m; i++) {
        p = perm[row1 + i];
        for (j=0; j<i; j++) {
            dsum = 0.0;
            q = perm[row1 + j];
            for (k=0; k<ncol; k++) {
                dif = x[p][k] - x[q][k];
                dsum += dif * dif;
            }
            sumxx += sqrt(dsum);
        }
    }
    sumxx /= (double)(m * m);  /* half the sum */
    sumyy = 0.0;
    for (i=1; i<n; i++) {
        p = perm[row2 + i];
        for (j=0; j<i; j++) {
            dsum = 0.0;
            q = perm[row2 + j];
            for (k=0; k<ncol; k++) {
                dif = x[p][k] - x[q][k];
                dsum += dif * dif;
            }
            sumyy += sqrt(dsum);
        }
    }
    sumyy /= (double)(n * n);  /* half the sum */
    w = (double)(m * n)/(double)(m + n);
    return 2.0 * w * (sumxy - sumxx - sumyy);
}


double multisampleE(double **D, int nsamples, int *sizes, int *perm)
{
    /*
      returns the multisample E statistic
      D is square Euclidean distance matrix
      perm is a permutation of the row indices
    */
    int i, j, k, m, n;
    int *M;
    double e;

    M = Calloc(nsamples, int);
    M[0] = 0;
    for (k=1; k<nsamples; k++)
        M[k] = M[k-1] + sizes[k-1]; /* index where sample k begins */

    e = 0.0;
    for (i=0; i<nsamples; i++) {
        m = sizes[i];
        for (j=i+1; j<nsamples; j++) {
            n = sizes[j];
            e += twosampleE(D, m, n, perm+M[i], perm+M[j]);
        }
    }
    Free(M);
    return(e);
}

double twosampleE(double **D, int m, int n, int *xrows, int *yrows)
{
    /*
       return the e-distance between two samples
       corresponding to samples indexed xrows[] and yrows[]
       D is square Euclidean distance matrix
    */
    int    i, j;
    double sumxx=0.0, sumyy=0.0, sumxy=0.0;

    if (m < 1 || n < 1) return 0.0;
    for (i=0; i<m; i++)
        for (j=i+1; j<m; j++)
            sumxx += D[xrows[i]][xrows[j]];
    sumxx *= 2.0/((double)(m*m));
    for (i=0; i<n; i++)
        for (j=i+1; j<n; j++)
            sumyy += D[yrows[i]][yrows[j]];
    sumyy *= 2.0/((double)(n*n));
    for (i=0; i<m; i++)
        for (j=0; j<n; j++)
            sumxy += D[xrows[i]][yrows[j]];
    sumxy /= ((double) (m*n));

    return (double)(m*n)/((double)(m+n)) * (2*sumxy - sumxx - sumyy);
}

double edist(double **D, int m, int n)
{
    /*
      return the e-distance between two samples size m and n
      D is square Euclidean distance matrix
    */
    int    i, j;
    double sumxx=0.0, sumyy=0.0, sumxy=0.0;

    if (m < 1 || n < 1) return 0.0;
    for (i=0; i<m; i++)
        for (j=i+1; j<m; j++)
            sumxx += D[i][j];
    sumxx *= 2.0/((double)(m*m));
    for (i=0; i<n; i++)
        for (j=i+1; j<n; j++)
            sumyy += D[i][j];
    sumyy *= 2.0/((double)(n*n));
    for (i=0; i<m; i++)
        for (j=0; j<n; j++)
            sumxy += D[i][j];
    sumxy /= ((double) (m*n));
    return (double)(m*n)/((double)(m+n)) * (2*sumxy - sumxx - sumyy);
}


double **alloc_matrix(int r, int c)
{
    /* allocate a matrix with r rows and c columns */
    int i;
    double **matrix;
    matrix = Calloc(r, double *);
    for (i = 0; i < r; i++)
    matrix[i] = Calloc(c, double);
    return matrix;
}


int **alloc_int_matrix(int r, int c)
{
    /* allocate an integer matrix with r rows and c columns */
    int i;
    int **matrix;
    matrix = Calloc(r, int *);
    for (i = 0; i < r; i++)
    matrix[i] = Calloc(c, int);
    return matrix;
}

void free_matrix(double **matrix, int r, int c)
{
    /* free a matrix with r rows and c columns */
    int i;
    for (i = 0; i < r; i++) Free(matrix[i]);
    Free(matrix);
}

void free_int_matrix(int **matrix, int r, int c)
{
    /* free an integer matrix with r rows and c columns */
    int i;
    for (i = 0; i < r; i++) Free(matrix[i]);
    Free(matrix);
}

void permute(int *J, int n)
{
    /*
       permute the first n integers of J
       if n is length(J), equivalent to R:
            J <- rev(sample(J, length(J), replace=FALSE))
    */
    int i, j, j0, m=n;
    for (i=0; i<n-1; i++) {
        j = m * unif_rand();
        m--;
        j0 = J[j];
        J[j] = J[m];
        J[m] = j0;
    }
}

void vector2matrix(double *x, double **y, int N, int d, int isroworder) {
    /* copy a d-variate sample into a matrix, N samples in rows */
    int i, k;
    if (isroworder == TRUE) {
        for (k=0; k<d; k++)
            for (i=0; i<N; i++)
                y[i][k] = (*(x+i*d+k));
        }
    else {
        for (k=0; k<N; k++)
            for (i=0; i<d; i++)
                y[i][k] = (*(x+k*N+i));
        }
    return;
}

void distance(double **data, double **D, int N, int d) {
    /*
       compute the distance matrix of sample in N by d matrix data
       equivalent R code is:  D <- as.matrix(dist(data))
    */
    int    i, j, k;
    double dif;
    for (i=0; i<N; i++) {
        D[i][i] = 0.0;
        for (j=i+1; j<N; j++) {
            D[i][j] = 0.0;
            for (k=0; k<d; k++) {
                dif = data[i][k] - data[j][k];
                D[i][j] += dif*dif;
            }
            D[i][j] = sqrt(D[i][j]);
            D[j][i] = D[i][j];
        }
    }
    return;
}

void roworder(double *x, int *byrow, int r, int c) {
    /*
      utility to convert a vector from column order to row order
      assume that x is r by c matrix as a vector in column order
    */
    int    i, j, k, n=r*c;
    double *y;
    if (*byrow == TRUE) return;
    y = Calloc(n, double);
    i = 0;
    for (j=0; j<r; j++) {
        for (k=0; k<n; k+=r) {
            y[i] = x[k+j];
            i++;
        }
    }
    for (i=0; i<n; i++)
        x[i] = y[i];
    Free(y);
    *byrow = TRUE;
    return;
}
