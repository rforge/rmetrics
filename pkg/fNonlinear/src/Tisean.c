/* -------------------------------------------------------------------------- */

#ifndef tseriesChaos_h
#define tseriesChaos_h


#include <math.h>
#include <R.h>
#define sqr(a) (a)*(a)
#define MIN(a,b) (a)<(b) ? (a) : (b)
#define MAX(a,b) (a)>(b) ? (a) : (b)


#endif


/* -------------------------------------------------------------------------- */


void C2(double *in_series, int *in_m, int *in_d, int *in_length, 
    int *in_t, double *in_eps, double *out) 
{
    double *series; 
    double eps, tmp;
    int m, d, length;
    long blength;
    int i, j, w, t, md;
    
    series = in_series; 
    m = *in_m;
    d = *in_d; 
    t = *in_t;
    eps = *in_eps; 
    eps = sqr(eps);
    length = *in_length;
    blength = length - (m-1)*d;
    
    md = m*d; *out=0;
    for(i=0; i<blength-t; i++) for(j=i+t; j<blength; j++) {
      tmp=0.0;
      for(w=0; (w<md) && (tmp<eps); w+=d) 
        tmp+=sqr(series[i+w]-series[j+w]);
      *out += (tmp<eps);
    }
    *out /= ( ((double)blength-t+1)*((double)blength-t)/2.0 );
}


/* -------------------------------------------------------------------------- */


/*
Sample correlation integral for multiple length scales and multiple 
    embedding dimensions.
    
in_series:          input time series
in_length:          time series length
in_m, in_d, in_t:   max embedding dimension, time delay and theiler window
in_neps:            number of length scales to evaluate
in_epsM:            max length scale
in_epsm:            min length scale
out:                matrix of results
*/


#define output1(i,j) out[(j)+(i)*neps]


void d2(double *in_series, int *in_length, int *in_m, int *in_d, int *in_t, 
    int *in_neps, double *in_epsM, double *in_epsm, double *out)
{
    double tmpd, **hist;
    int i,j,w;
    int length, m,d,t, neps, blength;
    double *series, epsM, epsm;
    double a, lepsM;

    series = in_series;
    length = *in_length;
    m = *in_m;
    d = *in_d;
    t = *in_t;
    neps = *in_neps;
    epsm = sqr(*in_epsm);
    epsM = sqr(*in_epsM);
    blength = length -(m-1)*d;
    lepsM = log(epsM);
    a = log(epsm/epsM)/(double)(neps-1);
    hist = (double**) R_alloc(m, sizeof(double*));
    for(i=0; i<m; i++) {
        hist[i] = (double*) R_alloc(neps, sizeof(double));
        for(j = 0; j<neps; j++)
            output1(i,j) = hist[i][j] = 0.0;
    }

    for(i = 0; i<(blength-t); i++) {
        R_CheckUserInterrupt();
        for(j=i+t; j<blength; j++) {
            tmpd = 0.0;
            for(w=0; w<m; w++) {
                tmpd += sqr(series[i+w*d] - series[j+w*d]);
                hist[w][MIN((long) ( (log(tmpd) - lepsM )/a ), neps-1 )] ++;
            }
        }
    }

    for(i=0; i<m; i++)
        for(j = 0; j<neps; j++)
            output1(i,j) = hist[i][j];
}


/* -------------------------------------------------------------------------- */


void falseNearest(double *in_series, int *in_length, int *in_m, int *in_d, 
int *in_t, double *in_eps, double *in_rt, double *out, int *out2) 
{

double eps, *series; 
double dst;
int m,d, t, length, blength;
int num, denum;
int i,j,w,md;
double rt;
int id;

/*
BIND PARAMETERS
*/
    m = *in_m;
    d = *in_d;
    t = *in_t;
    rt = *in_rt;
    eps=*in_eps;
    series=in_series;
    length=*in_length;
/**/
    blength = length - m*d - t;
    md = m*d;
    eps = sqr(eps);
    num=denum=0;
    
    for(i = 0; i<blength; i++) {
        id=0;
        for(j=0; j<blength; j++) {
            if(((i-t)<=j) && (j<=(i+t))) continue;
            dst=0.0;            
            for(w=0; (w<md) && (dst<eps); w+=d)
                dst += sqr(series[i+w] - series[j+w]);
            if(dst>=eps) continue;
            id++;
            dst = ( dst + sqr(series[i+w+d] - series[j+w+d]) )/ dst;
            if (dst>rt) num++;
        }
        denum+=id;      
    }
    (*out) = (double)num/(double)denum;
    (*out2)= denum;
}


/* -------------------------------------------------------------------------- */


#define output2(a,b) out[(b)*ref + (a)]


void find_nearest(double *in_series, int *in_m, int *in_d, int *in_t,
    int *in_length, double *in_eps, int *in_ref, int *in_k, int *in_s, 
    int *out) 
{

double eps, *series; 
int m,d, t, s, ref, k, length, blength;

int i,j,w,md;
double *dsts;
int id; int *ids;

/*
BIND PARAMETERS
*/
    m = *in_m;
    d = *in_d;
    t = *in_t;
    s = *in_s;
    ref=*in_ref;
    k = *in_k;
    eps=*in_eps;
    series=in_series;
    length=*in_length;
/**/

    blength = length - (m-1)*d - s;
    md = m*d;
    for(i = 0; i<ref; i++) 
        for(j=0; j<k; j++) 
            output2(i,j) = -1;
    dsts = (double*) R_alloc(blength, sizeof(double));
    ids = (int*) R_alloc(blength, sizeof(int));
    eps = sqr(eps);

    for(i = 0; i<ref; i++) {
        id=0;
        for(j=0; j<blength; j++) {
            if(((i-t)<=j) && (j<=(i+t))) continue;
            dsts[id]=0.0;
            for(w=0; (w<md) && (dsts[id]<eps); w+=d)
                dsts[id] += sqr(series[i+w] - series[j+w]);
            if(dsts[id]>=eps) continue;
            ids[id] = j;
            id++;
        }
        R_qsort_I(dsts, ids, 1, id);
        for(j=0; (j<k) && (j<id); j++)
            output2(i, j) = ids[j]+1;
    }
}


/* -------------------------------------------------------------------------- */


void follow_points(double *in_series, int *in_m, int *in_d, 
int *in_length, int *in_nref, int *in_totref, int *in_k, 
int *in_s, int *in_nearest, int *in_ref, double *lyap)
{

double *series; 
int m,d, s, nref, totref, k, length, *ref;

int i,j,a,b,w, time;
double tmp, res;
int **nearest;

/*
BIND PARAMETERS
*/
    m = *in_m;
    d = *in_d;
    s = *in_s;
    nref=*in_nref;
    totref=*in_totref;
    ref = in_ref;
    k = *in_k;
    series=in_series;
    length=*in_length;
    nearest= (int**) R_alloc(totref, sizeof(int*));
    for(i = 0; i<totref; i++) {
        nearest[i] = (int*) R_alloc(k, sizeof(int));
        for (j=0; j<k; j++) 
            nearest[i][j] = in_nearest[j*totref+i];
    }
    for(j=0; j<s; j++) lyap[j] = 0.0;
/**/

    for(time=0; time<s; time++) {
        for(i=0; i<nref; i++) { 
            tmp = 0.0;
            for(j=0; j<k; j++) {
                a = ref[i]+time-1; 
                b = nearest[ref[i]-1][j]+time-1;
                res=0.0;
                for(w=0; w<m*d; w+=d) res += sqr(series[a+w] - series[b+w]);
                tmp += sqrt(res);
            }
            lyap[time] += log(tmp/(double)k);
        }
        lyap[time] /= (double)nref;
    }
}


/* -------------------------------------------------------------------------- */


#define output3(i, j) out_hist[(i)*partitions + (j)]

void mutual(double *in_series, int *in_length, int *in_lag, 
int *in_partitions, double *out_hist) {
    int partitions, length, lag;
    int ix, iy, binx, biny, i, j;
    double *series;

    series = in_series;
    length = *in_length;
    lag =*in_lag;
    partitions=*in_partitions;

    for(i =0; i<partitions; i++) 
        for(j=0; j<partitions; j++)
            output3(i, j) = 0.0;

    for(ix = 0; ix < (length-lag); ix++) {
        iy = ix + lag;
        binx = MIN((int)(series[ix]*partitions),partitions-1);
        biny = MIN((int)(series[iy]*partitions),partitions-1);
        output3(binx, biny) ++;
    }
}


/* -------------------------------------------------------------------------- */


#define MEPS 1000
#define MFRAC 10


void stplot(double *in_series, int *in_length, int *in_m, int *in_d, 
    int *in_steps, int *in_idt, double *in_epsmax, double *out) 
{
    double tmp, need;
    int i,j, a, b, w, md, is, ieps, length, blength, m, d, steps, idt;
    double epsmax, *series, *hist, **stp;

    series = in_series;
    length = *in_length;
    m = *in_m;
    d = *in_d;
    md = m*d;
    steps = *in_steps;
    idt = *in_idt;
    epsmax = sqr(*in_epsmax);
    blength = length - (m-1)*d;

    stp = (double**) R_alloc(MFRAC, sizeof(double*));
    for(i=0; i<MFRAC; i++) stp[i] = (double*) R_alloc(steps, sizeof(double));
    hist = (double*) R_alloc(MEPS, sizeof(double));

    for(i=0; i<steps; i++) {
        for(j=0; j<MEPS; j++) hist[j] = 0.0;
        for(j=0; j<(blength-i*idt); j++) {
            a = j; b = j+i*idt;
            tmp=0.0;
            for(w=0; w<md; w+=d) tmp += sqr(series[a+w]-series[b+w]);
            hist[MIN((long)(tmp*MEPS/epsmax), MEPS-1)]++;
        }
        for(j=0; j<MFRAC; j++) {
            need = (blength - i*idt)*(j+1)/(double) MFRAC;
            for(is=0, ieps=0; ieps<MEPS && is<need; ieps++)
                is +=hist[ieps];
            stp[j][i] = ieps*(epsmax/(double)MEPS);
        }
    }
    for(i=0; i<steps; i++) for(j=0; j<MFRAC; j++) 
        out[i*MFRAC+j] = sqrt(stp[j][i]);
}


/* -------------------------------------------------------------------------- */
 
