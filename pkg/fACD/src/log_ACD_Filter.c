#include <R.h>
#include <Rmath.h>


void log_ACD_Filter(double *x,
                int *nr,
                double *w,
                double *q, 
                double *p,
                int *pLag,
                int *qLag,
                double *durOut)
{

*durOut=1;

double q_part=0;
double p_part=0;

int firstIdx=imax2(*pLag,*qLag);

for(int i=firstIdx; i < *nr; i++)
    {
    
        q_part=0;
        for (int i_q=0;i_q<*qLag;i_q++)
            q_part=*(q+i_q)*(log(*(x+i-1-i_q)))+q_part;
        
        p_part=0;
        for (int i_p=0;i_p<*pLag;i_p++)
            p_part=*(p+i_p)*(*(durOut+i-1-i_p))+p_part;

        
        *(durOut+i)=*w + q_part + p_part;
   
    }
  
}

