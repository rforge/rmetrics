#include <R.h>
#include <Rmath.h>

void BC_ACD_Filter(double *x,
                int *nr,
                double *w,
                double *q, 
                double *p,
                double *delta,
                int *pLag,
                int *qLag,
                double *durOut)
{

double q_part=0;
double p_part=0;

double e[*nr];

int firstIdx=imax2(*pLag,*qLag);


for (int i=0 ; i < *nr ; i++)
    e[i]=1.00;

for (int i=firstIdx; i < *nr ; i++)
    {
               
        q_part=0;
        for (int i_q=0;i_q<*qLag;i_q++)
            q_part=*(q+i_q)*( (pow(e[i-1-i_q],*(delta+i_q))-1)/(*(delta+i_q))  )+ q_part;  //PROBLEm IS HERE
        
        p_part=0;
        for (int i_p=0;i_p<*pLag;i_p++)
            p_part=*(p+i_p)*(*(durOut+i-1-i_p))+p_part;

        *(durOut+i)=*w + q_part + p_part;

        e[i]=(*(x+i))/(exp(*(durOut+i)));
    
    }
  
}

