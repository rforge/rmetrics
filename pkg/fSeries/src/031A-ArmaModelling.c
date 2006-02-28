/* Copyright (C) 1997-2001  Adrian Trapletti
  
   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Library General Public
   License as published by the Free Software Foundation; either
   version 2 of the License, or (at your option) any later version.
  
   This library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Library General Public License for more details.
  
   You should have received a copy of the GNU Library General Public
   License along with this library; if not, write to the Free
   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

   ARMA estimation */


void arma (double *x, double *u, double *a, int *ar, int *ma, int *arl, 
	   int *mal, int *max, int *n, int *intercept)
     /* compute conditional sum of squares */
{
  int i, j;
  double sum;
  
  for (i=(*max); i<(*n); i++)
  {
    if (*intercept) sum = a[(*mal)+(*arl)];
    else sum = 0.0;
    for (j=0; j<(*arl); j++)
      sum += a[j]*x[i-ar[j]];
    for (j=0; j<(*mal); j++)
      sum += a[j+(*arl)]*u[i-ma[j]];
    u[i]=x[i]-sum;
  }
}

