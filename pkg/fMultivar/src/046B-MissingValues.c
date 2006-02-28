/** #include "util.h" **/



#include <R.h>
#include <Rmath.h>

double code_miss=-9999999;

double stdd(double *vector,int *length, int *is_finite);
double **dmatrix(int nb_row,int nb_col);
void mat_vec(double *array_vec,int* nb_row,int *nb_col,double **array);
void vec_mat(double *array_vec,int* nb_row,int *nb_col,double **array);
double *dvector(int length, int init);
int *ivector(int length, int init);
double  mean_vec(double *vector,int *length);
void free_dmatrix(double **array, int nb_row);
void init_ivector(int *vector, int *length, int  value);
void quicksort2(double *a, double *b, int *p, int *r);
int partition2(double *a, double *b, int p, int r);
int rand_part2(double *a, double*b,  int p, int r);
int uni_rand(int min,int max);
void init_dvector(double *vector, int *length, int value);
double  sum_vec(double *vector,int *length);


/******************************************************************************/
/*                                                                   stdd                                                                                                  */
/*  Purpose:   Return  the standard deviation of a vector                                                                                       */
/* Argument description:                                                                                                                                       */
/* vector: The sample vector                                                                                                                                  */
/* length: the length of the vector                                                                                                                          */
/* is_finite : (output) The number of finite values in vector                                                                                   */
/******************************************************************************/ 


double stdd(double *vector,int *length, int *is_finite)
{
  /* Compute the standard deviation of a vector */

  int i,count=0;
  double sum=0;
  double x_bar;
  double result;
  
  
  x_bar=mean_vec(vector,length);
  if(x_bar==code_miss)
    return(code_miss);
  else
    {
      for(i=0;i<*length;i++)
	{
	  if(vector[i]!=code_miss)
	    {
	      count=count+1;
	      sum=sum+(vector[i]-x_bar)*(vector[i]-x_bar);
	    }
	}
      *is_finite=count;
      if(count>1)
	{
	  result=(sqrt(sum/((double)count-1)));
	  return(result);
	}
      else
	{
	  return(code_miss);
	}
    }
}

/******************************************************************************/
/*                                                                   vec_mat                                                                                            */
/*  Purpose:   Coerce a vector into a matrix                                                                                                           */
/* Argument description:                                                                                                                                       */
/* array_vec: The vector to coerce                                                                                                                          */
/* nb_row: The number of row for the matrix                                                                                                       */
/* nb_col: The number of column for the matrix                                                                                                    */
/* array :(output) The two dimmensional array                                                                                                     */
/******************************************************************************/ 
void vec_mat(double *array_vec,int* nb_row,int *nb_col,double **array)
{
  int i,j;

  for(i=0;i<*nb_row;i++)
    for(j=0;j<*nb_col;j++)
      array[i][j]=array_vec[i**nb_col+j];
}

/******************************************************************************/
/*                                                                   mat_vec                                                                                            */
/*  Purpose:   Coerce a matrix into a vector                                                                                                           */
/* Argument description:                                                                                                                                       */
/* array_vec: (outpout)The vector                                                                                                                         */
/* nb_row: The number of row for the matrix                                                                                                       */
/* nb_col: The number of column for the matrix                                                                                                    */
/* array : The two dimmensional array to coerce                                                                                                    */
/******************************************************************************/ 

void mat_vec(double *array_vec,int* nb_row,int *nb_col,double **array)
{
  int i,j;

  for(i=0;i<*nb_row;i++)
    for(j=0;j<*nb_col;j++)
      array_vec[i**nb_col+j]=array[i][j];
}

/******************************************************************************/
/*                                                               init_ivector                                                                                       */
/*  Purpose:   Initialize a vector of type int to zero                                                                                                */
/* Argument description:                                                                                                                                       */
/* vector: The vector to initialize                                                                                                                            */
/* length: the length of the vector                                                                                                                          */
/******************************************************************************/ 

void init_ivector(int *vector,int *length, int  value)
{
  int i;

  for(i=0;i<*length;i++)
    vector[i]=value;
}

/******************************************************************************/
/*                                                                   dmatrix                                                                                            */
/*  Purpose:  Allocate the memory for a matrix of type double                                                                             */
/* Argument description:                                                                                                                                       */
/* nb_row: The number of row for the matrix                                                                                                       */
/* nb_col: The number of column for the matrix                                                                                                    */
/******************************************************************************/ 
double **dmatrix(int nb_row,int nb_col)
{
  double **array;
  int i,j;

  /* Allocate the memory */
  array=Calloc(nb_row,double*);

  for(i=0;i<nb_row;i++)
    array[i]=Calloc(nb_col, double);

  /* Initialize to zero*/
  for(i=0;i<nb_row;i++)
    for(j=0;j<nb_col;j++)
      array[i][j]=0;

  return(array);
}
/******************************************************************************/
/*                                                                   dvector                                                                                             */
/*  Purpose:  Allocate the memory for a vector of type double                                                                              */
/* Argument description:                                                                                                                                       */
/* length: The length of the vector                                                                                                                         */
/* init: The value to initialize the vector                                                                                                                 */
/******************************************************************************/ 

double *dvector(int length, int init)
{
  int i;
  double *vector;

  /* Allocate the memory */
    vector=Calloc(length, double);
  
  /* Initialize the memory */
  for(i=0;i<length;i++)
    vector[i]=init;

  return(vector);
}

/******************************************************************************/
/*                                                                    ivector                                                                                             */
/*  Purpose:  Allocate the memory for a vector of type integer                                                                              */
/* Argument description:                                                                                                                                       */
/* length: The length of the vector                                                                                                                         */
/* init: The value to initialize the vector                                                                                                                 */
/******************************************************************************/ 


int *ivector(int length, int init)
{
  int i;
  int *vector;

  /* Allocate the memory */
  vector=Calloc(length, int);

  /* Initialize the memory */
  for(i=0;i<length;i++)
    vector[i]=init;

  return(vector);
}

/******************************************************************************/
/*                                                                mean_vec                                                                                             */
/*  Purpose:  Return the mean of vector (remove the missing values)                                                                   */
/* Argument description:                                                                                                                                       */
/* vector : The sample vector                                                                                                                                 */
/* length: The length of the vector                                                                                                                         */
/******************************************************************************/ 


double  mean_vec(double *vector,int *length)
{
  int i,count=0;
  double sum=0;

  for(i=0;i<*length;i++)
    {
      if(vector[i]!=code_miss)
	{
	  count=count+1;
	  sum=sum+vector[i];
	}
    }
  if (count>0)
    {
      return(sum/(double)count);
    }
  else
    {
      return(code_miss);
    }
  
}

/******************************************************************************/
/*                                                                free_dmatrix                                                                                        */
/*  Purpose:  Free the memory of a matrix of type double                                                                                     */
/* Argument description:                                                                                                                                       */
/* array: the two dimmensional array to free                                                                                                         */
/* nb_row : its number of row                                                                                                                               */
/******************************************************************************/ 


void free_dmatrix(double **array, int nb_row)
{

  int i;
  for(i=0;i<nb_row;i++)
    Free(array[i]);

  Free(array);
}
/******************************************************************************/
/*                                                                   quicksort2                                                                                        */
/*  Purpose:   Sort a vector using the quicksort algorithm  and move another vector at the same time                */
/* Argument description:                                                                                                                                       */
/* a: The vector to sort  from p to r                                                                                                                       */
/* b: The second vector to sort  from p to r                                                                                                            */
/* p: The first index                                                                                                                                                */
/* r: The last index                                                                                                                                                  */
/******************************************************************************/ 


void quicksort2(double *a, double *b, int *p, int *r)
{
  int q;
  int q_p;

  if (*p<*r)
    {
    q=rand_part2(a,b, *p, *r);
    quicksort2(a,b,p,&q);
    q_p=q+1;
    quicksort2(a,b,&q_p,r);
    }
}
int partition2(double *a, double *b, int p, int r)
{
  double x=a[p];
  int i=p-1;
  int j=r+1;
  double temp;

  for(;;)
    {
      do
	{
	  j--;
	}while(a[j]>x);
      do
	{
	  i++;
	}while(a[i]<x);
      if(i<j)
	{
	  temp=a[i];
	  a[i]=a[j];
	  a[j]=temp;
	  temp=b[i];
	  b[i]=b[j];
	  b[j]=temp;
	}
      else
	{
	  return(j);
	}
    }

}

int rand_part2(double *a, double*b,  int p, int r)
{
  int i;
  double temp;

  i=uni_rand(p,r);
  temp=a[p];
  a[p]=a[i];
  a[i]=temp;
  temp=b[p];
  b[p]=b[i];
  b[i]=temp;

  return(partition2(a,b,p,r));

}

/******************************************************************************/
/*                                                                  uni_rand                                                                                            */
/*  Purpose:  Return a random number between min and max                                                                               */
/******************************************************************************/ 

int uni_rand(int min,int max)
{
  int rand_nb;


  GetRNGstate();
  rand_nb=(int)(unif_rand()*(max-min)+min);
  PutRNGstate();
  
  return(rand_nb);

}


/******************************************************************************/
/*                                                              init_dvector                                                                                           */
/*  Purpose:  Initialize a vector of type double                                                                                                       */
/* Argument description:                                                                                                                                       */
/* vector : The vector                                                                                                                                             */
/* length: The length of the vector                                                                                                                         */
/* value: The value to initialize it with                                                                                                                    */
/******************************************************************************/ 

void init_dvector(double *vector, int *length, int value)
{
  int i;

 
  /* Initialize to value */
  for(i=0;i<*length;i++)
    vector[i]=value;
}

/******************************************************************************/
/*                                                                sum _vec                                                                                             */
/*  Purpose:  Return the sum  of vector (remove the missing values)                                                                   */
/* Argument description:                                                                                                                                       */
/* vector : The sample vector                                                                                                                                 */
/* length: The length of the vector                                                                                                                         */
/******************************************************************************/ 

double  sum_vec(double *vector,int *length)
{
  int i,count=0;
  double sum=0;

  for(i=0;i<*length;i++)
    {
      if(vector[i]!=code_miss)
	{
	  count=count+1;
	  sum=sum+vector[i];
	}
    }
  if (count>0)
    {
      return(sum);
    }
  else
    {
      return(code_miss);
    }
}


/******************************************************************************/
/******************************************************************************/



  
int is_na(double *array,int* nb_col,int *array_nb); 
void fill_up(double **array,double *row_nb,int *nb_col,int *k,int position, int* miss_pos, double *temp, double *dist_bound);
double distance(double *array1,double *array2, int *length);
double correlation(double *array1,double *array2, int *length);
int comp_na(double *vector, int *length, int *position);
void neighboors(double **array, int *nb_row, int *nb_col, int *n_position, int *nb_neighboors);
void fill_up_corr(double **array,double *row_nb,int *nb_col,int *k,int position, int* miss_pos, double *temp, double *dist_bound);

/******************************************************************************/
/*                                                    knn                                                                                                                */
/* Purpose: Replace the missing values in a two dimmensional array using a k-th nearest neighboors               */
/* algorithm. The algorithm select a row with missing values, then search the k-th nearest rows using the       */
/* Euclidian distance.  Then the missing values are replaced by the mean of the k-th nearest neighboors           */
/* The code for missing values is -99.                                                                                                                   */
/*                                                                                                                                                                           */
/* Argument description                                                                                                                                        */
/* array_vec: A one array vector that will be coerce into a 2 dimensionnal array                                                  */ 
/* nb_col: The number of columns                                                                                                                         */
/* nb_row : The number of rows                                                                                                                           */
/* k: The number of neighboors                                                                                                                            */
/******************************************************************************/



void knnc(double *array_vec,int *nb_col,int *nb_row, int*k, int *corre_flag, double *dist, double *dist_bound)
{
  int missing,i,j,ii;
  int count;
  double value;
  double *temp;
  double *row_nb;
  double ** array;
  int *miss_pos;
  int index;
  int *n_position;
  int* nb_neighboors;

  int min=0;
  int max=*k-1;
  
  array=dmatrix(*nb_row,*nb_col);

  /** contain the row numbers of the missing values **/
  miss_pos=ivector(*nb_col, code_miss);

  /** contains the distances of the neighboors **/
  temp=dvector(*k,code_miss); 
  /** contains the row numbers of the neighboors **/
  row_nb=dvector(*k,code_miss);
  /** initilize all the distances with the missing codes **/
  init_dvector(dist, nb_row, code_miss);

  n_position=ivector(*nb_row, code_miss); /** positions of potential neighboors **/
  nb_neighboors=ivector(1, code_miss); /** number of neighboors **/

  /** coerce the vector into a two dimmensional array **/
  vec_mat(array_vec,nb_row,nb_col,array); 
 
  neighboors(array, nb_row, nb_col, n_position, nb_neighboors);

  if(*nb_neighboors==0) /** Stop if no neighboors **/ 
    {
      error("No rows without missing values"); 
    }
  else 
    {
      if(*nb_neighboors<*k) /** If less than k neighboors give a warning **/
	warning("Only %d neighboors could be used", *nb_neighboors); 
      
      for(i=0;i<*nb_row;i++)
	{
	  /** Check for missing values **/
	  missing=is_na(array[i],nb_col,miss_pos);
	  
	  if (missing==1 && miss_pos[*nb_col-1]==code_miss) /**at least one missing value at most nb_col**/
	    {
	      if(*corre_flag==1 && miss_pos[*nb_col-2]!=code_miss) /** Give a warning if based on correlation and only one observation **/
		warning("Could not estimate the missing values for the row %d\n One observation is not enough to compute the sample correlation", i+1); 
	      else
		{
		  count=0;  
		  
		  for(j=0;j<*nb_neighboors;j++)  /** loop on the neighboors only **/
		    { 
		      index=n_position[j];
		      
		      if(*corre_flag==0)
			value=distance(array[i],array[index],nb_col);  /** compute the distance **/
		      else
			value=-correlation(array[i],array[index],nb_col);  /** compute the correlation **/
		      
		      if(value!=code_miss)
			{
			  if (count<*k) /** store the first k **/
			    {
			      temp[count]=value;
			      row_nb[count]=index;
			      count++;
			    }
			  else
			    {
			      quicksort2(temp,row_nb,&min,&max); /** sort the neighboors to keep the kth nearest **/
			      if (temp[*k-1]>value)  /** keep it if the distance is shorter **/
				{
				  temp[*k-1]=value;
				  row_nb[*k-1]=index;
				}       
			    } 
			}
		      
		    } 
		  
		  if(*corre_flag==0)
		    {
		      fill_up(array,row_nb,nb_col,k,i,miss_pos,temp, dist_bound); /** fill up the missing values by the averaging the distance**/
		      dist[i]=mean_vec(temp, k); /** Compute the average distances **/
		    }
		  else
		    {
		      fill_up_corr(array,row_nb, nb_col, k,i, miss_pos, temp, dist_bound); /** fill up the missing values based on correlations**/
		      dist[i]=-mean_vec(temp, k); /** Compute the average distances **/
		    }
		  
		  
		  init_dvector(row_nb, k, code_miss);    /** initialize row_nb with missing codes **/
		  init_dvector(temp, k, code_miss);        /** initialize temp with missing codes **/
		}
	    }
	  else if(missing==1 && miss_pos[*nb_col-1]!=code_miss)
	    warning("Could not estimate the missing values for the row %d\n The row only contains missing values", i+1); 
	}
    }
  
  /** recoerce the matrix into a vector **/
  mat_vec(array_vec, nb_row, nb_col,array); 

  /** free the memory **/
  free_dmatrix(array,*nb_row); 
  Free(miss_pos);
  Free(temp);
  Free(row_nb);
  Free(n_position);
  Free(nb_neighboors);
}

/******************************************************************************/
/*                                                    fill_up                                                                                                            */
/* Purpose: Compute the average of the neighboors and replace the missing values                                           */
/*                                                                                                                                                                          */
/* Argument description                                                                                                                                       */
/* array : the two dimmensional array                                                                                                                  */ 
/* nb_col: The number of columns                                                                                                                        */
/* nb_row : The number of rows                                                                                                                          */
/* k: The number of neighboors                                                                                                                           */  
/* position : the positions of the neighboors                                                                                                         */
/* miss_pos : the positions of the missing values                                                                                                   */
/* temp : the distances of the neighboors                                                                                                               */
/* dist_bound : the bound for the distances                                                                                                           */
/******************************************************************************/


void fill_up(double **array,double *row_nb,int *nb_col,int *k,int position, int* miss_pos, double *temp, double *dist_bound)
{
  int i,index,j,count;
  double sum;
  int missing_spot;
  int flag=0;
  
  i=0;
  /* The positions are stored at the beginning then zeros */
  while(miss_pos[i]!=code_miss) 
    {
      
      missing_spot=miss_pos[i];
      count=0;
      sum=0;
      for (j=0;j<*k;j++)
	{
	  index=row_nb[j];
	  if(index!=code_miss)
	    {
	      if(temp[j]<*dist_bound | *dist_bound==0)
		{
		  sum=sum+array[index][missing_spot];
		  count=count+1;
		}
	      else
		{
		  row_nb[j]=code_miss;
		  temp[j]=code_miss;
		}
	    }
	}
      if(count>0)
	{
	  /** replace the missing value by the average of the neighboors **/
	  array[position][missing_spot]=sum/count; 
	  flag=1; 
	}
      
      if(flag==0)
	 warning("Could not estimate the missing values for the row %d\n  dist.bound is too small", position+1); 

      i++;
    }
  
}


/******************************************************************************/
/*                                       fill_up based on the correlations                                                                   */
/* Purpose: Compute the average of the neighboors and replace the missing values                                           */
/*                                                                                                                                                                          */
/* Argument description                                                                                                                                       */
/* array : the two dimmensional array                                                                                                                  */ 
/* nb_col: The number of columns                                                                                                                        */
/* nb_row : The number of rows                                                                                                                          */
/* k: The number of neighboors                                                                                                                           */  
/* position : the positions of the neighboors                                                                                                         */
/* miss_pos : the positions of the missing values                                                                                                   */
/******************************************************************************/


void fill_up_corr(double **array,double *row_nb,int *nb_col,int *k,int position, int* miss_pos, double *temp, double *dist_bound)
{
  int i,index,j,count,kk;
  double sum;
  int missing_spot;
  int *finite;
  double sdx,meanx;
  double *tmp,*meany,*sdy;
  int flag=0;
  
  tmp=dvector(*nb_col,code_miss);
  meany=dvector(*k,code_miss);
  sdy=dvector(*k,code_miss);

  finite=ivector(1,code_miss);
  sdx=stdd(array[position],nb_col, finite);
  meanx=mean_vec(array[position],nb_col);

  for(kk=0;kk<*k;kk++)
    {
      index=row_nb[kk];
      if(index!=code_miss)
	{
	  for(j=0;j<*nb_col;j++)
	    {
	      if(array[position][j]!=code_miss)
		tmp[j]=array[index][j];
	      else
		tmp[j]=code_miss;
	    }
	  meany[kk]=mean_vec(tmp,nb_col);
	  sdy[kk]=stdd(tmp,nb_col,finite);
	}
    }

  i=0;
  /** The positions are stored at the beginning then code_miss **/
  while(miss_pos[i]!=code_miss )  
    {
      
      missing_spot=miss_pos[i];
      count=0;
      sum=0;
      for (j=0;j<*k;j++) /** Compute the average of the z-scores **/ 
	{
	  index=row_nb[j];
	  
    	  /** if not enough neighboors, do not use the missing ones **/ 
    	  if(index!=code_miss)
	    { 
	      if(temp[j]<-(*dist_bound) |*dist_bound==0)
		{
		  sum=sum+(array[index][missing_spot]-meany[j])/sdy[j];
		  count=count+1;
		}
	      else
		{
		  row_nb[j]=code_miss;
		  temp[j]=code_miss;
		}
	    }
	}
      if(count>0)
	{
	  array[position][missing_spot]=meanx+sdx*(sum/count); /** replace the missing value by the average of the standardized neighboors **/ 
	  flag=1;
	}

      i++;
    }
  if(flag==0)
    warning("Could not estimate the missing values for the row %d\n  dist.bound is too large", position+1); 

  /** free the memory **/

  Free(finite); 
  Free(tmp); 
  Free(sdy); 
  Free(meany); 

}


/******************************************************************************/
/*                                                    is_na                                                                                                              */
/* Purpose: Check if a vector contains missing values                                                                                          */
/*                                                                                                                                                                          */
/* Argument description                                                                                                                                       */
/* array : the one dimmensional array                                                                                                                  */ 
/* nb_col: The length of the array                                                                                                                         */
/* miss_pos: (output)the positions of the missing values                                                                                      */
/******************************************************************************/

int is_na(double *array,int* nb_col,int *miss_pos)
{
  int i;
  int count=0;
  
  init_ivector(miss_pos,nb_col,code_miss);
  
  for(i=0;i<*nb_col;i++)
    {
      if(array[i]==code_miss)
	{
	  miss_pos[count]=i;
	  count=count+1;
	}
    }
  if(count>0)
    return(1); /** one if missing's **/
  else
    return(0); /** 0 if no missing's **/
  
}
/******************************************************************************/
/*                                                    distance                                                                                                         */
/* Purpose: Compute the Euclidean distance between 2 vectors omitting the missing values                              */
/*                                                                                                                                                                          */
/* Argument description                                                                                                                                       */
/* array1 : the first one dimmensional array                                                                                                         */ 
/* array2 : the second  dimmensional array                                                                                                           */
/* length: the length of the arrays                                                                                                                         */
/******************************************************************************/
double distance(double *array1,double *array2, int *length)
{
  int i;
  int count=0;
  double value=0;
  
  for(i=0;i<*length;i++)
    {
 
      
      if(array1[i]!=code_miss) /** omit the missing values **/
	if(array2[i]!=code_miss)
	  {
	    count=count+1;
	    value=value+(array1[i]-array2[i])*(array1[i]-array2[i]);
	  }
    }
  if(count>0)
    {
  return(sqrt(value));
    }
  else
    {
      return(code_miss);
    }
}

/******************************************************************************/
/*                                                    correlation                                                                                                     */
/* Purpose: Compute the correlation between 2 vectors omitting the missing values                                          */
/*                                                                                                                                                                          */
/* Argument description                                                                                                                                       */
/* array1 : the first one dimmensional array                                                                                                         */ 
/* array2 : the second  dimmensional array                                                                                                           */
/* length: the length of the arrays                                                                                                                         */
/******************************************************************************/
double correlation(double *array1,double *array2, int *length)
{
  int i;
  int count=0;
  double value=0;
  double meanx,meany;
  double sdx,sdy;
  int *finite1,*finite2;
  double *temp;
  
  finite1=ivector(1,code_miss);
  finite2=ivector(1, code_miss);
  temp=dvector(*length,code_miss);

  for(i=0;i<*length;i++)
    {
      if(array1[i]!=code_miss)
	temp[i]=array2[i];
    }
  

  meanx=mean_vec(array1, length);
  meany=mean_vec(temp, length);

  sdx=stdd(array1,length,finite1);
  sdy=stdd(temp,length,finite2);
  

  if(*finite1 >1 && *finite2>1) /* check if more than two observations for both rows */
    {
      for(i=0;i<*length;i++)
	{
	  if(array1[i]!=code_miss) /** omit the missing values **/
	    if(array2[i]!=code_miss)
	      {
		count=count+1;
		value=value+(array1[i]-meanx)*(array2[i]-meany);
	      }
	}
      
      if(count>1)
	{
	  value=(value/(count-1))/(sdx*sdy);
	}
      else
	{
	  value=code_miss;
	}
    }
  else
    value=code_miss;


  /** Free the memory **/
  Free(temp); 
  Free(finite1);
  Free(finite2);


  return(value);
  
}


/******************************************************************************/
/*                                                    neighboors                                                                                                    */
/* Purpose: Look for the rows without missing values                                                                                        */
/*                                                                                                                                                                          */
/* Argument description                                                                                                                                       */
/* array : the data matrix with missing values                                                                                                      */ 
/* nb_row : the number of rows                                                                                                                           */
/* nb_col : the number of col                                                                                                                                */
/* n_position (output) : the indexes of the rows with no missing values                                                              */
/* nb_neighboors : the number of rows with no missing values  (i.e. the potential neighboors)                          */
/******************************************************************************/




void neighboors(double **array, int *nb_row, int *nb_col, int *n_position, int *nb_neighboors)
{
  int i;
  int *niet; /* ghost variable */
  int count=0;
  int missing;

  niet=ivector(*nb_col, code_miss);
  
  for(i=0;i<*nb_row;i++)
    {
      missing=is_na(array[i],nb_col,niet); /** check if the neighboor has missing values **/
      if(missing==0) 
	{
	  n_position[count]=i;
	  count++;
	}
    }
  *nb_neighboors=count;

  Free(niet);
}



/******************************************************************************/
