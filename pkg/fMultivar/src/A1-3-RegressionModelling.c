
/*
*  Copyright (C) 1997--2002  Charles Kooperberg and Martin O'Connor
*
*  This program is free software; you can redistribute it and/or modify
*  it under the terms of the GNU General Public License as published by
*  the Free Software Foundation; either version 2 of the License, or
*  (at your option) any later version.
*
*  This program is distributed in the hope that it will be useful,
*  but WITHOUT ANY WARRANTY; without even the implied warranty of
*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
*  GNU General Public License for more details.
*
*  The text of the GNU General Public License, version 2, is available
*  as http://www.gnu.org/copyleft or by writing to the Free Software
*  Foundation, 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
*/
/* This file contains the main body of the program to be loaded into Splus.  */
/* The only other file needed is ``f2c.h''                                   */


#include <stdio.h> 
#include <math.h>
#include "R.h"
#define Salloc(n, t)  (t *)R_alloc((long)(n), (int)sizeof(t))
/*#include "POLYMARS-x2c.h" */


/* f2c.h  --  Standard Fortran to C header file */
/**  barf  [ba:rf]  2.  "He suggested using FORTRAN, and everybody barfed."
   - From The Shogakukan DICTIONARY OF NEW ENGLISH (Second edition) */

#ifndef F2C_INCLUDE
#define F2C_INCLUDE

typedef long int integer;
typedef char *address;
typedef short int shortint;
typedef float real;
typedef double doublereal;
typedef long int logical;
typedef short int shortlogical;
typedef char logical1;
typedef char integer1;
/* typedef long long longint; */ /* system-dependent */

#define TRUE_ (1)
#define FALSE_ (0)

/* Extern is for use with -E */
#ifndef Extern
#define Extern extern
#endif

/* I/O stuff */

#ifdef f2c_i2
/* for -i2 */
typedef short flag;
typedef short ftnlen;
typedef short ftnint;
#else
typedef long flag;
typedef long ftnlen;
typedef long ftnint;
#endif

/*external read, write*/
typedef struct
{	flag cierr;
	ftnint ciunit;
	flag ciend;
	char *cifmt;
	ftnint cirec;
} cilist;

/*internal read, write*/
typedef struct
{	flag icierr;
	char *iciunit;
	flag iciend;
	char *icifmt;
	ftnint icirlen;
	ftnint icirnum;
} icilist;

/*open*/
typedef struct
{	flag oerr;
	ftnint ounit;
	char *ofnm;
	ftnlen ofnmlen;
	char *osta;
	char *oacc;
	char *ofm;
	ftnint orl;
	char *oblnk;
} olist;

/*close*/
typedef struct
{	flag cerr;
	ftnint cunit;
	char *csta;
} cllist;

/*rewind, backspace, endfile*/
typedef struct
{	flag aerr;
	ftnint aunit;
} alist;

/* inquire */
typedef struct
{	flag inerr;
	ftnint inunit;
	char *infile;
	ftnlen infilen;
	ftnint	*inex;	/*parameters in standard's order*/
	ftnint	*inopen;
	ftnint	*innum;
	ftnint	*innamed;
	char	*inname;
	ftnlen	innamlen;
	char	*inacc;
	ftnlen	inacclen;
	char	*inseq;
	ftnlen	inseqlen;
	char 	*indir;
	ftnlen	indirlen;
	char	*infmt;
	ftnlen	infmtlen;
	char	*inform;
	ftnint	informlen;
	char	*inunf;
	ftnlen	inunflen;
	ftnint	*inrecl;
	ftnint	*innrec;
	char	*inblank;
	ftnlen	inblanklen;
} inlist;

#define VOID void

union Multitype {	/* for multiple entry points */
	integer1 g;
	shortint h;
	integer i;
	/* longint j; */
	real r;
	doublereal d;
	};

typedef union Multitype Multitype;

typedef long Long;	/* No longer used; formerly in Namelist */

struct Vardesc {	/* for Namelist */
	char *name;
	char *addr;
	ftnlen *dims;
	int  type;
	};
typedef struct Vardesc Vardesc;

struct Namelist {
	char *name;
	Vardesc **vars;
	int nvars;
	};
typedef struct Namelist Namelist;

#define abs(x) ((x) >= 0 ? (x) : -(x))
#define dabs(x) (doublereal)abs(x)
#define min(a,b) ((a) <= (b) ? (a) : (b))
#define max(a,b) ((a) >= (b) ? (a) : (b))
#define dmin(a,b) (doublereal)min(a,b)
#define dmax(a,b) (doublereal)max(a,b)

/* procedure parameter types for -A and -C++ */

#define F2C_proc_par_types 1
#ifdef __cplusplus
typedef int /* Unknown procedure type */ (*U_fp)(...);
typedef shortint (*J_fp)(...);
typedef integer (*I_fp)(...);
typedef real (*R_fp)(...);
typedef doublereal (*D_fp)(...), (*E_fp)(...);
typedef /* Complex */ VOID (*C_fp)(...);
typedef /* Double Complex */ VOID (*Z_fp)(...);
typedef logical (*L_fp)(...);
typedef shortlogical (*K_fp)(...);
typedef /* Character */ VOID (*H_fp)(...);
typedef /* Subroutine */ int (*S_fp)(...);
#else
typedef int /* Unknown procedure type */ (*U_fp)();
typedef shortint (*J_fp)();
typedef integer (*I_fp)();
typedef real (*R_fp)();
typedef doublereal (*D_fp)(), (*E_fp)();
typedef /* Complex */ VOID (*C_fp)();
typedef /* Double Complex */ VOID (*Z_fp)();
typedef logical (*L_fp)();
typedef shortlogical (*K_fp)();
typedef /* Character */ VOID (*H_fp)();
typedef /* Subroutine */ int (*S_fp)();
#endif
/* E_fp is for real functions when -R is not specified */
typedef doublereal E_f;	/* real function with -R not specified */

/* undef any lower-case symbols that your C compiler predefines, e.g.: */

#ifndef Skip_f2c_Undefs
#undef cray
#undef gcos
#undef mc68010
#undef mc68020
#undef mips
#undef pdp11
#undef sgi
#undef sparc
#undef sun
#undef sun2
#undef sun3
#undef sun4
#undef u370
#undef u3b
#undef u3b2
#undef u3b5
#undef unix
#undef vax
#endif
#endif


/* END OF x2c.h */


#define TRUE  1 
#define FALSE 0


static double tolerance;


static int predictors;/*number of predictors in the dataset*/
static int responses;/*number of responses in dataset*/
static int cases;/*number of cases in dataset*/
static int max_knots;/*maximum number of knots allowed in any of the predictors*/
              /*This is used for memory allocation */
static int model_size; /*holds current model size in the fitting procedure*/
static int *knots_per_pred;/*a pointer to an array which holds the number of knots */
                    /*availible for each predictor in the model fitting */
static int max_model_size;/*maximum size that the model is allowed to grow to in */
                   /*fitting procedure*/
static int *order_keeper1;/*These matricies hold information about the oder in which*/
static int *order_keeper2;/*the spline functions can be added. A predictor must*/
static int *order_keeper3;/* have a linear spline before one with a knot*/
static int *best_model;/*A pointer to an array that holds the best model*/
static int additive;/*A boolean as to whether the user wanrts an additive model or not.*/
static double GCV;/*A boolean as to whether the user wanrts an additive model or not.*/
           /*measure for the best model so far. Used as critirium for choosing */
           /*best overall model.*/
static double GCVconstant;/*This value is used in calculating GCV and can be specified by */
                   /*the user.*/
static double *best_coefficents;/*This is a pointer to an array that stores the coefficients*/
                         /*of the best model so far*/
static double *coefficents;/*Used in testset to calculate coefficients for testset RSS. */
                    /*Usually the coefficeients are stored in a standardised form*/
                    /*for numerical stability.*/

static int *steps, step_count ;/*Information about each step of the fitting is recorded*/
static int *bestmodel_size;/*A pointer to the size of best bestmodel. The variable is */
                    /*passed in through Splus.*/
static double *best_XtXinv;/*The X transpose X matrix of the best model */
                    /* Used to compute the standard errors of the coefficients*/
                
static double *coef_sd_const;/*Constants to multiply the residual standard error by */
                       /* to get SEs for coefficients */

static double *rssgcv;/*A pointer to an array passed from Splus. Details of RSS and GCV */
               /*for each step stored here.*/

static int *startmodel;/*A starting model may be passed in by Splus*/
static double *startknots;/*A pointer to an array passed from Splus number of knots for */
                   /* specified by user*/
static int *weighted;/*A boolean as to whether weights are specified for the model fitting*/
static double weight_sum;/*A sum of the weights- used in calculating GCV in weighted */

static int interaction_specs_size;/*If certain predictor are not to has interaction*/
static int *interaction_specs;    /* terms together they are specified in an array*/
                           /*with the size of this array (divided by 2).*/
static int not_remove_size; /*If certain terms in the start model are not to be removed*/
static int *not_remove_specs;/* an array holds the information and a variable hold the */
                       /* size of this array*/
static int knot_space;/* The minimum number of order statistics between knots allowed when */
               /* the possible knots are computed(compute_mesh)*/
static int testset;/*A boolean as to whether the model are selected using a testset or not. */
static double *testset_weights;/*A pointer to an array of testset weights*/
static int testset_weighted;/*A boolean as to whether weights are specified for testset*/
static double *tset_RSS;/*Used  to hold testset RSS in function testset_RSS*/
static int *response_class;/*The class assigned to a fitted response in testset_RSS when*/
                    /*using classification.*/
static double *response_max;/* Used to classify in testset_RSS when using classification.*/
static int classification;/*A boolean to use classification or not(normally false)*/
static int Verbose;/*A boolean to use have the function printout as it goes along*/



static struct matrix1 *weight_matrix;
static struct matrix1 *YtY;/* Y are the responses-this is Y-transpose by Y*/
static struct matrix1 *data_matrix;
static struct matrix1 *testset_matrix;/* testset datamatrix*/
static struct matrix1 *X_matrix;/* X is the design matrix*/
static struct matrix1 *XtX_inverse;
static struct matrix1 *XtX_newinverse;
static struct matrix1 *new_XtXcolumn;
static struct matrix1 *function_values;/*- list of function values at knot 
	       		location-*/
static struct matrix1 *function_values_2;/*used in computing inner products*/
static struct matrix1 *Rao_B;/*These are matricies used in computing a new XtX*/
static struct matrix1 *Rao_F;/*matrix from an X matrix that has 1 column more or less*/
static struct matrix1 *Rao_E;/*than the previous one. From an example in Rao's linear*/
static struct matrix1 *Rao_E_inverse;/* algebra */
static struct matrix1 *Rao_F_E_inverse_Ft;
static struct matrix1 *Rao_F_E_inverse;
static struct matrix1 *YtXXtX_newinverseXtY;
static struct matrix1 *XtX_newinverseXtY;
static struct matrix1 *YtX;
static struct matrix1 *new_X_matrix;
static struct matrix1 *temp_matrix;/*Used in matrix multiplication when weights are*/
                            /* multiplied in fit_as_candidate and initial_model*/


static double *best_model_sd_mean; /*all columns of the X matrix are standardised*/
                            /* coefficients of the best model */
static double *model_sd_mean;

/*----------------------------------------------------------------*/

struct link{
struct link *next;
double *data;
struct basis_function  *function;
};

struct list
{
  struct link *list;
  int length;
};

struct matrix1 
{
  double *matrix;
  int nrow;
  int ncol;
};
/*--this matrix is used for the YtransposeY and XtY matricies
as column must be added and switched around--*/
struct matrix2
{
  struct link *column_list;
  int nrow;
  int ncol;
};

struct basis_function 
/* A basis function is a spline (truncated power spline) or tensor product of a 
   spline. These may have knots(elbow functions). There entries in the design (X) 
   matrix are given as z-scores : minus the mean of the column then diveded by the 
   standard dev. */
{
int predictor1;
int knot1_index;
double knot1_value;
int predictor2;
int knot2_index;
double knot2_value;
struct basis_function  *link;
double SD;
double mean;
};

struct basis_function_matrix 
/* This is a matrix with number of rows equal to the number of predictors and 
   number of columns variable for each row depending on the number of basis functions
   that are either candidates or in the model for that predictor (there is a model
   matrix and a candidates matrix.
   */
{
  struct basis_function  *functions;
  int predictor_index;
  int number_of_basis_functions;
  struct basis_function_matrix  *next_predictor;
  
};

/*Two types of matrix used */ 

static struct matrix1 *create_matrix1(int nrow, 
			     int ncol);

static struct matrix2 *create_matrix2(int nrow, 
			     int ncol);
/* for matrix2*/
static void switch_columns(int col1,
		 int col2,
		 struct matrix2 *object_matrix);

/*void print_matrix1(struct matrix1 *object_matrix );*/

/*void print_matrix2(struct matrix2 *object_matrix );*/
/*to multiply matricies of type 1*/
static void matrix_multiplication1(struct matrix1 *object_matrixA,
				     struct matrix1 *object_matrixB,
				     struct matrix1 *result,
				     int flag);

/*to multiple matrix of type 1 to type 2 */
static void matrix_multiplication2(struct matrix1 *object_matrixA,
			    struct matrix2 *object_matrixB,
			    struct matrix1 *result,
			    int flag);
/* prints a basis_function_matrix */
/*void print_functions(struct basis_function_matrix  *functions_matrix);*/
				    



/*------functions --*/
/* find possible knot locations for basis functions. Subset of order statistics.
   An array of double. knots_per_pred is used to access this array.
   Also holds levels of categorical variables---*/
static double *compute_mesh();


/* Sets up dynamically created variables controlls model selection*/
static int fit_model(double *mesh);

/*find candidate basis functions*/
static void find_candidates(struct matrix2 *YtXXtX_expanded,
		      double *mesh,
		      struct basis_function_matrix *model,
		      struct basis_function_matrix *candidates);
/*Is a function already in the model*/
static int in_model(int predictor1,
	     int knot1_index,
	     int predictor2,
	     int knot2_index,
	     struct basis_function_matrix *model);
/*Compute the relevant inner products for YtX and XtX as if it was in the model*/
static void fit_as_candidate(int predictor1, 
		      int knot1_index,
		      int predictor2,
		      int knot2_index,
		      struct matrix2 *YtXXtX_expanded,
		      double *mesh);
/*Function checks whether a basis function can become a new candidate and
if it can put it in the candidates matrix*/
static int new_candidate(int predictor1,
		  int knot1_index,
		  int predictor2,
		  int knot2_index,
		  struct basis_function_matrix *candidates,
		  struct basis_function_matrix *model,
		  struct matrix2 *YtXXtX_expanded);

/*find the candidate to be added to the model*/
static int find_best_candidate(struct matrix2 *YtXXtX_expanded,
			struct basis_function_matrix *model,
			struct basis_function_matrix *candidates,
			double *mesh);
/*After the best candidate to be added is found the modelis updated*/
static void update_model(struct matrix2 *YtXXtX_expanded,
		  struct basis_function_matrix *model,
		  struct basis_function_matrix *candidates,
		  int candidate,
		  double *mesh);
/*the deletion stage consists of one function to decide which basis function
  to delete and updates everything*/
static int reduce_model(struct matrix2 *YtXXtX_expanded,
		 struct basis_function_matrix *model);
/* fits an initial model if one is specified*/
static int initial_model(struct basis_function_matrix *model,
                   struct matrix2 **YtXXtX_expanded);
/* standardises each column of the X matrix substracting the mean and 
   dividing by the standard deviation*/
static void standardise_array(double *numbers,int length_of_list,double *mean,double *SD);
/*checks the initial model if given, as it must be consistant with how the
proceure adds functions*/
static int check_input();
/*use lapack inversion function. Some preprocessing of the matrix in done 
first*/
static int invert_matrix(struct matrix1 *object_matrix);
/* computes testset RSS if required*/
static double testset_RSS(struct matrix2 *YtXXtX_expanded,int model_size);


static logical lsame(char *, char *);
static int xerbla(char *, int *);
static int idamax(int *n, double *dx, int *incx);
static int dswap(int *, double *, int *, double *, int *);
static int dspr(char * , int *, double *, double *, int *, double *);
static int dscal(int *, double *, double *, int *);
static int dlaev2(double *, double *, double *, double *, double *, double *, double *);
static int drot(int *, double *, int *, double *, int *, double *, double *);
static int dcopy(int *, double *, int *, double *, int *); 
static int dspmv(char *, int *, double *, double *, double *, int *, double *, double *, int *);
static double ddot(int *, double *, int *, double *, int *);
static int dsptrf(char *uplo, int *n, double *ap, int * ipiv, int *info);
static int dsptri(char *uplo, int *n, double *ap, int * ipiv, double *work, int *info);
/*----------------------------------------------------------------*/
/* This function is called from Splus function polymars*/
/*==============================================================*/
void polymars(int *pred,
	  int *resp, 
	  int *ncases, 
	  double *datamatrix, 
	  int *knotinfo,
          double *given_mesh,
          int *is_mesh_specified,
	  int *maxmodel, 
	  double *gcvvalue, 
	  int *addflag, 
	  int *start_model_size,
	  int *start_model,
	  double *start_knots,
	  int *weights_indictor,
	  double *caseweights,
	  int *nointeraction,
	  int *nointeractionrule,
	  int *noremove,
	  int *noremoverule,
          int *knotspace,
          int *testset_flag,
          double *testsetmatrix,
          int *testset_ncases,
          int *testset_weights_indictor,
          double *testset_weights,
          int *classify,
          double *stability1,
          int* verbose,
	  int *model_returned,
	  double *coefs_returned,
	  int *steps_in_fitting,
	  double *rss_and_gcvs,
	  int *resultmodel_size,
	  double *model_knots,
          double *sd_constants,
	  int *end_condition,
	  int *nstep)
/*==============================================================*/
{

  double *mesh,*mesh_ptr;/*mesh is the matrix of possible knot values*/
  int i,j,k;
  
  

/*Matching up the S variables with global variables in this file  */
  startmodel = start_model;
  startknots = start_knots;
  model_size = *start_model_size;
  weighted =  weights_indictor;
  step_count = 0;
  predictors = *pred;
  responses = *resp;
  cases = *ncases;
  GCVconstant = *gcvvalue;
  max_model_size = *maxmodel;
  knots_per_pred = knotinfo;
  additive = *addflag;
  bestmodel_size = resultmodel_size;
  *bestmodel_size = 1;
  interaction_specs_size = *nointeraction;
  interaction_specs = nointeractionrule;
  not_remove_size = *noremove;
  not_remove_specs = noremoverule;
  knot_space = *knotspace;
  testset = *testset_flag;
  classification = *classify;
  testset_weighted = *testset_weights_indictor;
  best_model = model_returned;
  best_coefficents = coefs_returned;
  coef_sd_const = sd_constants;
  steps = steps_in_fitting;
  rssgcv = rss_and_gcvs;
  tolerance = *stability1;
  Verbose = *verbose;
  

  data_matrix = (struct matrix1 *)Salloc(1,struct matrix1);
  data_matrix->ncol =  predictors + responses;
  data_matrix->nrow = cases;
  data_matrix->matrix = datamatrix;

  
  if(*testset_flag == TRUE)
    {
     testset_matrix = (struct matrix1 *)Salloc(1,struct matrix1);
      
     testset_matrix->ncol = predictors + responses;
     testset_matrix->nrow = *testset_ncases;
     testset_matrix->matrix = testsetmatrix;
     
    }
  
  if(*weighted == TRUE)
    {
      weight_sum = 0.0;
      for(i=0;i<cases;i++)
	{
         weight_sum = weight_sum + caseweights[i];
	}
      weight_matrix=create_matrix1(0,0);
      
      weight_matrix->nrow = cases;
      weight_matrix->ncol = cases;
      weight_matrix->matrix = caseweights;
    } 
  else
    {
     weight_sum = cases;
    }
  
  
  
  *end_condition =0;/* records why the program ended*/
  /*checking that the input is consistant with requirements of the procedure*/
  /*Just checks that a startmodel doesn't contradict the hierarchy rules of which*/
  /*terms must be in the model before which*/
  
  *end_condition = check_input();
  
  if(*end_condition ==0)
    {
      
      if(*is_mesh_specified == FALSE)
	{mesh = compute_mesh();}
      else
	{mesh = given_mesh;}
      
      *end_condition = fit_model(mesh);

      /* number of steps of adding deleting given to variable to return to Splus*/
      *nstep = step_count;
    }
 
   if(*bestmodel_size !=1)
    {
      /*get the knots for the model from the mesh to return to the Splus function. 
       Only its index has been recorded by which it is found in the 'mesh'*/
      
      for(i=0;i<2*(*bestmodel_size-1);i++)
	{
	  
	  mesh_ptr = mesh;
	  if(best_model[(2*i)+1]!=0)
	    {
	    
	      for(j=0;j<best_model[2*i]-1;j++)
		{
		  for(k=0;k<abs(knots_per_pred[j]);k++)
		    {
		      mesh_ptr++;
		    }
		}
	      model_knots[i]=mesh_ptr[best_model[(2*i)+1]-1];
	      
	    }
	}
      
      /* with the knots recorded seperately the knot index is changed to 
        just a 1 indicating `function with knot' to be returned to Splus */     
      for(i=0;i<(*bestmodel_size)-1;i++)
	{
	  if(best_model[(i*4)+1] != 0){best_model[(i*4)+1] = 1;}
	  if(best_model[(i*4)+3] != 0){best_model[(i*4)+3] = 1;}
	  
	}
  
    }

   
}




/*==============================================================*/
static double *compute_mesh()
/*==============================================================*/
{
/*--------------------------------------------------------------
Computes the mesh  of possible knots for the spline functions.
The number of knots per predictor is an arguement to the main function
and they are stored in `knots_per_pred' 

Returns a pointer to the mesh which is a double array
--------------------------------------------------------------*/


  int i,j,k,l,m;
  double data_value;
  double *mesh;
  double *levels;
  int mesh_size, knots, nstartknots;
  int level_present;
  double *matrix_ptr,*mesh_ptr, *mesh_ptr2;
  
  matrix_ptr = data_matrix->matrix;
  mesh_size = 0;
/*-----------------------------------
Knots in the initial model are treated as extra and put
in after the usual knots for each predictor
-------------------------------------*/
  if(model_size >1)
    {
      for(i=0;i<2*(model_size-1);i++)
	{
	  if(startmodel[(i*2)+1] == 1 && knots_per_pred[startmodel[(i*2)]-1] >-1)
	    {
	      knots_per_pred[startmodel[(i*2)]-1]++;
	    }
	}
    }
/*------------------------------------------*/
/*--counting the total number of knots--*/

  for(i=0;i<predictors;i++)
    {
      if(knots_per_pred[i]>=0)
	{

	  mesh_size = mesh_size + knots_per_pred[i];
	}
    }
  
  
/*---levels is an array to hold the different levels of the categorical 
variables. The levels will be put into the mesh matrix as knots 
are put in for continuous variables it is also used to sort predictor values
into order statistics to calculate knots in continuous predictors-*/


  levels = (double *)Salloc(cases,double);
  
  
/*-the number of levels per pred are added to mesh size--*/    
  for(i=0;i<predictors;i++)
    {
    
      if(knots_per_pred[i]<0)/*---if it is categorical*/
	{
          
	  /*point to the values of this predictor*/
	  matrix_ptr = &data_matrix->matrix[(responses+i)*cases];
	  k=0;
	  for(j=0;j<cases;j++)
/*--scan thru the values it takes*/
	    {
	      level_present=FALSE;
/*-check if the current value is in the levels
  array for this predictor--*/   
	      for(l=0;l<k;l++)
/*-check up to no. of levels found so far in `levels'-*/
		{
		  if(levels[l] == matrix_ptr[j])
		    {
		      level_present=TRUE;
		    }
		}
	      if(level_present==FALSE)/*-add new level to levels matrix-*/
		{
                  
		  levels[k] = matrix_ptr[j];
		  k++;
		}
	    }
          
	  mesh_size=mesh_size+k;
	  knots_per_pred[i]= - k;
	  /*-this is where the fact that it is a categorical predictor is stored
	    negative indictaes number of levels instead of number of knots */
	}
      
    }
  if(mesh_size != 0)
    {
      mesh = (double *)Salloc(mesh_size,double);
      mesh_ptr = mesh;
      for (i=0;i<predictors;i++)/*loop thru reponses part of data matrix*/
	{
         nstartknots=0;
	  if(knots_per_pred[i]<0)/*if categorical*/
	    {
              mesh_ptr2 = mesh_ptr;
	      matrix_ptr = &data_matrix->matrix[(responses+i)*cases];
	      k=0;
	      for(j=0;j<cases;j++)
              /*-scan thru the values it takes*/
	        {
	         level_present=FALSE;
                 /*-check if the current value is in the levels
                  array for this predictor--*/   
	          for(l=0;l<k;l++)
                 /*-check up to no. of levels found so far in `levels'-*/
		   {
		     if(mesh_ptr2[l] == matrix_ptr[j])
		      {
		       level_present=TRUE;
		      }
		   }
	          if(level_present==FALSE)/*-add new level to levels matrix-*/
		   {
                    
		     *mesh_ptr = matrix_ptr[j];
                     mesh_ptr++;
		     k=k+1;
		     for(l=0;l<(model_size-1)*2;l++)
		       {
			 if(startmodel[l*2]-1 == i && ((int)startknots[l]==(int)matrix_ptr[j]))
			   {
			     startmodel[(l*2)+1] = k;
			   }

		       }
		   }
		}
	    }
	  else
/*--sort the values into order statistics for each continous variable*/
	    {
	      /*point to the values of this variable*/
	      matrix_ptr = &data_matrix->matrix[(responses+i)*cases];
              /*-add new level to levels matrix-*/
	      for(j=0;j<cases;j++)
		{
		  levels[j] = matrix_ptr[j];
		}
	    
	    
	      for(j=1;j<cases;j++)
		{
		  data_value = levels[j];
		  k=j-1;
		  while (k>=0 && levels[k] >data_value)
		    {
		      levels[k+1]=levels[k];
		      k--;
		  }
		  levels[k+1]=data_value;
		}
/*m counts the number of values that are the present more than once in a predictor*/
	    m = 0; 
	    for(j=0;j<cases-m;j++)
	      {
		if(levels[j] == levels[j+1])
		  {
		    k=2;
		    while(levels[j] == levels[j+k] &&
			  (j+k) < cases-m )
		      {
			k++;
		      }
		    for(l=j+1;l<cases-m;l++)
		      {
			levels[l]=levels[l+k-1];
		      }
		    m=m+k-1;
		  }
	      }
	    
	    /*take out levels that are in start model--*/
	    for(j=0;j<cases-m;j++)
	      {
		for(k = 0;k<(model_size-1)*2;k++)
		  {
		    if(startmodel[k*2]-1 == i && 
		       startknots[k] == levels[j]  && startmodel[(k*2)+1] != 0)
		      {
			
			for(l=j;l<cases-1-m;l++)
			  {
			    levels[l]=levels[l+1];
			  }
			m++;
                        nstartknots++;
		      }
		  }
	      }
            
	      knots = knots_per_pred[i];
	      if(model_size !=1)
		{
		  for(j=0;j<model_size-1;j++)
		    {
		      if(startmodel[j*4]-1 == i && startmodel[(j*4)+1]==1)
                       /*the 1 indictes that a knot is present in the initial model*/
			{
			  
			  knots --;
                          /*calculating the number of knots minus the number  
                         in the initial model*/		  
			}
		    }
		}
	      l = 0;
	      
	      for(k = 0;k<knots;k++)
		{
		  /*j is the index (for 1 to knots) of the unique sorted variable
                  values, which is to be a knot candidate*/ 
		  j = (int)((cases-m)/(knots+1))+k*floor(((cases-m)/(knots+1.0))+0.5);
		  if((j-l>=knot_space) && (j <=cases-m-knot_space) )
		    {
		      *mesh_ptr = levels[j];
		      mesh_ptr++;
		      l = j;
		    }
		  else
		    {
		      if(knots_per_pred[i]>0)
			{
			  knots_per_pred[i]=knots_per_pred[i]-1;
			}
		    }
		}
	      knots = knots_per_pred[i];
	      k=1;
	      for(j=0;j<(model_size-1)*2;j++)
		{
		  if(startmodel[j*2]-1 == i && startmodel[(j*2)+1]==1)
		    {
		      *mesh_ptr =startknots[j];
		      startmodel[(j*2)+1] = knots+k-nstartknots;
		      k=k+1;
		      mesh_ptr++;
		    }
		}
	    }
	 }
    }
  return mesh;
}

/*==============================================================*/
static int fit_model(double *mesh)
	     
/*==============================================================*/
{
/*--------------------------------------------------------------
YtXXtX_expanded is a matrix of YtX and XtX together, the first rows
being the YtX part where Y is the matrix of responses and X is the 
design matrix for basis function.
--------------------------------------------------------------*/

  struct basis_function_matrix *predictor_basis_functions;
  struct basis_function_matrix *candidate_basis_functions;
  struct basis_function_matrix *model=0;
  struct basis_function_matrix *candidates=0;
  int i,j,k,ok;
  int col_minder;
  struct matrix2 *YtXXtX_expanded;
  double standardise_const;
  double intercept_sd;

  if(Verbose == TRUE)
    {
     Rprintf("\n");
    }
/* gcv measure of fit to find overall model*/
  GCV= -1.0;
 


  /*--allocating space for data structures matrix sizes are calculated 
   as the maximum possible needed */

  XtX_inverse = create_matrix1(0,0);
  XtX_inverse->matrix 
	= (double *)Salloc(max_model_size*max_model_size,double);
   
  
  XtX_newinverse = create_matrix1(0,0);
  XtX_newinverse->matrix 
	= (double *)Salloc(max_model_size*max_model_size,double);
     
  new_XtXcolumn = create_matrix1(0,0);
  new_XtXcolumn->matrix 
	= (double *)Salloc(max_model_size,double);

  function_values = create_matrix1(0,0);
  function_values->matrix 
       = (double *)Salloc(cases,double);
  function_values->nrow=cases;
  function_values->ncol = 1;
  
  function_values_2 = create_matrix1(0,0);
  function_values_2->matrix 
       = (double *)Salloc(cases,double);
  
  function_values_2->nrow= 1;
  function_values_2->ncol = 1;
  
  Rao_B = create_matrix1(0,0);
  Rao_B->matrix 
       = (double *)Salloc(max_model_size-1,double);
  
  
  Rao_F = create_matrix1(0,0);
  Rao_F->matrix = (double *)Salloc(max_model_size-1,double);
  
  Rao_E = create_matrix1(0,0);/*always a 1x1 matrix*/

  Rao_E_inverse = create_matrix1(0,0);
  
  Rao_F_E_inverse_Ft = create_matrix1(0,0);
  Rao_F_E_inverse_Ft->matrix 
       = (double *)Salloc((max_model_size-1)*(max_model_size-1),double);
   
  
  Rao_F_E_inverse = create_matrix1(0,0);
  Rao_F_E_inverse->matrix = (double *)Salloc(max_model_size-1,double);
  

  YtXXtX_newinverseXtY = create_matrix1(0,0);
  YtXXtX_newinverseXtY->matrix 
       = (double *)Salloc(responses*responses,double);
  
  XtX_newinverseXtY = create_matrix1(0,0);
  XtX_newinverseXtY->matrix 
       = (double *)Salloc(max_model_size*responses,double);
    
 
  YtX = create_matrix1(0,0);
  YtX->matrix 
       = (double *)Salloc(max_model_size*responses,double);
  
  new_X_matrix = create_matrix1(0,0);
  new_X_matrix->matrix 
       = (double *)Salloc(max_model_size*cases,double);
  

  temp_matrix = create_matrix1(0,0);
  if(responses < max_model_size){i = max_model_size;}else{i=responses;}
  temp_matrix->matrix = (double *)Salloc(i*cases,double);
    
  best_model_sd_mean = (double *)Salloc(max_model_size*2,double);
 
  
  if(testset == TRUE)
    {
      model_sd_mean = (double *)Salloc(max_model_size*2,double);

      coefficents = (double *)Salloc(max_model_size*responses,double);
	
      if(classification == FALSE)
	{
	  tset_RSS = (double *)Salloc(responses,double);
	}
      else
	{ 
	  
	  response_class = (int *)Salloc(cases,int);
	  response_max = (double *)Salloc(cases,double);
	}

    }
  
  best_XtXinv = (double *)Salloc(max_model_size*max_model_size,double);

 
/*---Creating structure to hold the model functions------------------*/
  for(i = 0;i<predictors;i++)
    {
      predictor_basis_functions 
	   = (struct basis_function_matrix *)Salloc(1,struct basis_function_matrix);

      predictor_basis_functions->next_predictor = model;
      predictor_basis_functions->number_of_basis_functions=0;
      predictor_basis_functions->predictor_index=predictors-i;
      model = predictor_basis_functions;
    }

 
/*---Creating the structure for candidate functions--------------*/
  for(i = 0;i<predictors;i++)
    {
      candidate_basis_functions 
	   = (struct basis_function_matrix *)Salloc(1,struct basis_function_matrix);
	
	
      candidate_basis_functions->next_predictor = candidates;
      candidate_basis_functions->number_of_basis_functions=0;
      candidate_basis_functions->predictor_index=predictors-i;
      candidates = candidate_basis_functions;
    }
/*--computes YtY ----Y is just the first column(s) of the data matrix---*/

  col_minder = data_matrix -> ncol;
  data_matrix -> ncol = responses;
/*--reduce the dimensions of the matrix to just the part contain Y for
    multiplication--*/
  
  YtY=create_matrix1(0,0);
  YtY->nrow=responses;
  YtY->ncol=responses;
  YtY->matrix = (double *)Salloc (responses*responses,double);

  matrix_multiplication1(data_matrix,data_matrix,YtY,1);
  data_matrix -> ncol =col_minder;
/*--create matricies that control the order of addition and deletion. 
    A non-linear basis function can be added only where a linear one 
    already exists--*/

/*-- these matricies hold information on whether linear functions
are in model as a function with a knot can only be added after
a linear function
------------------------------------------------*/
      order_keeper1 = (int *)Salloc(predictors,int);
	
    
      
      for(i = 0;i<predictors;i++)
	{
	  order_keeper1[i]=0;
	}
/*--find out the maximum number of knots a predictor has */
      
      for(i=0;i<predictors;i++)
	{
	  
	  if(i==0)
	    {
	      max_knots=knots_per_pred[0];
	    }
	  else
	    {
	      if(knots_per_pred[i]>max_knots)
		{
		  max_knots = knots_per_pred[i];
		}
	      }
	}
      if(max_knots < 0)max_knots = 0;
      order_keeper2 = (int *)Salloc(predictors*predictors,int);
      for(i = 0;i<predictors*predictors;i++)
	{
	  order_keeper2[i]=0;
	}
      
      if(max_knots != 0)
	{
	  order_keeper3 = (int *)Salloc(predictors*max_knots,int);
	  for(i = 0;i<predictors*max_knots;i++)
	    {
	      order_keeper3[i]=0;
	    }
	}

/*-fit the initial model if one was specified or just the constant model--*/
  ok = initial_model(model,&YtXXtX_expanded);
  
  if(ok == 0)
    {
/*---FITTING THE MODEL------------------------------------------*/
      k = model_size;

      for(i=0;i<max_model_size-k;i++)
	{
          /*look for possible basis functions to fit to the model*/
          find_candidates(YtXXtX_expanded,
			  mesh,
			  model,
			  candidates);
    
	  j=find_best_candidate(YtXXtX_expanded,
				model,
				candidates,
				mesh);
	  
	  if(j==0)
	    {/*if no more candidates can be fit end search*/
              i = max_model_size;
	      ok=5;
	    }
           else
	     {
	      if(*weighted == TRUE)
		{
		  temp_matrix->nrow = model_size;
		  temp_matrix->ncol = cases;
		  matrix_multiplication1(X_matrix,weight_matrix,temp_matrix,3);
		  matrix_multiplication1(temp_matrix,X_matrix,XtX_inverse,0);
		}
	      else
		{
		  matrix_multiplication1(X_matrix,X_matrix,XtX_inverse,1);
		}
	      ok = invert_matrix(XtX_inverse);
	      
	     }
	  }
      
      j=model_size;
      for(i=0;i<j-1;i++)
	{
	  /*-reduce the model stepwise to its minimum size--*/
	 if(reduce_model(YtXXtX_expanded,model) == FALSE)
	   {
	     i=j;
	   }
	}
    
/*basis functions coefficents have to be converted back from their
standardised forms*/

      if(*bestmodel_size > 1)
	{
	  coef_sd_const[0] = best_XtXinv[0];
	  for(i=1;i<*bestmodel_size;i++)
	    {
	      coef_sd_const[0] = coef_sd_const[0] + 
		((best_model_sd_mean[((i-1)*2)+1]*best_model_sd_mean[((i-1)*2)+1])/
		 (best_model_sd_mean[((i-1)*2)]*best_model_sd_mean[((i-1)*2)]))*
		best_XtXinv[i+(*bestmodel_size)*i];
	    }
	  for(i=1;i<*bestmodel_size;i++)
	    {
	      coef_sd_const[0] = coef_sd_const[0]
		- 2*best_XtXinv[i]*best_model_sd_mean[((i-1)*2)+1]/
		best_model_sd_mean[((i-1)*2)];
	    }
	  for(i=1;i<*bestmodel_size;i++)
	    {
	      for(j=i+1;j<*bestmodel_size;j++)
		{
		  
		  coef_sd_const[0]  = coef_sd_const[0] 
		    + 2*best_XtXinv[i*(*bestmodel_size)+j]
		    *(best_model_sd_mean[((i-1)*2)+1]/
		      best_model_sd_mean[((i-1)*2)])
		    *best_model_sd_mean[((j-1)*2)+1]/
		    best_model_sd_mean[((j-1)*2)];
		}
	      
	      
	    }
	}
      else
	{
	  coef_sd_const[0] = 1;
	}
/*For the standard errors of the non-transformed basis functions the variances
 of the transformed functions are adjusted using their SD, means and covariances*/
 
    for(i=1;i<*bestmodel_size;i++)
      {
	coef_sd_const[i]=best_XtXinv[i*(*bestmodel_size)+i]
	  /(best_model_sd_mean[((i-1)*2)]*best_model_sd_mean[((i-1)*2)]);
      }

      for(i=0;i<responses;i++)
	{
	  standardise_const=0.0;
	  for(j=1;j<(*bestmodel_size);j++)
	    {
	      standardise_const=standardise_const+(best_coefficents[j+(i*(*bestmodel_size))]
						   *best_model_sd_mean[((j-1)*2)+1])/best_model_sd_mean[(j-1)*2];
	    }
	  best_coefficents[i*(*bestmodel_size)]
	    =best_coefficents[i*(*bestmodel_size)]-standardise_const;
	}
   
      for(i=0;i<responses;i++)
	{
	  for(j=1;j<(*bestmodel_size);j++)
	    {
	      
	      best_coefficents[j+(i*(*bestmodel_size))]
		=best_coefficents[j+(i*(*bestmodel_size))]/best_model_sd_mean[(j-1)*2];

      	    }
	}
      
    }
  else
    {
      ok =4;
    }
  
  return ok;
}

/*==============================================================*/
static void find_candidates(struct matrix2 *YtXXtX_expanded,
		     double *mesh,
		     struct basis_function_matrix *model,
		     struct basis_function_matrix *candidates)
/*==============================================================*/
{
/*---------------------------------------------------------------
Loops through all predictors and figures out what should be 
considered as a candidate for addition at this stage, must 
keep hierarchical rules and check with what already is a candidate
and in the model.
checks the model and candidates matricies to see what is already
there.
-----------------------------------------------------------------*/
  int i,j,k,l;
  int function_in_model;
  struct basis_function_matrix *current_predictor;
  struct basis_function_matrix *current_predictor2;

  current_predictor = model;
/*loop through the predictors looking for candidates to add to
  the list of candidates (possible basis functions to add to the
  current model. Candidates stay as candidates from one iteration 
  to the next*/
  for(i = 0;i<predictors;i++)
    {
/*on first search for candidates (model_size==1) all linear basis functions
  become candidates-*/
      if(step_count==0 && knots_per_pred[i]>=0)
	{
	  if(!(in_model(i+1,0,0,0,model))
	     && new_candidate(i+1,0,0,0,candidates,model,YtXXtX_expanded))
	    {
	      fit_as_candidate(i+1,0,0,0,YtXXtX_expanded,mesh);
	    }

	}
     /*if the linear function has been added basis functions
with knots can be added to candidates. In any case if it is categorical all 
possible levels become candidates at start.*/
      if(current_predictor->number_of_basis_functions !=0 
	 || (knots_per_pred[i]<0 &&model_size==1) )
	{
	  
	  for(j=0;j<(int)abs(knots_per_pred[i]);j++)
	    {
	      if(!(in_model(i+1,j+1,0,0,model))&& 
		 new_candidate(i+1,j+1,0,0,candidates,model,YtXXtX_expanded))
		{
		  fit_as_candidate(i+1,j+1,0,0,YtXXtX_expanded,mesh);
		  
		  
		}
	    }
	}
/*categorical variables are not allowed in interactions--*/
      if(additive == FALSE ||  knots_per_pred[i] <0) 
	{
	      
/*---------------------------------------------------------------------------
two term candidates - interaction terms
every predictor function in the model can be combined with another
according to hierarchical rules

----------------------------------------------------------------------------*/
	  if(current_predictor->number_of_basis_functions !=0 && i<predictors)
	    {
	      current_predictor2 = current_predictor->next_predictor;
	      for(j=i+1;j<predictors;j++)
		{
		  if(current_predictor2->number_of_basis_functions !=0)
		    {
		      function_in_model = in_model(i+1,0,j+1,0,model);
		      /*never true for categorical variables--*/
		      if((!function_in_model) &&
			 new_candidate(i+1,0,j+1,0,candidates,model,YtXXtX_expanded))
			{
			  fit_as_candidate(i+1,0,j+1,0,
					   YtXXtX_expanded,
					   mesh);
			}
		      if(function_in_model)
			{
		          for(k=0;k<knots_per_pred[i+1];k++)
			    {
			      if((!in_model(i+1,k+1,j+1,0,model))&&
				 new_candidate(i+1,k+1,j+1,0,
					       candidates,
					       model,
					       YtXXtX_expanded))
				{
				  fit_as_candidate(i+1,k+1,j+1,0,
						   YtXXtX_expanded,
						   mesh);
				}
			    }
			  for(k= -1;k<knots_per_pred[i+1];k++)
			    {
			      for(l= -1;l<knots_per_pred[j];l++)
				{
				  if(k!= -1 || l != -1)
				    {
				      if((!in_model(i+1,k+1,j+1,l+1,model))
					 &&
					 (new_candidate(i+1,k+1,j+1,l+1,
							candidates,model,
							YtXXtX_expanded)))
					{
					  fit_as_candidate(i+1,k+1,j+1,l+1,
							   YtXXtX_expanded,
							   mesh);
				   
					}
				    }
				}
			    }
			}
		    }
		  current_predictor2=current_predictor2->next_predictor;
		}
	      }  
	}
	
      current_predictor = current_predictor->next_predictor;
      
    }
  
 return;
}
/*==============================================================*/
static int in_model(int predictor1,
	     int knot1_index, 
	     int predictor2,
	     int knot2_index,
	     struct basis_function_matrix *model)
/*==============================================================*/
{
/* Check to see if a certain function given by its knots and predictor
   numbers is in the model */
/* the basis function is specified by it's predictor an knot indicies*/
  struct basis_function_matrix* current_predictor;
  struct basis_function *current_function;
  int i, predictor_functions;

  current_predictor = model;
  if(model_size == 1)
    {
      return FALSE;
    }
  else
    {
      
      while(current_predictor->predictor_index < predictor1)
	{
	  current_predictor = current_predictor->next_predictor;
	}
   
      predictor_functions = current_predictor->number_of_basis_functions;
      if(predictor_functions==0)
	{
	  return FALSE;
	}
      current_function = current_predictor->functions;
      /*scan through the predictors basis functions looking for a match*/
      /*Predictor1 is always a lower value than predictor2 so there isn't
       a basis function that is symatric (with predictor1 and predictor2
       swapped)*/
      for(i=0;i<predictor_functions;i++)
	{
      
	  if(current_function->knot1_index == knot1_index 
	     && current_function->predictor1==predictor1 
	     && current_function->knot2_index == knot2_index 
	     && current_function->predictor2== predictor2)
	    {
	      return TRUE;
	    }
	  if(i != predictor_functions-1)
	    {
	      current_function= current_function->link;
	    }
	}
    }
  return FALSE;
}
/*--------------------------------------------------------------*/
static void fit_as_candidate(int predictor1, 
		      int knot1_index,
		      int predictor2,
		      int knot2_index,
		      struct matrix2 *YtXXtX_expanded,
		      double *mesh)
/*--------------------------------------------------------------*/
{
/* fits a candidate which was found in "find candidate". The column 
of YtXXtX and the entry in the candidates matrix are already in place.
It must evaluate the inner product between the new candidate and the 
functions already in the model also its inner product with the 
responses and all is stored in the YtXXtX matrix*/
  int i,j;
  double *current_predictor1,*current_predictor2;
  int mesh_index;
  double knot1_value,knot2_value;
  double entry;
  struct link *new_column;/*----index of last row of matrix-----*/
  double *function_value,SD,mean;

/*--function value stores the value of the function at the predictor 
values for each case */ 

/*---put new entries into YtXXtX for new candidate-------------------*/
  new_column= YtXXtX_expanded->column_list;
  for(i=0;i<YtXXtX_expanded-> ncol-1;i++)
    {
      new_column=new_column->next;

    }

/*--evaluate and store candidate function value for each case ---*/
  function_value = function_values->matrix;
/*-find the knot values using the indicies for the mesh----------*/
  if(knot1_index != 0)
    {
      mesh_index=0;
      for(i=0;i<predictor1-1;i++)
	{
	  mesh_index = mesh_index+abs(knots_per_pred[i]);
	}
      knot1_value = mesh[mesh_index+knot1_index-1];
      new_column->function->knot1_value = knot1_value;
    }
  if(knot2_index != 0)
    {
      mesh_index=0;
      for(i=0;i<predictor2-1;i++)
	{
	  mesh_index = mesh_index+abs(knots_per_pred[i]);
	}
      knot2_value = mesh[mesh_index+knot2_index-1];
      new_column->function->knot2_value = knot2_value;
    }
  
/*--find the data values for the corresponding predictor(s) in the function
move to row of predictor of interest in data matrix---*/
  current_predictor1 = 
    &data_matrix->matrix[((predictor1-1)+responses)*cases];
current_predictor2 = 
    &data_matrix->matrix[((predictor2-1)+responses)*cases];

  for(i=0;i<cases;i++)
    {
      if(knots_per_pred[predictor1-1]>=0)
	{
	  *function_value = *current_predictor1;
	  if(knot1_index!=0)
	    {
	      *function_value = *function_value - knot1_value;
	      if(*function_value <0){*function_value = 0.0;}
	    }
	}
      else
        {
	  /*else it is categorical*/
	  if((int)*current_predictor1 == (int)knot1_value)
		{*function_value =1;}
	      else
		{*function_value =0;}
	}
      if(predictor2 != 0)
	{
         if(knots_per_pred[predictor2-1]>=0)
	   {
	     if(knot2_index == 0)
	       {
		 *function_value= *function_value* (*current_predictor2);
	       }
	     else
	       {
		 if(*current_predictor2-knot2_value < 0.0)
		   {*function_value =0.0;}
		 else
		   {*function_value =*function_value*(*current_predictor2-knot2_value);}
	       }
	     
	   }
	 else
	   {
	     /*else it is categorical*/
	     if((int)*current_predictor2 == (int)knot2_value)
	       {*function_value =1;}
	     else
	       {*function_value =0;}
	   }
	}
      function_value++;
      current_predictor1++;
      current_predictor2++;
    }

   
  mean = 0.0;
  SD = 1.0;
  standardise_array(function_values->matrix,cases,&mean,&SD);
  
  
/*--put the info about the mean and standard deviation into the function--*/
  new_column->function->SD=SD;
  new_column->function->mean=mean;
/*--YtX inner product--------------------------*/
  for(i = 0; i < responses; i++)
    {
      entry = 0.0;
      for(j=0;j<cases;j++)
	{
	  
	  entry=entry+ (data_matrix->matrix[(i*cases)+j])
	    *function_values->matrix[j];
	}
      new_column->data[i]=entry;
    }
/*get XtX inner products by matrix multiplication*/
 
  new_XtXcolumn->nrow=model_size;
  new_XtXcolumn->ncol=1;
  if(*weighted == TRUE)
    {
      temp_matrix->nrow=model_size;
      temp_matrix->ncol=cases;
      matrix_multiplication1(X_matrix,weight_matrix,temp_matrix,3);
      matrix_multiplication1(temp_matrix,function_values,new_XtXcolumn,0);
    }
  else
    {
      matrix_multiplication1(X_matrix,function_values,new_XtXcolumn,1);
    }
  for(i = 0; i < model_size; i++)
    {
     
      new_column->data[responses+i]=new_XtXcolumn->matrix[i];
    }

  if(*weighted == TRUE)
    {
      temp_matrix->nrow=1;
      temp_matrix->ncol=cases;
      matrix_multiplication1(function_values,
			     weight_matrix,
			     temp_matrix,
			     3);
      matrix_multiplication1(temp_matrix,
			     function_values,
			     function_values_2,
			     0);
    }
  else
    {
      matrix_multiplication1(function_values,
			     function_values,
			     function_values_2,
			     1);
    }
  new_column->data[responses+model_size]=function_values_2->matrix[0];
  return;
}


/*==============================================================*/
static int new_candidate(int predictor1,
		  int knot1_index, 
		  int predictor2,
		  int knot2_index,
		  struct basis_function_matrix *candidates,
		  struct basis_function_matrix *model,
		  struct matrix2 *YtXXtX_expanded)
/*==============================================================*/
{
/*Checks to see whether the function described by its predictor and
knot indicies is already a candidate. If it is not then it adds the 
the candidate to the "candidates matrix" and also create a column in 
YtXXtX for it. This function is only called if the function 'in model"
returns false when finding candidates */
  struct basis_function_matrix* current_predictor;
  struct basis_function *current_function;
  struct basis_function *new_function;
  struct link *current_column;
  struct link *new_column;
  double *new_column_data;
  int i;
  
  current_predictor = candidates;
  if(interaction_specs_size >0)
    {
      for(i=0;i<interaction_specs_size;i++)
	{
	  if(interaction_specs[i*2] == predictor1 
	     && interaction_specs[(i*2)+1] == predictor2)
	    {
	      
	      return FALSE;
	    }
	  if(interaction_specs[i*2] == predictor2 
	     && interaction_specs[(i*2)+1] == predictor1)
	    {
	      return FALSE;
	    }
	}
    }
    
/* Checks according to addition rules that the prerequisites for 
addition are in the model*/
  if( predictor2 != 0)
    {
      if(!(in_model(predictor2,knot2_index,0,0,model) &&
	   in_model(predictor1,knot1_index,0,0,model)))
	{
	  return FALSE;
	}
      if(knot1_index !=0 && knot2_index != 0)
	{
	  if(!in_model(predictor1,knot1_index,predictor2,0,model))
	    {
	      return FALSE;
	    }
	  if(!in_model(predictor1,0,predictor2,knot2_index,model))
	    {
	      return FALSE;
	    }
	}
    }
    
  /*--move to the predictors column in the candidate function matrix --
  -check if the function is there, 
  if not put it in and add a column to YtXXtX--*/
  while(current_predictor->predictor_index != predictor1)
    {
      current_predictor = current_predictor->next_predictor;
    }

  if(current_predictor->number_of_basis_functions ==0)
    {
      new_function= 
	   (struct basis_function *)Salloc (1,struct basis_function);
      new_function->knot1_index = knot1_index;
      new_function->predictor1 = predictor1;
      new_function->predictor2 = predictor2;
      new_function->knot2_index = knot2_index;
      if(new_function->knot1_index == 0) new_function->knot1_value = 0.0;
      if(new_function->knot2_index == 0) new_function->knot2_value = 0.0;
      current_predictor->number_of_basis_functions = 1;
      current_predictor->functions = new_function;
      
    }
  else
    {
      current_function = current_predictor->functions;
      for(i=0;i<current_predictor->number_of_basis_functions;i++)
	{
	  
	  if(current_function->knot1_index == knot1_index && 
	     current_function->predictor2 == predictor2 &&
	     current_function->knot2_index == knot2_index)
		{
		  return FALSE;
		}
	  if(i != current_predictor->number_of_basis_functions-1)
		{
		  current_function = current_function->link;
		}
	}
      new_function = (struct basis_function *)Salloc (1,struct basis_function);
      current_function->link = new_function;
      new_function->knot1_index = knot1_index;
      new_function->predictor1 = predictor1;
      new_function->predictor2 = predictor2;
      new_function->knot2_index = knot2_index;
      if(new_function->knot1_index == 0) new_function->knot1_value = 0.0;
      if(new_function->knot2_index == 0) new_function->knot2_value = 0.0;
      /*the actual values of the knots are entered  later*/
      current_predictor->number_of_basis_functions++;

    }

  
  /*--add a column to  YtXXtX_expanded--*/
  current_column = YtXXtX_expanded->column_list;
  for(i=0;i<YtXXtX_expanded->ncol-1;i++)
    {
      current_column=current_column->next;
    }
  new_column = (struct link *)Salloc(1,struct link);
   
  current_column->next = new_column;
 
  new_column_data = 
       (double *)Salloc(max_model_size+responses+1,double);
  
  new_column->data = new_column_data;
  new_column->function = new_function;
  YtXXtX_expanded->ncol++;
  return TRUE;
}


/*==============================================================*/
static int find_best_candidate(struct matrix2 *YtXXtX_expanded,
			struct basis_function_matrix *model,
			struct basis_function_matrix *candidates,
	       		double *mesh)
/*==============================================================*/
{
/*- calculates the best candidate to add by adding it to the model
and computing the "residual sum of squares". the function returns
an index to the best candidate it finds*/
  int number_of_candidates;
  int i, j, k,l,m,index;
  int nrow;
  double column_minder,row_minder;
  double E, E_inv;
  struct basis_function *model_function;
  struct link *YtXXtX_column;
  double Rao_D;
  struct link *current_column, *trailing_column;
  double RSS_so_far;
  double rss_for_model,gcv_for_model,gcv_so_far;
  int best_candidate;
  int candidate_found;

 
  RSS_so_far = -1;
  gcv_so_far = -1;
  best_candidate =0;
  candidate_found = FALSE;
  /*--computes XtX_inverse with new candidate added by Rao Linear algebra
    p33------*/
  number_of_candidates = (YtXXtX_expanded->ncol)-model_size; 
  XtX_newinverse->nrow=model_size+1;
  XtX_newinverse->ncol=model_size+1;
 
  for(i=0;i<number_of_candidates;i++)
    {
      trailing_column = YtXXtX_expanded->column_list;
      current_column = YtXXtX_expanded->column_list;
      
      for(k=0;k<model_size+i;k++)
	{
	  trailing_column = current_column;
	  current_column = current_column->next;
	}
      
      Rao_B->nrow = model_size;
      Rao_B->ncol = 1;
      for(j=0;j<model_size;j++)
	{
	  Rao_B->matrix[j]=current_column->data[j+responses];
	}
      
      Rao_D = current_column->data[model_size+responses];
      
      Rao_F->nrow = model_size;
      Rao_F->ncol = 1;
      matrix_multiplication1(XtX_inverse,
			     Rao_B,
			     Rao_F,
			     1);
      
      Rao_E->nrow=1;
      Rao_E->ncol=1;
      Rao_E->matrix = &E;
      matrix_multiplication1(Rao_B,
			     Rao_F,
			     Rao_E,
			     1);

      if(1/(Rao_D-Rao_E->matrix[0]) < tolerance 
         || Rao_D-Rao_E->matrix[0] < tolerance)
	{
	  trailing_column->next = current_column->next;
	  i--;
	  number_of_candidates--;
	  YtXXtX_expanded->ncol--;
	}
      else
	{
	  
	  Rao_E->matrix[0] = Rao_D-Rao_E->matrix[0];
	  Rao_E_inverse->nrow=1;
	  Rao_E_inverse->ncol=1;
	  Rao_E_inverse->matrix = &E_inv;
	  Rao_E_inverse->matrix[0]=1/Rao_E->matrix[0];
	  Rao_F_E_inverse->nrow = model_size;
	  Rao_F_E_inverse->ncol = 1;
	  matrix_multiplication1(Rao_F,
				 Rao_E_inverse,
				 Rao_F_E_inverse,
				 0);

	  Rao_F_E_inverse_Ft ->nrow = model_size;
	  Rao_F_E_inverse_Ft ->ncol = model_size;
	  matrix_multiplication1(Rao_F,Rao_F_E_inverse,Rao_F_E_inverse_Ft,2);
	  nrow = XtX_newinverse->nrow;
	  for(j=0;j<model_size;j++)
	    {
	      for(k=0;k<model_size;k++)
		{
		  index=(j*model_size)+k;
		  XtX_newinverse->matrix[(j*nrow)+k]
		    = XtX_inverse->matrix[index]
		      + Rao_F_E_inverse_Ft->matrix[index];
		}
	    }
	  for(j=0;j<model_size;j++)
	    {
	      index=(j*nrow)+model_size;
	      XtX_newinverse->matrix[index]
		= -Rao_F_E_inverse->matrix[j];
	      index=(model_size*nrow) + j;
	      XtX_newinverse->matrix[index]= -Rao_F_E_inverse->matrix[j];
	    }
	  index = (model_size*nrow) + model_size;
	  XtX_newinverse->matrix[index]=Rao_E_inverse->matrix[0];
	  	
	  
/*---computes YtY-YtX(XtX)^1XtY--and- sums the diagonal to get the RSS------*/
/*---uses data matrix with nrow changed for Yt-----*/
/*- the column of the YtXXtX matrix corresponding to the candidate 
function being considered*/

/*switch the column of the candidate so it is adjacent to the model columns*/
	  switch_columns(model_size+i+1,model_size+1,YtXXtX_expanded);

/*we wish to use only the YtX part of the YtXXtX matrix for the current 
model and the candidate being considered*/
	  column_minder=YtXXtX_expanded->ncol;
	  row_minder=YtXXtX_expanded->nrow;
	  YtXXtX_expanded->ncol=model_size+1;

	  YtXXtX_expanded->nrow=responses;
	  
	  XtX_newinverseXtY->nrow = model_size+1;
	  XtX_newinverseXtY->ncol = responses;
	  matrix_multiplication2(XtX_newinverse,
				 YtXXtX_expanded,
				 XtX_newinverseXtY,
				 0);

	  YtXXtX_newinverseXtY->nrow = responses;
	  YtXXtX_newinverseXtY->ncol = responses;
	  matrix_multiplication2(XtX_newinverseXtY,
				 YtXXtX_expanded,
				 YtXXtX_newinverseXtY,
				 1);
	  
	  YtXXtX_expanded->nrow=row_minder;
	  YtXXtX_expanded->ncol=column_minder;

	  rss_for_model =0.0;
	  for(j=0;j<responses;j++)
	    {
	      rss_for_model=rss_for_model
		-YtXXtX_newinverseXtY->matrix[j*(responses+1)]
		+YtY->matrix[j*(responses+1)];
              /* if the residual sum of squares is */ 
	      /* negative discard candidate  */
              if((YtY->matrix[j*(responses+1)]
                  -YtXXtX_newinverseXtY->matrix[j*(responses+1)])<0.0)
                {
		  rss_for_model = -1.0;
                  j=responses;
		}
	    }
	  if(rss_for_model>=0.0)
	    {
	      for(j=0;j<responses;j++)
		{
		  if((YtY->matrix[j*(responses+1)]
		      -YtXXtX_newinverseXtY->matrix[j*(responses+1)]) > 
		     rssgcv[(step_count)*(responses+1)+j])
		    {
		      /* if the residual sum of squares go up for any reason*/
		      /* the candidate is rejected.*/
		      rss_for_model = -1;
		    }
		}
	    }     
	  
	  if(rss_for_model > 0.0)
	    {
              candidate_found =TRUE;
	      if(RSS_so_far !=-1)/* if first candidate*/
		{
		  if(rss_for_model < RSS_so_far)
		    {
		      best_candidate = i;
		      RSS_so_far = rss_for_model;
		      for(j=0;j<responses;j++)
			{
/*puts the rss for each response in the rssgcv matrix for output */
			  rssgcv[(step_count+1)*(responses+1)+j]=
			    -YtXXtX_newinverseXtY->matrix[j*(responses+1)]
			    +YtY->matrix[j*(responses+1)];
			}
		    }
		}
	      else
		{
		  best_candidate = i;
		  RSS_so_far = rss_for_model;
		  for(j=0;j<responses;j++)
		    {
		      rssgcv[(step_count+1)*(responses+1)+j]=
			-YtXXtX_newinverseXtY->matrix[j*(responses+1)]
			+YtY->matrix[j*(responses+1)];
		    }
		}
	     
/* gcv (or other criterion) is calculated, the best model in this call 
of the function if it is better than the best global model gets saved, 
in any case the gcv for this case is saved in rssgcv */  
	      if(testset == FALSE)
		{
		  gcv_for_model = 
		    (rss_for_model /weight_sum)/
		    ((1.0 - (GCVconstant*(model_size+1)/cases))
		     *(1.0 - (GCVconstant*(model_size+1)/cases)));
		}
	      else
		{
                  gcv_for_model = testset_RSS(YtXXtX_expanded,model_size+1);
                }
	      if(model_size == 1 )
		{
		  if(GCV == -1)/*GCV was initially set to -1*/
		    {
		      GCV = gcv_for_model;
		    }
		}
	      if(gcv_for_model < gcv_so_far || gcv_so_far==-1.0)
		{
		  gcv_so_far = gcv_for_model;
		}
              
	      if(GCV == -1.0 || gcv_for_model <GCV)
		{
		  GCV = gcv_for_model;
		  for(l=0;l<(XtX_newinverseXtY->ncol*XtX_newinverseXtY->nrow);l++)
		    {
		      best_coefficents[l] = XtX_newinverseXtY->matrix[l];
                      
		     
                      
		    }
		  for(l=0;l<(XtX_newinverse->ncol);l++)
		    {
		      for(m=0;m<(XtX_newinverse->ncol);m++)
			{
			  best_XtXinv[l+m*XtX_newinverse->ncol]=
			    XtX_newinverse->matrix[l+m*XtX_newinverse->ncol];

			}
		      
		    }
                 
                 
                  
		
		  *bestmodel_size = model_size+1;
		  
		  YtXXtX_column = YtXXtX_expanded->column_list;
		  /*storing the best model */
                  
                  
		  for(l=0;l<model_size+1;l++)
		    {
		      if(l != 0)
			{
                          
			  model_function = YtXXtX_column->function;
			  best_model[(l-1)*4]= model_function->predictor1;
			  best_model[((l-1)*4)+1]=model_function->knot1_index;
			  best_model[((l-1)*4)+2]=model_function->predictor2;
			  best_model[((l-1)*4)+3]=model_function->knot2_index;
			  best_model_sd_mean[((l-1)*2)]=model_function->SD;
			  best_model_sd_mean[((l-1)*2)+1]=model_function->mean;
                  
                          
			}
		      YtXXtX_column = YtXXtX_column->next;
		    }
		  
		}
	    }
           
	  /* switch 'candidates column' back to its original position*/
	  switch_columns(model_size+i+1,model_size+1,YtXXtX_expanded);
	}
      rssgcv[(step_count+1)*(responses+1)+responses]=gcv_so_far;
    }
  if (candidate_found != FALSE)
    {
      update_model(YtXXtX_expanded,
		   model,
		   candidates,
		   best_candidate,
		   mesh);
   
      rss_for_model =0.0;
      RSS_so_far =0.0;
      for(i =0;i<responses;i++)
	{
         rss_for_model = rss_for_model +rssgcv[(step_count)*(responses+1)+i];
         RSS_so_far    = RSS_so_far+rssgcv[(step_count-1)*(responses+1)+i];
         
	}
   
    }
  /* returns whether a candidate was found or not*/
  return(candidate_found);
}

/*==============================================================*/
static void update_model(struct matrix2 *YtXXtX_expanded,
		  struct basis_function_matrix *model,
		  struct basis_function_matrix *candidates,
		  int candidate,
		  double *mesh)
		  
/*==============================================================*/
{
/*--update YtXXtX_expanded ; model taking from candidates,X_matrix---*/
/* candidate is an index corresponding to the candidates column (after the
   model columns) in the YtXXtX_expanded matrix, starts from 0*/
  int i,j,predictor1,knot1_index,predictor2,knot2_index,mesh_index,k,index,nrow;
  double *current_predictor_values1,*current_predictor_values2, knot1_value,knot2_value;

  struct basis_function_matrix* current_candidate_predictor;
  struct basis_function *current_candidate_function;
  struct basis_function *trailing_function;
  struct basis_function *new_model_function;
  struct basis_function_matrix* current_model_predictor;
  struct basis_function *current_model_function;
  struct link *candidates_column;
  struct link *current_column;
  struct basis_function *current_function;
  double *function_value,*X_matrix_ptr;
  double matrix_entry;
  double Rao_D;
  double E,E_inv;
  double dummy1,dummy2;

  
  steps[(step_count+1)*2]=1;
  steps[((step_count+1)*2)+1]=model_size+1;
  step_count++;
  model_size++;
  
  switch_columns(model_size,model_size+candidate,YtXXtX_expanded);
/*--------find function in YtXXtX_expanded-------------------------*/
  candidates_column=YtXXtX_expanded->column_list;
  for(i=0;i<model_size-1;i++)
    {
      candidates_column=candidates_column->next;
    }
  candidates_column->data[model_size+responses] = 0.0;
  new_model_function = candidates_column->function;
 
  if(Verbose == TRUE)
    {
      Rprintf("+ %d : %d ",model_size,new_model_function->predictor1);

      if(knots_per_pred[new_model_function->predictor1-1]>=0 && new_model_function->knot1_index != 0)
       {
	 Rprintf("%f ",new_model_function->knot1_value);
       }
      else
       {
         if(knots_per_pred[new_model_function->predictor1-1]>=0)
          {Rprintf("NA ");}
         if(knots_per_pred[new_model_function->predictor1-1]<0)
	   {Rprintf("%d ",(int)new_model_function->knot1_value);}
       }
      if(new_model_function->predictor2 == 0)
       {
         Rprintf("\n");
       }
      else
       {
        if(new_model_function->knot2_index==0)
         {Rprintf("%d NA\n",new_model_function->predictor2);}
        else
         {Rprintf("%d %f\n",new_model_function->predictor2,new_model_function->knot1_value);}
       }
    }
  /*fflush(stdout);*/
  predictor1 = new_model_function->predictor1;
  predictor2 = new_model_function->predictor2;
/*---------find function in candidates----------*/
  current_candidate_predictor = candidates;
  while(current_candidate_predictor->predictor_index != predictor1)
    {
      current_candidate_predictor = current_candidate_predictor->next_predictor;
    }
  current_candidate_function = current_candidate_predictor->functions;
  trailing_function = current_candidate_predictor->functions;
  while(current_candidate_function != new_model_function)
    {
      trailing_function = current_candidate_function;
      current_candidate_function = current_candidate_function->link;
    }
/* take out of the candidates matrix*/
  if(trailing_function == current_candidate_function)
    {
      current_candidate_predictor->functions 
	= current_candidate_function->link;
    }
  else
    {
      trailing_function->link = current_candidate_function->link;
    }
  current_candidate_predictor->number_of_basis_functions--;
/* add to model matrix*/
  current_model_predictor=model;
  while(current_model_predictor->predictor_index != predictor1)
    {
      current_model_predictor = current_model_predictor->next_predictor;
    }
  current_model_function=current_model_predictor->functions;
  for(i=0;i<current_model_predictor->number_of_basis_functions-1;i++)
    {
      current_model_function= current_model_function->link;
    }
  if(current_model_predictor->number_of_basis_functions==0)
    {
      current_model_predictor->functions= new_model_function;
    }
  else
    {
      current_model_function->link=new_model_function;
    }
  current_model_predictor->number_of_basis_functions++;
  knot1_index = new_model_function->knot1_index;
  knot2_index = new_model_function->knot2_index;
/*-----------------hierarchy terms----------------------------*/
  if(predictor2 == 0)
    {
      if(knot1_index !=0)
	{
	  order_keeper1[predictor1-1]++;
	}
    }
  else
    {
      if(knot1_index !=0 || knot2_index != 0)
	{
	  order_keeper2[((predictor1-1)*predictors)+predictor2-1]++;
	  order_keeper2[((predictor2-1)*predictors)+predictor1-1]++;
	}
      if(knot1_index ==0 || knot2_index == 0)
	{
	  order_keeper1[predictor1-1]++;
	  order_keeper1[predictor2-1]++;
	}
      if(max_knots != 0)
	{
	  if(knot1_index !=0)
	    {
	      order_keeper3[(predictor1-1)*max_knots+knot1_index-1]++;
	    }
	  if(knot2_index != 0)
	    {
	      order_keeper3[(predictor2-1)*max_knots+knot2_index-1]++;
	    }
	}
      }

  
/*------------------update X_matrix----------------------------*/
/*calculate the basis function values at the points of the data matrix*/
  X_matrix->ncol++;
  X_matrix_ptr = &X_matrix->matrix[(model_size-1)*cases];
/*--move to row of predictor of interest in data matrix---*/
  current_predictor_values1 = 
    &data_matrix->matrix[((predictor1-1)+responses)*cases];
  
  if(predictor2!=0)
    {
      current_predictor_values2 = 
	&data_matrix->matrix[((predictor2-1)+responses)*cases];
    }
/*---find the knot values for the basis function--*/
  if(new_model_function->knot1_index !=0)
    {
      knot1_value = new_model_function->knot1_value;
    }
   
  if(new_model_function->knot2_index !=0)
    {
      knot2_value = new_model_function->knot2_value;
    }
  for(i=0;i<cases;i++)
    {
      if(knots_per_pred[predictor1-1] >= 0)
	{
	  *X_matrix_ptr=current_predictor_values1[i];
	  if(knot1_index != 0)
	    {
              
	      *X_matrix_ptr = *X_matrix_ptr - knot1_value;
              
	      if(*X_matrix_ptr <0){*X_matrix_ptr = 0;}
	    }
	}
      else
	{
	  /*else it is categorical*/
	  if((int)current_predictor_values1[i] == (int)knot1_value)
	    {*X_matrix_ptr =1;}
	  else
	    {*X_matrix_ptr =0;}
	}
      if(predictor2 != 0)
	{
	  if(knots_per_pred[predictor2-1] >= 0)
	    {
	      if(knot2_index == 0)
		{
		  *X_matrix_ptr = *X_matrix_ptr * current_predictor_values2[i];
		}
	      else
		{
                  
		  if(current_predictor_values2[i] - knot2_value <0.0)
		    {*X_matrix_ptr = 0.0;}
		  else
		    {
		      *X_matrix_ptr
			= *X_matrix_ptr*(current_predictor_values2[i]
			- knot2_value);
		      
		    }
		}
	    }
	  else
	    {
             /*else it is categorical*/
	      if((int)current_predictor_values2[i] != (int)knot2_value)
		{*X_matrix_ptr=0;}
	    }
	}
      X_matrix_ptr++;
    }
 
  
/*--Standardise the new column of the X matrix---------------------*/
  
      
     for(i=0;i<cases;i++)
       {
	 X_matrix->matrix[((model_size-1)*cases)+i]=
	   (X_matrix->matrix[((model_size-1)*cases)+i]-
	    new_model_function->mean)/new_model_function->SD;
       }
 
  
 /*--computes XtX_inverse with new candidate added by Rao Linear algebra
	    p33------*/
 
  Rao_B->nrow = model_size-1;
  Rao_B->ncol = 1;
  for(j=0;j<model_size-1;j++)
    {
      Rao_B->matrix[j]=candidates_column->data[j+responses];
    }
  
  Rao_D = candidates_column->data[model_size-1+responses];
  
  /*-----------------------------------------------------------------*/
  Rao_F->nrow = model_size-1;
  Rao_F->ncol = 1;
  matrix_multiplication1(XtX_inverse,Rao_B,Rao_F,1);
  
  /*-----------------------------------------------------------------*/
      
  Rao_E->matrix = &E;
  matrix_multiplication1(Rao_B,Rao_F,Rao_E,1);
  Rao_E->matrix[0] = Rao_D-Rao_E->matrix[0];
  
  Rao_E_inverse->nrow =1;
  Rao_E_inverse->ncol = 1;
  Rao_E_inverse->matrix = &E_inv;
  Rao_E_inverse->matrix[0]=1/Rao_E->matrix[0];
  
  Rao_F_E_inverse->ncol= 1;
  Rao_F_E_inverse->nrow = model_size-1;
  matrix_multiplication1(Rao_F,Rao_E_inverse,Rao_F_E_inverse,0);
  
  Rao_F_E_inverse_Ft->nrow = model_size-1; 
  Rao_F_E_inverse_Ft->ncol = model_size-1;
  matrix_multiplication1(Rao_F,Rao_F_E_inverse,Rao_F_E_inverse_Ft,2);
  
  XtX_inverse->nrow++;
  XtX_inverse->ncol++;
  nrow = XtX_inverse->nrow;
  for(j=0;j<(model_size-1);j++)
    {
      for(k=0;k<(model_size-1);k++)
	{
	  
	  
	  XtX_newinverse->matrix[(model_size*j)+k]
	    = XtX_inverse->matrix[((model_size-1)*j)+k]
	    + Rao_F_E_inverse_Ft->matrix[((model_size-1)*j)+k];
	}
    }
  for(i=0;i<model_size*model_size;i++)
    {
      XtX_inverse->matrix[i]= XtX_newinverse->matrix[i];
    }
  for(j=0;j<model_size-1;j++)
    {
      index=(j*nrow)+model_size-1;
      XtX_inverse->matrix[index]
	= -Rao_F_E_inverse->matrix[j];
      index=(model_size-1)*nrow + j;
      XtX_inverse->matrix[index]= -Rao_F_E_inverse->matrix[j];
    }
  index = (model_size*model_size)-1;
  XtX_inverse->matrix[index]=Rao_E_inverse->matrix[0];
  
  
/*----------------------------------------------------------------
update YtXXtX_expanded the new inner product between candidate and 
new function in model are calculated and inserted, the number of
rows grows by 1
----------------------------------------------------------------*/  
  
  YtXXtX_expanded->nrow++;
  nrow = YtXXtX_expanded->nrow;
  current_column =  YtXXtX_expanded->column_list;
  for(i=0;i<model_size-1;i++)
    {
      current_column->data[nrow-2]= 
	candidates_column->data[responses+i];
      
      current_column = current_column->next;
    }
  
  function_values->nrow=cases;
  function_values->ncol=1;
/*--move to first candidate column of the matrix to update it--*/
  current_column = candidates_column->next;
/*move through all the candidates in the YtXXtX matrix */
  for(i=0;i<YtXXtX_expanded->ncol-model_size;i++)
    {
      current_function = current_column->function;
      knot1_index = current_function->knot1_index;
      predictor1=current_function->predictor1;
      knot2_index = current_function->knot2_index;
      predictor2=current_function->predictor2;
/*--move to row of predictor of interest in data matrix---*/
      current_predictor_values1 = 
	&data_matrix->matrix[(current_function->predictor1+responses-1)
			     *cases];
/*--evaluate the functions at each case-- to calculate inner products--*/
      if(predictor2!=0)
	{
	  current_predictor_values2=
	    &data_matrix->matrix[(current_function->predictor2+
				  responses-1)
				 *cases];
	}
      function_value = function_values->matrix;
      if(knot1_index==0)
	{
	  for(j=0;j<cases;j++)
	    {
	      function_value[j]= current_predictor_values1[j];
	    }
	}
      else
	{
	  if(knot1_index!=0)
	    {
	      mesh_index=0;
	      for(j=0;j<predictor1-1;j++)
		{
		  mesh_index = mesh_index+abs(knots_per_pred[j]);
		}
	      knot1_value = mesh[mesh_index+knot1_index-1];
	      if(knots_per_pred[predictor1-1]<0)/*if it is categorical*/
		{
		  for(j=0;j<cases;j++)
		    {
		      if((int)current_predictor_values1[j]==(int)knot1_value)
			{
			  function_value[j] = 1;
			}
		      else 
			{
			  function_value[j]=0;
			}
		    }
		}
	      else
		{
		  for(j=0;j<cases;j++)
		    {
		      matrix_entry =current_predictor_values1[j]-knot1_value;
		      if(matrix_entry>0)
			{
			  function_value[j] = matrix_entry;
			}
		      else
			{
			  function_value[j] = 0.0;
			}
		    }
		}
	    }
	}
      
	
      if(predictor2!=0)
	{
	  if(knot2_index==0)
	    {
	      for(j=0;j<cases;j++)
		{
		  function_value[j]= function_value[j]
		    *current_predictor_values2[j];
		}
	    }
	  else
	    {
	      mesh_index=0;
	      for(j=0;j<predictor2-1;j++)
		{
		  mesh_index = mesh_index+abs(knots_per_pred[j]);
		}
	      knot2_value = mesh[mesh_index+knot2_index-1];
	      for(j=0;j<cases;j++)
		{
		  if(knots_per_pred[predictor2-1]<0)/*---if it is categorical*/
		    {
		      if((int)current_predictor_values2[j]==(int)knot2_value)
			{
			  function_value[j] = function_value[j];
			}
		      else 
			{
			  function_value[j]=0.0;
			}
		    }
		  else
		    {
		      if(current_predictor_values2[j]-knot2_value>0)
			{
			  function_value[j] = function_value[j]
			    *(current_predictor_values2[j]-
			      knot1_value);
			}
		      else
			{
			  function_value[j] = 0.0;
			}
		    }
		}
	      }
	  }
      standardise_array(function_value,cases,&dummy1,&dummy2);
      
      matrix_entry =0.0;
      for(j=0;j<cases;j++)
	{
	  if(*weighted == TRUE)
	    {
	      matrix_entry = matrix_entry+
		(function_value[j]*X_matrix->matrix[(model_size-1)*cases+j]
		 *weight_matrix->matrix[j]);
	    }
	  else
	    {
	      matrix_entry = matrix_entry+
		function_value[j]*X_matrix->matrix[(model_size-1)*cases+j];
	    }
	  
	}
      current_column->data[YtXXtX_expanded->nrow-1]= 
	current_column->data[YtXXtX_expanded->nrow-2]; 
      current_column->data[YtXXtX_expanded->nrow-2]= matrix_entry;
      current_column = current_column->next;
    }
  
  }
  
/*================================================================*/
static int reduce_model(struct matrix2 *YtXXtX_expanded,
		  struct basis_function_matrix *model)
/*================================================================*/
{
/*function reduces model by one function. Takes out each candidate in
turn, computes RSS and the one resulting in the lowest RSS is taken
out and everthing is updated. some function may have been specified to
stay in the model*/ 
  int i,j,k,l,m;
  int predictor1, predictor2,knot1_index,knot2_index;  
  int column_minder;
  int best_candidate;
  int candidate_to_remove;
  double *switch_matrix;
  double rss_for_model,rss_so_far=0,gcv_for_model,gcv_so_far ;
  struct link *current_predictor_col, *trailing_column;
  struct basis_function *current_predictor;
  struct basis_function *trailing_function;
  struct basis_function *current_model_function;
  struct basis_function *discard_model_function;
  struct basis_function_matrix* current_model_predictor;
  int cant_remove;/*boolean*/

 

  gcv_so_far = -1.0;
  steps[(step_count+1)*2]=0;
  steps[(step_count+1)*2+1]=model_size-1;
  
  best_candidate = -1;
  XtX_newinverse->nrow = model_size-1;
  XtX_newinverse->ncol = model_size-1;
  new_X_matrix->nrow=cases;
  new_X_matrix->ncol =model_size-1;
  current_predictor_col = YtXXtX_expanded->column_list;
 
  candidate_to_remove = FALSE;
  for(i=1;i<model_size;i++)
    {
      
      cant_remove = FALSE;
      current_predictor_col=current_predictor_col->next;
      current_predictor=current_predictor_col->function;
/*-------------------------------
  Check is the function must remain in the model because of input
specifications
-------------------------------*/

      if(not_remove_size > 0)
	{
	  for(j=0;j<not_remove_size;j++)
	    {
	
	      if(startmodel[(not_remove_specs[j]-1)*4] == current_predictor->predictor1 &&
		 startmodel[((not_remove_specs[j]-1)*4)+1] == current_predictor->knot1_index &&
		 startmodel[((not_remove_specs[j]-1)*4)+2] == current_predictor->predictor2 &&
		 startmodel[((not_remove_specs[j]-1)*4)+3] == current_predictor->knot2_index)
		{
		  cant_remove= TRUE;
		}    
	    }
	}


/*------------------------------------------------------------
  Certain order of removal rules are followed
  if 1 knot is 0 check that there is no double knot interaction
  if 1 term with knot check that it is not part of interaction
  if linear check that there no knot terms or interaction etc.
-------------------------------------------------------------*/
      if(current_predictor->predictor2 ==0)
	{
 
	  if(current_predictor->knot1_index == 0)
	    {
	      if(order_keeper1[current_predictor->predictor1-1]>0)
		{cant_remove = TRUE;}
	    }
	  else
	    {
	      if(max_knots != 0)
		{

		  if(order_keeper3[(current_predictor->predictor1-1)
				   *max_knots+current_predictor->knot1_index-1]>0)
		    {cant_remove = TRUE;}
		}
	    }
	 
	}
      else
	{
	 
		     
              if(current_predictor->knot1_index == 0)
		{
		 for(j=0;j<abs(knots_per_pred[current_predictor->predictor1-1]);j++)
		    {
		      if(in_model(current_predictor->predictor1,
				  j+1,
				  current_predictor->predictor2,
				  current_predictor->knot2_index,
				  model))
			{cant_remove = TRUE;}
		    }
		}

	      if(current_predictor->knot2_index == 0)
		{
		  for(j=0;j<abs(knots_per_pred[current_predictor->predictor2-1]);j++)
		    {

		      if(in_model(current_predictor->predictor1,
				  current_predictor->knot1_index,
				  current_predictor->predictor2,
				  j+1,
				  model))
			{
			  cant_remove = TRUE;
			}
		    }
		}
	      /* }*/
	}

      if(cant_remove != TRUE)
	{
	  candidate_to_remove = TRUE;
	      
/*-------create new XtX_inverse_without the one predictor----------*/
/*using backwards version of the method in addition stage*/
	  
	  for(j = 0;j<model_size-1;j++)
	    {
	      for(k = 0;k<model_size-1;k++)
		{ 
		  if(j>=i){l=j+1;}else{l=j;}
		  if(k>=i){m=k+1;}else{m=k;}
		  XtX_newinverse->matrix[(j*(model_size-1))+k] 
		    = XtX_inverse->matrix[(l*model_size)+m]
		    - ((XtX_inverse->matrix[l*(model_size)+i])
		       *(XtX_inverse->matrix[i*(model_size)+m])
		       /XtX_inverse->matrix[(i)*(model_size)+i]);
		}
	    }


	  /*making a new X matrix*/
	  for(j=0;j<model_size-1;j++)
	    {
	      for(k=0;k<cases;k++)
		{
		  if(i<=j){l=(((j+1)*cases)+k);}else{l=(j*cases)+k;}
		  new_X_matrix->matrix[(j*cases)+k] = X_matrix->matrix[l];
		}
	    }
	  
/*calculating RSS */
     
	  column_minder = data_matrix->ncol;
	  data_matrix->ncol = responses;
	  YtX->nrow = responses;
	  YtX->ncol = model_size-1;
          
	  matrix_multiplication1(data_matrix,new_X_matrix,YtX,1);
	  data_matrix->ncol = column_minder;
	  XtX_newinverseXtY->nrow=model_size-1;
	  XtX_newinverseXtY->ncol= responses;
          
	  matrix_multiplication1(XtX_newinverse,YtX,XtX_newinverseXtY,2);
	  YtXXtX_newinverseXtY->nrow=responses;
	  YtXXtX_newinverseXtY->ncol=responses;
          
	  matrix_multiplication1(YtX,XtX_newinverseXtY,YtXXtX_newinverseXtY,0);
	  rss_for_model =0.0;
	  for(j=0;j<responses;j++)
	    {
	      rss_for_model=rss_for_model
		-YtXXtX_newinverseXtY->matrix[j*(responses+1)]
		+YtY->matrix[j*(responses+1)];
	      
	      
	      
	    }
	  if(testset == FALSE)
	    {
	      gcv_for_model = 
		( rss_for_model/weight_sum)/
		((1.0 - (GCVconstant*(model_size-1)/cases))*
		 (1.0 - (GCVconstant*(model_size-1)/cases)));
	    }
	  else
	    {
	      gcv_for_model =testset_RSS(YtXXtX_expanded,model_size-1);
	    }

	  if(best_candidate !=-1)/* if it is not the first iteration*/
	    {
	      if(rss_for_model < rss_so_far)
		{
		  best_candidate = i;
		  rss_so_far = rss_for_model;
		  for(j=0;j<responses;j++)
		    {
		      rssgcv[(step_count+1)*(responses+1)+j]
			= -YtXXtX_newinverseXtY->matrix[j*(responses+1)]
			+YtY->matrix[j*(responses+1)];
		    }
		}
	    }
	  else
	    {
	      best_candidate = i;
	      rss_so_far = rss_for_model;
	      for(j=0;j<responses;j++)
		{
		  rssgcv[(step_count+1)*(responses+1)+j]=
		    -YtXXtX_newinverseXtY->matrix[j*(responses+1)]
		    +YtY->matrix[j*(responses+1)];
		}
	    }
	  
      
	  if(gcv_so_far == -1.0)
	    {
	      gcv_so_far = gcv_for_model;
	     }
	  else
	    {
	      if(gcv_for_model < gcv_so_far)
		{
		  gcv_so_far = gcv_for_model;
		}
	    }
	  if(gcv_for_model < GCV)
	    {
	    for(j=0;j<(XtX_newinverseXtY->ncol*XtX_newinverseXtY->nrow);j++)
	      {
                best_coefficents[j] = XtX_newinverseXtY->matrix[j];
                
	      }
	    for(l=0;l<(XtX_newinverse->ncol);l++)
	      {
		for(m=0;m<(XtX_newinverse->ncol);m++)
		  {
		    best_XtXinv[l+m*XtX_newinverse->ncol]=
		      XtX_newinverse->matrix[l+m*XtX_newinverse->ncol];
		    
		  }
		
	      }
	    }
	}
      } 
 if(candidate_to_remove == FALSE)
   {
     return FALSE;
   }
  step_count++;
  rssgcv[(step_count)*(responses+1)+responses]=gcv_so_far;
  

  
/*-----------------------------------------------------------------*/
/*candidate to remove is found and now it will be removed*/
/*reduce model*/
/*-----------------------------------------------------------------*/
/*-find the function in the YtXXtX matrix by its index--*/
  current_predictor_col = YtXXtX_expanded->column_list;
  for(i=0;i<best_candidate;i++)
    {
      trailing_column = current_predictor_col;
      current_predictor_col=current_predictor_col->next;
    }
  trailing_column->next = current_predictor_col->next;
  discard_model_function=current_predictor_col->function;
  if(Verbose == TRUE)
    {
      Rprintf("- %d : %d ",model_size-1,discard_model_function->predictor1);

      if(knots_per_pred[discard_model_function->predictor1-1]>=0 && discard_model_function->knot1_index != 0)
       {
	 Rprintf("%f ",discard_model_function->knot1_value);
       }
      else
       {
         if(knots_per_pred[discard_model_function->predictor1-1]>=0)
          {Rprintf("NA ");}
         if(knots_per_pred[discard_model_function->predictor1-1]<0)
	   {Rprintf("%d ",(int)discard_model_function->knot1_value);}
       }
      if(discard_model_function->predictor2 == 0)
       {
         Rprintf("\n");
       }
      else
       {
        if(discard_model_function->knot2_index==0)
         {Rprintf("%d NA\n",discard_model_function->predictor2);}
        else
         {Rprintf("%d %f\n",discard_model_function->predictor2,discard_model_function->knot1_value);}
       }
    }
  /*fflush(stdout);*/
  /*-find the function in the model matrix and remove it--*/
  predictor1 = discard_model_function->predictor1;
  current_model_predictor = model;
  while(current_model_predictor->predictor_index != predictor1)
    {
      current_model_predictor = current_model_predictor->next_predictor;
    }
  current_model_function = current_model_predictor->functions;
  trailing_function = current_model_predictor->functions;
  while(current_model_function != discard_model_function)
    {
      trailing_function = current_model_function;
      current_model_function = current_model_function->link;
    }
  if(trailing_function== current_model_function)
    {
      current_model_predictor->functions= current_model_function->link;
    }
  else
    {
      trailing_function->link = current_model_function->link;
    }
  current_model_predictor->number_of_basis_functions--;
  
 /*-update the matrix for deletion order-*/

  knot1_index = discard_model_function->knot1_index;
  predictor2 = discard_model_function->predictor2;
  knot2_index = discard_model_function->knot2_index;
  if(predictor2 ==0)
    {
      if(knot1_index!=0)
	{
	  order_keeper1[predictor1-1]--;
	}
    }
  else
    {
      if(knot1_index !=0 || knot2_index !=0)
	{
	  order_keeper2[((predictor1-1)*predictors)+predictor2-1]--;
	  order_keeper2[((predictor2-1)*predictors)+predictor1-1]--;
	}
      if(knot1_index ==0 || knot2_index ==0)
	{
	  order_keeper1[predictor1-1]--;
	  order_keeper1[predictor2-1]--;
	}
      if(max_knots != 0)
	{
	  if(knot1_index!= 0)
	    {
	      order_keeper3[(predictor1-1)*max_knots+knot1_index-1]--;
	    }
	  if(knot2_index!= 0)
	    {
	      order_keeper3[(predictor2-1)*max_knots+knot2_index-1]--;
	    }
	}
    }

  
/*--------------update---X_matrix--------------------------------*/
  for(j=1;j<model_size-1;j++)
    {
      for(k=0;k<cases;k++)
	{
	  if(best_candidate<=j){l=(((j+1)*cases)+k);}else{l=(j*cases)+k;}
	  new_X_matrix->matrix[(j*cases)+k] = X_matrix->matrix[l];
	}
    }

  switch_matrix =new_X_matrix->matrix;
  new_X_matrix->matrix = X_matrix->matrix;
  X_matrix->matrix = switch_matrix;
  X_matrix->ncol--;


/*--------------update---XtX_invmatrix--------------------------------*/
 

  XtX_inverse->ncol--;
  XtX_inverse->nrow--;
  if(*weighted == TRUE)
    {
      temp_matrix->nrow = model_size;
      temp_matrix->ncol = cases;
      matrix_multiplication1(X_matrix,weight_matrix,temp_matrix,3);
      matrix_multiplication1(temp_matrix,X_matrix,XtX_inverse,0);
    }
  else
    {
      matrix_multiplication1(X_matrix,X_matrix,XtX_inverse,1);
    }
  invert_matrix(XtX_inverse);
  model_size--;
 
/*-save new global best model if this iteration produced better gcv*/
  if(gcv_so_far < GCV)
    {
      GCV = gcv_so_far;
      *bestmodel_size = model_size;
      current_predictor_col = YtXXtX_expanded->column_list;
      
      for(l=0;l<model_size;l++)
	{
	  if(l != 0)
	    {
	      current_predictor = current_predictor_col->function;
	      best_model[(l-1)*4]= current_predictor->predictor1;
	      best_model[((l-1)*4)+1]=current_predictor->knot1_index;
	      best_model[((l-1)*4)+2]=current_predictor->predictor2;
	      best_model[((l-1)*4)+3]=current_predictor->knot2_index;
	      best_model_sd_mean[(l-1)*2]= current_predictor->SD;
	      best_model_sd_mean[((l-1)*2)+1]= current_predictor->mean;
             

	    }
	  current_predictor_col = current_predictor_col->next;
	}
      
    }
  
return TRUE;
}
/*----------------------------------------------------------*/
static int initial_model(struct basis_function_matrix *model,
                   struct matrix2 **YtXXtX_expanded)
/*---------------------------------------------------------*/
{
/* Set up the initial model, by default it is a model containing only
the intercept*/
  int i,j,l,m,predictor_1,predictor_2,ok,knot_1_index,knot_2_index,mesh_index;
  double *X_ptr , knot_1_value,knot_2_value;
  double rss_for_model;
  struct link *YtXXtX_column;
  int column_minder;
  double mean,SD;
  double *means,*SDs;
  struct basis_function_matrix* current_predictor;
  struct basis_function *current_function;
  struct basis_function *new_function;
  ok = 0;
  if(model_size != 1)
    {
      means= (double *)Salloc (model_size-1,double);
      SDs= (double *)Salloc (model_size-1,double);
    }
 
/*create matrix to hold YtX and XtX for model and candidates */
  (*YtXXtX_expanded)= create_matrix2(max_model_size+responses+1,model_size);
  (*YtXXtX_expanded)->nrow = responses+model_size+1;
  (*YtXXtX_expanded)->ncol = model_size;
  X_matrix=create_matrix1(0,0);
  X_matrix->ncol=model_size;
  X_matrix->nrow = cases;
  X_matrix->matrix= (double *)Salloc (max_model_size*cases,double);
  X_ptr = X_matrix->matrix;
/*  fit the constant function (intercept) over all predictors    */  

  for(i=0;i<cases;i++)
    {
      *X_ptr=1;
      X_ptr++;
    }
  for(i = 0;i< model_size-1;i++)
    {
      /*-terms may need to be swapped as the procedure expects the first component
of a interaction term to have the lower index--*/
      if((startmodel[(i*4)+2] != 0) & (startmodel[(i*4)+2] < startmodel[(i*4)]))
	{
	  predictor_1 = startmodel[(i*4)];
	  startmodel[(i*4)]= startmodel[(i*4)+2];
          startmodel[(i*4)+2] = predictor_1;

          knot_1_index = startmodel[(i*4)+1];
	  startmodel[(i*4)+1] = startmodel[(i*4)+3];
	  startmodel[(i*4)+3] = knot_1_index;
	  
	  knot_1_value = startknots[i*2];
	  startknots[i*2] = startknots[(i*2)+1];
	  startknots[(i*2)+1] = knot_1_value;
	}

    }
/* make the X matrix, each column contains the values of the basis functions
        at the points in the data matrix*/
  for(i = 0;i< model_size-1;i++)
    {
      predictor_1 = startmodel[(i*4)];
      predictor_2 = startmodel[(i*4)+2];
      
      knot_1_index = startmodel[(i*4)+1];
      knot_2_index = startmodel[(i*4)+3];

      if(knot_1_index!=0)
	{
	  knot_1_value = startknots[i*2];
	}
      if(knot_2_index != 0)
	{
	  knot_2_value = startknots[(i*2)+1];
	}
      for(j=0;j<cases;j++)
	{
	  if(knots_per_pred[predictor_1-1] >= 0)
	    {
	      *X_ptr = data_matrix->matrix[((responses+(predictor_1-1))*cases)+j];
	      if(knot_1_index != 0)
		{
		  *X_ptr = *X_ptr - knot_1_value;
		  if (*X_ptr < 0.0){*X_ptr = 0.0;}
		}
	    }
	  else
	    {
	      /*else it is categorical*/
              
	      if((int)data_matrix->matrix[((responses+(predictor_1-1))*cases)+j] 
		 == (int)knot_1_value)
		{*X_ptr =1;}
	      else
		{*X_ptr =0;}
	    }
 
	  if(predictor_2 != 0)
	    {
	      if(knots_per_pred[predictor_2-1] >= 0)
		{
		  if(knot_2_index == 0)
		    {
		      *X_ptr = *X_ptr*data_matrix
			->matrix[((responses+(predictor_2-1))*cases)+j];
		    }
		  else
		    {
		      if(data_matrix->matrix[((responses+(predictor_2-1))*cases)+j]
			 - knot_2_value < 0.0)
			{
			  *X_ptr =0.0;
			}
		      else
			{
			  *X_ptr = *X_ptr * 
			(data_matrix->matrix[((responses+(predictor_2-1))
					      *cases)+j]- knot_2_value);
			}
		    }
		}
              else
		{
		  /*else it is categorical*/
		  if((int)data_matrix->matrix[((responses+(predictor_1-1))*cases)+j] 
		     != (int)knot_2_value)
		    {*X_ptr =0;}
		}
	    }
	  X_ptr++;
	}
  
      
      mean=0.0;
      SD=1.0;
      standardise_array(X_ptr-cases,cases,&mean,&SD);
      means[i]=mean;
      SDs[i] = SD;
    }

/*--Making the XtY bit of the the YtXXtX matrix------------*/
  if(*weighted == TRUE)
    {
      column_minder = data_matrix->ncol;
      data_matrix->ncol = responses;
      YtX->nrow = responses;
      YtX->ncol = model_size;
      temp_matrix->ncol = cases;
      temp_matrix->nrow = responses;
      matrix_multiplication1(data_matrix,weight_matrix,temp_matrix,3);
      YtX->nrow = responses;
      YtX->ncol = model_size;
      matrix_multiplication1(temp_matrix,X_matrix,YtX,0);
    }
  else
    {
      column_minder = data_matrix->ncol;
      data_matrix->ncol = responses;
      YtX->nrow = responses;
      YtX->ncol = model_size;
      matrix_multiplication1(data_matrix,X_matrix,YtX,1);
    }
  data_matrix->ncol = column_minder;
  YtXXtX_column = (*YtXXtX_expanded)->column_list;
  for(i=0;i <model_size;i++)
    {
      for(j=0;j<responses;j++) 
	{
	  YtXXtX_column->data[j]=YtX->matrix[(i*responses)+j];
	}
      if(i != model_size-1){YtXXtX_column = YtXXtX_column->next;}
    }

/*--Making the XtX bit of the the YtXXtX matrix------------*/
  XtX_inverse->ncol = model_size;
  XtX_inverse->nrow = model_size;
  if(*weighted == TRUE)
    {
      temp_matrix->nrow = model_size;
      temp_matrix->ncol = cases;
      matrix_multiplication1(X_matrix,weight_matrix,temp_matrix,3);
      matrix_multiplication1(temp_matrix,X_matrix,XtX_inverse,0);
    }
  else
    {
      matrix_multiplication1(X_matrix,X_matrix,XtX_inverse,1);
    }
  
  YtXXtX_column = (*YtXXtX_expanded)->column_list;
  for(i=0;i <model_size;i++)
    {
      for(j=0;j<model_size;j++)
	{
	  YtXXtX_column->data[j+responses]=XtX_inverse->matrix[(i*model_size)+j];
	}
      if(i != model_size-1){YtXXtX_column = YtXXtX_column->next;}
    }
 
/*---Inserting the model into the model functions structure--*/

  YtXXtX_column = (*YtXXtX_expanded)->column_list;

  for(i=0;i <model_size-1;i++)
    {

      YtXXtX_column = YtXXtX_column->next;
      current_predictor = model;
      predictor_1= startmodel[(i*4)];
      
      knot_1_index = startmodel[(i*4)+1];
      predictor_2 = startmodel[(i*4)+2];
      knot_2_index = startmodel[(i*4)+3];
      while(current_predictor->predictor_index != predictor_1)
	{
	  current_predictor = current_predictor->next_predictor;
	}
      if(current_predictor->number_of_basis_functions ==0)
	{
	  new_function= 
		(struct basis_function *)Salloc (1,struct basis_function);
	  new_function->knot1_index = knot_1_index;
	  new_function->knot1_value =startknots[i*2];
	  new_function->predictor1 = predictor_1;
	  new_function->predictor2 = predictor_2;
	  new_function->knot2_index = knot_2_index;
	  new_function->knot2_value =startknots[(i*2)+1];
	  new_function->SD = SDs[i];
	  new_function->mean= means[i]; 
	  current_predictor->number_of_basis_functions = 1;
	  current_predictor->functions = new_function;
	}
      else
	{
          /* move to the end of the column to add a new basis function*/
	  current_function = current_predictor->functions;
	  for(j=0;j<current_predictor->number_of_basis_functions;j++)
	    {
	      
	      if(j != current_predictor->number_of_basis_functions-1)
		{
		  current_function = current_function->link;
		}
	    }
	  new_function 
		= (struct basis_function *)Salloc (1,struct basis_function);
	  current_function->link = new_function;
	  new_function->knot1_index = knot_1_index;
	  new_function->predictor1 = predictor_1;
	  new_function->predictor2 = predictor_2;
	  new_function->knot2_index = knot_2_index;
	  new_function->knot1_value =startknots[i*2];
	  new_function->knot2_value =startknots[(i*2)+1];
	  new_function->SD = SDs[i];
	  new_function->mean =means[i];
	  current_predictor->number_of_basis_functions++;
	}
      YtXXtX_column->function = new_function;
      
      
/*-updating the matricies which control the order in which new candidates can be added-*/
      if(predictor_2 == 0)
	{
	  if(knot_1_index !=0)
	    {
	      order_keeper1[predictor_1-1]++;
	    }
	}
      else
	{
	  if(knot_1_index !=0 || knot_2_index != 0)
	    {
	      order_keeper2[((predictor_1-1)*predictors)+predictor_2-1]++;
	      order_keeper2[((predictor_2-1)*predictors)+predictor_1-1]++;
	    }
	  if(knot_1_index ==0 || knot_2_index == 0)
	    {
	      order_keeper1[predictor_1-1]++;
	      order_keeper1[predictor_2-1]++;
	    }
	  if(max_knots != 0)
	    {
	      if(knot_1_index !=0)
		{
		  order_keeper3[(predictor_1-1)*max_knots+knot_1_index-1]++;
		}
	      if(knot_2_index != 0)
		{
		  order_keeper3[(predictor_2-1)*max_knots+knot_2_index-1]++;
		}
	    }
	  
	}
    }


/*--inverting the XtX_matrix----------------*/
 
  if(model_size ==1)
    {
      XtX_inverse->matrix[0] = 1/XtX_inverse->matrix[0];
    }
  else
    {
/*--Lapack inversion for indefinite double precision real symmetric
matricies----two step--factorisation and inversion-------*/
    
      ok = invert_matrix(XtX_inverse);
    }
  if (ok == 0)
    {

/*computes YtY-YtX(XtX)^1XtY--and- sums the diagonal to get the RSS*/
/*uses data matrix with number of rows  changed to be number of responses 
for Yt. */

      (*YtXXtX_expanded)->nrow=responses;
      XtX_newinverseXtY->nrow = model_size;
      XtX_newinverseXtY->ncol = responses;
      matrix_multiplication2(XtX_inverse,
			     *YtXXtX_expanded,
			     XtX_newinverseXtY,
			     0);

      YtXXtX_newinverseXtY->nrow = responses;
      YtXXtX_newinverseXtY->ncol = responses;
      matrix_multiplication2(XtX_newinverseXtY,
			     *YtXXtX_expanded,
			     YtXXtX_newinverseXtY,
			     1);
      (*YtXXtX_expanded)->nrow=responses+model_size+1;
      rss_for_model =0.0;
      for(j=0;j<responses;j++)
	{
	    rss_for_model=rss_for_model
	      -YtXXtX_newinverseXtY->matrix[j*(responses+1)]
	      +YtY->matrix[j*(responses+1)];
	}

      for(j=0;j<responses;j++)
	{
         
/*puts the rss for each response in the rssgcv matrix for output */
	  rssgcv[j]=
	    -YtXXtX_newinverseXtY->matrix[j*(responses+1)]
	    +YtY->matrix[j*(responses+1)];
          
	}
      if(testset == FALSE)
	{
          
	  GCV = 
	    (rss_for_model /weight_sum)/
	    ((1.0 - (GCVconstant*(model_size)/cases))
	     *(1.0 - (GCVconstant*(model_size)/cases)));
	}
      else
        {
          
	  GCV = testset_RSS((*YtXXtX_expanded),model_size);
          
	}

      rssgcv[responses]=GCV;
      for(j=0;j<(XtX_newinverseXtY->nrow*XtX_newinverseXtY->ncol);j++)
	{
	  best_coefficents[j] = XtX_newinverseXtY->matrix[j];
	  
	}
       for(l=0;l<(XtX_newinverse->ncol);l++)
	{
	  for(m=0;m<(XtX_newinverse->ncol);m++)
	    {
	      best_XtXinv[l+m*XtX_newinverse->ncol]=
		XtX_newinverse->matrix[l+m*XtX_newinverse->ncol];
	      
	    }
	  
	}
      *bestmodel_size = model_size;
      YtXXtX_column = (*YtXXtX_expanded)->column_list;

/*storing the best model */
      
      for(i=0;i<model_size;i++)
	{
	  if(i != 0)
	    {
	      current_function = YtXXtX_column->function;
	      best_model[(i-1)*4]= current_function->predictor1;
	      best_model[((i-1)*4)+1]=current_function->knot1_index;
	      best_model[((i-1)*4)+2]=current_function->predictor2;
	      best_model[((i-1)*4)+3]=current_function->knot2_index;
	      best_model_sd_mean[(i-1)*2]=current_function->SD;
	      best_model_sd_mean[((i-1)*2)+1]=current_function->mean;
	      
	      
	    }

	  YtXXtX_column = YtXXtX_column->next;
	}
 
      steps[0]=1;
      steps[1]=model_size;
    
    }
  return ok; 
}

/*==============================================================*/
static void standardise_array(double *numbers,int length_of_list,double *mean,double *SD)
/*==============================================================*/
{
/*----------------------------------------------------------------
Finds the mean and standard deviation of a list of numbers and
returns the array standard normalised, with mean and standard 
deviatioN
------------------------------------------------------------------*/
  int i,mina,maxi;
  

  *mean = 0;
  mina = numbers[0];
  maxi = numbers[0];
  for(i=0;i<length_of_list;i++)
    {
      *mean = *mean + numbers[i];
      if(numbers[i] < mina){mina=numbers[i];}
      if(numbers[i] > maxi){maxi=numbers[i];}
    }
  
  if((mina-maxi)*(mina-maxi)>=1)
    {
  
      *mean = (*mean)/length_of_list;
      for(i=0;i<length_of_list;i++)
	{
          
          
	  numbers[i] = numbers[i]-(*mean);
	}
     
      *SD = 0.0;
      for(i=0;i<length_of_list;i++)
	{
	  *SD = *SD + (numbers[i]*numbers[i]);
	}
      *SD = sqrt(*SD/(length_of_list-1));
      if((*SD) != 0.0)
	{
	  for(i=0;i<length_of_list;i++)
	    {
	      
	      numbers[i] = numbers[i]/(*SD);
	      
	    }
	}
    }
  else
    {
      *SD=1.0;
      *mean=0.0;
    }

}
/*==============================================================*/
static int check_input()
/*==============================================================*/
{
  int i,j;
  int prereq,prereq2;


  
  if(model_size < 1)
    {
      for(i = 0;i< (model_size-1)*2;i++)
	{
	 
	  if(!(startmodel[(i*2)+1] == 0 || startmodel[(i*2)+1]== 1))
	    {
	      
	      return 1; 
	    }
	  if(startmodel[(i*2)+1] == 1 && knots_per_pred[startmodel[(i*2)]-1]>=0)
	    {
	      
	      prereq =FALSE;
	      for(j = 0;j< model_size-1;j++)
		{
		   /*---checks that linear is present when 
              initial model includes knot term-*/
		  if((startmodel[(j*4)] == startmodel[i*2]
		      && startmodel[(j*4)+1] ==0)
		     && startmodel[(j*4)+2] ==0)
		    {
		      prereq = TRUE;
		    }
		}
	      if(prereq == FALSE){return 2;}
	    }
	}
      
/*-checks the prerequisites for compound functions and whether the initial model contains the same terms more than once--*/ 
      for(i = 0;i< model_size-1;i++)
	{
	  for(j = 0;j< model_size-1;j++)
	    {
	      if(i !=j)
		{
		  if(startmodel[(i*4)] == startmodel[(j*4)]
		     && startmodel[(i*4)+1] == startmodel[(j*4)+1]
		     && startmodel[(i*4)+2] == startmodel[(j*4)+2]
		     && startmodel[(i*4)+3] == startmodel[(j*4)+3])
		    {
		      /*if both are linear*/
		      if(startmodel[(i*4)+1] == 0 && startmodel[(i*4)+3]==0)
			{
			  
			  return 1;
			}
                      /* if knots are the same */
		      if(startmodel[(i*4)+1] == 0 && startmodel[(i*4)+3] != 0)
			{
			  if(startknots[(i*2)+1] == startknots[(j*2)+1])
			    {
			      return 1;
			    }
			}
		      if(startmodel[(i*4)+1] != 0 && startmodel[(i*4)+3] == 0)
			{
			  if(startknots[i*2] == startknots[j*2])
			    {
			      
			      return 1;
			    }
			}
		      if(startmodel[(i*4)] ==startmodel[(j*4)+2]
			 && startmodel[(i*4)+1] == startmodel[(j*4)+3]
			 && startmodel[(i*4)+2] == startmodel[(j*4)]
			 && startmodel[(i*4)+3] == startmodel[(j*4)+1])
			{
			  if(startmodel[(i*4)+1] == 0 && startmodel[(i*4)+3]==0)
			    {
			      
			      return 1;
			    }
			  if(startmodel[(i*4)+1] == 0 && startmodel[(i*4)+3] != 0)
			    {
			      if(startknots[(i*2)+1] == startknots[j*2])
				{
			      
				  return 1;
				}
			    }
			  if(startmodel[(i*4)+1] != 0 && startmodel[(i*4)+3] == 0)
			    {
			      if(startknots[i*2] == startknots[(j*2)+1])
				{
				  
				  return 1;
				}
			    }
			  
			}
		    }
		}    
	    }
	  /* a basis function with 2 linear terms then must have each linear term also
             in model*/
	  if((startmodel[(i*4)] !=0 
	      && startmodel[(i*4)+1] == 0)
	     && startmodel[(i*4)+2] != 0 
	     && startmodel[(i*4)+3] ==0)
	    {
	      prereq = FALSE;
	      prereq2 = FALSE;
	      for(j = 0;j< model_size-1;j++)
		{
		  if(startmodel[(j*4)] ==startmodel[(i*4)]
		     && startmodel[(j*4)+1] == 0
		     && startmodel[(j*4)+2] == 0
		     && startmodel[(j*4)+3] == 0)
		    {prereq = TRUE;}
		  if(startmodel[(j*4)] ==startmodel[(i*4)+2]
		     && startmodel[(j*4)+1] == 0
		     && startmodel[(j*4)+2] == 0
		     && startmodel[(j*4)+3] == 0)
		    {prereq2 = TRUE;}
		}
	      if(prereq == FALSE || prereq2 == FALSE)
		{return 3;}
	      
	    }
/*if a model has two knoted terms the two linear term model must be present, also
each two term with both predictors and one knot*/ 
	  if((startmodel[(i*4)] !=0 
	      && startmodel[(i*4)+2] != 0)
	     && startmodel[(i*4)+1] != 0 
	     && startmodel[(i*4)+3] !=0)
	    {
	      prereq=FALSE;
	      for(j = 0;j< model_size-1;j++)
		{
		  if((startmodel[(i*4)] == startmodel[j*4] &&  startmodel[(j*4)+1] == 0
		      && startmodel[(i*4)+2] == startmodel[(j*4)+2] &&  startmodel[(i*4)+3] ==0)
		     ||
		     (startmodel[(i*4)] == startmodel[(j*4)+2] && startmodel[(i*4)+3] ==0
		      && startmodel[(i*4)+2] == startmodel[j*4] &&  startmodel[(j*4)+1] == 0 ))
		    {
		      prereq = TRUE;
		    }
		}
	      if(prereq==FALSE){return 3;}
	      
	    
	  
	      prereq=FALSE;
	      prereq2=FALSE;
	      for(j = 0;j< model_size-1;j++)
		{
		  if(startmodel[(i*4)] == startmodel[j*4]
		     && 0             ==startmodel[(j*4)+1]
		     && startmodel[(i*4)+2] == startmodel[(j*4)+2]
		     && startmodel[(i*4)+3] == startmodel[(j*4)+3])
		    {
		      prereq = TRUE;
		    }
		  if(startmodel[(i*4)] == startmodel[(j*4)+2]
		     && 0 == startmodel[(j*4)+3]
		     && startmodel[(i*4)+2] == startmodel[(j*4)]
		     && startmodel[(i*4)+3] == startmodel[(j*4)+1])
		    {
		      prereq = TRUE;
		    }
		  if(startmodel[(i*4)] == startmodel[j*4] 
		     &&startmodel[(i*4)+1] == startmodel[(j*4)+1]
		     &&startmodel[(i*4)+2] == startmodel[(j*4)+2]
		     && 0 == startmodel[(j*4)+3])
		    {
		      prereq2 = TRUE;
		    }
		  if(startmodel[(i*4)] == startmodel[(j*4)+2] 
		     &&startmodel[(i*4)+1] == startmodel[(j*4)+3]
		     &&startmodel[(i*4)+2] == startmodel[j*4]
		     && 0 == startmodel[(j*4)+1])
		    {
		      prereq2 = TRUE;
		    }
		}
	      if(prereq == FALSE || prereq2 == FALSE){return 3;}
	      
	    }
	}
    }
  return 0;
}
/*==============================================================*/
static double testset_RSS(struct matrix2 *YtXXtX_expanded,int model_size )
/*==============================================================*/
{
/*computes test set RSS, coefficients must be transformed as they
apply to the standarised data   */
 double totalRSS,fitted;
 struct basis_function *model_function;
 struct link *YtXXtX_column;
 double standardise_const;
 int i,j,k,cases;
 double temp_value1,temp_value2,x;
 cases = testset_matrix->nrow;
 YtXXtX_column = YtXXtX_expanded->column_list;
 for(i=0;i<(XtX_newinverseXtY->ncol*XtX_newinverseXtY->nrow);i++)
   {
     coefficents[i]=XtX_newinverseXtY->matrix[i];
   }
/*get mean and standard deviations */
 for(i=0;i<model_size-1;i++)
   {
     YtXXtX_column = YtXXtX_column->next;
     model_sd_mean[i*2] = YtXXtX_column->function->SD;
     model_sd_mean[(i*2)+1] = YtXXtX_column->function->mean;
   }

 
/*untransform the transformed data*/

 for(i=0;i<responses;i++)
   {
     YtXXtX_column = YtXXtX_expanded->column_list;
     standardise_const=0.0;
     
     for(j=1;j<model_size;j++)
       {
         YtXXtX_column = YtXXtX_column->next;
        
	 standardise_const = standardise_const
	   +(coefficents[j+(i*model_size)]*model_sd_mean[((j-1)*2)+1])/model_sd_mean[(j-1)*2];
       }
     coefficents[i*(model_size)] = coefficents[i*model_size]-standardise_const;
   }

 for(i=0;i<responses;i++)
   {

     for(j=1;j<model_size;j++)
       {

	 if(!(knots_per_pred[YtXXtX_column->function->predictor1] < 0 
	      && YtXXtX_column->function->predictor2 == 0 ))
	   {
	     coefficents[j+(i*model_size)]
	       =coefficents[j+(i*model_size)]
	       /model_sd_mean[(j-1)*2];
	   }
       }
   }
 
 for(i=0;i<responses;i++)
   {

    if(classification != TRUE)tset_RSS[i] =0.0;
     for(j=0;j<cases;j++)
       {
        YtXXtX_column = YtXXtX_expanded->column_list;
        fitted = coefficents[i*(XtX_newinverseXtY->nrow)];
        for(k=0;k<model_size-1;k++)
	  {

	    YtXXtX_column = YtXXtX_column->next;
            model_function = YtXXtX_column->function;
	    temp_value2 = 1;
	    if(knots_per_pred[model_function->predictor1-1]>=0)
	      {
		temp_value1 = 
		  testset_matrix->matrix[((responses+model_function->predictor1-1)*cases)+j];
	    
		if(model_function->knot1_index != 0)
		  {
		    x=temp_value1 - model_function->knot1_value;
		    if(x>0){temp_value1 = x;}else{temp_value1 = 0.0;}
		  }
	      }
	    else
	      {
		if((int)testset_matrix->matrix[((responses+model_function->predictor1-1)*cases)+j]
		   == (int)model_function->knot1_value)
		  {temp_value1 =1.0;}else{temp_value1 = 0.0;}
	      }
            if(model_function->predictor2 != 0)
              {
		temp_value2 = 
		  testset_matrix->matrix[((responses+model_function->predictor2-1)*cases)+j];
		if(model_function->knot1_index != 0)
		  {
		    x=temp_value2 - model_function->knot2_value;
		    if(x>0){temp_value2 = x;}else{temp_value2 = 0.0;}
		  }
	      }

            fitted = fitted + (temp_value1*temp_value2*coefficents[i*(XtX_newinverseXtY->nrow)+k+1]);
            
	  }
        

	if(classification != TRUE)
	  {
	    tset_RSS[i] = tset_RSS[i] +
	      (fitted - testset_matrix->matrix[i*cases+j])*
	      (fitted - testset_matrix->matrix[i*cases+j]);
	  }
        else
          {
/* in classification the class is the index of the largest  response for a case*/
	    if(i==0)
	      {
		response_max[j] = fitted;
                response_class[j] = 0;
	      }
	    else
	      {
		if(fitted>response_max[j])
		  {
		    response_max[j]=fitted;
                    response_class[j] = i;
		  }
	      }
	  }
	
       }
    
     
     
   }

 totalRSS = 0.0;


 if(classification == TRUE)
   {
    
    for(j=0;j<cases;j++)
     {
      if(((int)testset_matrix->matrix[response_class[j]*cases+j]) ==0 )
       {
	totalRSS = totalRSS + 1;
       }
     }
   }
 else
   {

    if(testset_weighted == TRUE)
      {
	for(i=0;i<responses;i++)
	  {
	    totalRSS = totalRSS+(tset_RSS[i]*testset_weights[i]);
	  }
      }
    else
      {
	for(i=0;i<responses;i++)
	  {
	    totalRSS = totalRSS+tset_RSS[i];
	  }

      }

   }
 
 
 return totalRSS;
}

/*--------------------------------------------------------------*/
/*----data structure functions----------------------------------*/
/*--------------------------------------------------------------*/

/*==============================================================*/
static struct matrix1 *create_matrix1(int nrow, 
			     int ncol)
/*==============================================================*/

{
/*---------------------------------------------------------------
Create a matrix structure that points to a block of memory that
will contain the matrix's value by column in one long line.
-----------------------------------------------------------------*/
  struct matrix1 *new_matrix;
  
  new_matrix = (struct matrix1 *)Salloc(1,struct matrix1);
   
  
  new_matrix->ncol = ncol;
  new_matrix->nrow = nrow;
  return(new_matrix);
}
/*==============================================================*/
static struct matrix2 *create_matrix2(int nrow, 
			     int ncol)
/*==============================================================*/

{
/*---------------------------------------------------------------
Creates a matrix which is a linked list of columns. Each link also
has a linkl to the model or candidate matrix as each column corresponds
to one basis function
-----------------------------------------------------------------*/
  int i;
  struct matrix2 *new_matrix;
  struct link *column;
  struct link *current_link=0;
  new_matrix = (struct matrix2 *)Salloc(1,struct matrix2);
  
  
  for(i=0;i<ncol;i++)
    {
      column = (struct link *)Salloc(1,struct link);

      column->data =(double *)Salloc(nrow,double);
      
      column->next = current_link;
      current_link= column;
    }
   
  
  new_matrix->ncol = ncol;
  new_matrix->nrow = nrow;
  new_matrix->column_list = current_link;
      
  return(new_matrix);
}


/*==============================================================*/
static void switch_columns(int col1,
		 int col2,
		 struct matrix2 *object_matrix)
/*==============================================================*/
{
/*----------------------------------------------------------------
Switching rows means just swapping the pointers to the rows in the
list that binds the column together
----------------------------------------------------------------*/
  int i;
  struct link *current_column1, *current_column2;
  struct basis_function  *function_holder;
  double *ptr_holder;
  if(col1==col2){return;}
  current_column1= object_matrix->column_list;
  current_column2= object_matrix->column_list;
  for(i=0;i<col1-1;i++)
    {
      current_column1=current_column1->next;
    } 
  for(i=0;i<col2-1;i++)
    {
      current_column2=current_column2->next;
    } 
  ptr_holder= current_column1->data;
  function_holder = current_column1->function;
  current_column1->function = current_column2->function;
  current_column1->data= current_column2->data;
  current_column2->data=ptr_holder;
  current_column2->function = function_holder;
  
}




/*==============================================================
static void print_matrix1(struct matrix1 *object_matrix )
==============================================================
{
  int i,j,nrow,ncol;
  nrow = object_matrix->nrow;
  ncol = object_matrix->ncol;
  
  for(i=0;i<nrow;i++)
    {
      for(j=0;j<ncol;j++)
	{
	  Rprintf("%f\t",object_matrix->matrix[(j*nrow)+i]);
	}
      Rprintf("\n");
    }


} 
*/
/*==============================================================
static void print_matrix2(struct matrix2 *object_matrix )
==============================================================
{
  int i,j,k,nrow,ncol;
  struct link *current_column;

  nrow = object_matrix->nrow;
  ncol = object_matrix->ncol;
  current_column = object_matrix->column_list;
  for(i=0;i<nrow;i++)
    {
      for(j=0;j<ncol;j++)
	{
	  for(k=0;k<j;k++)
	    {
	      current_column= current_column->next;
	    }
	  Rprintf("%f\t",current_column->data[i]);
	  current_column = object_matrix->column_list;
	}
      Rprintf("\n");
    }
}
*/
/*==============================================================*/
static void matrix_multiplication1(struct matrix1 *object_matrixA,
				     struct matrix1 *object_matrixB,
				     struct matrix1 *result,
				     int flag)
/*==============================================================*/
{
/*----------------------------------------------------------------
Flag: 0 for AB (regular multiplicaion (A,B,0)->AB
      1 for AB (multiply transpose of A by B (A,B,1)->AtB)
      2 for AB (multiply A by transpose of B (A,B,2)->ABt
      3 for special multiplication AW where W is a weight matrix 
      n*n with only the n diagonal elements stored
----------------------------------------------------------------*/
  int i,j,k,nrowA,nrowB,ncolA,ncolB;
  double *A_pointer;
  double *B_pointer;
  double product;
  
  nrowA = object_matrixA-> nrow;
  ncolA = object_matrixA-> ncol;
  nrowB = object_matrixB-> nrow;
  ncolB = object_matrixB-> ncol;
  
  
  if(flag==1)
    {
      /*if(nrowA != nrowB)
	{
	  Rprintf("Multiplication error \n");
	  exit(1);
	}
      if(ncolA != result->nrow || ncolB != result->ncol)
	{
	  Rprintf("Multiplication error \n");
	  exit(1);
      }*/
  
      for(i=0;i<ncolA;i++)
	{
	  B_pointer = object_matrixB->matrix;
	  for(j=0;j<ncolB;j++)
	    {
	    product=0.0;
	    A_pointer = &object_matrixA->matrix[i*nrowA];
	    for(k=0;k<nrowA;k++)
	      {
		product = product+((*A_pointer)*(*B_pointer));
		A_pointer++;
		B_pointer++;
		
	      }
	    result->matrix[j*ncolA+i]=product;
	    }
	  
	  
      }
    }
  if(flag==0)
    {
      A_pointer = object_matrixA->matrix;
      B_pointer = object_matrixB->matrix;
      /*if(ncolA != nrowB)
	{
	  Rprintf("Multiplication error3 \n");
	  exit(1);
	}
      if(nrowA != result->nrow || ncolB != result->ncol)
	{
	  Rprintf("Multiplication error4 \n");
	  exit(1);
	}
	*/
      for(i=0;i<nrowA;i++)
      {
	B_pointer = object_matrixB->matrix;
	for(j=0;j<ncolB;j++)
	  {
	    product=0.0;
	    
	    for(k=0;k<ncolA;k++)
	      {
		product = product+
		  (A_pointer[i+(nrowA*k)]*(*B_pointer));
		B_pointer++;
	      }
	    result->matrix[i+result->nrow*j]=product;
	    
	    
	  }
	
      }
    }
  if(flag==2)
    {
      /*
	if(ncolA != ncolB)
      {
	Rprintf("Multiplication error4 \n");
	exit(1);
      }
    if(nrowA != result->nrow || nrowB != result->ncol)
	{
	  Rprintf("Multiplication error \n");
	  exit(1);
	}
	*/
    
    A_pointer = object_matrixA->matrix;
    B_pointer = object_matrixB->matrix;
    for(i=0;i<nrowA;i++)
      {
	
	for(j=0;j<nrowB;j++)
	  {
	    product=0.0;
	    for(k=0;k<ncolA;k++)
	      {
		product = product+(A_pointer[i+nrowA*k])*B_pointer[j+(nrowB*k)];
		
		
	      }
	    result->matrix[i+nrowA*j]=product;
	  }
       
	
      }
    }
  if(flag==3)
  {
    
   /*
    if(nrowA != nrowB)
      {
	Rprintf("Multiplication error \n");
	exit(1);
      }
    if(ncolA != result->nrow || ncolB != result->ncol)
      {
	Rprintf("Multiplication error\n");
	exit(1);
      }
      */
    B_pointer = object_matrixB->matrix;
    for(i=0;i<ncolA;i++)
      {

	A_pointer = &object_matrixA->matrix[i*nrowA];
	k=0;
	for(j=0;j<ncolB;j++)
	  {
	    
	    product = A_pointer[j]*B_pointer[j];
	    result->matrix[j*ncolA+i]=product;
	  }
       
	
      }
  }
  return ;
}

/*==============================================================*/
static void matrix_multiplication2(struct matrix1 *object_matrixA,
				      struct matrix2 *object_matrixB,
				      struct matrix1 *result,
				      int flag) 
/*==============================================================*/
{
/* multipling a matrix1 with a matrix2
   flag for whether matrix2 is transposed or not*/
  int i,j,k,l,nrowA,nrowB,ncolA,ncolB;
  double *A_pointer;
  double *B_pointer;
  double product;
  
  struct link *column_pointer;
  
  nrowA = object_matrixA-> nrow;
  ncolA = object_matrixA-> ncol;
  nrowB = object_matrixB-> nrow;
  ncolB = object_matrixB-> ncol;
  
  
  if(flag==0)
    {
      /*if(ncolA != ncolB)
      {
	Rprintf("Multiplication error \n");
	 exit(1);
      } 
      if (nrowA != result->nrow || nrowB != result->ncol)
	{ 
	   Rprintf("Multiplication error \n");
	  exit(1);
	}
	*/
      A_pointer=object_matrixA->matrix;
      for(i=0;i<nrowA;i++)
	{
	  
	 
	  for(j=0;j<nrowB;j++)
	    {
	      product = 0.0;
	      for(k=0;k<ncolB;k++)  
		{
		  column_pointer=object_matrixB->column_list;
		  for(l=0;l<k;l++)
		    {
		      column_pointer=column_pointer->next;
		      
		    }
		  B_pointer=column_pointer->data;
		  product=product+(A_pointer[i+nrowA*k]*B_pointer[j]);
		}
	      result->matrix[i+(nrowA*j)]=product;
	    }
	 
	}
    }
  else
    {
     /*if(nrowA != ncolB)
      {
	Rprintf("Multiplication error\n");
	exit(1);
      }*/
      /*if(nrowB != result->nrow || ncolA != result->ncol)
	{
	  Rprintf("Multiplication error \n");
	   exit(1);
	}
	*/
    for(i=0;i<nrowB;i++)
      {
	A_pointer=object_matrixA->matrix;
	for(j=0;j<ncolA;j++)
	  {
	   
	    product = 0.0;
	    for(k=0;k<ncolB;k++)
	      {
		column_pointer=object_matrixB->column_list;
		for(l=0;l<k;l++)
		  {
		    column_pointer=column_pointer->next;
		  }
		B_pointer=column_pointer->data;
		product = product + B_pointer[i]*A_pointer[0];
		A_pointer++;
	      }
	    result->matrix[i+(nrowB*j)]=product;
	  }
      
      }
      
    }
  return ;
}

/*==============================================================
static void print_functions(struct basis_function_matrix  *functions_matrix)
				    
==============================================================

{
  int i,j;
  struct basis_function_matrix  *current_predictor;
  struct basis_function  *function;
  
  current_predictor = functions_matrix;
  for(i=0;i<predictors;i++)
    {
      
      if(current_predictor->number_of_basis_functions != 0)
	{function = current_predictor->functions;}
      for(j=0;j<current_predictor->number_of_basis_functions;j++)
	{
          Rprintf("%d\t%d\t%f\t%d\t%d\t%f\n",function->predictor1,
                                               function->knot1_index,
                                               function->knot1_value,
                                               function->predictor2,
                                               function->knot2_index,
                                               function->knot2_value);
	 
	  if(j!=current_predictor->number_of_basis_functions-1)
	    {function = function->link;}
	}
     
      current_predictor= current_predictor->next_predictor;
    }
  
}
	    
*/
/*---------------------------------------------------------------*/
  




static logical lsame(char *ca, char *cb)
{
/*  -- LAPACK auxiliary routine (version 2.0) --   
       Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,   
       Courant Institute, Argonne National Lab, and Rice University   
       September 30, 1994   


    Purpose   
    =======   

    LSAME returns .TRUE. if CA is the same letter as CB regardless of   
    case.   

    Arguments   
    =========   

    CA      (input) CHARACTER*1   
    CB      (input) CHARACTER*1   
            CA and CB specify the single characters to be compared.   

   ===================================================================== 
  


       Test if the characters are equal */
    /* System generated locals */
    logical ret_val;
    /* Local variables */
    static int inta, intb, zcode;


    ret_val = *(unsigned char *)ca == *(unsigned char *)cb;
    if (ret_val) {
	return ret_val;
    }

/*     Now test for equivalence if both characters are alphabetic. */

    zcode = 'Z';

/*     Use 'Z' rather than 'A' so that ASCII can be detected on Prime   
       machines, on which ICHAR returns a value with bit 8 set.   
       ICHAR('A') on Prime machines returns 193 which is the same as   
       ICHAR('A') on an EBCDIC machine. */

    inta = *(unsigned char *)ca;
    intb = *(unsigned char *)cb;

    if (zcode == 90 || zcode == 122) {

/*        ASCII is assumed - ZCODE is the ASCII code of either lower o
r   
          upper case 'Z'. */

	if (inta >= 97 && inta <= 122) {
	    inta += -32;
	}
	if (intb >= 97 && intb <= 122) {
	    intb += -32;
	}

    } else if (zcode == 233 || zcode == 169) {

/*        EBCDIC is assumed - ZCODE is the EBCDIC code of either lower
 or   
          upper case 'Z'. */

	if (inta >= 129 && inta <= 137 || inta >= 145 && inta <= 153 || inta 
		>= 162 && inta <= 169) {
	    inta += 64;
	}
	if (intb >= 129 && intb <= 137 || intb >= 145 && intb <= 153 || intb 
		>= 162 && intb <= 169) {
	    intb += 64;
	}

    } else if (zcode == 218 || zcode == 250) {

/*        ASCII is assumed, on Prime machines - ZCODE is the ASCII cod
e   
          plus 128 of either lower or upper case 'Z'. */

	if (inta >= 225 && inta <= 250) {
	    inta += -32;
	}
	if (intb >= 225 && intb <= 250) {
	    intb += -32;
	}
    }
    ret_val = inta == intb;

/*     RETURN   

       End of LSAME */

    return ret_val;
} /* lsame_ */



static int xerbla(char *srname, int *info)
{
/*  -- LAPACK auxiliary routine (version 2.0) --   
       Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,   
       Courant Institute, Argonne National Lab, and Rice University   
       September 30, 1994   


    Purpose   
    =======   

    XERBLA  is an error handler for the LAPACK routines.   
    It is called by an LAPACK routine if an input parameter has an   
    invalid value.  A message is printed and execution stops.   

    Installers may consider modifying the STOP statement in order to   
    call system-specific exception-handling facilities.   

    Arguments   
    =========   

    SRNAME  (input) CHARACTER*6   
            The name of the routine which called XERBLA.   

    INFO    (input) INT   
            The position of the invalid parameter in the parameter list   

            of the calling routine.   

   ===================================================================== 
*/

    Rprintf("** On entry to %6s, parameter number %2i had an illegal value\n",
		srname, *info);

/*     End of XERBLA */

    return 0;
} /* xerbla */


/*  -- translated by f2c (version 19940927).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/



static int idamax(int *n, double *dx, int *incx)
{


    /* System generated locals */
    int ret_val, i__1;
    double d__1;

    /* Local variables */
    static double dmax__;
    static int i, ix;


/*     finds the index of element having max. absolute value.   
       jack dongarra, linpack, 3/11/78.   
       modified 3/93 to return if incx .le. 0.   
       modified 12/3/93, array(1) declarations changed to array(*)   


    
   Parameter adjustments   
       Function Body */
#define DX(I) dx[(I)-1]


    ret_val = 0;
    if (*n < 1 || *incx <= 0) {
	return ret_val;
    }
    ret_val = 1;
    if (*n == 1) {
	return ret_val;
    }
    if (*incx == 1) {
	goto L20;
    }

/*        code for increment not equal to 1 */

    ix = 1;
    dmax__ = abs(DX(1));
    ix += *incx;
    i__1 = *n;
    for (i = 2; i <= *n; ++i) {
	if ((d__1 = DX(ix), abs(d__1)) <= dmax__) {
	    goto L5;
	}
	ret_val = i;
	dmax__ = (d__1 = DX(ix), abs(d__1));
L5:
	ix += *incx;
/* L10: */
    }
    return ret_val;

/*        code for increment equal to 1 */

L20:
    dmax__ = abs(DX(1));
    i__1 = *n;
    for (i = 2; i <= *n; ++i) {
	if ((d__1 = DX(i), abs(d__1)) <= dmax__) {
	    goto L30;
	}
	ret_val = i;
	dmax__ = (d__1 = DX(i), abs(d__1));
L30:
	;
    }

    return ret_val;
} /* idamax */


/*  -- translated by f2c (version 19940927).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/



static int dswap(int *n, double *dx, int *incx, 
	double *dy, int *incy)
{


    /* System generated locals */
    int i__1;

    /* Local variables */
    static int i, m;
    static double dtemp;
    static int ix, iy, mp1;


/*     interchanges two vectors.   
       uses unrolled loops for increments equal one.   
       jack dongarra, linpack, 3/11/78.   
       modified 12/3/93, array(1) declarations changed to array(*)   


    
   Parameter adjustments   
       Function Body */
#define DY(I) dy[(I)-1]



    if (*n <= 0) {
	return 0;
    }
    if (*incx == 1 && *incy == 1) {
	goto L20;
    }

/*       code for unequal increments or equal increments not equal   
           to 1 */

    ix = 1;
    iy = 1;
    if (*incx < 0) {
	ix = (-(*n) + 1) * *incx + 1;
    }
    if (*incy < 0) {
	iy = (-(*n) + 1) * *incy + 1;
    }
    i__1 = *n;
    for (i = 1; i <= *n; ++i) {
	dtemp = DX(ix);
	DX(ix) = DY(iy);
	DY(iy) = dtemp;
	ix += *incx;
	iy += *incy;
/* L10: */
    }
    return 0;

/*       code for both increments equal to 1   


         clean-up loop */

L20:
    m = *n % 3;
    if (m == 0) {
	goto L40;
    }
    i__1 = m;
    for (i = 1; i <= m; ++i) {
	dtemp = DX(i);
	DX(i) = DY(i);
	DY(i) = dtemp;
/* L30: */
    }
    if (*n < 3) {
	return 0;
    }
L40:
    mp1 = m + 1;
    i__1 = *n;
    for (i = mp1; i <= *n; i += 3) {
	dtemp = DX(i);
	DX(i) = DY(i);
	DY(i) = dtemp;
	dtemp = DX(i + 1);
	DX(i + 1) = DY(i + 1);
	DY(i + 1) = dtemp;
	dtemp = DX(i + 2);
	DX(i + 2) = DY(i + 2);
	DY(i + 2) = dtemp;
/* L50: */
    }


    return 0;
} /* dswap */


/*  -- translated by f2c (version 19940927).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/



static int dspr(char *uplo, int *n, double *alpha, 
	double *x, int *incx, double *ap)
{


    /* System generated locals */
    int i__1, i__2;

    /* Local variables */
    static int info;
    static double temp;
    static int i, j, k;
    static int kk, ix, jx, kx;


/*  Purpose   
    =======   

    DSPR    performs the symmetric rank 1 operation   

       A := alpha*x*x' + A,   

    where alpha is a real scalar, x is an n element vector and A is an   
    n by n symmetric matrix, supplied in packed form.   

    Parameters   
    ==========   

    UPLO   - CHARACTER*1.   
             On entry, UPLO specifies whether the upper or lower   
             triangular part of the matrix A is supplied in the packed   
             array AP as follows:   

                UPLO = 'U' or 'u'   The upper triangular part of A is   
                                    supplied in AP.   

                UPLO = 'L' or 'l'   The lower triangular part of A is   
                                    supplied in AP.   

             Unchanged on exit.   

    N      - INT.   
             On entry, N specifies the order of the matrix A.   
             N must be at least zero.   
             Unchanged on exit.   

    ALPHA  - DOUBLE PRECISION.   
             On entry, ALPHA specifies the scalar alpha.   
             Unchanged on exit.   

    X      - DOUBLE PRECISION array of dimension at least   
             ( 1 + ( n - 1 )*abs( INCX ) ).   
             Before entry, the incremented array X must contain the n   
             element vector x.   
             Unchanged on exit.   

    INCX   - INT.   
             On entry, INCX specifies the increment for the elements of   
             X. INCX must not be zero.   
             Unchanged on exit.   

    AP     - DOUBLE PRECISION array of DIMENSION at least   
             ( ( n*( n + 1 ) )/2 ).   
             Before entry with  UPLO = 'U' or 'u', the array AP must   
             contain the upper triangular part of the symmetric matrix   
             packed sequentially, column by column, so that AP( 1 )   
             contains a( 1, 1 ), AP( 2 ) and AP( 3 ) contain a( 1, 2 )   
             and a( 2, 2 ) respectively, and so on. On exit, the array   
             AP is overwritten by the upper triangular part of the   
             updated matrix.   
             Before entry with UPLO = 'L' or 'l', the array AP must   
             contain the lower triangular part of the symmetric matrix   
             packed sequentially, column by column, so that AP( 1 )   
             contains a( 1, 1 ), AP( 2 ) and AP( 3 ) contain a( 2, 1 )   
             and a( 3, 1 ) respectively, and so on. On exit, the array   
             AP is overwritten by the lower triangular part of the   
             updated matrix.   


    Level 2 Blas routine.   

    -- Written on 22-October-1986.   
       Jack Dongarra, Argonne National Lab.   
       Jeremy Du Croz, Nag Central Office.   
       Sven Hammarling, Nag Central Office.   
       Richard Hanson, Sandia National Labs.   



       Test the input parameters.   

    
   Parameter adjustments   
       Function Body */
#define AP(I) ap[(I)-1]
#define X(I) x[(I)-1]


    info = 0;
    if (! lsame(uplo, "U") && ! lsame(uplo, "L")) {
	info = 1;
    } else if (*n < 0) {
	info = 2;
    } else if (*incx == 0) {
	info = 5;
    }
    if (info != 0) {
	xerbla("DSPR  ", &info);
	return 0;
    }

/*     Quick return if possible. */

    if (*n == 0 || *alpha == 0.) {
	return 0;
    }

/*     Set the start point in X if the increment is not unity. */

    if (*incx <= 0) {
	kx = 1 - (*n - 1) * *incx;
    } else if (*incx != 1) {
	kx = 1;
    }

/*     Start the operations. In this version the elements of the array AP 
  
       are accessed sequentially with one pass through AP. */

    kk = 1;
    if (lsame(uplo, "U")) {

/*        Form  A  when upper triangle is stored in AP. */

	if (*incx == 1) {
	    i__1 = *n;
	    for (j = 1; j <= *n; ++j) {
		if (X(j) != 0.) {
		    temp = *alpha * X(j);
		    k = kk;
		    i__2 = j;
		    for (i = 1; i <= j; ++i) {
			AP(k) += X(i) * temp;
			++k;
/* L10: */
		    }
		}
		kk += j;
/* L20: */
	    }
	} else {
	    jx = kx;
	    i__1 = *n;
	    for (j = 1; j <= *n; ++j) {
		if (X(jx) != 0.) {
		    temp = *alpha * X(jx);
		    ix = kx;
		    i__2 = kk + j - 1;
		    for (k = kk; k <= kk+j-1; ++k) {
			AP(k) += X(ix) * temp;
			ix += *incx;
/* L30: */
		    }
		}
		jx += *incx;
		kk += j;
/* L40: */
	    }
	}
    } else {

/*        Form  A  when lower triangle is stored in AP. */

	if (*incx == 1) {
	    i__1 = *n;
	    for (j = 1; j <= *n; ++j) {
		if (X(j) != 0.) {
		    temp = *alpha * X(j);
		    k = kk;
		    i__2 = *n;
		    for (i = j; i <= *n; ++i) {
			AP(k) += X(i) * temp;
			++k;
/* L50: */
		    }
		}
		kk = kk + *n - j + 1;
/* L60: */
	    }
	} else {
	    jx = kx;
	    i__1 = *n;
	    for (j = 1; j <= *n; ++j) {
		if (X(jx) != 0.) {
		    temp = *alpha * X(jx);
		    ix = jx;
		    i__2 = kk + *n - j;
		    for (k = kk; k <= kk+*n-j; ++k) {
			AP(k) += X(ix) * temp;
			ix += *incx;
/* L70: */
		    }
		}
		jx += *incx;
		kk = kk + *n - j + 1;
/* L80: */
	    }
	}
    }

    return 0;

/*     End of DSPR  . */

} /* dspr */


/*  -- translated by f2c (version 19940927).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/



static int dscal(int *n, double *da, double *dx, int *incx)
{


    /* System generated locals */
    int i__1, i__2;

    /* Local variables */
    static int i, m, nincx, mp1;


/*     scales a vector by a constant.   
       uses unrolled loops for increment equal to one.   
       jack dongarra, linpack, 3/11/78.   
       modified 3/93 to return if incx .le. 0.   
       modified 12/3/93, array(1) declarations changed to array(*)   


    
   Parameter adjustments   
       Function Body */



    if (*n <= 0 || *incx <= 0) {
	return 0;
    }
    if (*incx == 1) {
	goto L20;
    }

/*        code for increment not equal to 1 */

    nincx = *n * *incx;
    i__1 = nincx;
    i__2 = *incx;
    for (i = 1; *incx < 0 ? i >= nincx : i <= nincx; i += *incx) {
	DX(i) = *da * DX(i);
/* L10: */
    }
    return 0;

/*        code for increment equal to 1   


          clean-up loop */

L20:
    m = *n % 5;
    if (m == 0) {
	goto L40;
    }
    i__2 = m;
    for (i = 1; i <= m; ++i) {
	DX(i) = *da * DX(i);
/* L30: */
    }
    if (*n < 5) {
	return 0;
    }
L40:
    mp1 = m + 1;
    i__2 = *n;
    for (i = mp1; i <= *n; i += 5) {
	DX(i) = *da * DX(i);
	DX(i + 1) = *da * DX(i + 1);
	DX(i + 2) = *da * DX(i + 2);
	DX(i + 3) = *da * DX(i + 3);
	DX(i + 4) = *da * DX(i + 4);
/* L50: */
    }

    return 0;
} /* dscal */



static int dlaev2(double *a, double *b, double *c, 
	double *rt1, double *rt2, double *cs1, double *sn1)
{
/*  -- LAPACK auxiliary routine (version 2.0) --   
       Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,   
       Courant Institute, Argonne National Lab, and Rice University   
       October 31, 1992   


    Purpose   
    =======   

    DLAEV2 computes the eigendecomposition of a 2-by-2 symmetric matrix   
       [  A   B  ]   
       [  B   C  ].   
    On return, RT1 is the eigenvalue of larger absolute value, RT2 is the 
  
    eigenvalue of smaller absolute value, and (CS1,SN1) is the unit right 
  
    eigenvector for RT1, giving the decomposition   

       [ CS1  SN1 ] [  A   B  ] [ CS1 -SN1 ]  =  [ RT1  0  ]   
       [-SN1  CS1 ] [  B   C  ] [ SN1  CS1 ]     [  0  RT2 ].   

    Arguments   
    =========   

    A       (input) DOUBLE PRECISION   
            The (1,1) element of the 2-by-2 matrix.   

    B       (input) DOUBLE PRECISION   
            The (1,2) element and the conjugate of the (2,1) element of   
            the 2-by-2 matrix.   

    C       (input) DOUBLE PRECISION   
            The (2,2) element of the 2-by-2 matrix.   

    RT1     (output) DOUBLE PRECISION   
            The eigenvalue of larger absolute value.   

    RT2     (output) DOUBLE PRECISION   
            The eigenvalue of smaller absolute value.   

    CS1     (output) DOUBLE PRECISION   
    SN1     (output) DOUBLE PRECISION   
            The vector (CS1, SN1) is a unit right eigenvector for RT1.   

    Further Details   
    ===============   

    RT1 is accurate to a few ulps barring over/underflow.   

    RT2 may be inaccurate if there is massive cancellation in the   
    determinant A*C-B*B; higher precision or correctly rounded or   
    correctly truncated arithmetic would be needed to compute RT2   
    accurately in all cases.   

    CS1 and SN1 are accurate to a few ulps barring over/underflow.   

    Overflow is possible only if RT1 is within a factor of 5 of overflow. 
  
    Underflow is harmless if the input data is 0 or exceeds   
       underflow_threshold / macheps.   

   ===================================================================== 
  


       Compute the eigenvalues */
    /* System generated locals */
    double d__1;
    /* Builtin functions */
    double sqrt(double);
    /* Local variables */
    static double acmn, acmx, ab, df, cs, ct, tb, sm, tn, rt, adf, acs;
    static int sgn1, sgn2;


    sm = *a + *c;
    df = *a - *c;
    adf = abs(df);
    tb = *b + *b;
    ab = abs(tb);
    if (abs(*a) > abs(*c)) {
	acmx = *a;
	acmn = *c;
    } else {
	acmx = *c;
	acmn = *a;
    }
    if (adf > ab) {
/* Computing 2nd power */
	d__1 = ab / adf;
	rt = adf * sqrt(d__1 * d__1 + 1.);
    } else if (adf < ab) {
/* Computing 2nd power */
	d__1 = adf / ab;
	rt = ab * sqrt(d__1 * d__1 + 1.);
    } else {

/*        Includes case AB=ADF=0 */

	rt = ab * sqrt(2.);
    }
    if (sm < 0.) {
	*rt1 = (sm - rt) * .5;
	sgn1 = -1;

/*        Order of execution important.   
          To get fully accurate smaller eigenvalue,   
          next line needs to be executed in higher precision. */

	*rt2 = acmx / *rt1 * acmn - *b / *rt1 * *b;
    } else if (sm > 0.) {
	*rt1 = (sm + rt) * .5;
	sgn1 = 1;

/*        Order of execution important.   
          To get fully accurate smaller eigenvalue,   
          next line needs to be executed in higher precision. */

	*rt2 = acmx / *rt1 * acmn - *b / *rt1 * *b;
    } else {

/*        Includes case RT1 = RT2 = 0 */

	*rt1 = rt * .5;
	*rt2 = rt * -.5;
	sgn1 = 1;
    }

/*     Compute the eigenvector */

    if (df >= 0.) {
	cs = df + rt;
	sgn2 = 1;
    } else {
	cs = df - rt;
	sgn2 = -1;
    }
    acs = abs(cs);
    if (acs > ab) {
	ct = -tb / cs;
	*sn1 = 1. / sqrt(ct * ct + 1.);
	*cs1 = ct * *sn1;
    } else {
	if (ab == 0.) {
	    *cs1 = 1.;
	    *sn1 = 0.;
	} else {
	    tn = -cs / tb;
	    *cs1 = 1. / sqrt(tn * tn + 1.);
	    *sn1 = tn * *cs1;
	}
    }
    if (sgn1 == sgn2) {
	tn = *cs1;
	*cs1 = -(*sn1);
	*sn1 = tn;
    }
    return 0;

/*     End of DLAEV2 */

} /* dlaev2 */


/*  -- translated by f2c (version 19940927).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/



static int drot(int *n, double *dx, int *incx, double *dy, int *incy, double *c, double *s)
{


    /* System generated locals */
    int i__1;

    /* Local variables */
    static int i;
    static double dtemp;
    static int ix, iy;


/*     applies a plane rotation.   
       jack dongarra, linpack, 3/11/78.   
       modified 12/3/93, array(1) declarations changed to array(*)   


    
   Parameter adjustments   
       Function Body */



    if (*n <= 0) {
	return 0;
    }
    if (*incx == 1 && *incy == 1) {
	goto L20;
    }

/*       code for unequal increments or equal increments not equal   
           to 1 */

    ix = 1;
    iy = 1;
    if (*incx < 0) {
	ix = (-(*n) + 1) * *incx + 1;
    }
    if (*incy < 0) {
	iy = (-(*n) + 1) * *incy + 1;
    }
    i__1 = *n;
    for (i = 1; i <= *n; ++i) {
	dtemp = *c * DX(ix) + *s * DY(iy);
	DY(iy) = *c * DY(iy) - *s * DX(ix);
	DX(ix) = dtemp;
	ix += *incx;
	iy += *incy;
/* L10: */
    }
    return 0;

/*       code for both increments equal to 1 */

L20:
    i__1 = *n;
    for (i = 1; i <= *n; ++i) {
	dtemp = *c * DX(i) + *s * DY(i);
	DY(i) = *c * DY(i) - *s * DX(i);
	DX(i) = dtemp;
/* L30: */
    }

    return 0;
} /* drot */


/*  -- translated by f2c (version 19940927).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/



static int dcopy(int *n, double *dx, int *incx, double *dy, int *incy)
{


    /* System generated locals */
    int i__1;

    /* Local variables */
    static int i, m, ix, iy, mp1;


/*     copies a vector, x, to a vector, y.   
       uses unrolled loops for increments equal to one.   
       jack dongarra, linpack, 3/11/78.   
       modified 12/3/93, array(1) declarations changed to array(*)   


    
   Parameter adjustments   
       Function Body */



    if (*n <= 0) {
	return 0;
    }
    if (*incx == 1 && *incy == 1) {
	goto L20;
    }

/*        code for unequal increments or equal increments   
            not equal to 1 */

    ix = 1;
    iy = 1;
    if (*incx < 0) {
	ix = (-(*n) + 1) * *incx + 1;
    }
    if (*incy < 0) {
	iy = (-(*n) + 1) * *incy + 1;
    }
    i__1 = *n;
    for (i = 1; i <= *n; ++i) {
	DY(iy) = DX(ix);
	ix += *incx;
	iy += *incy;
/* L10: */
    }
    return 0;

/*        code for both increments equal to 1   


          clean-up loop */

L20:
    m = *n % 7;
    if (m == 0) {
	goto L40;
    }
    i__1 = m;
    for (i = 1; i <= m; ++i) {
	DY(i) = DX(i);
/* L30: */
    }
    if (*n < 7) {
	return 0;
    }
L40:
    mp1 = m + 1;
    i__1 = *n;
    for (i = mp1; i <= *n; i += 7) {
	DY(i) = DX(i);
	DY(i + 1) = DX(i + 1);
	DY(i + 2) = DX(i + 2);
	DY(i + 3) = DX(i + 3);
	DY(i + 4) = DX(i + 4);
	DY(i + 5) = DX(i + 5);
	DY(i + 6) = DX(i + 6);
/* L50: */
    }

    return 0;
} /* dcopy */


/*  -- translated by f2c (version 19940927).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/



static int dspmv(char *uplo, int *n, double *alpha, 
	double *ap, double *x, int *incx, double *beta, 
	double *y, int *incy)
{


    /* System generated locals */
    int i__1, i__2;

    /* Local variables */
    static int info;
    static double temp1, temp2;
    static int i, j, k;
    static int kk, ix, iy, jx, jy, kx, ky;


/*  Purpose   
    =======   

    DSPMV  performs the matrix-vector operation   

       y := alpha*A*x + beta*y,   

    where alpha and beta are scalars, x and y are n element vectors and   
    A is an n by n symmetric matrix, supplied in packed form.   

    Parameters   
    ==========   

    UPLO   - CHARACTER*1.   
             On entry, UPLO specifies whether the upper or lower   
             triangular part of the matrix A is supplied in the packed   
             array AP as follows:   

                UPLO = 'U' or 'u'   The upper triangular part of A is   
                                    supplied in AP.   

                UPLO = 'L' or 'l'   The lower triangular part of A is   
                                    supplied in AP.   

             Unchanged on exit.   

    N      - INT.   
             On entry, N specifies the order of the matrix A.   
             N must be at least zero.   
             Unchanged on exit.   

    ALPHA  - DOUBLE PRECISION.   
             On entry, ALPHA specifies the scalar alpha.   
             Unchanged on exit.   

    AP     - DOUBLE PRECISION array of DIMENSION at least   
             ( ( n*( n + 1 ) )/2 ).   
             Before entry with UPLO = 'U' or 'u', the array AP must   
             contain the upper triangular part of the symmetric matrix   
             packed sequentially, column by column, so that AP( 1 )   
             contains a( 1, 1 ), AP( 2 ) and AP( 3 ) contain a( 1, 2 )   
             and a( 2, 2 ) respectively, and so on.   
             Before entry with UPLO = 'L' or 'l', the array AP must   
             contain the lower triangular part of the symmetric matrix   
             packed sequentially, column by column, so that AP( 1 )   
             contains a( 1, 1 ), AP( 2 ) and AP( 3 ) contain a( 2, 1 )   
             and a( 3, 1 ) respectively, and so on.   
             Unchanged on exit.   

    X      - DOUBLE PRECISION array of dimension at least   
             ( 1 + ( n - 1 )*abs( INCX ) ).   
             Before entry, the incremented array X must contain the n   
             element vector x.   
             Unchanged on exit.   

    INCX   - INT.   
             On entry, INCX specifies the increment for the elements of   
             X. INCX must not be zero.   
             Unchanged on exit.   

    BETA   - DOUBLE PRECISION.   
             On entry, BETA specifies the scalar beta. When BETA is   
             supplied as zero then Y need not be set on input.   
             Unchanged on exit.   

    Y      - DOUBLE PRECISION array of dimension at least   
             ( 1 + ( n - 1 )*abs( INCY ) ).   
             Before entry, the incremented array Y must contain the n   
             element vector y. On exit, Y is overwritten by the updated   
             vector y.   

    INCY   - INT.   
             On entry, INCY specifies the increment for the elements of   
             Y. INCY must not be zero.   
             Unchanged on exit.   


    Level 2 Blas routine.   

    -- Written on 22-October-1986.   
       Jack Dongarra, Argonne National Lab.   
       Jeremy Du Croz, Nag Central Office.   
       Sven Hammarling, Nag Central Office.   
       Richard Hanson, Sandia National Labs.   



       Test the input parameters.   

    
   Parameter adjustments   
       Function Body */
#define Y(I) y[(I)-1]



    info = 0;
    if (! lsame(uplo, "U") && ! lsame(uplo, "L")) {
	info = 1;
    } else if (*n < 0) {
	info = 2;
    } else if (*incx == 0) {
	info = 6;
    } else if (*incy == 0) {
	info = 9;
    }
    if (info != 0) {
	xerbla("DSPMV ", &info);
	return 0;
    }

/*     Quick return if possible. */

    if (*n == 0 || *alpha == 0. && *beta == 1.) {
	return 0;
    }

/*     Set up the start points in  X  and  Y. */

    if (*incx > 0) {
	kx = 1;
    } else {
	kx = 1 - (*n - 1) * *incx;
    }
    if (*incy > 0) {
	ky = 1;
    } else {
	ky = 1 - (*n - 1) * *incy;
    }

/*     Start the operations. In this version the elements of the array AP 
  
       are accessed sequentially with one pass through AP.   

       First form  y := beta*y. */

    if (*beta != 1.) {
	if (*incy == 1) {
	    if (*beta == 0.) {
		i__1 = *n;
		for (i = 1; i <= *n; ++i) {
		    Y(i) = 0.;
/* L10: */
		}
	    } else {
		i__1 = *n;
		for (i = 1; i <= *n; ++i) {
		    Y(i) = *beta * Y(i);
/* L20: */
		}
	    }
	} else {
	    iy = ky;
	    if (*beta == 0.) {
		i__1 = *n;
		for (i = 1; i <= *n; ++i) {
		    Y(iy) = 0.;
		    iy += *incy;
/* L30: */
		}
	    } else {
		i__1 = *n;
		for (i = 1; i <= *n; ++i) {
		    Y(iy) = *beta * Y(iy);
		    iy += *incy;
/* L40: */
		}
	    }
	}
    }
    if (*alpha == 0.) {
	return 0;
    }
    kk = 1;
    if (lsame(uplo, "U")) {

/*        Form  y  when AP contains the upper triangle. */

	if (*incx == 1 && *incy == 1) {
	    i__1 = *n;
	    for (j = 1; j <= *n; ++j) {
		temp1 = *alpha * X(j);
		temp2 = 0.;
		k = kk;
		i__2 = j - 1;
		for (i = 1; i <= j-1; ++i) {
		    Y(i) += temp1 * AP(k);
		    temp2 += AP(k) * X(i);
		    ++k;
/* L50: */
		}
		Y(j) = Y(j) + temp1 * AP(kk + j - 1) + *alpha * temp2;
		kk += j;
/* L60: */
	    }
	} else {
	    jx = kx;
	    jy = ky;
	    i__1 = *n;
	    for (j = 1; j <= *n; ++j) {
		temp1 = *alpha * X(jx);
		temp2 = 0.;
		ix = kx;
		iy = ky;
		i__2 = kk + j - 2;
		for (k = kk; k <= kk+j-2; ++k) {
		    Y(iy) += temp1 * AP(k);
		    temp2 += AP(k) * X(ix);
		    ix += *incx;
		    iy += *incy;
/* L70: */
		}
		Y(jy) = Y(jy) + temp1 * AP(kk + j - 1) + *alpha * temp2;
		jx += *incx;
		jy += *incy;
		kk += j;
/* L80: */
	    }
	}
    } else {

/*        Form  y  when AP contains the lower triangle. */

	if (*incx == 1 && *incy == 1) {
	    i__1 = *n;
	    for (j = 1; j <= *n; ++j) {
		temp1 = *alpha * X(j);
		temp2 = 0.;
		Y(j) += temp1 * AP(kk);
		k = kk + 1;
		i__2 = *n;
		for (i = j + 1; i <= *n; ++i) {
		    Y(i) += temp1 * AP(k);
		    temp2 += AP(k) * X(i);
		    ++k;
/* L90: */
		}
		Y(j) += *alpha * temp2;
		kk += *n - j + 1;
/* L100: */
	    }
	} else {
	    jx = kx;
	    jy = ky;
	    i__1 = *n;
	    for (j = 1; j <= *n; ++j) {
		temp1 = *alpha * X(jx);
		temp2 = 0.;
		Y(jy) += temp1 * AP(kk);
		ix = jx;
		iy = jy;
		i__2 = kk + *n - j;
		for (k = kk + 1; k <= kk+*n-j; ++k) {
		    ix += *incx;
		    iy += *incy;
		    Y(iy) += temp1 * AP(k);
		    temp2 += AP(k) * X(ix);
/* L110: */
		}
		Y(jy) += *alpha * temp2;
		jx += *incx;
		jy += *incy;
		kk += *n - j + 1;
/* L120: */
	    }
	}
    }

    return 0;

/*     End of DSPMV . */

} /* dspmv */


/*  -- translated by f2c (version 19940927).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/



static double ddot(int *n, double *dx, int *incx, double *dy, int *incy)
{


    /* System generated locals */
    int i__1;
    double ret_val;

    /* Local variables */
    static int i, m;
    static double dtemp;
    static int ix, iy, mp1;


/*     forms the dot product of two vectors.   
       uses unrolled loops for increments equal to one.   
       jack dongarra, linpack, 3/11/78.   
       modified 12/3/93, array(1) declarations changed to array(*)   


    
   Parameter adjustments   
       Function Body */



    ret_val = 0.;
    dtemp = 0.;
    if (*n <= 0) {
	return ret_val;
    }
    if (*incx == 1 && *incy == 1) {
	goto L20;
    }

/*        code for unequal increments or equal increments   
            not equal to 1 */

    ix = 1;
    iy = 1;
    if (*incx < 0) {
	ix = (-(*n) + 1) * *incx + 1;
    }
    if (*incy < 0) {
	iy = (-(*n) + 1) * *incy + 1;
    }
    i__1 = *n;
    for (i = 1; i <= *n; ++i) {
	dtemp += DX(ix) * DY(iy);
	ix += *incx;
	iy += *incy;
/* L10: */
    }
    ret_val = dtemp;
    return ret_val;

/*        code for both increments equal to 1   


          clean-up loop */

L20:
    m = *n % 5;
    if (m == 0) {
	goto L40;
    }
    i__1 = m;
    for (i = 1; i <= m; ++i) {
	dtemp += DX(i) * DY(i);
/* L30: */
    }
    if (*n < 5) {
	goto L60;
    }
L40:
    mp1 = m + 1;
    i__1 = *n;
    for (i = mp1; i <= *n; i += 5) {
	dtemp = dtemp + DX(i) * DY(i) + DX(i + 1) * DY(i + 1) + DX(i + 2) * 
		DY(i + 2) + DX(i + 3) * DY(i + 3) + DX(i + 4) * DY(i + 4);
/* L50: */
    }
L60:
    ret_val = dtemp;

    return ret_val;
} /* ddot */


static int invert_matrix(struct matrix1 *object_matrix)
{
  char flag;
  int i,j,dim,k;
  int ok,info;
  int *int_array;
  double *packed_matrix,*workspace;
  
  

  ok =0;
  dim = object_matrix->nrow;
  
/*--pack top half of this matrix into array for inversion routine-*/
  packed_matrix= (double *)Salloc(1+(dim*(dim+1))/2,double);
  
  k=-1;
  for(i=0;i<dim;i++)
    {
      for(j=0;j<=i;j++)
	{
	  k++;
	  packed_matrix[k]=object_matrix->matrix[(i*dim)+j];
	}
    }
  int_array = (int *)Salloc(dim+1,int);
   
  flag = 'U';
  info=0;
  
  
/*--factorisation step -----------------------------------*/
  ok = dsptrf(&flag,&dim,packed_matrix,int_array,&info);
  
/*--inversion step----------------------------------------*/
  workspace = (double *)Salloc(dim*dim+1,double);
    
  ok = dsptri(&flag,&dim,packed_matrix,int_array,workspace,&info);
  
  
  
/*--unpacking into back into matrix ----------------------*/
  k=0;
  for(j=0;j<dim;j++)
    {
      for(i=0;i<=j;i++)
	{
	  
	  object_matrix->matrix[(j*dim)+i]=packed_matrix[i + j*(j+1)/2];
	  
	}
    }
  for(j=1;j<dim;j++)
    {
      for(i=0;i<j;i++)
	{
	  object_matrix->matrix[j+(i*dim)]=object_matrix->matrix[(j*dim)+i];
	}
    }
  
  return ok;

}

static int dsptrf(char *uplo, int *n, double *ap, int * ipiv, int *info)
{
/*  -- LAPACK routine (version 2.0) --   
       Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,   
       Courant Institute, Argonne National Lab, and Rice University   
       March 31, 1993   


    Purpose   
    =======   

    DSPTRF computes the factorization of a real symmetric matrix A stored 
  
    in packed format using the Bunch-Kaufman diagonal pivoting method:   

       A = U*D*U**T  or  A = L*D*L**T   

    where U (or L) is a product of permutation and unit upper (lower)   
    triangular matrices, and D is symmetric and block diagonal with   
    1-by-1 and 2-by-2 diagonal blocks.   

    Arguments   
    =========   

    UPLO    (input) CHARACTER*1   
            = 'U':  Upper triangle of A is stored;   
            = 'L':  Lower triangle of A is stored.   

    N       (input) INT   
            The order of the matrix A.  N >= 0.   

    AP      (input/output) DOUBLE PRECISION array, dimension (N*(N+1)/2) 
  
            On entry, the upper or lower triangle of the symmetric matrix 
  
            A, packed columnwise in a linear array.  The j-th column of A 
  
            is stored in the array AP as follows:   
            if UPLO = 'U', AP(i + (j-1)*j/2) = A(i,j) for 1<=i<=j;   
            if UPLO = 'L', AP(i + (j-1)*(2n-j)/2) = A(i,j) for j<=i<=n.   

            On exit, the block diagonal matrix D and the multipliers used 
  
            to obtain the factor U or L, stored as a packed triangular   
            matrix overwriting A (see below for further details).   

    IPIV    (output) INT array, dimension (N)   
            Details of the interchanges and the block structure of D.   
            If IPIV(k) > 0, then rows and columns k and IPIV(k) were   
            interchanged and D(k,k) is a 1-by-1 diagonal block.   
            If UPLO = 'U' and IPIV(k) = IPIV(k-1) < 0, then rows and   
            columns k-1 and -IPIV(k) were interchanged and D(k-1:k,k-1:k) 
  
            is a 2-by-2 diagonal block.  If UPLO = 'L' and IPIV(k) =   
            IPIV(k+1) < 0, then rows and columns k+1 and -IPIV(k) were   
            interchanged and D(k:k+1,k:k+1) is a 2-by-2 diagonal block.   

    INFO    (output) INT   
            = 0: successful exit   
            < 0: if INFO = -i, the i-th argument had an illegal value   
            > 0: if INFO = i, D(i,i) is exactly zero.  The factorization 
  
                 has been completed, but the block diagonal matrix D is   
                 exactly singular, and division by zero will occur if it 
  
                 is used to solve a system of equations.   

    Further Details   
    ===============   

    If UPLO = 'U', then A = U*D*U', where   
       U = P(n)*U(n)* ... *P(k)U(k)* ...,   
    i.e., U is a product of terms P(k)*U(k), where k decreases from n to 
  
    1 in steps of 1 or 2, and D is a block diagonal matrix with 1-by-1   
    and 2-by-2 diagonal blocks D(k).  P(k) is a permutation matrix as   
    defined by IPIV(k), and U(k) is a unit upper triangular matrix, such 
  
    that if the diagonal block D(k) is of order s (s = 1 or 2), then   

               (   I    v    0   )   k-s   
       U(k) =  (   0    I    0   )   s   
               (   0    0    I   )   n-k   
                  k-s   s   n-k   

    If s = 1, D(k) overwrites A(k,k), and v overwrites A(1:k-1,k).   
    If s = 2, the upper triangle of D(k) overwrites A(k-1,k-1), A(k-1,k), 
  
    and A(k,k), and v overwrites A(1:k-2,k-1:k).   

    If UPLO = 'L', then A = L*D*L', where   
       L = P(1)*L(1)* ... *P(k)*L(k)* ...,   
    i.e., L is a product of terms P(k)*L(k), where k increases from 1 to 
  
    n in steps of 1 or 2, and D is a block diagonal matrix with 1-by-1   
    and 2-by-2 diagonal blocks D(k).  P(k) is a permutation matrix as   
    defined by IPIV(k), and L(k) is a unit lower triangular matrix, such 
  
    that if the diagonal block D(k) is of order s (s = 1 or 2), then   

               (   I    0     0   )  k-1   
       L(k) =  (   0    I     0   )  s   
               (   0    v     I   )  n-k-s+1   
                  k-1   s  n-k-s+1   

    If s = 1, D(k) overwrites A(k,k), and v overwrites A(k+1:n,k).   
    If s = 2, the lower triangle of D(k) overwrites A(k,k), A(k+1,k),   
    and A(k+1,k+1), and v overwrites A(k+2:n,k:k+1).   

    ===================================================================== 
  


       Test the input parameters.   

    
   Parameter adjustments   
       Function Body */
    /* Table of constant values */
    static int c__1 = 1;
    
    /* System generated locals */
    int i__1;
    double d__1, d__2, d__3;
    /* Builtin functions */
    double sqrt(double);
    /* Local variables */
    static int imax, jmax;
    static double c;
    static int j, k;
    static double s, t, alpha;
    static int kstep;
    static logical upper;
    static double r1, r2;
    static int kc, kk, kp;
    static double absakk;
    static int kx;
    static double colmax, rowmax;
    static int knc, kpc, npp;



#define IPIV(I) ipiv[(I)-1]
#define AP(I) ap[(I)-1]


    *info = 0;
    upper = lsame(uplo, "U");
    if (! upper && ! lsame(uplo, "L")) {
	*info = -1;
    } else if (*n < 0) {
	*info = -2;
    }
    if (*info != 0) {
	i__1 = -(*info);
	xerbla("DSPTRF", &i__1);
	return 0;
    }

/*     Initialize ALPHA for use in choosing pivot block size. */

    alpha = (sqrt(17.) + 1.) / 8.;

    if (upper) {

/*        Factorize A as U*D*U' using the upper triangle of A   

          K is the main loop index, decreasing from N to 1 in steps of
   
          1 or 2 */

	k = *n;
	kc = (*n - 1) * *n / 2 + 1;
L10:
	knc = kc;

/*        If K < 1, exit from loop */

	if (k < 1) {
	    goto L70;
	}
	kstep = 1;

/*        Determine rows and columns to be interchanged and whether   
          a 1-by-1 or 2-by-2 pivot block will be used */

	absakk = (d__1 = AP(kc + k - 1), abs(d__1));

/*        IMAX is the row-index of the largest off-diagonal element in
   
          column K, and COLMAX is its absolute value */

	if (k > 1) {
	    i__1 = k - 1;
	    imax = idamax(&i__1, &AP(kc), &c__1);
	    colmax = (d__1 = AP(kc + imax - 1), abs(d__1));
	} else {
	    colmax = 0.;
	}

	if (max(absakk,colmax) == 0.) {

/*           Column K is zero: set INFO and continue */

	    if (*info == 0) {
		*info = k;
	    }
	    kp = k;
	} else {
	    if (absakk >= alpha * colmax) {

/*              no interchange, use 1-by-1 pivot block */

		kp = k;
	    } else {

/*              JMAX is the column-index of the largest off-di
agonal   
                element in row IMAX, and ROWMAX is its absolut
e value */

		rowmax = 0.;
		jmax = imax;
		kx = imax * (imax + 1) / 2 + imax;
		i__1 = k;
		for (j = imax + 1; j <= k; ++j) {
		    if ((d__1 = AP(kx), abs(d__1)) > rowmax) {
			rowmax = (d__1 = AP(kx), abs(d__1));
			jmax = j;
		    }
		    kx += j;
/* L20: */
		}
		kpc = (imax - 1) * imax / 2 + 1;
		if (imax > 1) {
		    i__1 = imax - 1;
		    jmax = idamax(&i__1, &AP(kpc), &c__1);
/* Computing MAX */
		    d__2 = rowmax, d__3 = (d__1 = AP(kpc + jmax - 1), abs(
			    d__1));
		    rowmax = max(d__2,d__3);
		}

		if (absakk >= alpha * colmax * (colmax / rowmax)) {

/*                 no interchange, use 1-by-1 pivot block 
*/

		    kp = k;
		} else if ((d__1 = AP(kpc + imax - 1), abs(d__1)) >= alpha * 
			rowmax) {

/*                 interchange rows and columns K and IMAX
, use 1-by-1   
                   pivot block */

		    kp = imax;
		} else {

/*                 interchange rows and columns K-1 and IM
AX, use 2-by-2   
                   pivot block */

		    kp = imax;
		    kstep = 2;
		}
	    }

	    kk = k - kstep + 1;
	    if (kstep == 2) {
		knc = knc - k + 1;
	    }
	    if (kp != kk) {

/*              Interchange rows and columns KK and KP in the 
leading   
                submatrix A(1:k,1:k) */

		i__1 = kp - 1;
		dswap(&i__1, &AP(knc), &c__1, &AP(kpc), &c__1);
		kx = kpc + kp - 1;
		i__1 = kk - 1;
		for (j = kp + 1; j <= kk-1; ++j) {
		    kx = kx + j - 1;
		    t = AP(knc + j - 1);
		    AP(knc + j - 1) = AP(kx);
		    AP(kx) = t;
/* L30: */
		}
		t = AP(knc + kk - 1);
		AP(knc + kk - 1) = AP(kpc + kp - 1);
		AP(kpc + kp - 1) = t;
		if (kstep == 2) {
		    t = AP(kc + k - 2);
		    AP(kc + k - 2) = AP(kc + kp - 1);
		    AP(kc + kp - 1) = t;
		}
	    }

/*           Update the leading submatrix */

	    if (kstep == 1) {

/*              1-by-1 pivot block D(k): column k now holds   

                W(k) = U(k)*D(k)   

                where U(k) is the k-th column of U   

                Perform a rank-1 update of A(1:k-1,1:k-1) as 
  

                A := A - U(k)*D(k)*U(k)' = A - W(k)*1/D(k)*W(k
)' */

		r1 = 1. / AP(kc + k - 1);
		i__1 = k - 1;
		d__1 = -r1;
		dspr(uplo, &i__1, &d__1, &AP(kc), &c__1, &AP(1));

/*              Store U(k) in column k */

		i__1 = k - 1;
		dscal(&i__1, &r1, &AP(kc), &c__1);
	    } else {

/*              2-by-2 pivot block D(k): columns k and k-1 now
 hold   

                ( W(k-1) W(k) ) = ( U(k-1) U(k) )*D(k)   

                where U(k) and U(k-1) are the k-th and (k-1)-t
h columns   
                of U   

                Perform a rank-2 update of A(1:k-2,1:k-2) as 
  

                A := A - ( U(k-1) U(k) )*D(k)*( U(k-1) U(k) )'
   
                   = A - ( W(k-1) W(k) )*inv(D(k))*( W(k-1) W(
k) )'   

                Convert this to two rank-1 updates by using th
e eigen-   
                decomposition of D(k) */

		dlaev2(&AP(kc - 1), &AP(kc + k - 2), &AP(kc + k - 1), &r1, &
			r2, &c, &s);
		r1 = 1. / r1;
		r2 = 1. / r2;
		i__1 = k - 2;
		drot(&i__1, &AP(knc), &c__1, &AP(kc), &c__1, &c, &s);
		i__1 = k - 2;
		d__1 = -r1;
		dspr(uplo, &i__1, &d__1, &AP(knc), &c__1, &AP(1));
		i__1 = k - 2;
		d__1 = -r2;
		dspr(uplo, &i__1, &d__1, &AP(kc), &c__1, &AP(1));

/*              Store U(k) and U(k-1) in columns k and k-1 */

		i__1 = k - 2;
		dscal(&i__1, &r1, &AP(knc), &c__1);
		i__1 = k - 2;
		dscal(&i__1, &r2, &AP(kc), &c__1);
		i__1 = k - 2;
		d__1 = -s;
		drot(&i__1, &AP(knc), &c__1, &AP(kc), &c__1, &c, &d__1);
	    }
	}

/*        Store details of the interchanges in IPIV */

	if (kstep == 1) {
	    IPIV(k) = kp;
	} else {
	    IPIV(k) = -kp;
	    IPIV(k - 1) = -kp;
	}

/*        Decrease K and return to the start of the main loop */

	k -= kstep;
	kc = knc - k;
	goto L10;

    } else {

/*        Factorize A as L*D*L' using the lower triangle of A   

          K is the main loop index, increasing from 1 to N in steps of
   
          1 or 2 */

	k = 1;
	kc = 1;
	npp = *n * (*n + 1) / 2;
L40:
	knc = kc;

/*        If K > N, exit from loop */

	if (k > *n) {
	    goto L70;
	}
	kstep = 1;

/*        Determine rows and columns to be interchanged and whether   
          a 1-by-1 or 2-by-2 pivot block will be used */

	absakk = (d__1 = AP(kc), abs(d__1));

/*        IMAX is the row-index of the largest off-diagonal element in
   
          column K, and COLMAX is its absolute value */

	if (k < *n) {
	    i__1 = *n - k;
	    imax = k + idamax(&i__1, &AP(kc + 1), &c__1);
	    colmax = (d__1 = AP(kc + imax - k), abs(d__1));
	} else {
	    colmax = 0.;
	}

	if (max(absakk,colmax) == 0.) {

/*           Column K is zero: set INFO and continue */

	    if (*info == 0) {
		*info = k;
	    }
	    kp = k;
	} else {
	    if (absakk >= alpha * colmax) {

/*              no interchange, use 1-by-1 pivot block */

		kp = k;
	    } else {

/*              JMAX is the column-index of the largest off-di
agonal   
                element in row IMAX, and ROWMAX is its absolut
e value */

		rowmax = 0.;
		kx = kc + imax - k;
		i__1 = imax - 1;
		for (j = k; j <= imax-1; ++j) {
		    if ((d__1 = AP(kx), abs(d__1)) > rowmax) {
			rowmax = (d__1 = AP(kx), abs(d__1));
			jmax = j;
		    }
		    kx = kx + *n - j;
/* L50: */
		}
		kpc = npp - (*n - imax + 1) * (*n - imax + 2) / 2 + 1;
		if (imax < *n) {
		    i__1 = *n - imax;
		    jmax = imax + idamax(&i__1, &AP(kpc + 1), &c__1);
/* Computing MAX */
		    d__2 = rowmax, d__3 = (d__1 = AP(kpc + jmax - imax), abs(
			    d__1));
		    rowmax = max(d__2,d__3);
		}

		if (absakk >= alpha * colmax * (colmax / rowmax)) {

/*                 no interchange, use 1-by-1 pivot block 
*/

		    kp = k;
		} else if ((d__1 = AP(kpc), abs(d__1)) >= alpha * rowmax) {

/*                 interchange rows and columns K and IMAX
, use 1-by-1   
                   pivot block */

		    kp = imax;
		} else {

/*                 interchange rows and columns K+1 and IM
AX, use 2-by-2   
                   pivot block */

		    kp = imax;
		    kstep = 2;
		}
	    }

	    kk = k + kstep - 1;
	    if (kstep == 2) {
		knc = knc + *n - k + 1;
	    }
	    if (kp != kk) {

/*              Interchange rows and columns KK and KP in the 
trailing   
                submatrix A(k:n,k:n) */

		if (kp < *n) {
		    i__1 = *n - kp;
		    dswap(&i__1, &AP(knc + kp - kk + 1), &c__1, &AP(kpc + 1),
			     &c__1);
		}
		kx = knc + kp - kk;
		i__1 = kp - 1;
		for (j = kk + 1; j <= kp-1; ++j) {
		    kx = kx + *n - j + 1;
		    t = AP(knc + j - kk);
		    AP(knc + j - kk) = AP(kx);
		    AP(kx) = t;
/* L60: */
		}
		t = AP(knc);
		AP(knc) = AP(kpc);
		AP(kpc) = t;
		if (kstep == 2) {
		    t = AP(kc + 1);
		    AP(kc + 1) = AP(kc + kp - k);
		    AP(kc + kp - k) = t;
		}
	    }

/*           Update the trailing submatrix */

	    if (kstep == 1) {

/*              1-by-1 pivot block D(k): column k now holds   

                W(k) = L(k)*D(k)   

                where L(k) is the k-th column of L */

		if (k < *n) {

/*                 Perform a rank-1 update of A(k+1:n,k+1:
n) as   

                   A := A - L(k)*D(k)*L(k)' = A - W(k)*(1/
D(k))*W(k)' */

		    r1 = 1. / AP(kc);
		    i__1 = *n - k;
		    d__1 = -r1;
		    dspr(uplo, &i__1, &d__1, &AP(kc + 1), &c__1, &AP(kc + *n 
			    - k + 1));

/*                 Store L(k) in column K */

		    i__1 = *n - k;
		    dscal(&i__1, &r1, &AP(kc + 1), &c__1);
		}
	    } else {

/*              2-by-2 pivot block D(k): columns K and K+1 now
 hold   

                ( W(k) W(k+1) ) = ( L(k) L(k+1) )*D(k)   

                where L(k) and L(k+1) are the k-th and (k+1)-t
h columns   
                of L */

		if (k < *n - 1) {

/*                 Perform a rank-2 update of A(k+2:n,k+2:
n) as   

                   A := A - ( L(k) L(k+1) )*D(k)*( L(k) L(
k+1) )'   
                      = A - ( W(k) W(k+1) )*inv(D(k))*( W(
k) W(k+1) )'   

                   Convert this to two rank-1 updates by u
sing the eigen-   
                   decomposition of D(k) */

		    dlaev2(&AP(kc), &AP(kc + 1), &AP(knc), &r1, &r2, &c, &s);
		    r1 = 1. / r1;
		    r2 = 1. / r2;
		    i__1 = *n - k - 1;
		    drot(&i__1, &AP(kc + 2), &c__1, &AP(knc + 1), &c__1, &c, 
			    &s);
		    i__1 = *n - k - 1;
		    d__1 = -r1;
		    dspr(uplo, &i__1, &d__1, &AP(kc + 2), &c__1, &AP(knc + *
			    n - k));
		    i__1 = *n - k - 1;
		    d__1 = -r2;
		    dspr(uplo, &i__1, &d__1, &AP(knc + 1), &c__1, &AP(knc + *
			    n - k));

/*                 Store L(k) and L(k+1) in columns k and 
k+1 */

		    i__1 = *n - k - 1;
		    dscal(&i__1, &r1, &AP(kc + 2), &c__1);
		    i__1 = *n - k - 1;
		    dscal(&i__1, &r2, &AP(knc + 1), &c__1);
		    i__1 = *n - k - 1;
		    d__1 = -s;
		    drot(&i__1, &AP(kc + 2), &c__1, &AP(knc + 1), &c__1, &c, 
			    &d__1);
		}
	    }
	}

/*        Store details of the interchanges in IPIV */

	if (kstep == 1) {
	    IPIV(k) = kp;
	} else {
	    IPIV(k) = -kp;
	    IPIV(k + 1) = -kp;
	}

/*        Increase K and return to the start of the main loop */

	k += kstep;
	kc = knc + *n - k + 2;
	goto L40;

    }

L70:
    return 0;

/*     End of DSPTRF */

} /* dsptrf_ */

static int dsptri(char *uplo, int *n, double *ap, int * ipiv, double *work, int *info)
{
/*  -- LAPACK routine (version 2.0) --   
       Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,   
       Courant Institute, Argonne National Lab, and Rice University   
       March 31, 1993   


    Purpose   
    =======   

    DSPTRI computes the inverse of a double symmetric indefinite matrix   
    A in packed storage using the factorization A = U*D*U**T or   
    A = L*D*L**T computed by DSPTRF.   

    Arguments   
    =========   

    UPLO    (input) CHARACTER*1   
            Specifies whether the details of the factorization are stored 
  
            as an upper or lower triangular matrix.   
            = 'U':  Upper triangular, form is A = U*D*U**T;   
            = 'L':  Lower triangular, form is A = L*D*L**T.   

    N       (input) INT   
            The order of the matrix A.  N >= 0.   

    AP      (input/output) DOUBLE PRECISION array, dimension (N*(N+1)/2) 
  
            On entry, the block diagonal matrix D and the multipliers   
            used to obtain the factor U or L as computed by DSPTRF,   
            stored as a packed triangular matrix.   

            On exit, if INFO = 0, the (symmetric) inverse of the original 
  
            matrix, stored as a packed triangular matrix. The j-th column 
  
            of inv(A) is stored in the array AP as follows:   
            if UPLO = 'U', AP(i + (j-1)*j/2) = inv(A)(i,j) for 1<=i<=j;   
            if UPLO = 'L',   
               AP(i + (j-1)*(2n-j)/2) = inv(A)(i,j) for j<=i<=n.   

    IPIV    (input) INT array, dimension (N)   
            Details of the interchanges and the block structure of D   
            as determined by DSPTRF.   

    WORK    (workspace) DOUBLE PRECISION array, dimension (N)   

    INFO    (output) INT   
            = 0: successful exit   
            < 0: if INFO = -i, the i-th argument had an illegal value   
            > 0: if INFO = i, D(i,i) = 0; the matrix is singular and its 
  
                 inverse could not be computed.   

    ===================================================================== 
  


       Test the input parameters.   

    
   Parameter adjustments   
       Function Body */
    /* Table of constant values */
    static int c__1 = 1;
    static double c_b11 = -1.;
    static double c_b13 = 0.;
    
    /* System generated locals */
    int i__1;
    double d__1;
    /* Local variables */
    static double temp, akkp1, d;
    static int j, k;
    static double t;
    static int kstep;
    static logical upper;
    static double ak;
    static int kc, kp, kx;
    static int kcnext, kpc, npp;
    static double akp1;



#define WORK(I) work[(I)-1]
#define IPIV(I) ipiv[(I)-1]
#define AP(I) ap[(I)-1]


    *info = 0;
    upper = lsame(uplo, "U");
    if (! upper && ! lsame(uplo, "L")) {
	*info = -1;
    } else if (*n < 0) {
	*info = -2;
    }
    if (*info != 0) {
	i__1 = -(*info);
	xerbla("DSPTRI", &i__1);
	return 0;
    }

/*     Quick return if possible */

    if (*n == 0) {
	return 0;
    }

/*     Check that the diagonal matrix D is nonsingular. */

    if (upper) {

/*        Upper triangular storage: examine D from bottom to top */

	kp = *n * (*n + 1) / 2;
	for (*info = *n; *info >= 1; --(*info)) {
	    if (IPIV(*info) > 0 && AP(kp) == 0.) {
		return 0;
	    }
	    kp -= *info;
/* L10: */
	}
    } else {

/*        Lower triangular storage: examine D from top to bottom. */

	kp = 1;
	i__1 = *n;
	for (*info = 1; *info <= i__1; ++(*info)) {
	    if (IPIV(*info) > 0 && AP(kp) == 0.) {
		return 0;
	    }
	    kp = kp + *n - *info + 1;
/* L20: */
	}
    }
    *info = 0;

    if (upper) {

/*        Compute inv(A) from the factorization A = U*D*U'.   

          K is the main loop index, increasing from 1 to N in steps of
   
          1 or 2, depending on the size of the diagonal blocks. */

	k = 1;
	kc = 1;
L30:

/*        If K > N, exit from loop. */

	if (k > *n) {
	    goto L50;
	}

	kcnext = kc + k;
	if (IPIV(k) > 0) {

/*           1 x 1 diagonal block   

             Invert the diagonal block. */

	    AP(kc + k - 1) = 1. / AP(kc + k - 1);

/*           Compute column K of the inverse. */

	    if (k > 1) {
		i__1 = k - 1;
		dcopy(&i__1, &AP(kc), &c__1, &WORK(1), &c__1);
		i__1 = k - 1;
		dspmv(uplo, &i__1, &c_b11, &AP(1), &WORK(1), &c__1, &c_b13, &
			AP(kc), &c__1);
		i__1 = k - 1;
		AP(kc + k - 1) -= ddot(&i__1, &WORK(1), &c__1, &AP(kc), &
			c__1);
	    }
	    kstep = 1;
	} else {

/*           2 x 2 diagonal block   

             Invert the diagonal block. */

	    t = (d__1 = AP(kcnext + k - 1), abs(d__1));
	    ak = AP(kc + k - 1) / t;
	    akp1 = AP(kcnext + k) / t;
	    akkp1 = AP(kcnext + k - 1) / t;
	    d = t * (ak * akp1 - 1.);
	    AP(kc + k - 1) = akp1 / d;
	    AP(kcnext + k) = ak / d;
	    AP(kcnext + k - 1) = -akkp1 / d;

/*           Compute columns K and K+1 of the inverse. */

	    if (k > 1) {
		i__1 = k - 1;
		dcopy(&i__1, &AP(kc), &c__1, &WORK(1), &c__1);
		i__1 = k - 1;
		dspmv(uplo, &i__1, &c_b11, &AP(1), &WORK(1), &c__1, &c_b13, &
			AP(kc), &c__1);
		i__1 = k - 1;
		AP(kc + k - 1) -= ddot(&i__1, &WORK(1), &c__1, &AP(kc), &
			c__1);
		i__1 = k - 1;
		AP(kcnext + k - 1) -= ddot(&i__1, &AP(kc), &c__1, &AP(kcnext)
			, &c__1);
		i__1 = k - 1;
		dcopy(&i__1, &AP(kcnext), &c__1, &WORK(1), &c__1);
		i__1 = k - 1;
		dspmv(uplo, &i__1, &c_b11, &AP(1), &WORK(1), &c__1, &c_b13, &
			AP(kcnext), &c__1);
		i__1 = k - 1;
		AP(kcnext + k) -= ddot(&i__1, &WORK(1), &c__1, &AP(kcnext), &
			c__1);
	    }
	    kstep = 2;
	    kcnext = kcnext + k + 1;
	}

	kp = (i__1 = IPIV(k), abs(i__1));
	if (kp != k) {

/*           Interchange rows and columns K and KP in the leading 
  
             submatrix A(1:k+1,1:k+1) */

	    kpc = (kp - 1) * kp / 2 + 1;
	    i__1 = kp - 1;
	    dswap(&i__1, &AP(kc), &c__1, &AP(kpc), &c__1);
	    kx = kpc + kp - 1;
	    i__1 = k - 1;
	    for (j = kp + 1; j <= k-1; ++j) {
		kx = kx + j - 1;
		temp = AP(kc + j - 1);
		AP(kc + j - 1) = AP(kx);
		AP(kx) = temp;
/* L40: */
	    }
	    temp = AP(kc + k - 1);
	    AP(kc + k - 1) = AP(kpc + kp - 1);
	    AP(kpc + kp - 1) = temp;
	    if (kstep == 2) {
		temp = AP(kc + k + k - 1);
		AP(kc + k + k - 1) = AP(kc + k + kp - 1);
		AP(kc + k + kp - 1) = temp;
	    }
	}

	k += kstep;
	kc = kcnext;
	goto L30;
L50:

	;
    } else {

/*        Compute inv(A) from the factorization A = L*D*L'.   

          K is the main loop index, increasing from 1 to N in steps of
   
          1 or 2, depending on the size of the diagonal blocks. */

	npp = *n * (*n + 1) / 2;
	k = *n;
	kc = npp;
L60:

/*        If K < 1, exit from loop. */

	if (k < 1) {
	    goto L80;
	}

	kcnext = kc - (*n - k + 2);
	if (IPIV(k) > 0) {

/*           1 x 1 diagonal block   

             Invert the diagonal block. */

	    AP(kc) = 1. / AP(kc);

/*           Compute column K of the inverse. */

	    if (k < *n) {
		i__1 = *n - k;
		dcopy(&i__1, &AP(kc + 1), &c__1, &WORK(1), &c__1);
		i__1 = *n - k;
		dspmv(uplo, &i__1, &c_b11, &AP(kc + *n - k + 1), &WORK(1), &
			c__1, &c_b13, &AP(kc + 1), &c__1);
		i__1 = *n - k;
		AP(kc) -= ddot(&i__1, &WORK(1), &c__1, &AP(kc + 1), &c__1);
	    }
	    kstep = 1;
	} else {

/*           2 x 2 diagonal block   

             Invert the diagonal block. */

	    t = (d__1 = AP(kcnext + 1), abs(d__1));
	    ak = AP(kcnext) / t;
	    akp1 = AP(kc) / t;
	    akkp1 = AP(kcnext + 1) / t;
	    d = t * (ak * akp1 - 1.);
	    AP(kcnext) = akp1 / d;
	    AP(kc) = ak / d;
	    AP(kcnext + 1) = -akkp1 / d;

/*           Compute columns K-1 and K of the inverse. */

	    if (k < *n) {
		i__1 = *n - k;
		dcopy(&i__1, &AP(kc + 1), &c__1, &WORK(1), &c__1);
		i__1 = *n - k;
		dspmv(uplo, &i__1, &c_b11, &AP(kc + (*n - k + 1)), &WORK(1), 
			&c__1, &c_b13, &AP(kc + 1), &c__1);
		i__1 = *n - k;
		AP(kc) -= ddot(&i__1, &WORK(1), &c__1, &AP(kc + 1), &c__1);
		i__1 = *n - k;
		AP(kcnext + 1) -= ddot(&i__1, &AP(kc + 1), &c__1, &AP(kcnext 
			+ 2), &c__1);
		i__1 = *n - k;
		dcopy(&i__1, &AP(kcnext + 2), &c__1, &WORK(1), &c__1);
		i__1 = *n - k;
		dspmv(uplo, &i__1, &c_b11, &AP(kc + (*n - k + 1)), &WORK(1), 
			&c__1, &c_b13, &AP(kcnext + 2), &c__1);
		i__1 = *n - k;
		AP(kcnext) -= ddot(&i__1, &WORK(1), &c__1, &AP(kcnext + 2), &
			c__1);
	    }
	    kstep = 2;
	    kcnext -= *n - k + 3;
	}

	kp = (i__1 = IPIV(k), abs(i__1));
	if (kp != k) {

/*           Interchange rows and columns K and KP in the trailing
   
             submatrix A(k-1:n,k-1:n) */

	    kpc = npp - (*n - kp + 1) * (*n - kp + 2) / 2 + 1;
	    if (kp < *n) {
		i__1 = *n - kp;
		dswap(&i__1, &AP(kc + kp - k + 1), &c__1, &AP(kpc + 1), &c__1);
	    }
	    kx = kc + kp - k;
	    i__1 = kp - 1;
	    for (j = k + 1; j <= kp-1; ++j) {
		kx = kx + *n - j + 1;
		temp = AP(kc + j - k);
		AP(kc + j - k) = AP(kx);
		AP(kx) = temp;
/* L70: */
	    }
	    temp = AP(kc);
	    AP(kc) = AP(kpc);
	    AP(kpc) = temp;
	    if (kstep == 2) {
		temp = AP(kc - *n + k - 1);
		AP(kc - *n + k - 1) = AP(kc - *n + kp - 1);
		AP(kc - *n + kp - 1) = temp;
	    }
	}

	k -= kstep;
	kc = kcnext;
	goto L60;
L80:
	;
    }

    return 0;

/*     End of DSPTRI */

} /* dsptri_ */




