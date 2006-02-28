

/**
 * Copyright (C) 2004 Harald SCHMIDBAUER - Vehbi Sinan TUNALIOGLU
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *
 * A copy of the GNU General Public License is available via WWW at
 * http://www.gnu.org/copyleft/gpl.html.  You can also obtain it by
 * writing to the Free Software Foundation, Inc., 59 Temple Place,
 * Suite 330, Boston, MA  02111-1307  USA.
 */

#include <stdio.h>
#include <stdlib.h>
#include <math.h>

#ifndef MATRIXLIB
#define MATRIXLIB

/**
 * FUNCTION DEFINITIONS
 */

/* a macro for getting the maximum value of two given values */
#define maxval(p,q)				(p > q ? p : q)

/* memory allocation routine for a given type. quantity to be allocated is 1 */
#define NEW(type)				((type *) calloc(1, sizeof(type)))

/* memory allocation routine for a given type. quantity to be allocated is user specified*/
#define NEW_A(quantity,type)	((type *) calloc((unsigned int)(quantity), sizeof(type)))

/**
 * set value of a matrix at the given row and column.
 * also check the boundries.
 */
#define m_set_val(A, i, j, val)							\
						((A)->me[i][j] = (				\
						(i) < 0 ||						\
						(j) < 0 ||						\
						(i) >= (A)->rows ||				\
						(j) >= (A)->cols ?				\
						err_exit(ERR_BOUNDS, "m_set_val"), 0.0 : (val)))

/**
 * set the m_foutput's file pointer to the stdout
 */
#define m_output(a)		(m_foutput(stdout, a))


/**
 * ERROR CODE DEFINITIONS
 */
#define ERR_DIMNEG				"Dimensions should be greater then 0"
#define ERR_MEM					"Cannot allocate memory for matrix initialization"
#define ERR_BOUNDS				"Setting value out of boundries"
#define ERR_SRCNULL				"Source matrix cannot be NULL"
#define ERR_DESTNULL			"Destination matrix should be pre-initialized"
#define ERR_DIFFDIMS			"Matrices differ in dimensions"
#define ERR_OPSNULL				"Operands cannot be NULL"
#define ERR_RESNULL				"Result matrix should be pre-initialized"
#define ERR_TRANSPDIFFDIMS		"Cannot transpose matrix. Dimensions don't match."
#define ERR_OPDIFFDIMS			"Cannot execute the operation. Dimensions don't match."
#define ERR_OPRESSAME			"Operands and result cannot be same"


/**
 * STRUCTURE DEFINITIONS
 */
typedef	struct
{
	unsigned int rows;	/* number of rows */
	unsigned int cols;	/* number of columns */
	double **me;		/* an array of dimension 'rows' by 'cols' */
} MAT;

int err_exit(char * err_message, char * func_name);
void mem_copy(int * source, int * destination, unsigned int length);
void m_transp(MAT * in, MAT * out);
void m_foutput(FILE * fp, MAT * a);
MAT * m_get(int m, int n);
void m_copy(MAT * source, MAT * destination);
void m_add(MAT * mat1, MAT * mat2, MAT * result);
void m_mlt(MAT * A, MAT * B, MAT * OUT);
#endif


/* ************************************************************************** */


/**
 * Copyright (C) 2004 Harald SCHMIDBAUER - Vehbi Sinan TUNALIOGLU
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *
 * A copy of the GNU General Public License is available via WWW at
 * http://www.gnu.org/copyleft/gpl.html.  You can also obtain it by
 * writing to the Free Software Foundation, Inc., 59 Temple Place,
 * Suite 330, Boston, MA  02111-1307  USA.
 */

#include <stdio.h>
#include <math.h>
/* #include "matrixlib.h" */

#define VERYBIGNUMBER	1.0e+10
#define ERROR			1.0e+10

#define maxval(p,q)  (p > q ? p : q)

void loglikelihood(
					double *params,			/* the parameter list*/
					int    *fixed_indexs,	/* index list for fixed parameters	*/
					double *fixed_values,	/* value list for fixed parameters */
					int    *fixed_length,	/* length fixed parameters */
					double *series_1,		/* first series	*/
					double *series_2,		/* second series */
					int    *series_length,	/* second series */
					int    *switcher,		/* the switcher array for BEKK's p and q */
					double *returnval		/* value to be returned at the end */
				)
{
	/**
	 * VARIABLE DECLARATIONS
	 */
		
	/* Paramter Matrices */
	MAT *C = NULL;					/* declare the C parameter matrix*/
	MAT *C_t = NULL;				/* declare the transposed C parameter matrix*/
	MAT *C_term = NULL;				/* C'C term */
	MAT **A = NULL, **G = NULL;		/* declare the arch and garch parameter matrix arrays*/
	MAT **A_t = NULL, **G_t = NULL;	/* declare the transposed arch and garch parameter matrix arrays*/

	/* H Matrices */
	MAT *H = NULL;					/* H term */
	MAT **HOLD = NULL;				/* an array holding former H terms */

	/* temporary matrices used for calculations */
	MAT *T0 = NULL;							/* 1x1 matrices */
	MAT *T1 = NULL, *T7 =NULL;				/* 2x1 matrices */
	MAT *T3 = NULL, *T4 = NULL, *T5 = NULL;	/* 2x2 matrices */
	MAT *T6 = NULL, *T2 = NULL, *T8 = NULL;	/* 1x2 matrices */		

	int count, counttemp;			/* will be used for loop counter */
	double temp, buffer;			/* will be used for temporary calculations */
	double detcomp;					/* the inner part of sqrt() function for determinant calculation */
		
	double * buffer_params;
	int shifted;
	int temp_length;
	int check_point;
		
	/**
	 * TODO
	 * check all the function arguments
	 */
	
	/**
	 * TODO
	 * check the model specification,
	 * whether p or q can be `0' or not
	 *
	 * check the switcher values of BEKK model.
	 * these are p and q values of BEKK(p,q,K) model.
	 * These can not be less then 0
	 */
	
	if(switcher[0] < 0)	/* check p switch */
	{
		printf("ERROR! invalid p switch. ( p = %d )\n", switcher[0]);
		*returnval = ERROR;
		return;
	}
	
	if(switcher[1] < 0)	/* check q switch */
	{
		printf("ERROR! invalid q switch. ( q = %d )\n", switcher[1]);
		*returnval = ERROR;
		return;
	}	
	/**
	 * initialize the variables
	 */

	/* memory allocation for the parameter matrix arrays */
	A   = (MAT **) malloc(switcher[1] * sizeof(MAT *));  
	A_t = (MAT **) malloc(switcher[1] * sizeof(MAT *));  
	G   = (MAT **) malloc(switcher[0] * sizeof(MAT *));  
	G_t = (MAT **) malloc(switcher[0] * sizeof(MAT *));  

	/* memory allocation for the array that holds the previous H terms */
	HOLD   = (MAT **) malloc(switcher[0] * sizeof(MAT *));  
	
	/**
	 * ATTENTION!
	 * HERE, WE WILL INSERT THE FIXED PARAMETERS
	 */

	if(*fixed_length)
	{
		shifted = 0;
		temp_length = (3 + 4 * switcher[0] + 4 * switcher[1]);
		buffer_params = (double *) malloc(temp_length * sizeof(double));
		
		for(count = 0; count < temp_length; count++)
		{
			check_point = 0;
			for(counttemp = 0; counttemp < *fixed_length; counttemp++)
			{
				if(count == (fixed_indexs[counttemp] - 1))
				{
					check_point = 1;
					shifted++;
					buffer_params[count] = fixed_values[counttemp];
					break;
				}
			}
			if(check_point == 0)
			{
				buffer_params[count] = params[count - shifted];
			}
		}

		params = buffer_params; /* copy the modified parameter array into params*/
	}
	
	/* initialize the C parameter matrix*/
	C = m_get(2, 2);				/* make a 2 by 2 matrix */
	m_set_val(C, 0, 0, params[0]);
	m_set_val(C, 1, 0, 0);
	m_set_val(C, 0, 1, params[1]);
	m_set_val(C, 1, 1, params[2]);

	/* initialize the transposed C parameter matrix*/
	C_t = m_get(2, 2);				/* make a 2 by 2 matrix */
	m_transp(C, C_t);				/* transpose the matrix */
	
	/* initialize the C term by multiplying the C and C_t matrices */
	C_term = m_get(2, 2);			/* make a 2 by 2 matrix */
	m_mlt(C_t, C, C_term);			/* initialize the C'C term */
	
	/* We have the C term. Now, do the critical A and G parameter matrices allocation*/
	for(count = 0; count < switcher[1]; count++)
	{
		A[count] = m_get(2, 2);				/* make a 2 by 2 matrix */
		m_set_val(A[count], 0, 0, params[3 + (4 * count)]);
		m_set_val(A[count], 1, 0, params[4 + (4 * count)]);
		m_set_val(A[count], 0, 1, params[5 + (4 * count)]);
		m_set_val(A[count], 1, 1, params[6 + (4 * count)]);

		A_t[count] = m_get(2, 2);			/* make a 2 by 2 matrix */
		m_transp(A[count], A_t[count]);		/* transpose the matrix */
	}
	
	counttemp = count;	/* keep the counter where we left in the params matrix */
	
	for(count = 0; count < switcher[0]; count++)
	{
		G[count] = m_get(2, 2);				/* make a 2 by 2 matrix */
		m_set_val(G[count], 0, 0, params[3 + (4 * count) + (4 * counttemp)]);
		m_set_val(G[count], 1, 0, params[4 + (4 * count) + (4 * counttemp)]);
		m_set_val(G[count], 0, 1, params[5 + (4 * count) + (4 * counttemp)]);
		m_set_val(G[count], 1, 1, params[6 + (4 * count) + (4 * counttemp)]);

		G_t[count] = m_get(2, 2);			/* make a 2 by 2 matrix */
		m_transp(G[count], G_t[count]);		/* transpose the matrix */
	}
	
	/* initialize the H matrix. Then, do the critical buffer HOLD array for previous H terms */
	H = m_get(2, 2);				/* make a 2 by 2 matrix */
	m_set_val(H, 0, 0, 1);
	m_set_val(H, 0, 1, 0);
	m_set_val(H, 1, 0, 0);
	m_set_val(H, 1, 1, 1);
	
	for(count = 0; count < switcher[0]; count++)
	{
		HOLD[count] = m_get(2, 2);				/* make a 2 by 2 matrix */
		m_set_val(HOLD[count], 0, 0, 1);
		m_set_val(HOLD[count], 1, 0, 0);
		m_set_val(HOLD[count], 0, 1, 0);
		m_set_val(HOLD[count], 1, 1, 1);
	}
	
	/* initialize the temporary matrices */
	T0 = m_get(1, 1);				/* make a 1 by 1 matrix */
	
	T1 = m_get(2, 1);				/* make a 2 by 1 matrix */
	T2 = m_get(1, 2);				/* make a 1 by 2 matrix */
	
	T3 = m_get(2, 2);				/* make a 2 by 2 matrix */
	T4 = m_get(2, 2);				/* make a 2 by 2 matrix */
	T5 = m_get(2, 2);				/* make a 2 by 2 matrix */
	
	T6 = m_get(1, 2);				/* make a 1 by 2 matrix */
	T7 = m_get(2, 1);				/* make a 2 by 1 matrix */
	T8 = m_get(1, 2);				/* make a 1 by 2 matrix */
	
	/**
	 * now doing the calculations
	 */

	*returnval = 0.0;					/* reset the return value */
	/**
	 * ATTENTION:
	 * we will now set the counter to its initial value.
	 * it is the max of the two switchers
	 * count = max(p,q)
	 */

	
	count = maxval(switcher[0], switcher[1]);
	while(count < *series_length) /* begin the loop */
	{
		/**
		 * CRITICAL!
		 * shift the H TERMS to previous HOLD items.
		 * HOLD[1] becomes H, HOLD[2] becomes HOLD[1] and so on
		 */
	
		for(counttemp = switcher[0] - 1; counttemp > 0; counttemp--)
		{
			m_copy(HOLD[counttemp - 1], HOLD[counttemp]);
		}

		/**
		 * apply if GARCH parameter is greater then zero
		 */
		if(switcher[0] > 0)
		{
			m_copy(H, HOLD[0]);
		}

		/**
		 * a bit complicated but following explanation will be useful hopefully
		 * H = (C')x(C) + (A')(E_t-1)(E_t-1')(A) + (B')(E_t-2)(E_t-2')(B) + ... +  (G')(H_t-1)(G) + (L')(H_t-2)(L) + ... 
		 *                    |_____________|          |_____________|             |____________|   |____________| |_____|
		 *                        E1 TERM                  E2 TERM                     G1 TERM         G2 TERM     G3.G4..
		 *                |____________________|   |____________________| |_____|
		 *                        A1 TERM                  A2 TERM        A3.A4..
		 *     |______|  |_____________________________________________________|  |______________________________________|  
		 *      C TERM                         A TERM                                              G TERM
		 */

		m_copy(C_term, H);

		/**
		 * first, do the A TERM part...
		 */
		for(counttemp = 0; counttemp < switcher[1]; counttemp++)
		{
			/**
			 * calculate the (E_t-x %*% E_t-x') term
			 */
			
			m_set_val(T1, 0, 0, series_1[count - 1 - counttemp]);
			m_set_val(T1, 1, 0, series_2[count - 1 - counttemp]);
			m_transp(T1, T2);
			m_mlt(T1, T2, T3); /* we got the term in T3*/

			/**
			 * calculate the (A_x' %*% E_t-x %*% E_t-x' %*% A_x) term
			 */
			
			m_mlt(A_t[counttemp], T3, T4);
			m_mlt(T4, A[counttemp], T3); /* we got the term in T3 */

			/**
			 * accumulate the T3 term in H
			 */

			m_add(H, T3, T4); /* addition completed, but assign the T4 back to H */
			m_copy(T4, H);
		}

		/**
		 * A term calculated and accumulated to H term.
		 * Continue with the G term
		 */
		
		for(counttemp = 0; counttemp < switcher[0]; counttemp++)
		{
			/**
			 * calculate the (G_x' %*% H_x %*% G_x) term
			 */
			
			m_mlt(G_t[counttemp], HOLD[counttemp], T4);
			m_mlt(T4, G[counttemp], T3); /* we got the term in T3 */

			/**
			 * accumulate the T3 term in H
			 */

			m_add(H, T3, T4); /* addition completed, but assign the T4 back to H */
			m_copy(T4, H);
		}

		/**
		 * we got the H TERM.
		 *
		 * CRITICAL PART
		 * multivariate density normal distribution (???)
		 *
		 * MULTIVARIATE NORMAL DENSITY FUNCTION
		 *
		 * f(x) = (1 / (2*pi)^(n/2) * |E|^(1/2)) * (e^( -(x - m)' * E^(-1) * (x - m) / 2))
		 *
		 * MULTIVARIATE LOGNORMAL DENSITY FUNCTION
		 * 
		 * log[f(x)] = - log[(2 * pi)^(n/2) * (1 / sqrt(|E|))] - (x - m)' * E^(-1) * (x - m) / 2
		 *                                                       |____________ ___________|
		 *                                                                    V
		 *                                                        this will be stored in temp
		 * 
		 */
	
		/**
		 * CALCULATING TEMP VALUE
		 */	
		temp = 0;	/* reset the temp variable */
		
		/**
		 * CRITICAL!
		 * check whether the det(H) is 0 or not.
		 * if it is 0, that means that the H term is not invertable.
		 * if it is not invertable, we will add following matrix to the 
		 * H term: (CHANGED: WE ONLY MULTIPLY THE H[0,0] WITH 1.01)
		 * 
		 * NEW_H_TERM = H * | 0.01  0.00 |
		 *                  | 0.00  0.00 |
		 */
		detcomp = H->me[0][0] * H->me[1][1] - H->me[0][1] * H->me[1][0];
		
		/**
		 * DEBUG
		 * printf("det(H)² = %f\n", detcomp);
		 */
		
		if( detcomp == 0 )
		{
			/**
			 * H is not invertable
			 */
			printf("H IS SINGULAR!...\n");
			
			H->me[0][0] *= 1.01;
			detcomp = H->me[0][0] * H->me[1][1] - H->me[0][1] * H->me[1][0];
		}
		else if(detcomp < 0)
		{
			/**
			 * it should not be negative... else sqrt(detcomp) = undefined...
			 */
			*returnval = VERYBIGNUMBER;
			return;
		}
		

		/**
		 * calculate  E^(-1)
		 * TODO
		 * that doesn't work sometimes. Thus we do it by hand:
		 * m_pow(H, -1, T3);
		 */
		
		m_set_val(T3, 0, 0, H->me[1][1] / detcomp);
		m_set_val(T3, 0, 1, -1 * H->me[0][1] / detcomp);
		m_set_val(T3, 1, 0, -1 * H->me[1][0] / detcomp);
		m_set_val(T3, 1, 1, H->me[0][0] / detcomp);
		
		
		m_set_val(T1, 0, 0, series_1[count]); /* initializing (x - m) term (since m = 0; x)*/
		m_set_val(T1, 1, 0, series_2[count]); /* initializing (x - m) term (since m = 0; x)*/				
		
		m_transp(T1, T8);

		m_mlt(T3, T1, T7);
		m_mlt(T8, T7, T0);
		
		temp = T0->me[0][0];

		buffer = (-1) * log(2 * M_PI * sqrt(detcomp)) - (temp / 2);
		*returnval += buffer;
		
		count++;					/* next period */
	}

	*returnval *= -1; 
	if(isnan(*returnval))
	{
		*returnval = VERYBIGNUMBER;
	}
	/**
	 * DEBUG
	 * printf("\nreturnval : %f\n", *returnval);
	 */

}



/* ************************************************************************** */


/**
 * Copyright (C) 2004 Harald SCHMIDBAUER - Vehbi Sinan TUNALIOGLU
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *
 * A copy of the GNU General Public License is available via WWW at
 * http://www.gnu.org/copyleft/gpl.html.  You can also obtain it by
 * writing to the Free Software Foundation, Inc., 59 Temple Place,
 * Suite 330, Boston, MA  02111-1307  USA.
 */

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
/* #include "matrixlib.h" */

/**
 * functions to be written:
 * !m_get(int rows, int cols);
 * !m_set_val(MAT * matrix_to_be_filled, int row_number, int col_number, double value);
 * m_transp(MAT * matrix_to_be_transposed);
 * m_mlt(MAT * first, MAT * second, MAT * result);
 * !m_copy(MAT * source, MAT * destination);
 * !m_add(MAT * first, MAT * second, MAT * result);
 */

int err_exit(char * err_message, char * func_name)
{
	printf("\nERROR in matrixlib:\n\t %s", err_message);
	printf("\n(in function %s)", func_name);
	printf("\nExiting program\n");
	exit(0);
}

void mem_copy(int * source, int * destination, unsigned int length)
{
	int i;
	if(source < destination)
	{
		for(i = 0; i < length; i++)
		{
			*(destination++) = *(source++);
		}
	}
	else
	{
		source += length;
		destination += length;

		for(i = 0; i < length; i++)
		{
			*(--destination) = *(--source);
		}
	}
	return;
}

void m_transp(MAT * in, MAT * out)
{
	int	i, j;
	int	same;
	double tmp;

	if(in == NULL)
	{
		err_exit(ERR_SRCNULL, "m_transp");
	}

	if(out == NULL)
	{
		err_exit(ERR_DESTNULL, "m_transp");
	}
	
	same = ( in == out );
	
	if(same && in->rows != in->cols)
	{
		err_exit(ERR_TRANSPDIFFDIMS, "m_transp");
	}
	
	if(out->rows != in->cols || out->cols != in->rows)
	{
		err_exit(ERR_TRANSPDIFFDIMS, "m_transp");
	}

	if(!same)
	{
		for(i = 0; i < in->rows; i++)
		{
			for(j = 0; j < in->cols; j++)
			{
				out->me[j][i] = in->me[i][j];
			}
		}
	}
	else
	{
		for(i = 1; i < in->rows; i++)
		{
			for (j = 0; j < i; j++)
			{	tmp = in->me[i][j];
				in->me[i][j] = in->me[j][i];
				in->me[j][i] = tmp;
			}
		}
	}

	return;
}

void m_foutput(FILE * fp, MAT * a)
{
	unsigned int i, j, tmp;
	
	if(a == NULL) /* check whether the matrix exist or not*/
	{
		fprintf(fp, "MATRIX: NULL\n");
		return;
	}
	
	fprintf(fp, "Matrix: %d by %d\n", a->rows, a->cols);
	
	if(a->me == NULL) /* check the content array */
	{
		fprintf(fp, "NULL\n");
	}

	for(i = 0; i < a->rows; i++) /* for each row */
	{
		fprintf(fp, "row %u: ", i);
		for(j = 0, tmp = 2; j < a->cols; j++, tmp++) /* for each column */
		{
			fprintf(fp, "%f ", a->me[i][j]);
			if(!(tmp % 5))
			{
				putc('\n', fp);
			}
		}
		if((tmp % 5) != 1)
		{
			putc('\n', fp);
		}
	}
}

MAT * m_get(int m, int n)
{
	MAT * matrix;	/* matrix to be returned */
	int	i;			/* temporary variable */
	
	/* check the dimensions, both should be equal or greater then zero */
	if(m < 0 || n < 0)
	{
		err_exit(ERR_DIMNEG, "m_get");
	}

	/* atempt to allocate memory for the matrix */
	matrix = NEW(MAT);
	if(matrix == (MAT *) NULL)
	{
		err_exit(ERR_MEM, "m_get");
	}
	
	matrix->rows = m;
	matrix->cols = n;

	matrix->me = NEW_A(m, double*);
	if(matrix->me == NULL)
	{
		free(matrix);
		err_exit(ERR_MEM, "m_get");
	}
	
	for(i = 0; i < m; i++)
	{
		matrix->me[i] = NEW_A(n, double);
		if(matrix->me[i] == NULL)
		{
			free(matrix->me);
			free(matrix);
			err_exit(ERR_MEM, "m_get");
		}
	}
	return matrix;
}

void m_copy(MAT * source, MAT * destination)
{
	unsigned int i, j;
	
	if(source == NULL) /* source cannot be NULL */
	{
		err_exit(ERR_SRCNULL, "m_copy");
	}

	if(destination == NULL) /* destination should be initialized */
	{
		err_exit(ERR_DESTNULL, "m_copy");
	}

	if(
		(source->cols != destination->cols) || 
		(source->rows != destination->cols)) /* not same-dimensioned matrices */
	{
		err_exit(ERR_DIFFDIMS, "m_copy");
	}
	
	if(source == destination) /* nothing to be done */
	{
		return;
	}

	for( i = 0; i < source->rows; i++)
	{
		/**
		 * TODO 
		 * THERE IS A SIGNIFICANT BUG IN MEM_COPY FUNCTION, OR JUST HERE IN THE BELOW LINE
		 * mem_copy((int *)&(source->me[i][0]), (int *)&(destination->me[i][0]), source->cols * sizeof(double));
		 */
		for(j = 0; j < source->cols; j++)
		{
			destination->me[i][j] = source->me[i][j];
		}
	}

	return;
}

void m_add(MAT * mat1, MAT * mat2, MAT * result)
{
	unsigned int i, j;
	
	if(mat1 == NULL || mat2 == NULL) /* operands cannot be NULL */
	{
		err_exit(ERR_OPSNULL, "m_add");
	}

	if(result == NULL) /* result should be initialized */
	{
		err_exit(ERR_RESNULL, "m_add");
	}

	if(
		(mat1->cols != mat2->cols) || 
		(mat1->cols != result->cols) || 
		(mat1->rows != mat2->cols) || 
		(mat1->rows != result->rows)) /* not same-dimensioned matrices */
	{
		err_exit(ERR_DIFFDIMS, "m_add");
	}

	/**
	 * TODO
	 * Think about whether the values of the operands would be overwritten
	 * or not if we allow that the result is the same matrix of one of the
	 * operands. It seems ok, since the mathematical operations like 
	 * "a = a + b" are ok.
	 */

	for( i = 0; i < mat1->rows; i++)
	{
		for(j = 0; j < mat1->cols; j++)
		{
			result->me[i][j] = mat1->me[i][j] + mat2->me[i][j];
		}
	}

	return;
}

void m_mlt(MAT * A, MAT * B, MAT * OUT)
{
	unsigned int i, j, k, m, n, p;
	double **A_v, **B_v, sum;

	if(A == NULL || B == NULL)
	{
		err_exit(ERR_OPSNULL, "m_mlt");
	}
	
	if(OUT == NULL)
	{
		err_exit(ERR_RESNULL, "m_mlt");
	}
	
 	if(A->cols != B->rows)
	{
		/**
		 * DEBUG
		 * printf("A->cols = %d\n", A->cols);
		 * printf("B->rows = %d\n", B->rows);
		 */
		err_exit(ERR_OPDIFFDIMS, "m_mlt");
	}
	
	if(A == OUT || B == OUT)
	{
		err_exit(ERR_OPRESSAME, "m_mlt");
	}
	
	m = A->rows;
	n = A->cols;
	p = B->cols;
	
	A_v = A->me;
	B_v = B->me;

	if(OUT->rows != A->rows || OUT->cols != B->cols)
	{
		/**
		 * DEBUG
		 * printf("A->rows = %d, B->cols = %d,\n OUT->rows = %d,\n OUT->cols = %d\n", A->rows, B->cols, OUT->rows, OUT->cols);
		 */
		err_exit(ERR_OPDIFFDIMS, "m_mlt");
	}

	for(i = 0; i < m; i++)
	{
		for(j = 0; j < p; j++)
		{
			sum = 0.0;
			for(k = 0; k < n; k++)
			{
				sum += A_v[i][k] * B_v[k][j];
			}
			OUT->me[i][j] = sum;
		}
	}
	
	return;
}
