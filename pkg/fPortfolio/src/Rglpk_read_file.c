/* These are the interface functions to GLPK's MPS reader/writer
 */

#include "Rglpk.h"
#include <stdio.h>

// read in all necessary elements for retrieving the LP/MILP
void Rglpk_read_file (char **file, int *type, 
		      int *lp_direction_of_optimization,
		      int *lp_n_constraints, int *lp_n_objective_vars,
		      int *lp_n_values_in_constraint_matrix,
		      int *lp_n_integer_vars, int *lp_n_binary_vars,
		      int *lp_verbosity) {

  int status;
  glp_prob *lp;
  
  // Turn on/off Terminal Output
  if (*lp_verbosity==1)
    glp_term_out(GLP_ON);
  else
    glp_term_out(GLP_OFF);

  // create problem object 
  lp = glp_create_prob();

  // read file -> gets stored as an GLPK problem object 'lp'
  // which file type do we have?
  switch (*type){
  case 1: 
    // Fixed (ancient) MPS Format, param argument currently NULL
    status = glp_read_mps(lp, GLP_MPS_DECK, NULL, *file);
    break;
  case 2:
    // Free (modern) MPS format, param argument currently NULL
    status = glp_read_mps(lp, GLP_MPS_FILE, NULL, *file);
    break;
  case 3:
    // CPLEX LP Format
    status = glp_read_lp(lp, NULL, *file);
    break;
  }

  // if file read successfully glp_read_* returns zero
  if ( status != 0 ) {
    error("Reading file %s failed", *file);
  }

  // retrieve optimization direction flag
  *lp_direction_of_optimization = glp_get_obj_dir(lp);  

  // retrieve number of constraints
  *lp_n_constraints = glp_get_num_rows(lp);  

  // retrieve number of objective variables
  *lp_n_objective_vars = glp_get_num_cols(lp);

  // retrieve number of non-zero elements in constraint matrix
  *lp_n_values_in_constraint_matrix = glp_get_num_nz(lp);

  // retrieve number of integer variables
  *lp_n_integer_vars = glp_get_num_int(lp);
  
  // retrieve number of binary variables
  *lp_n_binary_vars = glp_get_num_bin(lp);
  
  // delete problem object
  glp_delete_prob(lp);
}

// retrieve all missing values of LP/MILP
void Rglpk_retrieve_MP_from_file (char **file, int *type,
				  int *lp_n_constraints,
				  int *lp_n_objective_vars,
				  double *lp_objective_coefficients,
				  int *lp_constraint_matrix_i,
				  int *lp_constraint_matrix_j,
				  double *lp_constraint_matrix_values,
				  int *lp_direction_of_constraints,
				  double *lp_right_hand_side,
				  int *lp_objective_var_is_integer,
				  int *lp_objective_var_is_binary,
				  int *lp_bounds_type,
				  double *lp_bounds_lower,
				  double *lp_bounds_upper,
				  int *lp_ignore_first_row,
				  int *lp_verbosity) {
  glp_prob *lp;
  int i, j, lp_column_kind, tmp;
  int ind_offset, status;

  // Turn on/off Terminal Output
  if (*lp_verbosity==1)
    glp_term_out(GLP_ON);
  else
    glp_term_out(GLP_OFF);

  // create problem object 
  lp = glp_create_prob();

  // read file -> gets stored as an GLPK problem object 'lp'
  // which file type do we have?
  switch (*type){
  case 1: 
    // Fixed (ancient) MPS Format, param argument currently NULL
    status = glp_read_mps(lp, GLP_MPS_DECK, NULL, *file);
    break;
  case 2:
    // Free (modern) MPS format, param argument currently NULL
    status = glp_read_mps(lp, GLP_MPS_FILE, NULL, *file);
    break;
  case 3:
    // CPLEX LP Format
    status = glp_read_lp(lp, NULL, *file);
    break;
  }

  // if file read successfully glp_read_* returns zero
  if ( status != 0 ) {
    error("Reading file %c failed", *file);
  }
  
  // retrieve column specific data (values, bounds and type)
  for (i = 0; i < *lp_n_objective_vars; i++) {
    lp_objective_coefficients[i] = glp_get_obj_coef(lp, i+1);
    lp_bounds_type[i]            = glp_get_col_type(lp, i+1);
    lp_bounds_lower[i]           = glp_get_col_lb  (lp, i+1);
    lp_bounds_upper[i]           = glp_get_col_ub  (lp, i+1);
    lp_column_kind               = glp_get_col_kind(lp, i+1);
    // set to TRUE if objective variable is integer or binary  
    switch (lp_column_kind){
    case GLP_IV: 
      lp_objective_var_is_integer[i] = 1;
      break;
    case GLP_BV:
      lp_objective_var_is_binary[i] = 1;
      break;
    }
  }
  
  ind_offset = 0;
  // retrieve row specific data (right hand side, direction of constraints)
  for (i = *lp_ignore_first_row; i < *lp_n_constraints; i++) {
    lp_direction_of_constraints[i] = glp_get_row_type(lp, i+1);

    // the right hand side. Note we don't allow for double bounded or
    // free auxiliary variables 
    if( lp_direction_of_constraints[i] == GLP_LO )
      lp_right_hand_side[i] = glp_get_row_lb(lp, i+1);
    if( lp_direction_of_constraints[i] == GLP_UP )
      lp_right_hand_side[i] = glp_get_row_ub(lp, i+1);
    if( lp_direction_of_constraints[i] == GLP_FX )
      lp_right_hand_side[i] = glp_get_row_lb(lp, i+1);
    
    tmp = glp_get_mat_row(lp, i+1, &lp_constraint_matrix_j[ind_offset-1],
			           &lp_constraint_matrix_values[ind_offset-1]);
    if (tmp > 0)
      for (j = 0; j < tmp; j++)
	lp_constraint_matrix_i[ind_offset+j] = i+1;
	ind_offset += tmp; 
  }
  
  // delete problem object
  glp_delete_prob(lp);
}

