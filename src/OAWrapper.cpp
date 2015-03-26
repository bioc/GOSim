#include "hungarian2.h"
#include <stdlib.h>
//#include <R_ext/Utils.h>
//#include <R.h>
//#include <Rdefines.h>
#include <R_ext/Rdynload.h>
#include <vector>
#include <string>
#include <map>
#include <Rcpp.h>


RcppExport SEXP OAWrapper(SEXP Rpre, SEXP mod);
	

RcppExport SEXP OAWrapper(SEXP Rpre, SEXP mod){
	hungarian_problem_t prob;		
	int i, j;
	Rcpp::NumericMatrix M(Rpre);
	double** M2 = (double**) R_alloc(M.nrow(), sizeof(double*));
	double maxweight = -999999;
	for(i = 0; i < M.nrow(); i++){
		M2[i] = (double*) R_alloc(M.ncol(), sizeof(double));
		for(j = 0; j < M.ncol(); j++){
			M2[i][j] = M(i,j);
			if(M(i,j) > maxweight)
				maxweight = M(i, j);
		}	
	}			
	int mode = (INTEGER(mod)[0] == 1)? HUNGARIAN_MODE_MAXIMIZE_UTIL: HUNGARIAN_MODE_MINIMIZE_COST;
	int dim = hungarian_init(&prob, M2, M.nrow(), M.ncol(), mode);
  	double cost=hungarian_solve(&prob); 	
  	if(mode == HUNGARIAN_MODE_MAXIMIZE_UTIL)	
		cost = dim * maxweight - cost;
  	Rcpp::NumericMatrix assignment(prob.num_rows, prob.num_cols);
  	for(i = 0; i < prob.num_rows; i++){
  	  	for(j = 0; j < prob.num_cols; j++)
  	  		assignment(i, j) = (double)prob.assignment[i][j];
	}

  	hungarian_free(&prob);
  	return Rcpp::List::create(Rcpp::Named("score", cost), Rcpp::Named("assignment", assignment));
}


extern "C"{
R_CallMethodDef callMethods[]  = {
  {"OAWrapper", (DL_FUNC) &OAWrapper, 2},
  {NULL, NULL, 0}
};

void
R_init_GOSim(DllInfo *info)
{
   R_registerRoutines(info, NULL, callMethods, NULL, NULL);
}
}
