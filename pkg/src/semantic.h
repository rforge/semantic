/*
 * semantic.h
 *
 *  Created on: 11.12.2014
 *      Author: wolfgang
 */

#ifndef SEMANTIC_H_
#define SEMANTIC_H_

#include <R.h>
#include <Rinternals.h>
#include <Rdefines.h>
#include <R_ext/PrtUtil.h>
#include <R_ext/Rdynload.h> // DllInfo


/*
#include <iostream>
#include <vector>
#include <list>
#include <cstring>
#include <string>

#include <stdexcept>
using namespace std;
*/

#include <vector>
# include "rdf.h"

extern "C" {
static void iri_finalize_vector(SEXP ptr);
SEXP iri_create_vector();
SEXP iri_parse_text(SEXP pPtr, SEXP pChar, SEXP pDelim, SEXP pNdelim);
SEXP iri_get_prefix(SEXP pVec);
SEXP iri_get_term(SEXP pVec);
SEXP iri_get_text(SEXP pVec, SEXP pEnv);

} // extern "C"


#endif /* SEMANTIC_H_ */
