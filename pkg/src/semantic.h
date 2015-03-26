/*
 * semantic.h
 *
 *  Created on: 09.02.2015
 *      Author: kaisers
 */

#ifndef SEMANTIC_H_
#define SEMANTIC_H_

#include <R.h>
#include <Rinternals.h>
#include <Rdefines.h>
#include <R_ext/PrtUtil.h>
#include <R_ext/Rdynload.h> // DllInfo

#include <vector>
using namespace std;

#include "extptr.h"
#include "rdf.h"
#include "sparql.h"

extern "C" {

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - //
// IriTerm
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - //
SEXP get_iri_term();
SEXP iri_term_get_text(SEXP pVec, SEXP pEnv);
SEXP iri_parse_text(SEXP pPtr, SEXP pChar, SEXP pDelim, SEXP pNdelim);
SEXP iri_get_term(SEXP pVec);
SEXP iri_get_prefix(SEXP pVec);


// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - //
// sparql_namespace
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - //
SEXP get_sparql_namespace();
SEXP sparql_ns_add_prefix(SEXP pNameSpace, SEXP pPrefix, SEXP pIri);
SEXP sparql_ns_get_prefix(SEXP pNameSpace, SEXP pIndex);
SEXP sparql_ns_get_size(SEXP pRob);
SEXP sparql_ns_get_text(SEXP pRob, SEXP pX);
SEXP sparql_ns_get_text_vec(SEXP pRob, SEXP pX);

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - //
// rdf_term
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - //
SEXP init_rdf_term(SEXP pText);
SEXP rdf_term_get_text(SEXP pRdf);
SEXP rdf_term_get_size(SEXP pRdf);
SEXP rdf_term_add_term(SEXP pRdf, SEXP pTerm);

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - //
// blank_nodes (node creator)
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - //
SEXP get_blank_nodes();
SEXP blank_nodes_get_text(SEXP pNodes, SEXP pN);

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - //
// query_variable
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - //
SEXP get_query_variables();
SEXP query_variables_get_node(SEXP pQryVar, SEXP pText);

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - //
// rdf_triple
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - //
SEXP get_rdf_triple(SEXP pSub, SEXP pPred, SEXP pObj);
SEXP rdf_triple_get_size(SEXP pRdf);
SEXP rdf_triple_get_text(SEXP pRdf);


// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - //
// group_pattern
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - //
SEXP get_group_pattern();
SEXP group_pattern_add_pattern(SEXP pGPat, SEXP pAdd);
SEXP group_pattern_add_rdf_triple(SEXP pGPat, SEXP pAdd);
SEXP group_pattern_get_text(SEXP pPattern);


// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - //
// rdf_graph
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - //
SEXP get_rdf_graph();
SEXP rdf_graph_add_triple(SEXP pRdf, SEXP pSubj, SEXP pPred, SEXP pObj);
SEXP rdf_graph_get_text(SEXP pRdf);


// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - //
// result_def
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - //
SEXP get_result_def();
SEXP result_def_add_term(SEXP pResDef, SEXP pAdd);
SEXP result_def_get_text(SEXP pResDef);


// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - //
// sparql_query
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - //
SEXP get_sparql_query();
SEXP sparql_query_set_namespace(SEXP pSparql, SEXP pNs);
SEXP sparql_query_set_result_def(SEXP pSparql, SEXP pResDef);
SEXP sparql_query_set_query_form(SEXP pSparql, SEXP pQueryForm);
SEXP sparql_query_set_group_pattern(SEXP pSparql, SEXP pPattern);
SEXP sparql_query_get_text(SEXP pSparql);

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - //
// rdf_data
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - //
SEXP get_rdf_data();
SEXP get_rdf_data_ns(SEXP pNameSpace);
SEXP rdf_data_set_namespace(SEXP pRdf, SEXP pNs);
SEXP rdf_data_set_rdf_graph(SEXP pRdf, SEXP pGraph);
SEXP rdf_data_get_text(SEXP pRdf);

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - //
// CONCAT
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - //
SEXP get_concat(SEXP pAs);
SEXP add_concat(SEXP pConcat, SEXP pAdd);
SEXP concat_get_text(SEXP pRdf);



} // extern "C"







#endif /* SEMANTIC_H_ */
