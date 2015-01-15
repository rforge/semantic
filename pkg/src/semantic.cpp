/*
 * semantic.cpp
 *
 *  Created on: 11.12.2014
 *      Author: wolfgang
 */


#include "semantic.h"

extern "C" {

static void iri_finalize_vector(SEXP ptr)
{
	if(TYPEOF(ptr) != EXTPTRSXP)
		error("[iri_finalize_vector] No external pointer!\n");

	if(!R_ExternalPtrAddr(ptr)) return;

	std::vector<rdf::iriTerm> * v = (std::vector<rdf::iriTerm> *) (R_ExternalPtrAddr(ptr));
	delete v;
    R_ClearExternalPtr(ptr);
}

SEXP iri_create_vector()
{
	std::vector<rdf::iriTerm> * v = new(std::vector<rdf::iriTerm>);

	SEXP pVec;
	PROTECT(pVec = R_MakeExternalPtr((void*)(v), R_NilValue, R_NilValue));
	R_RegisterCFinalizer(pVec, iri_finalize_vector);

	UNPROTECT(1);
	return pVec;
}


SEXP iri_parse_text(SEXP pPtr, SEXP pChar, SEXP pDelim, SEXP pNdelim)
{
	if(TYPEOF(pPtr) != EXTPTRSXP)
		error("[iri_term_parse_text] No external pointer!\n");

	// That should not happen
	if(!R_ExternalPtrAddr(pPtr))
		return R_NilValue;

	if(TYPEOF(pChar) != STRSXP)
		error("[iri_term_parse_text] pChar must be character!");

	if(TYPEOF(pDelim) != STRSXP)
		error("[iri_term_parse_text] pDelim must be character!");

	if(TYPEOF(pNdelim) != INTSXP)
		error("[iri_term_parse_text] pNdelim must be integer!");

	// delim separates term from prefix e.g. "#:/"
	// const char *delim = "#:/";
	const char *delim = CHAR(STRING_ELT(pDelim, 0));
	unsigned nDelim = (unsigned) INTEGER(pNdelim)[0];

	int n = length(pChar);

	// Prepare container
	std::vector<rdf::iriTerm> * v = (std::vector<rdf::iriTerm> *) (R_ExternalPtrAddr(pPtr));
	v->clear();
	v->resize(n);

	rdf::iriTerm term;
	int i;
	for(i=0; i < n; ++i)
	{
		term.parse(CHAR(STRING_ELT(pChar, i)), delim, nDelim);
		(*v)[i] = term;
	}

	return R_NilValue;
}


SEXP iri_get_prefix(SEXP pVec)
{
	if(TYPEOF(pVec) != EXTPTRSXP)
		error("[iri_get_prefix] pVec must be external pointer!");

	std::vector<rdf::iriTerm> * v = (std::vector<rdf::iriTerm> *) (R_ExternalPtrAddr(pVec));

	unsigned long int i, n;
	n = v->size();
	std::vector<rdf::iriTerm>::const_iterator iter;

	SEXP pRes = PROTECT(allocVector(STRSXP, n));
	for(i=0, iter=v->begin(); iter != v->end(); ++i, ++iter)
		SET_STRING_ELT(pRes, i, mkChar(iter->prefix.c_str()));

	UNPROTECT(1);
	return pRes;
}

SEXP iri_get_term(SEXP pVec)
{
	if(TYPEOF(pVec) != EXTPTRSXP)
		error("[iri_get_term] pVec must be external pointer!");

	std::vector<rdf::iriTerm> * v = (std::vector<rdf::iriTerm> *) (R_ExternalPtrAddr(pVec));

	unsigned long int i, n;
	n = v->size();
	std::vector<rdf::iriTerm>::const_iterator iter;

	SEXP pRes = PROTECT(allocVector(STRSXP, n));
	for(i=0, iter=v->begin(); iter != v->end(); ++i, ++iter)
		SET_STRING_ELT(pRes, i, mkChar(iter->term.c_str()));

	UNPROTECT(1);
	return pRes;
}

SEXP iri_get_text(SEXP pVec, SEXP pEnv)
{
	if(TYPEOF(pVec) != EXTPTRSXP)
		error("[iri_get_text] pVec must be external pointer!");

	if(TYPEOF(pEnv) != INTSXP)
		error("[iri_get_text] pEnv must be Integer!");

	std::vector<rdf::iriTerm> * v = (std::vector<rdf::iriTerm> *) (R_ExternalPtrAddr(pVec));

	bool env;
	if(INTEGER(pEnv)[0])
		env = true;
	else
		env = false;


	unsigned long int i, n;
	n = v->size();
	std::vector<rdf::iriTerm>::const_iterator iter;

	SEXP pRes = PROTECT(allocVector(STRSXP, n));
	for(i=0, iter=v->begin(); iter != v->end(); ++i, ++iter)
		SET_STRING_ELT(pRes, i, mkChar(iter->getText(env).c_str()));

	UNPROTECT(1);
	return pRes;
}

} // extern C
