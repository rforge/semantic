/*
 * semantic.cpp
 *
 *  Created on: 09.02.2015
 *      Author: kaisers
 */


#include "semantic.h"
#include <vector>

#include <string>
using namespace std;

using namespace rdf;



extern "C" {


// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - //
// IriTerm : Vector of iriTerm's
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - //

SEXP get_iri_term()
{
	extptr<std::vector<rdf::iriTerm> > ptr;
	return ptr;
}

SEXP iri_term_get_text(SEXP pVec, SEXP pEnv)
{
	if(TYPEOF(pEnv) != INTSXP)
		error("[iri_get_text] pEnv must be Integer!");

	extptr<std::vector<rdf::iriTerm> > v(pVec);

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


SEXP iri_parse_text(SEXP pPtr, SEXP pChar, SEXP pDelim, SEXP pNdelim)
{
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
	//typedef std::vector<rdf::iriTerm> virit;
	extptr<std::vector<rdf::iriTerm> > v(pPtr);

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

SEXP iri_get_term(SEXP pVec)
{
	extptr<std::vector<rdf::iriTerm> > v(pVec);
	unsigned long int i, n;
	n = v->size();
	std::vector<rdf::iriTerm>::const_iterator iter;

	SEXP pRes = PROTECT(allocVector(STRSXP, n));
	for(i=0, iter=v->begin(); iter != v->end(); ++i, ++iter)
		SET_STRING_ELT(pRes, i, mkChar(iter->term.c_str()));

	UNPROTECT(1);
	return pRes;
}

SEXP iri_get_prefix(SEXP pVec)
{
	unsigned long int i, n;
	extptr<std::vector<rdf::iriTerm> > v(pVec);

	n = v->size();
	std::vector<rdf::iriTerm>::const_iterator iter;

	SEXP pRes = PROTECT(allocVector(STRSXP, n));
	for(i=0, iter=v->begin(); iter != v->end(); ++i, ++iter)
		SET_STRING_ELT(pRes, i, mkChar(iter->prefix.c_str()));

	UNPROTECT(1);
	return pRes;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - //
// sparql_namespace
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - //

SEXP get_sparql_namespace()
{
	extptr<sparql_namespace> xptr;
	return xptr;
}


SEXP sparql_ns_add_prefix(SEXP pNameSpace, SEXP pPrefix, SEXP pIri)
{
	if(TYPEOF(pPrefix) != STRSXP)
		error("[sparql_ns_add_prefix] pPrefix must be STRSXP!");

	if(TYPEOF(pIri) != STRSXP)
		error("[sparql_ns_add_prefix] pIri must be STRSXP!");

	int i, n = length(pPrefix);

	if(length(pIri) != n)
		error("[sparql_ns_add_prefix] pPrefix and pIri must have equal length!");

	// Do add prefix
	extptr<sparql_namespace> v(pNameSpace);
	for(i=0; i < n; ++i)
		v->addPrefix(string(CHAR(STRING_ELT(pPrefix, i))), string(CHAR(STRING_ELT(pIri, i))));

	return R_NilValue;
}

SEXP sparql_ns_get_prefix(SEXP pNameSpace, SEXP pIndex)
{
	extptr<sparql_namespace> ptr(pNameSpace);

	if(TYPEOF(pIndex) != INTSXP)
		error("pIndex must be INTEGER!");

	int i, n = length(pIndex);

	SEXP pRes = PROTECT(allocVector(STRSXP, n));
	for(i=0; i < n; ++i)
		SET_STRING_ELT(pRes, i, mkChar(ptr->getPrefix(INTEGER(pIndex)[i]).c_str()));

	UNPROTECT(1);
	return pRes;
}


SEXP sparql_ns_get_size(SEXP pRob)
{
	extptr<sparql_namespace> ptr(pRob);

	SEXP pRes = PROTECT(allocVector(INTSXP, 1));
	INTEGER(pRes)[0] = (int) ptr->size();
	UNPROTECT(1);
	return pRes;
}

SEXP sparql_ns_get_text(SEXP pRob, SEXP pX)
{
	extptr<sparql_namespace> ptr(pRob);
	atmptr<char> x(pX);

	// Provide prefix and suffix via pX:
	// 0: prefix
	// 1: suffix
	string prefix, suffix;
	x.get(0, prefix);
	x.get(1, suffix);
	return to_string(ptr->getText(prefix, suffix));
}

SEXP sparql_ns_get_text_vec(SEXP pRob, SEXP pX)
{
	extptr<sparql_namespace> ptr(pRob);
	atmptr<char> x(pX);

	// Provide prefix and suffix via pX:
	// 0: prefix
	// 1: suffix
	string prefix, suffix;
	x.get(0, prefix);
	x.get(1, suffix);

	unsigned i, n = (unsigned) ptr->size();
	atmptr<char> res(n);

	for(i=0; i < n; ++i)
		res.set(i, ptr->getText(i, prefix, suffix));

	return res;
}


// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - //
// rdf_term
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - //
SEXP init_rdf_term(SEXP pText)
{
	atmptr<char> txt(pText);
	extptr<rdf_term> xptr;
	string s;
	int i, n = length(txt);

	for(i = 0; i < n; ++i)
	{
		txt.get(i,s);
		xptr->push_back(s);
	}
	return xptr;
}


SEXP rdf_term_get_text(SEXP pRdf)
{
	extptr<rdf_term> xptr(pRdf);
	size_t i, n = xptr->size();
	atmptr<char> txt((unsigned) n);
	string s;

	xptr->rewind();
	for(i=0; i < n; ++i)
	{
		xptr->getNext(s);
		txt.set((unsigned) i, s);
	}
	return txt;
}

SEXP rdf_term_get_size(SEXP pRdf)
{
	extptr<rdf_term> xptr(pRdf);
	atmptr<int> ret(1);
	ret[0] = (int) xptr->size();
	return ret;
}


SEXP rdf_term_add_term(SEXP pRdf, SEXP pTerm)
{
	extptr<rdf_term> xptr(pRdf);
	atmptr<char> term(pTerm);

	int i, n = length(term);
	string s;

	for(i=0; i < n; ++i)
	{
		term.get(i, s);
		xptr->push_back(s);
	}
	return R_NilValue;
}


// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - //
// blank_nodes (node creator)
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - //

SEXP get_blank_nodes()
{
	extptr<blank_nodes> xptr;
	return xptr;
}

SEXP blank_nodes_get_text(SEXP pNodes, SEXP pN)
{
	extptr<blank_nodes> xptr(pNodes);
	atmptr<int> xn(pN);

	vector<string> v;
	xptr->getNodes(xn[0], v);

	// Copy into STRSXP
	atmptr<char> xc(v);
	return xc;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - //
// query_variable
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - //
SEXP get_query_variables()
{
	extptr<query_variables> xptr;
	return xptr;
}

SEXP query_variables_get_node(SEXP pQryVar, SEXP pText)
{
	extptr<query_variables> xptr(pQryVar);
	atmptr<char> text(pText);
	extptr<rdf_term> xres;

	vector<string> v;
	text.fill_string_vector(v);
	xptr->getNodes(*xres, v);
	return xres;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - //
// rdf_triple
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - //

// ToDo: 5.1 Basic graph patterns are sets of triple patterns ...


SEXP get_rdf_triple(SEXP pSub, SEXP pPred, SEXP pObj)
{
	extptr<rdf_term> rs(pSub);
	extptr<rdf_term> rp(pPred);
	extptr<rdf_term> ro(pObj);

	size_t i , n = rs->size();

	if(n != rp->size() || n != ro->size())
		error("[get_rdf_triple] Subject, Predicate and Object must be equal sized!");

	string s, p, o;
	extptr<rdf_triple> xptr;
	rs->rewind();
	rp->rewind();
	ro->rewind();

	for(i = 0; i < n; ++i)
	{
		rs->getNext(s);
		rp->getNext(p);
		ro->getNext(o);
		xptr->push_back(s, p, o);
	}
	return xptr;
}

SEXP rdf_triple_get_size(SEXP pRdf)
{
	extptr<rdf_triple> xp(pRdf);
	atmptr<int> res(1);
	res[0] = (int) xp->size();
	return res;
}

SEXP rdf_triple_get_text(SEXP pRdf)
{
	extptr<rdf_triple> xptr(pRdf);
	return to_string(xptr);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - //
// group_pattern
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - //
SEXP get_group_pattern()
{
	extptr<group_pattern> xptr;
	return xptr;
}

SEXP group_pattern_add_pattern(SEXP pGPat, SEXP pAdd)
{
	extptr<group_pattern> gp(pGPat);
	extptr<basic_group_pattern> add(pAdd);

	gp->addPattern(*add);
	return R_NilValue;
}

SEXP group_pattern_add_rdf_triple(SEXP pGPat, SEXP pAdd)
{
	extptr<group_pattern> gp(pGPat);
	extptr<rdf_triple> add(pAdd);
	gp->addPattern(*add);
	return R_NilValue;
}

SEXP group_pattern_get_text(SEXP pPattern)
{
	return to_string(extptr<group_pattern>(pPattern));
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - //
// rdf_graph
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - //
SEXP get_rdf_graph()
{
	extptr<rdf_graph> rg;
	return rg;
}

SEXP rdf_graph_add_triple(SEXP pRdf, SEXP pSubj, SEXP pPred, SEXP pObj)
{
	extptr<rdf_graph> rg(pRdf);
	atmptr<char> subject(pSubj);
	atmptr<char> predicate(pPred);
	atmptr<char> object(pObj);

	int n = subject.length();
	if(n != predicate.length() || n != object.length())
		error("Subject, predicate and object must have equal length!");

	rdf_triple rt;
	string sts, stp, sto;
	for(int i=0; i < n; ++i)
	{
		subject.get(i, sts);
		predicate.get(i, stp);
		object.get(i, sto);
		rt.push_back(sts, stp, sto);
	}

	rg->addTriple(rt);
	return R_NilValue;
}

SEXP rdf_graph_get_text(SEXP pRdf)
{
	return to_string(extptr<rdf_graph>(pRdf));
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - //
// result_def
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - //
SEXP get_result_def()
{
	extptr<result_def> rd;
	return rd;
}

SEXP result_def_add_term(SEXP pResDef, SEXP pAdd)
{
	extptr<result_def> rd(pResDef);
	extptr<rdf_term> add(pAdd);
	rd->addTerm(*add);
	return R_NilValue;
}

SEXP result_def_get_text(SEXP pResDef)
{
	return to_string(extptr<result_def>(pResDef));
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - //
// sparql_query
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - //
SEXP get_sparql_query()
{
	extptr<sparql_query> sq;
	// Default
	sq->setQueryForm("SELECT");
	return sq;
}

SEXP sparql_query_set_namespace(SEXP pSparql, SEXP pNs)
{
	extptr<sparql_query> sq(pSparql);
	extptr<sparql_namespace> ns(pNs);
	sq->setNameSpace(ns);
	return R_NilValue;
}

SEXP sparql_query_set_result_def(SEXP pSparql, SEXP pResDef)
{
	extptr<sparql_query> sq(pSparql);
	extptr<result_def> rd(pResDef);
	sq->setResultDef(*rd);
	return R_NilValue;
}

SEXP sparql_query_set_query_form(SEXP pSparql, SEXP pQueryForm)
{
	if(TYPEOF(pQueryForm) != STRSXP)
		error("[sparql_query_set_query_form] pQueryForm must be STRSXP!");

	extptr<sparql_query> sq(pSparql);
	sq->setQueryForm(CHAR(STRING_ELT(pQueryForm, 0)));
	return R_NilValue;
}

SEXP sparql_query_set_group_pattern(SEXP pSparql, SEXP pPattern)
{
	extptr<sparql_query> sq(pSparql);
	extptr<group_pattern> qp(pPattern);
	sq->setGroupPattern(*qp);
	return R_NilValue;
}

SEXP sparql_query_get_text(SEXP pSparql)
{
	return to_string(extptr<sparql_query>(pSparql));
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - //
// rdf_data
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - //
SEXP get_rdf_data()
{
	extptr<rdf_data> xptr;
	return xptr;
}


SEXP get_rdf_data_ns(SEXP pNameSpace)
{
	extptr<sparql_namespace> xns(pNameSpace);
	shared_ptr<rdf_data> sp = make_shared<rdf_data>(*xns);
	extptr<rdf_data> xrd(*sp);
	return xrd;
}



SEXP rdf_data_set_namespace(SEXP pRdf, SEXP pNs)
{
	extptr<rdf_data> rdf(pRdf);
	extptr<sparql_namespace> ns(pNs);

	rdf->setNamespace(*ns);
	return R_NilValue;
}

SEXP rdf_data_set_rdf_graph(SEXP pRdf, SEXP pGraph)
{
	extptr<rdf_data> rdf(pRdf);
	extptr<rdf_graph> gph(pGraph);
	rdf->setRdfGraph(*gph);
	return R_NilValue;
}

SEXP rdf_data_get_text(SEXP pRdf)
{ return to_string(extptr<rdf_data>(pRdf)); }



} // extern "C"


