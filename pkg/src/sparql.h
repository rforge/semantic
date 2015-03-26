/*
 * sparql.h
 *
 *  Created on: 13.01.2015
 *      Author: wolfgang
 *
 *      See http://gumzo.de/post/175/ (shared_ptr)
 */

#ifndef SPARQL_H_
#define SPARQL_H_

#include <memory>
#include <vector>
#include <list>
#include <utility>
#include <string>
#include <sstream>

// class num_base and char_encode
#include "basics.h"
#include "triple.h"

using namespace std;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - //
// Manages prefix entries for sparql queryies
// and rdf data.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - //

class sparql_namespace
{
public:
	sparql_namespace() : base_iri("") {}

	size_t size() const { return v_.size(); }
	void addPrefix(const string& prefix, const string & iri)
	{
		pss p;
		p.first = prefix;
		p.second = iri;
		v_.push_back(p);
	}

	// BASE related functions, see section 4.2
	void setBaseIri(const string &s) { base_iri = s; }
	string getBaseIri() const { return base_iri; }
	void clearBaseIri() { base_iri.clear(); }
	bool baseIriEmpty() const { return base_iri.empty(); }

	string getText(const string & prefix, const string &suffix) const;
	string getText(unsigned i, const string & prefix, const string &suffix) const;
	string getPrefix(const unsigned index) const
	{
		if(index > v_.size())
			return string("");

		return v_[index].first;
	}

	void get(unsigned i, pair<string, string> &ps) const  { ps = v_[i]; }

	friend class sparql_query;
	friend class rdf_data;

	// Declaration for ostream insertion operator
	template<typename charT, typename traits>
	friend basic_ostream<charT, traits> & operator<<(basic_ostream<charT, traits> &os, const sparql_namespace &obj);


private:

	typedef pair<string, string> pss;
	typedef vector<pss> vpss;
	vpss v_;
	string base_iri;

	string getQueryText() const;	// SPARQL query version
	string getDataText() const; 	// RDF data version
};


string sparql_namespace::getQueryText() const
{
	vpss::const_iterator iter;
	stringstream sst;

	if(!baseIriEmpty())
		sst << "BASE " << base_iri  << "\n";

	for(iter = v_.begin(); iter != v_.end(); ++iter)
		sst << "PREFIX " << iter->first << ": " << iter->second << "\n";

	return sst.str();
}

string sparql_namespace::getDataText() const
{
	vpss::const_iterator iter;
	stringstream sst;

	if(!baseIriEmpty())
		sst << "@prefix : " << base_iri  << " .\n";

	for(iter = v_.begin(); iter != v_.end(); ++iter)
		sst << "@prefix " << iter->first << ": " << iter->second << " .\n";

	return sst.str();
}


string sparql_namespace::getText(const string & prefix, const string & suffix) const
{
	vpss::const_iterator iter;
	stringstream sst;

	for(iter = v_.begin(); iter != v_.end(); ++iter)
		sst << prefix << " " << iter->first << ": " << iter->second << suffix << "\n";

	return sst.str();
}

string sparql_namespace::getText(unsigned i, const string & prefix, const string &suffix) const
{
	stringstream sst;
	sst << prefix << " " << v_[i].first << ": " << v_[i].second << suffix;
	return sst.str();
}


// Implementation of ostream insert operator
template<typename charT, typename traits>
basic_ostream<charT, traits> &operator<<(basic_ostream<charT, traits> &os, const sparql_namespace &obj)
{
	os << obj.getDataText();
	return os;
}



// - - - - - - - - - - - - - - - - - - - - - - - - - - - - //
// 1.2.4: rdf term = IRI, blank node or literal
//
// The rdf_term implementation uses text contained
// in shared_ptr objects which allows sharing identical
// content in multiple objects.
//
// This will be necessary for usage of elements ad multiple
// sites.
// Example: Using a result term (?a) in result definition
// as well as in RDF query graph.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - //

class rdf_term
{
public:
	rdf_term() {}
	rdf_term(const char * c) { text_.push_back(string(c)); rewind(); }
	rdf_term(const string & c) { text_.push_back(c); rewind(); }
	virtual ~rdf_term() {}

	// Copy constructor: copies whole list
	rdf_term(const rdf_term &rhs)
	{ text_ = rhs.text_; rewind(); }

	// Assignment operator: Copies whole list
	rdf_term & operator=(const rdf_term &rhs)
	{
		if(this == &rhs)
			return *this;

		text_ = rhs.text_;
		rewind();
		return *this;
	}

	void rewind()
	{
		if(text_.size())
			iter=text_.begin();
		else
			iter=text_.end();
	}


	bool getNext(string &s)
	{
		if(iter!=text_.end())
		{
			s = *iter;
			++iter;
			return true;
		}

		s.clear();
		return false;
	}

	size_t size() const { return text_.size(); }
	void push_back(const string &s) { text_.push_back(s); }
	void clear() { text_.clear(); iter=text_.end(); }

	// Type conversion operator
	operator string() const { return do_getText(); }

	// Declaration for ostream insertion operator
	template<typename charT, typename traits>
	friend basic_ostream<charT, traits> & operator<<(basic_ostream<charT, traits> &os, const rdf_term &obj);

protected:
	list<string> text_;
	list<string>::const_iterator iter;


	virtual string do_getText() const
	{
		stringstream sst;
		list<string>::const_iterator iter;
		for(iter = text_.begin(); iter != text_.end(); ++iter)
			sst << *iter << " ";

		return sst.str();
	}
};


// Implementation of ostream insert operator
template<typename charT, typename traits>
basic_ostream<charT, traits> &operator<<(basic_ostream<charT, traits> &os, const rdf_term &obj)
{
	os << obj.do_getText();
	return os;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - //
// Class encodes production of static prefixed nodes.
// Encapsulated in constructing class because keeping id otherwise had to
// be located in a static variable (inside a c-file, resulting in a
// compiled object file.)
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - //

class static_nodes
{
public:
	static_nodes(string prefix, int nchar = 26, int offset = 97):
		prefix_(prefix), ce(char_encode(nchar, offset)) {}

	static_nodes(const char  *prefix, int nchar = 26, int offset = 97):
		prefix_(string(prefix)), ce(char_encode(nchar, offset)) {}

	virtual ~static_nodes() {}

	// Retrieve consecutive node labels
	void getNextNode(string & s) { s = string(prefix_ + ce.encodeNextId()); }
	void getNodes(unsigned n, rdf_term &term);
	void getNodes(unsigned n, vector<string> &v);
	void getNodes(rdf_term &term, vector<string> &v);

	// Use user provided node labels
	rdf_term getNode(const string & label)
	{ return rdf_term(prefix_ + label); }
	rdf_term getNode(const char * label)
	{ return rdf_term(prefix_ + string(label)); }


private:
	const string prefix_;
	char_encode ce;
};

void static_nodes::getNodes(unsigned n, rdf_term &term)
{
	string s;
	unsigned i;
	for(i=0; i < n; ++i)
	{
		getNextNode(s);
		term.push_back(s);
	}
}

void static_nodes::getNodes(unsigned n, vector<string> &v)
{
	v.clear();
	unsigned i;
	string s;
	for(i=0; i < n; ++i)
	{
		getNextNode(s);
		v.push_back(s);
	}
}

void static_nodes::getNodes(rdf_term &term, vector<string> &v)
{
	term.clear();
	vector<string>::const_iterator iter;
	for(iter=v.begin(); iter!=v.end(); ++iter)
		term.push_back(prefix_ + *iter);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - //
// Static plain nodes (e.g. "cat")
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - //

class plain_nodes : public static_nodes
{
public:
	plain_nodes(): static_nodes("") {}
};


// - - - - - - - - - - - - - - - - - - - - - - - - - - - - //
// Blank nodes 4.1.4
// Node containing share-able static text
// prefixed with "_:".
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - //
class blank_nodes : public static_nodes
{
public:
	blank_nodes(): static_nodes("_:", 26, 97) {}
};

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - //
// 4.1.3 Query Variables
// Node containing share-able static text
// prefixed with "?" (alternatively "$")
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - //

class query_variables : public static_nodes
{
public:
	query_variables(const char * prefix="?"):
		static_nodes(prefix, 26, 97) {}
};

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - //
// RDF literal: 4.1.2
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - //
class literal_term: public rdf_term
{
public:
	literal_term(): rdf_term("") {}
	literal_term(string s): rdf_term(s) {}
};

// 1.2.4 simple literal
literal_term simpleLiteral(const char *s)
{
	stringstream sst;
	sst << "'" << s << "'";
	return literal_term(sst.str());
}
literal_term simpleLiteral(string s) { return simpleLiteral(s.c_str()); }


// 4.1.2 Syntax for literals
literal_term taggedLiteral(const char *s, const char *language_tag)
{
	stringstream sst;
	sst << "'" << s << "'@" << language_tag;
	return literal_term(sst.str());
}

template<typename T>
literal_term typedLiteral(const T& t);

template<>
literal_term typedLiteral<int>(const int& t)
{
	stringstream sst;
	sst << "'" << t << "'^^^xsd:integer";
	return literal_term(sst.str());
}

template<>
literal_term typedLiteral<unsigned int>(const unsigned int& t)
{
	stringstream sst;
	sst << "'" << t << "'^^^xsd:integer";
	return literal_term(sst.str());
}

template<>
literal_term typedLiteral<float>(const float& t)
{
	stringstream sst;
	sst << "'" << t << "'^^^xsd:decimal";
	return literal_term(sst.str());
}

template<>
literal_term typedLiteral<double>(const double& t)
{
	stringstream sst;
	sst << "'" << t << "'^^^xsd:double";
	return literal_term(sst.str());
}

template<>
literal_term typedLiteral<bool>(const bool& t)
{
	stringstream sst;
	sst << "'" << boolalpha << t << "'^^^xsd:boolean";
	return literal_term(sst.str());
}


// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - //
// Group patterns and rdf graphs
// Base class for:
//			- group_pattern
//			- rdf_triple
//			- concat
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - //
class rdf_triple;

class basic_group_pattern
{
public:
	basic_group_pattern() {}
	virtual ~basic_group_pattern() {}

	operator string() const { return  do_get_string(); }


protected:
	// Represents empty group pattern
	virtual string do_get_string() const { return string(); }
};

// Implementation of ostream insert operator
// Adds enveloping curly brackets
template<typename charT, typename traits>
basic_ostream<charT, traits> &operator<<(basic_ostream<charT, traits> &os, const basic_group_pattern &obj)
{
	os << "{\n" << string(obj) << "}";
	return os;
}

class group_pattern : public basic_group_pattern
{
public:
	typedef shared_ptr<basic_group_pattern> pbg;
	typedef list<shared_ptr<basic_group_pattern> > lpbg;

public:
	group_pattern():
		basic_group_pattern(), pl_(make_shared<lpbg>()) {}
	virtual ~group_pattern() {}

	void addPattern(basic_group_pattern &obj)
	{
		// Do not add myself to containing list
		// ToDo: Throw exception?
		if(this == &obj)
			return;
		shared_ptr<basic_group_pattern> ptr = make_shared<basic_group_pattern>();
		*ptr = obj;
		pl_->push_back(ptr);
	}

	// Overloaded version is needed in order to provide
	// two step initialization.
	// Necessary for usage of virtual functions.
	void addPattern(rdf_triple &obj);

	size_t size() const { return pl_->size(); }

private:
	shared_ptr<lpbg>  pl_;
	virtual string do_get_string() const;
};

string group_pattern::do_get_string() const
{
	list<shared_ptr<basic_group_pattern> >::const_iterator iter;
	stringstream sst;

	// Braces are added by operator << (os, basic_group_pattern)
	for(iter = pl_->begin(); iter!= pl_->end(); ++iter)
		sst << **iter << "\n";
	return sst.str();
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - //
// rdf_triple: Subject / Predicate / Object
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - //

// ToDo: 5.1 Basic graph patterns are sets of triple patterns ...

class rdf_triple : public basic_group_pattern
{
public:
	rdf_triple(): iter(l_.end()), delim_(" ") {}
	void rewind()
	{
		if(l_.size())
			iter = l_.begin();
		else
			iter = l_.end();
	}

	rdf_triple(const char* s, const char* p, const char* o): delim_("")
	{
		l_.push_back(make_triple(string(s), string(p), string(o)));
		iter = l_.begin();
	}

	rdf_triple(string &s, string &p, string &o): delim_("")
	{
		l_.push_back(make_triple(s,p,o));
		iter = l_.begin();
	}

	virtual ~rdf_triple() {}

	bool getNext(string &s, string &p, string &o)
	{
		if(iter!=l_.end())
		{
			s=iter->first;
			p=iter->second;
			o=iter->third;
			++iter;
			return true;
		}

		s.clear();
		p.clear();
		o.clear();
		return false;
	}

	void push_back(const char* s, const char* p, const char* o)
	{ l_.push_back(make_triple(string(s), string(p), string(o))); }

	void push_back(const string & s, const string &p, const string &o)
	{ l_.push_back(make_triple(s, p, o)); }

	void setDelim(const string &s) { delim_ = s; }

	size_t size() const { return l_.size(); }

	// Also used in ostream insertion
	// ToDo: Make " " exchangable ??
	operator string() const
	{
		stringstream sst;
		list<triple<string, string, string> >::const_iterator i;
		for(i=l_.begin(); i!=l_.end(); ++i)
			sst << i->first << delim_ << i->second << delim_ << i->third << " .\n";

		return sst.str();
	}

protected:
	list<triple<string, string, string> > l_;
	list<triple<string, string, string> >::const_iterator iter;
	string delim_;

	// Returns group_pattern like string
	virtual string do_get_string() const { return string(*this); }
};

// Implementation of ostream insert operator
template<typename charT, typename traits>
basic_ostream<charT, traits> &operator<<(basic_ostream<charT, traits> &os, const rdf_triple &obj)
{
	os << string(obj);
	return os;
}


void group_pattern::addPattern(rdf_triple &obj)
{
	// Two step initialization is needed for usage of
	// virtual functions
	shared_ptr<rdf_triple> prdf = make_shared<rdf_triple>();
	*prdf = obj;
	shared_ptr<basic_group_pattern> pbg = make_shared<basic_group_pattern>();
	pbg = prdf;
	pl_->push_back(pbg);
}


// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - //
// concat: section 2.5 Creating Values with Expressions
// Concats may be part of result-set and of query graph pattern
// This class is intended for usage as query graph pattern.
//
// Therefore, the CONCAT(...) AS ?. has to be enclosed in BIND(...)
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - //


class concat: public basic_group_pattern
{
public:
	concat(const string as):as_(as) {}
	virtual ~concat() {}

	size_t size() const { return concat_.size(); }
	void addTerm(const string &term) { concat_.push_back(term); }

private:
	string as_;
	list<string> concat_;
	virtual string do_get_string() const;
};


string concat::do_get_string() const
{
	if(!concat_.size())
		return string();

	stringstream sst;

	// Prefix
	sst << "BIND(CONCAT(";

	// Do concatenate ....
	list<string>::const_iterator iter = concat_.begin();
	if(!strcmp(iter->c_str(), " "))
		sst << "\" \"";		// writes " " for empty space...
	else
		sst << *iter;


	for(++iter; iter!=concat_.end(); ++iter)
	{
		if(!strcmp(iter->c_str(), " "))
			sst << ", " << "\" \"";		// writes " " for empty space...
		else
			sst << ", " << *iter;
	}

	// Suffix
	sst << " AS " << as_ << ")";

	return sst.str();
}


// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - //
// rdf_graph: List of rdf_triples
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - //

class rdf_graph
{
private:
	typedef list<rdf_triple> lrt;

public:
	rdf_graph() {}
	virtual ~rdf_graph() {}

	void addTriple(const rdf_triple &t) { l_.push_back(t); }
	size_t size() const { return l_.size(); }

	operator string() const
	{
		stringstream sst;
		lrt::const_iterator iter;

		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - //
		// "string" will also add a terminating "\n":
		// *iter returns string representation from rdf_triple which already
		// contains a terminating "\n", so "\n" is omitted here.
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - //
		for(iter = l_.begin(); iter != l_.end(); ++iter)
			sst << *iter;

		return sst.str();
	}

	// Declaration for ostream insertion operator
	template<typename charT, typename traits>
	friend basic_ostream<charT, traits> & operator<<(basic_ostream<charT, traits> &os, const rdf_graph &obj);

private:
	lrt l_;
};


// Implementation of ostream insert operator
template<typename charT, typename traits>
basic_ostream<charT, traits> &operator<<(basic_ostream<charT, traits> &os, const rdf_graph &obj)
{
	os << string(obj);
	return os;
}



// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - //
// Definition of result set for SPARQL query
// e.g.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - //

class result_def
{
public:
	typedef shared_ptr<rdf_term> spr;
	typedef list<shared_ptr<rdf_term> > lspr;

public:
	result_def(bool all=false):all_variables(all) {}
	virtual ~result_def() {}

	void addTerm(const rdf_term & term)
	{
		spr p = std::make_shared<rdf_term>();
		*p = term;
		l_.push_back(p);
	}

	size_t size() const { return l_.size(); }
	operator std::string() const
	{
		if(all_variables)
			return string("*");

		// Concatenate all elements
		stringstream sst;
		lspr::const_iterator iter;
		for(iter = l_.begin(); iter != l_.end(); ++iter)
			sst << **iter << " ";

		return sst.str();
	}

	// Declaration for ostream insertion operator
	template<typename charT, typename traits>
	friend basic_ostream<charT, traits> & operator<<(basic_ostream<charT, traits> &os, const result_def &obj);

private:
	lspr l_;
	bool all_variables;
};


// Implementation of ostream insert operator
template<typename charT, typename traits>
basic_ostream<charT, traits> &operator<<(basic_ostream<charT, traits> &os, const result_def &obj)
{
	os << string(obj);
	return os;
}


// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - //
// Base class for sparql_query and rdf_data
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - //
class basic_rdf
{
public:
	typedef shared_ptr<sparql_namespace> snp;
	typedef shared_ptr<rdf_graph> rgp;

public:
	basic_rdf(): sn_(new sparql_namespace),
	rg_(std::make_shared<rdf_graph>())
	{}

	basic_rdf(sparql_namespace &sn) :
		sn_(std::make_shared<sparql_namespace>(sn)),
		rg_(std::make_shared<rdf_graph>())
		{ *sn_ = sn; }

	basic_rdf(snp &p):
		rg_(std::make_shared<rdf_graph>())
		{ sn_ = p; }

	virtual ~basic_rdf() {}

	operator std::string() const { return do_get_string(); }
	void setNamespace(sparql_namespace &sn) { *sn_ = sn; }
	void setRdfGraph(rdf_graph &rg) { *rg_ = rg; }

	friend ostream &operator<<(ostream &os, const basic_rdf &obj);

protected:
	snp sn_;
	rgp rg_;
	virtual string do_get_string() const = 0;
};

ostream & operator<<(ostream &os, const basic_rdf &obj)
{
	os << obj.do_get_string();
	return os;
}




// ToDo: Query forms: SELECT, CONSTRUCT.
// ToDo: resultDef

class sparql_query: public basic_rdf
{
public:
	typedef shared_ptr<sparql_namespace> snp;
	typedef shared_ptr<rdf_graph> rgp;

public:
	sparql_query():
		basic_rdf(),  query_form_(string("SELECT")),
		rd_(result_def()), gp_(group_pattern()) {}

	sparql_query(sparql_namespace &sn):
		basic_rdf(sn), query_form_(string("SELECT")),
		rd_(result_def()), gp_(group_pattern()){}

	sparql_query(snp p):
		basic_rdf(p), query_form_(string("SELECT")),
		rd_(result_def()), gp_(group_pattern()){}

	virtual ~sparql_query() {}

	void setNameSpace(const shared_ptr<sparql_namespace> &ns) { sn_ = ns; }
	void setResultDef(const result_def &res) { rd_ = res; }
	void setQueryForm(string & form) { query_form_ = form; }
	void setQueryForm(const char * form) { query_form_ = string(form); }
	void setGroupPattern(group_pattern &gp) { gp_ = gp; }


private:
	string query_form_;
	result_def rd_;
	group_pattern gp_;


	string do_get_string() const
	{
		stringstream sst;
		sst << sn_->getQueryText();
		sst << query_form_ << " " << rd_;
		sst << "WHERE\n";
		sst << gp_ << "\n";
		return sst.str();
	}
};


class rdf_data: public basic_rdf
{
public:
	rdf_data() {}
	rdf_data(sparql_namespace sn): basic_rdf(sn) {}
	rdf_data(snp p): basic_rdf(p) {}
	virtual ~rdf_data() {}

private:
	string do_get_string() const
	{
		stringstream sst;
		if(sn_->size())
			sst << sn_->getDataText() << "\n";

		sst << string(*rg_);
		return sst.str();
	}
};

#endif /* SPARQL_H_ */
