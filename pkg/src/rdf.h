/*
 * rdf.h
 *
 *  Created on: 30.12.2014
 *      Author: wolfgang
 */

#ifndef RDF_H_
#define RDF_H_

#include <string>
//#include <stdexcept>

using namespace std;

namespace rdf
{

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - //
// Splits string of the forms
// A) <http://example.org/push>
// B) <http://example.org/push#one>
// C) prefix:base
// into prefix and term.
// Enveloping '<>' will be removed.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - //

static const char * env = "<>";

struct iriTerm
{
public:
	string prefix;
	string term;
	string delim;

public:
	iriTerm()
	{
		envelope[0] = ' ';
		envelope[1] = ' ';
		env_is_empty[0] = true;
		env_is_empty[1] = true;
	}

	void clear()
	{
		prefix.clear();
		term.clear();
		delim.clear();
		envelope[0] = ' ';
		envelope[1] = ' ';
		env_is_empty[0] = true;
		env_is_empty[1] = true;
	}

	iriTerm & operator=(const iriTerm &rhs);

	void setEnvelope(char c = ' ', int index = 0);

	// Parse input
	bool parse(const string &s, const char *delimiters, unsigned nDelim);
	bool parse(const char* c, const char *delimiters, unsigned nDelim)
		{ return parse(string(c), delimiters, nDelim); }

	// Return content
	string getText(bool env=false) const;
	operator string () const { return getText(); }

private:
	char envelope  [2];
	bool env_is_empty [2];
};

iriTerm & iriTerm::operator =(const iriTerm &rhs)
{
	if(this == &rhs)
		return *this;

	prefix = rhs.prefix;
	term = rhs.term;
	delim = rhs.delim;

	envelope[0] = rhs.envelope[0];
	envelope[1] = rhs.envelope[1];
	env_is_empty[0] = rhs.env_is_empty[0];
	env_is_empty[1] = rhs.env_is_empty[1];

	return *this;
}

void iriTerm::setEnvelope(char c, int index)
{
	if(index == 0)
	{
		envelope[0] = c;
		env_is_empty[0] = false;
	}
	if(index == 1)
	{
		envelope[1] = c;
		env_is_empty[1] = false;
	}
}

string iriTerm::getText(bool env) const
{
	if(env)
	{
		if(env_is_empty[0] == true && env_is_empty[1] == true)
			return prefix + delim + term;
		else if(env_is_empty[0] == true && env_is_empty[1] == false)
			return prefix + delim + term + envelope[1];
		else if(env_is_empty[0] == false && env_is_empty[1] == true)
			return envelope[0] + prefix + delim + term;

		return envelope[0] + prefix + delim + term + envelope[1];
	}

	return prefix + delim + term;
}



bool iriTerm::parse(const string &s, const char *delimiters, unsigned nDelim)
{
	bool found = false;
	unsigned j;
	string::const_iterator iter, begin, end;

	clear();

	// - - - - - - - - - - - - - - - - - - - - //
	// Copies needed because boundaries are
	// subsequently shifted inwards
	// - - - - - - - - - - - - - - - - - - - - //
	begin = s.begin();
	end = s.end();

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - //
	// Process enveloping angles: <xyz>
	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - //
	if(*begin == env[0])
	{
		++begin;
		setEnvelope(env[0], 0);
		if(begin == end)
		{
			throw(runtime_error("Empty string after envelope prefix : " + s + "'!"));
		}
	}

	--end;
	if(*end == env[1])
	{
		setEnvelope(env[1], 1);
		if(begin == end)
		{
			throw(runtime_error("Empty string before envelope term : " + s + "'!"));
		}
	}else{
		++end;
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - //
	// Split text on last occurrence of delimiter:
	// Iterate from end of string backward. Break when delimiter is found.
	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - //
	iter = end;
	while(iter != begin)
	{
		for(j=0; j < nDelim; ++j)
		{
			if(*iter == delimiters[j])
			{
				found = true;
				delim = string(delimiters + j, 1);
				break;
			}
		}
		if(!found)
			--iter;
		else
			break;
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - //
	// iter has walked all the way back to begin()
	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - //
	if(iter==begin)
	{
		// - - - - - - - - - - - - - - - - - - - - - //
		// Previous while routine will does not
		// check for delim's at first position
		// So, this has to be done here.
		// - - - - - - - - - - - - - - - - - - - - - //
		for(j=0; j < nDelim; ++j)
		{
			if(*iter == delimiters[j])
			{
				found = true;
				delim = string(delimiters + j, 1) ;
				break;
			}
		}

		// - - - - - - - - - - - - - - - - - - - - - //
		// When delimiter is found at first position
		// it will be excluded from second fragment
		// (in case query string contains more
		// than one character)
		// - - - - - - - - - - - - - - - - - - - - - //
		if(found)
		{
			if(s.size() > 1)
				++iter;
		}

		// - - - - - - - - - - - - - - - - - - - - - //
		// Otherwise no delimiter found:
		// Second fragment will contain whole string
		// - - - - - - - - - - - - - - - - - - - - - //
		prefix.clear();
		term = string(iter, end);
	}
	else if(iter==end)
	{
		prefix = s;
		term.clear();
	}
	else
	{
		prefix = string(begin, iter);
		term = string(iter + 1, end);
	}

	return found;
}



} // namespace rdf

#endif /* RDF_H_ */
