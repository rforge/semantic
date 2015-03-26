/*
 * basics.h
 *
 *  Created on: 15.01.2015
 *      Author: wolfgang
 */

#ifndef BASICS_H_
#define BASICS_H_

#include <list>
using namespace std;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - //
// Calculates representatives of integral numbers
// for a given base and returns a list (of "digits")
// e.g.
//		6    (base 2 ) = 1  1  0
// 		1000 (base 13) = 5 11 12
//
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - //
// Example:
// num_base nb(26);
// list<unsigned> l = nb.getList(50)
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - //

class num_base
{
public:
	num_base(unsigned s): s_(s) {}
	virtual ~num_base() {}
	typedef unsigned val_t;

	unsigned getBase() const { return s_; }

	list<val_t> getList(val_t val)
	{
		list<val_t> l;
		// npos = number of digits in representation (=3 for 101)
		unsigned npos = (unsigned) (log(val) / log(s_));
		// Actual position for which digit is calculated.
		// Must be int (unsigned -> infinite loop)
		int pos;
		// Numeric value of digit in actual position
		unsigned posval;
		// Residual rest of val which must be accounted for in
		// following digits
		unsigned rest;

		for(pos = (int) npos, rest = val; pos >= 0; --pos)
		{
			posval = (unsigned) pow(s_, pos);
			val = (unsigned) (rest / posval);
			l.push_back(val);
			rest -= val * posval;
		}
		return l;
	}
private:
	unsigned s_;
};

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - //
// Returns character encoded representations for
// integral values.
//
// "encode" does this for a list of values.
//
// The class maintains an id-counter
// "encodeNextId" increases the counter and
// returns the encoded representation for the new id.
//
// Provides production of character based
// (short) unique identifiers.
//
//
// Encoding:
// nchar =  26, offset =  97 	for lower case letters
// nchar =  26, offset =  65    for upper case letters
//
// Example:
//
// 	char_encode e(26, 97);
//  for(; e.getId() < 50;)
//  {
//  	cout << setw(2) << e.getId() << ": " << e.encodeNextId() << "\n";
//  }
//
//  For multi digit encodings, the first value starts with
//  encoding offset + 1 (e.g. b for nchar=26, offset=97)
//  because the coding for 0 = a will not be printed.
//  From the previous example: x, y, z, ba, bb, bc, ...
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - //


class char_encode
{
public:
	// Constructors
	char_encode(int nchar, int offset):
		id_(0), nchar_(nchar),
		offset_(offset), nb(num_base(nchar)) {}
	~char_encode() {}

	void encode(const list<unsigned> &val, list<string> &res);

	unsigned getId() const { return id_; }
	string encodeNextId();

private:
	typedef std::list<unsigned> lus;
	unsigned id_;
	int nchar_;
	int offset_;
	num_base nb;

	void do_encode(unsigned val, string &s)
	{
		lus l = nb.getList(val);
		lus::const_iterator iter;

		stringstream sst;
		for(iter = l.begin(); iter != l.end(); ++iter)
			sst << (unsigned char) (*iter + offset_);

		s = sst.str();
	}
};

void char_encode::encode(const list<unsigned> &val, list<string> &res)
{
	lus::const_iterator iter;
	string s;
	for(iter = val.begin(); iter != val.end(); ++iter)
	{
		do_encode(*iter, s);
		res.push_back(s);
	}
}

string char_encode::encodeNextId()
{
	string s;
	do_encode(id_++, s);
	return s;
}


#endif /* BASICS_H_ */
