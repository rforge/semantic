/*
 * triple.h
 *
 *  Created on: 20.02.2015
 *      Author: kaisers
 */

#ifndef TRIPLE_H_
#define TRIPLE_H_


template<typename _T1, typename _T2, typename _T3>
struct triple
{
	typedef _T1 first_type;
	typedef _T2 second_type;
	typedef _T3 third_type;

     _T1 first;
     _T2 second;
     _T3 third;


     triple(): first(), second() , third() { }

     triple(const _T1& __a, const _T2& __b, const _T3& __c)
     : first(__a), second(__b), third(__c) { }

     // Copy constructor
     template<typename _U1, typename _U2, typename _U3>
     triple(const triple<_U1, _U2, _U3>& __t)
     : first(__t.first), second(__t.second), third(__t.third){ }


     // ToDo: less-than operator?
     // https://gcc.gnu.org/onlinedocs/libstdc++/manual/pairs.html
};

/// Two triples of the same type are equal iff their members are equal.
template<typename _T1, typename _T2, typename _T3>
inline  bool
operator==(const triple<_T1, _T2, _T3>& __x, const triple<_T1, _T2, _T3>& __y)
   { return __x.first == __y.first && __x.second == __y.second && __x.third == __y.third; }

template<typename _T1, typename _T2, typename _T3>
inline bool
operator!=(const triple<_T1, _T2, _T3>& __x, const triple<_T1, _T2, _T3>& __y)
{ return !(__x == __y); }

template<typename _T1, typename _T2, typename _T3>
	inline triple<_T1, _T2, _T3>
  	make_triple(_T1 __x, _T2 __y, _T3 __z)
  	{ return triple<_T1, _T2, _T3>(__x, __y, __z); }



#endif /* TRIPLE_H_ */
