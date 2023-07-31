:- use_module(library(clpfd)).

a(N, AN) :-
	M is N - 1,
	v(M, AN).

v(4, V4) :- v4(V4).
v(5, V5) :- v5(V5).
v(6, V6) :- v6(V6).

v4(V4) :- aggregate_all(count, solution4(_), V4).
v5(V5) :- aggregate_all(count, solution5(_), V5).
v6(V6) :- aggregate_all(count, solution6(_), V6).

solution4(Q) :-
	Q = [
		Q00,
		Q10, Q01,
		Q20, Q11, Q02,
		Q30, Q21, Q12, Q03
	],

	0 #=< Q00, Q00 #=< 1,

	Q10 #=< Q00,
	Q01 #=< Q00,

	Q20 #=< Q10,
	Q11 #=< Q10 + Q01,
	Q02 #=<       Q01,
	
	Q30 #=< Q20,
	Q21 #=< Q20 + Q11,
	Q12 #=<       Q11 + Q02,
	Q03 #=<             Q02,
	
	Q40 #=< Q30,
	Q31 #=< Q30 + Q21,
	Q22 #=<       Q21 + Q12,
	Q13 #=<             Q12 + Q03,
	Q04 #=<                   Q03,

	Q40 = 0,
	Q31 = 0,
	Q22 = 0,
	Q13 = 0,
	Q04 = 0,
	
	Q00 +
	Q10 + Q01 +
	Q20 + Q11 + Q02 +
	Q30 + Q21 + Q12 + Q03
	#= 4, % nombre d'antidiagonales

	indomain(Q00),
	indomain(Q10), indomain(Q01),
	indomain(Q20), indomain(Q11), indomain(Q02),
	indomain(Q30), indomain(Q21), indomain(Q12), indomain(Q03).
	
solution5(Q) :-
	Q = [
		Q00,
		Q10, Q01,
		Q20, Q11, Q02,
		Q30, Q21, Q12, Q03,
		Q40, Q31, Q22, Q13, Q04
	],

	0 #=< Q00, Q00 #=< 1,

	Q10 #=< Q00,
	Q01 #=< Q00,

	Q20 #=< Q10,
	Q11 #=< Q10 + Q01,
	Q02 #=<       Q01,
	
	Q30 #=< Q20,
	Q21 #=< Q20 + Q11,
	Q12 #=<       Q11 + Q02,
	Q03 #=<             Q02,
	
	Q40 #=< Q30,
	Q31 #=< Q30 + Q21,
	Q22 #=<       Q21 + Q12,
	Q13 #=<             Q12 + Q03,
	Q04 #=<                   Q03,

	Q50 #=< Q40,
	Q41 #=< Q40 + Q31,
	Q32 #=<       Q31 + Q22,
	Q23 #=<             Q22 + Q13,
	Q14 #=<                   Q13 + Q04,
	Q05 #=<                         Q04,

	Q50 = 0,
	Q41 = 0,
	Q32 = 0,
	Q23 = 0,
	Q14 = 0,
	Q05 = 0,
	
	Q00 +
	Q10 + Q01 +
	Q20 + Q11 + Q02 +
	Q30 + Q21 + Q12 + Q03 +
	Q40 + Q31 + Q22 + Q13 + Q04
	#= 5, % nombre d'antidiagonales

	indomain(Q00),
	indomain(Q10), indomain(Q01),
	indomain(Q20), indomain(Q11), indomain(Q02),
	indomain(Q30), indomain(Q21), indomain(Q12), indomain(Q03),
	indomain(Q40), indomain(Q31), indomain(Q22), indomain(Q13), indomain(Q04).
	
solution6(Q) :-
	Q = [
		Q00,
		Q10, Q01,
		Q20, Q11, Q02,
		Q30, Q21, Q12, Q03,
		Q40, Q31, Q22, Q13, Q04,
		Q50, Q41, Q32, Q23, Q14, Q05
	],

	0 #=< Q00, Q00 #=< 1,

	Q10 #=< Q00,
	Q01 #=< Q00,

	Q20 #=< Q10,
	Q11 #=< Q10 + Q01,
	Q02 #=<       Q01,
	
	Q30 #=< Q20,
	Q21 #=< Q20 + Q11,
	Q12 #=<       Q11 + Q02,
	Q03 #=<             Q02,
	
	Q40 #=< Q30,
	Q31 #=< Q30 + Q21,
	Q22 #=<       Q21 + Q12,
	Q13 #=<             Q12 + Q03,
	Q04 #=<                   Q03,

	Q50 #=< Q40,
	Q41 #=< Q40 + Q31,
	Q32 #=<       Q31 + Q22,
	Q23 #=<             Q22 + Q13,
	Q14 #=<                   Q13 + Q04,
	Q05 #=<                         Q04,

	Q60 #=< Q50,
	Q51 #=< Q50 + Q41,
	Q42 #=<       Q41 + Q32,
	Q33 #=<             Q32 + Q23,
	Q24 #=<                   Q23 + Q14,
	Q15 #=<                         Q14 + Q05,
	Q06 #=<                               Q05,

	Q60 = 0,
	Q51 = 0,
	Q42 = 0,
	Q33 = 0,
	Q24 = 0,
	Q15 = 0,
	Q06 = 0,
	
	Q00 +
	Q10 + Q01 +
	Q20 + Q11 + Q02 +
	Q30 + Q21 + Q12 + Q03 +
	Q40 + Q31 + Q22 + Q13 + Q04 +
	Q50 + Q41 + Q32 + Q23 + Q14 + Q05
	#= 6, % nombre d'antidiagonales

	indomain(Q00),
	indomain(Q10), indomain(Q01),
	indomain(Q20), indomain(Q11), indomain(Q02),
	indomain(Q30), indomain(Q21), indomain(Q12), indomain(Q03),
	indomain(Q40), indomain(Q31), indomain(Q22), indomain(Q13), indomain(Q04),
	indomain(Q50), indomain(Q41), indomain(Q32), indomain(Q23), indomain(Q14), indomain(Q05).