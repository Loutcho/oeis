(* ::Package:: *)

(* ::Input:: *)
(*a[n_]:=Module[{x=Differences[Divisors[n]]},Plus@@(x*Reverse[x])]*)


(* ::Input:: *)
(*Table[a[n],{n,1,40}]*)


(* ::Input:: *)
(*b1[n_]:=Module[{x=a[n]},If[x==(n-1)^ 2,0,x]](*primes*)*)


(* ::Input:: *)
(*b2[n_]:=Module[{x=b1[n]},If[x==(n/2-1)^ 2+3,0,x]](*2*)*)


(* ::Input:: *)
(*b3[n_]:=Module[{x=b2[n]},If[x==(n/3+1)^ 2+8,0,x]](*3*)*)


(* ::Input:: *)
(*b4[n_]:=Module[{x=b3[n]},If[x==(n/4+5)^ 2-9,0,x]](*4*)(*FAUX*)*)


(* ::Input:: *)
(*b5[n_]:=Module[{x=b4[n]},If[x==(n/5+11)^ 2-96,0,x]](*5*)*)


(* ::Input:: *)
(*b7[n_]:=Module[{x=b5[n]},If[x==(n/7+29)^ 2-792,0,x]](*7*)*)


(* ::Input:: *)
(*b11[n_]:=Module[{x=b7[n]},If[x==(n/11+89)^ 2-7800,0,x]](*11*)*)


(* ::Input:: *)
(*ListPlot[Table[b3[n],{n,1,5000}]]*)


(* ::Input:: *)
(*F[x_]:=x^2-x-1*)


(* ::Input:: *)
(*G[p_]:={F[p-1],p^2-F[p-1]^2}*)


(* ::Input:: *)
(*G[1]*)
