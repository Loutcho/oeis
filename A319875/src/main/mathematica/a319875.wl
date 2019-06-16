(* ::Package:: *)

(* ::Input:: *)
(*a[n_]:=Module[{E=Range[n],P=Table[Prime[k],{k,1,PrimePi[n]}],X,C},X=DeleteDuplicates[Union[Flatten[Outer[Times,P,E]]]];C=Complement[X,E];Length[C]]*)


(* ::Input:: *)
(*Table[a[n],{n,1,60}]*)


(* ::Input:: *)
(*ListPlot[Table[a[n],{n,1,250}]]*)


(* ::Input:: *)
(*(* GPF[n_]:=FactorInteger[n][[-1,1]] -- the greatest prime factor of n -- see A006530 *)*)


(* ::Input:: *)
(*b[n_]:=b[n]=If[n==1,0,b[n-1]+If[PrimeQ[n],n-1,PrimePi[n]-PrimePi[FactorInteger[n][[-1,1]]]]]*)


(* ::Input:: *)
(*Table[b[n],{n,1,50}]*)


(* ::Input:: *)
(*(* empiric proof that a[n] and b[n] are the same: *)*)


(* ::Input:: *)
(*Table[a[n]-b[n],{n,1,350}]//DeleteDuplicates*)


(* ::Input:: *)
(*ListPlot[Table[b[n],{n,1,1000}]]*)
