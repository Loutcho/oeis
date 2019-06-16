(* ::Package:: *)

(* ::Input:: *)
(*L[n_]:=Table[Binomial[n,k],{k,1,Floor[n/2]}]*)


(* ::Input:: *)
(*c[n_]:=Complement[Prime/@Range[PrimePi[n]],First/@FactorInteger[Times@@L[n]]]*)


(* ::Input:: *)
(*a[n_]:=Module[{x=c[n]},If[x=={},1,First[x]]]*)


(* ::Input:: *)
(*Table[a[n],{n,1,100}]*)


(* ::Input:: *)
(*file=OpenWrite["b056609.txt"];*)
(*WriteString[file,"# A056609\n"];*)
(*For[n=1,n<=1000,n++,WriteString[file, n, " ",a[n],"\n"]];*)
(*Close[file]*)
