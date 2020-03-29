(* ::Package:: *)

(* ::Input:: *)
(*a[n_]:=Module[{p},p=Table[Prime[i]-2,{i,2,PrimePi[2*n+3]}];Length[IntegerPartitions[2*n+1,{0,Infinity},p]]]*)


(* ::InheritFromParent:: *)
(*Table[a[n],{n,0,60}]*)
