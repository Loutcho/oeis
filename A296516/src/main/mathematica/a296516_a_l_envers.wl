(* ::Package:: *)

(* ::Input:: *)
(*a[n_]:=If[n==0,x,a[n-1]*c[n-1]]*)


(* ::Input:: *)
(*b[n_]:=If[n==0,1,b[n-1]*d[n-1]]*)


(* ::Input:: *)
(*c[n_]:=If[n==0,y,a[n-1]*c[n-1]]*)


(* ::Input:: *)
(*d[n_]:=If[n==0,1,a[n-1]*d[n-1]+b[n-1]*c[n-1]]*)


(* ::Input:: *)
(**)


(* ::Input:: *)
(*Map[MatrixPlot,(CoefficientList[#,{x,y}]&)/@Table[{b[n]},{n,1,10}],{2}]*)
