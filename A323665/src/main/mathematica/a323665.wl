(* ::Package:: *)

(* ::Input:: *)
(*nEdges[n_]:=If[n==0,0,Module[{c,xx,k},c=IntegerExponent[n,2];xx=n/2^c;k=(xx-1)/2;Boole[c!=0]*(1+nEdges[c])+Boole[k!=0]*(1+nEdges[k])]]*)


(* ::Input:: *)
(*a[n_]:=nEdges[n]+1*)


(* ::Input:: *)
(*a[8!]*)


(* ::Input:: *)
(*b[n_]:=If[n==0,Null,Module[{c,xx,k},c=IntegerExponent[n,2];xx=n/2^c;k=(xx-1)/2;o[If[c!=0,b[c],Null],If[k!=0,b[k],Null]]]]*)


(* ::Input:: *)
(*Table[b[n]//TreeForm,{n,1,10 }]*)


(* ::Input:: *)
(*IntegerExponent[720,2]*)


(* ::Input:: *)
(*720/2^ 4*)


(* ::Input:: *)
(*a[6!]*)


(* ::Input:: *)
(*b[6!]//TreeForm*)


(* ::Input:: *)
(*Table[a[n],{n,1,80}]*)


(* ::Input:: *)
(*Table[a[n],{n,1,50000}]//ListPlot*)
