(* ::Package:: *)

(* ::Input:: *)
(*R[n_]:=R[n]=If[n==0,1,Expand[D[R[n-1]u[x],x]]]*)


(* ::Input:: *)
(*ARow[n_]:=ReplaceAll[R[n],{Plus->List,Derivative[k_][u][x]->1,u[x]->1}]*)


(* ::Input:: *)
(*For[n=0,n<=10,n++,Print[ARow[n]]]*)


(* ::Input:: *)
(*(* Attention. L'ordre des valeurs est incorrect. Exemple, ligne n=6, ici on a 1,57,180,34,122, ... tandis que sur OEIS on a 1,57,180,122,34, ...*)
(*L'ordre Mathematica n'est pas exactement l'inverse de l'ordre "Abramowitz and Stegun p. 831" *)*)
