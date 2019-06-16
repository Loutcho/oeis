(* ::Package:: *)

(* ::Input:: *)
(*U[n_]:=Floor[Fibonacci[n+1]/2]+2*Sum[Floor[(Fibonacci[n+1]-d)/2],{d,1,n-2}]+2*)


(* ::Input:: *)
(*U[1]*)


(* ::Input:: *)
(*U[2]*)


(* ::Input:: *)
(*U[3]*)


(* ::Input:: *)
(*U[4]*)


(* ::Input:: *)
(*Table[U[n],{n,1,20}]*)


(* ::Input:: *)
(*(* different from A5207 ??? Une erreur quelque part ? *)*)


(* ::Input:: *)
(*UU[n_]:=(Fibonacci[2n-1]+Fibonacci[n+1])/2+1*)


(* ::Input:: *)
(*Table[UU[n],{n,1,20}]*)


(* ::Input:: *)
(*Table[(UU[n]-U[n])/2,{n,1,20}]*)
