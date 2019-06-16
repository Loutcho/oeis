(* ::Package:: *)

(* ::Input:: *)
(*F[n_] := Floor[(1/4)*n^2]*)


(* ::Input:: *)
(*A[n_]:=(Sum[DivisorSigma[0,k],{k,1,F[n+1]}]-n*(n+1)/2)/2*)


(* ::Input:: *)
(*Table[A[n],{n,1,100}]*)
