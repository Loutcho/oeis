(* ::Package:: *)

(* ::Input:: *)
(*T[n_]:=n*(n+1)/2*)
(*R[n_]:=(Sqrt[8*n+1]-1)/2*)
(*S[0]:=0*)
(*S[d_]:=S[d]=Module[{r=R[d]},If[IntegerQ[r],r++;r+T[r],Min[S[#[[1]]]+S[#[[2]]]&/@IntegerPartitions[d,{2}]]]]*)
(*A0[n_]:=Sum[Boole[d+S[d]<=2*n],{d,0,n}]*)
(*A[n_]:=A0[T[n]]*)
(*For[n=1,n<=500,n++,Print[n," ",A[n]]]*)

