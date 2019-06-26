(* ::Package:: *)

(* ::Input:: *)
(*L[n_]:=Which[*)
(*n==0,2,*)
(*n==1,P,*)
(*True,Expand[P^n-Sum[Binomial[n,k]*L[n-2*k]*Q^k,{k,1,Floor[(n-1)/2]}]]-If[EvenQ[n],Binomial[n,n/2]*Q^(n/2),0]*)
(*]*)


(* ::Input:: *)
(*Reverse/@CoefficientList[Table[L[n],{n,0,20}]/.P->x/.Q->1,x]//Grid*)


(* ::Input:: *)
(*(* Up to the sign, it appears to be A162514 *)*)
(**)
