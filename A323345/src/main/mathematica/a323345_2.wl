(* ::Package:: *)

(* ::Input:: *)
(*a[n_,k_]:=Module[{c=0,x=n,i=1},While[x>=1,If[IntegerQ[x],c++];i++;x=n/i-(k/2)*(i-1)];c]*)


(* ::Input:: *)
(*S[n_,k_]:=Module[{L={},x=n,i=1},While[x>=1,If[IntegerQ[x],AppendTo[L,i]];i++;x=n/i-(k/2)*(i-1)];L]*)


(* ::Input:: *)
(*S[12,5]*)


(* ::Input:: *)
(*P[n_,k_]:=n*(n*(k-2)-k+4)/2 (* nth k-gonal number *)*)


(* ::Input:: *)
(*b[n_,k_]:=Module[{c=0,i=1,x},x=P[i,k+2];While[n>=x,c+=Boole[i\[Divides](n-x)];i++;x=P[i,k+2]];c]*)


(* ::Input:: *)
(*Table[{a[n,k],b[n,k]},{n,1,50},{k,0,9}]//Grid*)


(* ::Input:: *)
(*TT[n_,k_]:=Module[{c=0,i=1,x=n},While[x>=1,If[IntegerQ[x],c++];i++;x=(i-1)*(x-k)/i];c]*)
(*A004736[n_]:=Binomial[Floor[3/2+Sqrt[2*n]],2]-n+1*)
(*A002260[n_]:=n-Binomial[Floor[1/2+Sqrt[2*n]],2]*)
(*a[n_]:=TT[A004736[n],A002260[n]-1]*)
(*Table[a[n],{n,1,91}]*)
