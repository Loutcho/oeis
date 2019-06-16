(* ::Package:: *)

(* ::Input:: *)
(*T[A_,k_]:=If[KeyExistsQ[A,k],A[k],0]*)


(* ::Input:: *)
(*G[n_]:=Module[{A=<||>,k,j,i},For[j=2,j<=n,j++,For[i=1,i<j,i++,k=Floor[n*(1/i-1/j)];A[k]=T[A,k]+1]];A]*)


(* ::Input:: *)
(*For[n=1,n<=23,n++,Print[Block[{A=G[n]},T[A,#]&/@Range[0,n-1]]]]*)


(* ::Input:: *)
(*GG[n_]:=GG[n]=G[n]*)


(* ::Input:: *)
(*TT[n_,k_]:=T[GG[n],k]*)


(* ::Input:: *)
(*Table[Log[0.1+TT[n,k]],{n,1,300},{k,0,n-1}]//ArrayPlot[#,ColorFunction->ColorData["RustTones"],PixelConstrained->{1,1}]&*)


(* ::Input:: *)
(**)


(* ::Input:: *)
(**)
