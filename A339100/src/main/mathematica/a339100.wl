(* ::Package:: *)

(* ::Input:: *)
(*T[n_,k_]:=T[n,k]=If[n==0&&k==0,1,If[n==0||k<0||k>n,0,(4*n-2*k-1)*T[n-1,k-1]+(2*k+1)*T[n-1,k]]]*)


(* ::Input:: *)
(*A[n_]:=Table[(2*n-k)*T[n,k]+(k+1)*T[n,k+1],{k,0,n}]/.{List->GCD}*)


(* ::Input:: *)
(*Table[A[n],{n,1,100}]*)


(* ::Input:: *)
(*SchemaRow[n_,kMax_]:=If[#==0," ",#]&@IntegerExponent[n,#]&/@Table[Prime[i],{i,1,kMax}]*)


(* ::Input:: *)
(*Schema[n_]:=Grid[Table[F[A[2*k],PrimePi[2*n]],{k,1,n}]]*)


(* ::Input:: *)
(*Schema[80]*)
