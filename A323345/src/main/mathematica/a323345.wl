(* ::Package:: *)

(* ::Input:: *)
(*T[n_,k_]:=Module[{c=0,i=0,x=n},While[x>=1,If[IntegerQ[x],c++];i++;x=i*(x-k)/(i+1)];c]*)


(* ::Input:: *)
(*Table[T[n,0],{n,1,30}](*A000005*)*)


(* ::Input:: *)
(*Table[T[n,1],{n,1,30}](*A001227*)*)


(* ::Input:: *)
(*Table[T[n,2],{n,1,30}] (*A038548*)*)


(* ::Input:: *)
(*Table[T[n,3],{n,1,30}](*A117277*)*)


(* ::Input:: *)
(*{MatrixPlot[#],Grid[#]}&@Table[T[n,k],{n,1,20},{k,0,19}]*)


(* ::Input:: *)
(*(*Variante (inefficace) bas\[EAcute]e sur la d\[EAcute]finition par partitions en progression arithm\[EAcute]tique de raison k: *)*)


(* ::Input:: *)
(*ArithmeticProgressionQ[L_,k_]:=ContainsAll[{k},Differences[L]]*)


(* ::Input:: *)
(*TT[n_,k_]:=Length[Select[IntegerPartitions[n],ArithmeticProgressionQ[#,-k ]&]]*)
(*(*Fin de variante*)*)


(* ::Input:: *)
(*A004736[n_]:=Binomial[Floor[3/2+Sqrt[2*n]],2]-n+1*)


(* ::Input:: *)
(*A002260[n_]:=n-Binomial[Floor[1/2+Sqrt[2*n]],2]*)


(* ::Input:: *)
(*a[n_]:=T[A004736[n],A002260[n]-1]*)


(* ::Input:: *)
(*Table[a[n],{n,1,91}]*)


(* ::Input:: *)
(*(* Pattern r\[EAcute]cursif int\[EAcute]ressant : *)*)


(* ::Input:: *)
(*U[a_,n_,k_]:=If[a==0,T[n,k],U[a-1,n+a*k,k]-Boole[a\[Divides]n]]*)


(* ::Input:: *)
(*Table[U[10,n,k],{n,1,11},{k,0,9}]//Grid*)


(* ::Input:: *)
(*V[n_,k_]:=T[n,k]-T[n,k+1]-Boole[n==3*(k+1)]*)


(* ::Input:: *)
(*W[n_,k_]:=V[n,k]-V[n,k+2]*)


(* ::Input:: *)
(*Table[T[n,k],{n,1,100},{k,0,99}]//MatrixPlot*)


(* ::Input:: *)
(*P[n_,k_]:=Module[{z=n+n*(n-1)/2*k},Print[z];((T[z,k])==(Sum[Boole[i\[Divides](n+Sum[j,{j,i,n-1}])],{i,1,n}]))]*)


(* ::Input:: *)
(*P[12,4]*)



