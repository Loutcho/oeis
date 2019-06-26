(* ::Package:: *)

(* ::Input:: *)
(*AA[(a[m_]~times~b[i_,j_])~ times~(a[n_]~ times~b[k_,l_])]:=*)
(*Module[*)
(*{s=m+n,d=Abs[m-n],o=Min[m,n]},*)
(*If[*)
(*s==d,*)
(*a[s]~times~b[i+k,j+l],*)
(*(a[s]~times~b[i+k,j+l])~plus~(a[d]~times~b[i+k+o,j+l+o])*)
(*]*)
(*]*)


(* ::Input:: *)
(*AA[sum[P_]~times~sum[Q_]]:=sum[AA/@Flatten[Outer[times,P,Q],1]]*)


(* ::Input:: *)
(*(* TBC \[Equal] FIXME: EquilibrateSum[List[L_]]:=GatherBy[L,MatchQ[#,plus[_,_]]&] *)*)


(* ::Input:: *)
(*AA[sum[P_]~plus~sum[Q_]]:=sum[Union[P, Q]]*)


(* ::Input:: *)
(*P1=sum[{a[1]~times~b[0,0]}];P1//TreeForm*)


(* ::Input:: *)
(*Q1=sum[{a[0]~times~b[1,1]}];Q1//TreeForm*)


(* ::Input:: *)
(*P2=AA[P1~plus~Q1];P2//TreeForm*)


(* ::Input:: *)
(*Q2=AA[P1~times~Q1];Q2//TreeForm*)


(* ::Input:: *)
(*P3=AA[P2~plus~Q2];P3//TreeForm (* FIXME: factorize by a(1) *)*)


(* ::Input:: *)
(*Q3=AA[P2~times~Q2];Q3//TreeForm (* FIXME: sum(a, b+c) is sum(a,b,c) *)*)
