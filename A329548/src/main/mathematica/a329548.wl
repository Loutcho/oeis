(* ::Package:: *)

(* ::Input:: *)
(*(*Prog OEIS:*)*)


(* ::Input:: *)
(*l[k_]:=Sort[(Join[#,1/#]&)@Table[m^2/k,{m,1,k}]]*)


(* ::Input:: *)
(*s[c_,d_]:={Min[c,d],Max[c,d]}*)


(* ::Input:: *)
(*f[k_]:=Module[{L,LL},L=l[k];LL=Take[l[k+1],{2,2*k+1}];MapThread[s,{Ratios[L],Ratios[LL]}]]*)


(* ::Input:: *)
(*a[n_]:=Length[DeleteDuplicates[f[n]]]*)


(* ::Input:: *)
(*Table[a[n],{n,1,50}]*)


(* ::Input:: *)
(*(*Experimentations:*)*)


(* ::Input:: *)
(*n=1;While[n<=100,WriteString["stdout", a[n], ", "];n++]*)


(* ::Input:: *)
(*l[3]*)


(* ::Input:: *)
(*f[3]*)


(* ::Input:: *)
(*ListPlot[Table[n-a[n],{n,1,500}]]*)


(* ::Input:: *)
(*500-a[500]*)


(* ::Input:: *)
(*estim[n_]:=(n-a[n])/Sqrt[n]*)


(* ::Input:: *)
(*estim[90000]*)


(* ::Input:: *)
(*%//N*)


(* ::Input:: *)
(*graph[nmax_]:=ListPlot[Table[n/a[n],{n,1,nmax}]]*)


(* ::Input:: *)
(*graph[1000]*)
