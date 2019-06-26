(* ::Package:: *)

(* ::Input:: *)
(*f[0]:=x*)
(*f[n_]:=Module[{c,k},c=IntegerExponent[n,2];k=(n/2^c-1)/2;o[f[c],f[k]]]*)


(* ::Input:: *)
(*g[x]:=0*)
(*g[o[C_,K_]]:=(2^g[C])(2g[K]+1)*)


(* ::Input:: *)
(*r[x]:=x*)
(*r[o[C_,K_]]:=o[r[K],r[C]]*)


(* ::Input:: *)
(*(s[x]:=x);(s[o[SC_,SK_]]:=o[SK,SC])*)


(* ::Input:: *)
(*a[n_]:=g@r@f[n]*)


(* ::Input:: *)
(*b[n_]:=g@s@f[n]*)


(* ::Input:: *)
(*Table[a[n],{n,1,30}]*)


(* ::Input:: *)
(*Table[b[n],{n,1,30}]*)


(* ::Input:: *)
(*g[r[f[39]][[1]]]*)


(* ::Input:: *)
(*{f[21]//TreeForm,s[f[21]]//TreeForm,r[f[21]]//TreeForm}*)


(* ::Input:: *)
(*Composition*)


(* ::Input:: *)
(*2^ 14*7*)


(* ::Input:: *)
(*a[409600]*)


(* ::Input:: *)
(*f[50176]//TreeForm*)


(* ::Input:: *)
(*2^ 400*)
