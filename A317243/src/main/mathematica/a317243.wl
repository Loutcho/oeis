(* ::Package:: *)

(* ::Input:: *)
(*A317243[n_]:=Length[Union[Floor/@Select[n/#&/@Range[n],Not[IntegerQ[#]]&]]];Table[A317243[n],{n,1,80}]*)


(* ::Input:: *)
(*(*EXAMPLE*)*)


(* ::Input:: *)
(*9/#&/@Range[9]*)


(* ::Input:: *)
(*Select[9/#&/@Range[9],Not[IntegerQ[#]]&]*)


(* ::Input:: *)
(*{#,Floor[#]}&/@Select[9/#&/@Range[9],Not[IntegerQ[#]]&]*)


(* ::Input:: *)
(*(*when k is authorized to divide n:*)*)


(* ::Input:: *)
(*A055086[n_]:=Length[Union[Floor/@(n/#&/@Range[n])]];Table[A055086[n],{n,1,80}]*)
