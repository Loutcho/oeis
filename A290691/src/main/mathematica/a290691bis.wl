(* ::Package:: *)

(* ::Input:: *)
(*(* MP = version Positive = tableau des ((+n) % k) tri\[EAcute] dans l'ordre croissant des |k-n/k| *)*)


(* ::Input:: *)
(*MP[N_ ]:=(Last/@#&)/@Sort /@Table[{Abs[k-n/k],Mod[n,k]},{n,1,N},{k,1,N}]*)


(* ::Input:: *)
(*(* MN = version N\[EAcute]gative = tableau des ((-n) % k) tri\[EAcute] dans l'ordre croissant des |k-n/k| *)*)


(* ::Input:: *)
(*MN[N_]:=(Last/@#&)/@Sort /@Table[{Abs[k-n/k],Mod[-n,k]},{n,1,N},{k,1,N}]*)


(* ::Input:: *)
(*F[N_]:=MatrixForm/@{MP[N],MN[N],(MP[N]+MN[N]),(MP[N]-MN[N])}*)


(* ::Input:: *)
(*F[10]*)


(* ::Input:: *)
(*Eigensystem[MN[4]]*)


(* ::Input:: *)
(*Factor[CharacteristicPolynomial[MN[4],x] ,Extension->GoldenRatio]*)


(* ::Input:: *)
(*Schema[NMax_]:=Table[ArrayPlot[MN[2^N],ColorRules->{0->Green}],{N,1,NMax}]*)


(* ::Input:: *)
(*Schema[8]*)
