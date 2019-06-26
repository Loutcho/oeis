(* ::Package:: *)

(* ::Input:: *)
(*Ev[E_]:=Module[{x,dx},x=First[E];dx=Last[E];If[x==0 && dx<0, {-dx,-dx},{x+dx,dx}]]*)


(* ::Input:: *)
(*EvL[n_,L_]:=Module[{LL},LL=Ev/@L;LL=Sort[LL];LL=Append[LL,{n-1,-1/n}];LL]*)


(* ::Input:: *)
(*It[nStart_,nEnd_,LStart_]:=Module[{n,LL},For[n=nStart;LL=LStart,n<=nEnd,n++,LL=EvL[n,LL]];LL]*)


(* ::Input:: *)
(*Encours[n_]:=It[2,n,{}]*)


(* ::Input:: *)
(*Countdown[x_,dx_]:=If[dx>0,(Ceiling[x]-x)/dx,(Floor[x]-x)/dx]*)


(* ::Input:: *)
(*A[n_]:=Drop[Apply[Countdown,#]&/@Encours[n],-1]*)


(* ::Input:: *)
(*Table[A[n],{n,2,25}]//Flatten*)
