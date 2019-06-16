(* ::Package:: *)

(* ::Input:: *)
(*F[\[Theta]_]:=Sin[\[Theta]]/\[Theta]*)


(* ::Input:: *)
(*(* F est la fonction qui relie theta \[AGrave] rho en coordonn\[EAcute]es polaires *)*)
(*(* Equation de la tangente \[AGrave] la courbe \[Rule] chercher l'angle V dans le rep\[EGrave]re mobile (u,v), entre u et le vecteur directeur de la tangente *) *)
(*(* tan(V) satisfait alors tan(V) = F(theta) / F'(theta) *)*)
(*(* V + theta est l'angle entre le vecteur i du rep\[EGrave]re fixe et le vecteur directeur de la tangente *)*)
(*(* cette fiche de calculs contient des manipulation alg\[EAcute]briques autour du calcul de tan(V + theta) et tente de voir s'il n'y aurait pas une simplification cach\[EAcute]e... *)*)


(* ::Input:: *)
(*F'[\[Theta]]*)


(* ::Input:: *)
(*Simplify[Tan[V+\[Theta]]==(Tan[V]+Tan[\[Theta]])/(1-Tan[V]*Tan[\[Theta]])]*)


(* ::Input:: *)
(*Simplify[F[\[Theta]]/F'[\[Theta]]]*)


(* ::Input:: *)
(*Simplify[(Tan[V]+Tan[\[Theta]])/(1-Tan[V]*Tan[\[Theta]])/. Tan[V]->F[\[Theta]]/F'[\[Theta]]]*)


(* ::Input:: *)
(*Simplify[(Tan[\[Theta]]-2\[Theta])/(1+\[Theta]*(Tan[\[Theta]]-Cot[\[Theta]]))] *)


(* ::Input:: *)
(*Simplify[(-2 \[Theta]+Tan[\[Theta]])/(1-\[Theta] Cot[\[Theta]]+\[Theta] Tan[\[Theta]])==(Sin[\[Theta]] (-2 \[Theta]+Tan[\[Theta]]))/(-\[Theta] Cos[\[Theta]]+Sin[\[Theta]]+\[Theta] Sin[\[Theta]] Tan[\[Theta]])]*)


(* ::Input:: *)
(*Simplify[Tan[\[Theta]]-Cot[\[Theta]]==2Tan[2\[Theta]-Pi/2]]*)


(* ::Input:: *)
(*Simplify[Tan[\[Theta]]-Cot[\[Theta]]==-2Cot[2\[Theta]]]*)


(* ::Input:: *)
(*G[\[Theta]_]:=(-2 \[Theta]+Tan[\[Theta]])/(1+2\[Theta] Cot[2\[Theta]])*)


(* ::Input:: *)
(*H[\[Theta]_]:=Mod[ArcTan[G[\[Theta]]]+2*\[Theta],Pi,-Pi/2]*)


(* ::Input:: *)
(*Plot[{H[\[Theta]],H'[\[Theta]]},{\[Theta],-2*Pi,2*Pi}]*)


(* ::Input:: *)
(*FullSimplify[Integrate[H'[\[Theta]],\[Theta]]]*)


(* ::Input:: *)
(*H[Pi/2]*)


(* ::Input:: *)
(*H[0.00000001]//N*)


(* ::Input:: *)
(*K[o_, \[Theta]_]:=-Sin[o]^2/(o*Sin[\[Theta]-2*o]-Sin[o]*Sin[\[Theta]-o])*)


(* ::Input:: *)
(*K1[o_]:=PolarPlot[{Sinc[\[Theta]],K[o,\[Theta]]},{\[Theta],0,4*Pi},PlotRange->{{-GoldenRatio,+GoldenRatio}, {-1,+1}}]*)


(* ::Input:: *)
(*K1[Pi/2]*)


(* ::Input:: *)
(**)
