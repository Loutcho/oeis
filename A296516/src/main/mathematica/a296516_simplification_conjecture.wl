(* ::Package:: *)

(* ::Input:: *)
(*F[n_]:=Fibonacci[n]*)
(*S[n_]:=Sum[F[k],{k,1,n}]*)


(* ::Input:: *)
(*a[n_,l_]:=n-2-l*)


(* ::Input:: *)
(*B[n_]:=F[n+2]-1*)


(* ::Input:: *)
(*Table[B[n]==S[n],{n,1,10}]*)


(* ::Input:: *)
(*Table[B[l]-B[l-1]==F[l],{l,1,10}]*)


(* ::Input:: *)
(*v[n_,k_]:=Sum[(n-2-l)*F[l],{l,k+1,n-3}]*)


(* ::Input:: *)
(*v1[n_,k_]:=Sum[(n-2-l)*(B[l]-B[l-1]),{l,k+1,n-3}]*)


(* ::Input:: *)
(*v2[n_,k_]:=Sum[(n-2-l)*B[l],{l,k+1,n-3}]-Sum[(n-2-l)*B[l-1],{l,k+1,n-3}]*)


(* ::Input:: *)
(*v3[n_,k_]:=Sum[a[n,l]*B[l],{l,k+1,n-3}]-Sum[a[n,L+1]*B[L],{L,k,n-4}]*)


(* ::Input:: *)
(*v4[n_,k_]:=-Sum[(a[n,l+1]-a[n,l])*B[l],{l,k+1,n-4}]+a[n,n-3]*B[n-3]-a[n,k+1]*B[k]*)


(* ::Input:: *)
(*v5[n_,k_]:=Sum[F[l+2]-1,{l,k+1,n-4}]+F[n-1]-1-(n-k-3)*(F[k+2]-1)*)


(* ::Input:: *)
(*v6[n_,k_]:=F[n]+F[n-1]-(n-k-3)*F[k+2]-F[k+4]*)


(* ::Input:: *)
(*v7[n_,k_]:=F[n+1]-(n-k-3)*F[k+2]-F[k+4]*)


(* ::Input:: *)
(*cmp[n_,k_]:=If[n-k<4,Red,v[n,k]-v7[n,k]]*)


(* ::Input:: *)
(*Table[cmp[n,k],{n,1,10},{k,1,10}]//MatrixForm*)


(* ::Input:: *)
(*w[n_]:=Sum[(n-k-3)*F[k]*F[k+2],{k,1,n-4}]*)


(* ::Input:: *)
(*a[n_,k_]:=n-k-3*)


(* ::Input:: *)
(*b[n_]:=F[n]*F[n+2]*)


(* ::Input:: *)
(*B[n_]:=Sum[b[k],{k,1,n}]*)


(* ::Input:: *)
(*Table[w[n],{n,1,10}]*)


(* ::Input:: *)
(*w1[n_]:=Sum[a[n,k]*b[k],{k,1,n-4}]*)


(* ::Input:: *)
(*w2[n_]:=Sum[a[n,k]*(B[k]-B[k-1]),{k,1,n-4}]*)


(* ::Input:: *)
(*w3[n_]:=Sum[a[n,k]*B[k],{k,1,n-4}]-Sum[a[n,k]*B[k-1],{k,1,n-4}]*)


(* ::Input:: *)
(*w4[n_]:=Sum[a[n,k]*B[k],{k,1,n-4}]-Sum[a[n,j+1]*B[j],{j,0,n-5}]*)


(* ::Input:: *)
(*w5[n_]:=Sum[F[l]*F[l+2],{l,1,n-4}]+Sum[Sum[F[l]F[l+2],{l,1,k}],{k,1,n-5}]*)


(* ::Input:: *)
(*w5w[n_]:=Sum[F[l]*F[l+2],{l,1,n-4}]*)


(* ::Input:: *)
(*w5w1[n_]:=If[n>=5,F[n-3]*F[n-2]-((-1)^(n-4)+1)/2*F[2],0]*)


(* ::Input:: *)
(*w5ww[n_]:=Sum[Sum[F[l]F[l+2],{l,1,k}],{k,1,n-5}]*)


(* ::Input:: *)
(*w5ww1[n_]:=Sum[F[k+1]*F[k+2]-Mod[k+1,2],{k,1,n-5}]*)


(* ::Input:: *)
(*w6[n_]:=If[n>=5,F[n-3]*F[n-2]-Mod[n+1,2],0]+Sum[F[k+1]*F[k+2]-Mod[k+1,2],{k,1,n-5}]*)


(* ::Input:: *)
(*w7[n_]:=If[n>=5,F[n-3]*F[n-2]-Mod[n+1,2],0]+Sum[F[k+1]*F[k+2],{k,1,n-5}]-If[n>=5,Floor[(n-5)/2],0]*)


(* ::Input:: *)
(*w8[n_]:=If[n>=5,F[n-3]*F[n-2]-Mod[n+1,2]-Floor[(n-5)/2],0]+Sum[F[k+1]*F[k+2],{k,1,n-5}]*)


(* ::Input:: *)
(*w9[n_]:=If[n>=5,F[n-3]*F[n-2]-Mod[n+1,2]-Floor[(n-5)/2],0]+Sum[F[l]*F[l+1],{l,2,n-4}]*)


(* ::Input:: *)
(*w10[n_]:=If[n>=5,F[n-3]*F[n-2]-Mod[n+1,2]-Floor[(n-5)/2],0]+Sum[F[l]*F[l+1],{l,1,n-4}]-If[n>=5,1,0]*)


(* ::Input:: *)
(*w11[n_]:=If[n>=5,F[n-3]*F[n-2]-Mod[n+1,2]-Floor[(n-5)/2]-1,0]+Sum[F[l]*F[l+1],{l,1,n-4}]*)


(* ::Input:: *)
(*w12[n_]:=If[n>=5,F[n-3]*F[n-2]-Mod[n+1,2]-Floor[(n-5)/2]-1,0]+If[n>=5,F[n-3]*F[n-3]-Mod[(n+1),2],0]*)


(* ::Input:: *)
(*w13[n_]:=If[n>=5,F[n-3]*F[n-2]-Mod[n+1,2]-Floor[(n-5)/2]-1+F[n-3]*F[n-3]-Mod[(n+1),2],0]*)


(* ::Input:: *)
(*w14[n_]:=If[n>=5,F[n-3]*F[n-2]-2*Mod[n+1,2]-Floor[(n-5)/2]-1+F[n-3]*F[n-3],0]*)


(* ::Input:: *)
(*w15[n_]:=If[n>=5,F[n-3]*F[n-1]-2*Mod[n+1,2]-Floor[(n-5)/2]-1,0]*)


(* ::Input:: *)
(*w16[n_]:=If[n>=4,F[n-3]*F[n-1]-2*Mod[n+1,2]-Floor[(n-3)/2],0]*)


(* ::Input:: *)
(*cmpw[n_]:=w[n]-w16[n]*)


(* ::Input:: *)
(*Table[cmpw[n],{n,1,20}]*)


(* ::Input:: *)
(*u[n_]:=Sum[F[k],{k,1,n-4}]*)


(* ::Input:: *)
(*Table[u[n],{n,1,20}]*)


(* ::Input:: *)
(*u1[n_]:=If[n>=4,F[n-2]-1,0]*)


(* ::Input:: *)
(*Table[u1[n]-u[n],{n,1,20}]*)


(* ::Input:: *)
(*z[n_]:=Sum[F[k]*F[k+4],{k,1,n-4}]*)


(* ::Input:: *)
(*Table[z[n],{n,1,20}]*)


(* ::Input:: *)
(*z1[n_]:=If[n>=4,F[n-3]*F[n]-3*Mod[n+1,2],0]*)


(* ::Input:: *)
(*Table[z1[n]-z[n],{n,1,20}]*)


(* ::Input:: *)
(*T[n_]:=n*(n+1)/2*)


(* ::Input:: *)
(*V[n_]:=Sum[F[k]*(Sum[(n-2-l)*F[l],{l,k+1,n-3}]),{k,1,n-4}]*)


(* ::Input:: *)
(*W[n_]:=Sum[(n-2-l)*T[F[l]],{l,1,n-3}]*)


(* ::Input:: *)
(*VV1[n_]:=F[n+1]*Sum[F[k],{k,1,n-4}]-Sum[(n-k-3)*F[k]*F[k+2],{k,1,n-4}]-Sum[F[k]*F[k+4],{k,1,n-4}]*)


(* ::Input:: *)
(*VV2[n_]:=F[n+1]*If[n>=4,F[n-2]-1,0]-If[n>=4,F[n-3]*F[n-1]-2*Mod[n+1,2]-Floor[(n-3)/2],0]-If[n>=4,F[n-3]*F[n]-3*Mod[n+1,2],0]*)


(* ::Input:: *)
(*VV3[n_]:=If[n>=4,F[n+1]*(F[n-2]-1),0]-If[n>=4,F[n-3]*F[n-1]-2*Mod[n+1,2]-Floor[(n-3)/2],0]-If[n>=4,F[n-3]*F[n]-3*Mod[n+1,2],0]*)


(* ::Input:: *)
(*VV4[n_]:=If[n>=4,F[n+1]*(F[n-2]-1)-(F[n-3]*F[n-1]-2*Mod[n+1,2]-Floor[(n-3)/2])-(F[n-3]*F[n]-3*Mod[n+1,2]),0]*)


(* ::Input:: *)
(*VV5[n_]:=If[n>=4,-F[n+1]+F[n+1]*F[n-2]-F[n-3]*F[n-1]+2*Mod[n+1,2]+Floor[(n-3)/2]-F[n-3]*F[n]+3*Mod[n+1,2],0]*)


(* ::Input:: *)
(*VV6[n_]:=If[n>=4,-F[n+1]+F[n+1]*F[n-2]-F[n-3]*F[n]-F[n-3]*F[n-1]+2*Mod[n+1,2]+Floor[(n-3)/2]+3*Mod[n+1,2],0]*)


(* ::Input:: *)
(*VV7[n_]:=If[n>=4,-F[n+1]+F[n+1]*F[n-2]-F[n-3]*F[n+1]+5*Mod[n+1,2]+Floor[(n-3)/2],0]*)


(* ::Input:: *)
(*Table[V[n],{n,1,20}]*)


(* ::Input:: *)
(*Table[VV7[n],{n,1,20}]*)


(* ::Input:: *)
(*y[n_]:=Sum[(n-2-k)*F[k],{k,1,n-3}]*)


(* ::Input:: *)
(*Table[y[n],{n,1,20}]*)


(* ::Input:: *)
(*y1[n_]:=If[n>=4,F[n-1]-1+Sum[F[k+2]-1,{k,1,n-4}],0]*)


(* ::Input:: *)
(*y2[n_]:=If[n>=4,F[n-1]-1+Sum[F[k+2],{k,1,n-4}]-Sum[1,{k,1,n-4}],0]*)


(* ::Input:: *)
(*y3[n_]:=If[n>=4,F[n-1]-1+Sum[F[k],{k,3,n-2}]-Sum[1,{k,1,n-4}],0]*)


(* ::Input:: *)
(*y4[n_]:=If[n>=4,F[n-1]-1+F[n]-3-Sum[1,{k,1,n-4}],0]*)


(* ::Input:: *)
(*y5[n_]:=If[n>=4,F[n+1]-4-Sum[1,{k,1,n-4}],0]*)


(* ::Input:: *)
(*y6[n_]:=If[n>=4,F[n+1]-n,0]*)


(* ::Input:: *)
(*Table[y6[n],{n,1,20}]*)


(* ::Input:: *)
(*x[n_]:=Sum[(n-2-k)*F[k]^2,{k,1,n-3}]*)


(* ::Input:: *)
(*Table[x[n],{n,1,20}]*)


(* ::Input:: *)
(*x1[n_]:=Sum[F[j]^ 2,{j,1,n-3}]+Sum[Sum[F[j]^ 2,{j,1,k}],{k,1,n-4}]*)


(* ::Input:: *)
(*x2[n_]:=If[n>=4,F[n-3]*F[n-1]-Mod[n+1,2],0]*)


(* ::Input:: *)
(*Table[x2[n]-x[n],{n,1,20}]*)


(* ::Input:: *)
(*WW1[n_]:=If[n>=4,F[n+1]-n,0]+If[n>=4,F[n-3]*F[n-1]-Mod[n+1,2],0]*)


(* ::Input:: *)
(*Table[Column[{W[n],WW1[n]/2}],{n,1,15}]*)


(* ::Input:: *)
(*AA[n_]:=(F[n+1])^2-T[n-1]-T[F[n-1]]-2*V[n]-2W[n]*)


(* ::Input:: *)
(*AA1[n_]:=(F[n+1])^2-T[n-1]-T[F[n-1]]-2*(V[n]+W[n])*)


(* ::Input:: *)
(*AA2[n_]:=(F[n+1])^2-T[n-1]-T[F[n-1]]-If[n>=4,2*(-F[n+1]+F[n+1]*F[n-2]-F[n-3]*F[n+1]+5*Mod[n+1,2]+Floor[(n-3)/2])+(F[n+1]-n+F[n-3]*F[n-1]-Mod[n+1,2]),0]*)


(* ::Input:: *)
(*AA3[n_]:=(F[n+1])^2-T[n-1]-T[F[n-1]]-If[n>=4,(-2*F[n+1]+2*F[n+1]*F[n-2]-2*F[n-3]*F[n+1]+10*Mod[n+1,2]+2*Floor[(n-3)/2])+(F[n+1]-n+F[n-3]*F[n-1]-Mod[n+1,2]),0]*)


(* ::Input:: *)
(*AA4[n_]:=(F[n+1])^2-T[n-1]-T[F[n-1]]-If[n>=4,-F[n+1]+2*F[n+1]*F[n-2]-2*F[n-3]*F[n+1]+9*Mod[n+1,2]+Mod[n,2]-4+F[n-3]*F[n-1],0]*)


(* ::Input:: *)
(*AA5[n_]:=(F[n+1])^2-T[n-1]-T[F[n-1]]-If[n>=4,-F[n+1]+2*F[n+1]*F[n-2]-2*F[n-3]*F[n+1]+8*Mod[n+1,2]-3+F[n-3]*F[n-1],0]*)


(* ::Input:: *)
(*AA5[n_]:=(F[n+1])^2-T[n-1]-T[F[n-1]]-If[n>=4,F[n+1]*(-1+2*F[n-4])+8*Mod[n+1,2]-3+F[n-3]*F[n-1],0]*)


(* ::Input:: *)
(*AA6[n_]:=(F[n+1])^2-T[n-1]-T[F[n-1]]-If[n>=4,F[n-1]*F[n-3]+F[n+1]*(2*F[n-4]-1)+5-8*Mod[n,2],0]*)


(* ::Input:: *)
(*AA7[n_]:=(F[n+1])^2-T[n-1]-T[F[n-1]]-If[n<4,0,F[n-1]*F[n-3]+F[n+1]*(2*F[n-4]-1)+5-8*Mod[n,2]]*)


(* ::Input:: *)
(*Table[Column[{AA[n],AA7[n]}],{n,1,15}]*)
