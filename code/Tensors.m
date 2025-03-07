BeginPackage["Tensors`"]
Needs["ComputerArithmetic`"]
Needs["Settings`"]
rotmatk::usage="rotmatk[p] yields rotation matrix in kelvin notation for turning about angle p" 
distancetostratum::usage="distance to strata of elasticity tensors according to vianello 1997"
rotmath::usage="rotmath[p] yields rotation matrix in harmonic base for turning about angle p" 
rotmat::usage="rotmat[p] yields rotation matrix in R2 for turning about angle p" 
mirrormat = {{1, 0, 0}, {0,1,0},{0,0,-1}}
harmdecomp::usage="harmdecomp[tens] yields harmonic decomposition of 4th order tensor in Voigt notation" 
invariants::usage="invariants[tens] yields 5 invariants of 4th order tensor according to Eq (12) of auffray and desmorat geometric journey" 
cauchyschwartz::usage="cauchyschwartz[I2_,J2_,J3_] yields cauchy schwartz inegality" 
thk::usage="tensor as a function of harmonic decomposition in kelvin notation"
ktoh::usage="turn[T,ktoh] turns tensor from kelvin base to harmonic base"
Kscal::usage="Scale to pass from mandel to kelvin-voigt"
turn::usage="turn[a,r] turns 2/th or 4/t tensor a by rotation tensor r"
deductsymmetryclassplane::usage="deductsymmetryclassplane[I2,J2,J3,norm] deducts symmetryclass based on IB"
Begin["`Private`"]
ktoh = Transpose[{{Sqrt[2]/2, 0, Sqrt[2]/2}, {-Sqrt[2]/2, 0, Sqrt[2]/2}, {0, 1, 
    0}}];(*transformation matrix between kelvin and harmonic \
representation*)
Kscal = {{1, 1, Sqrt[2]}, {1, 1, Sqrt[2]}, {Sqrt[2], Sqrt[2], 
    2}};(*to scale tensor into kelvin form from voigt*)
turn[t : _?MatrixQ, r : _?MatrixQ] := FullSimplify[r . t . Transpose[r] ]
(*turn 4th order tensor t*)
turn[t : _?VectorQ, r : _?MatrixQ] := FullSimplify[r . t ]
(*turn 2th order tensor t*)

(*harmonic decomposition of tensor in kelvin notation, following auffray and desmorats geometric journey EQ 6, except:
 h (in the code) is d (in the paper),
 H (in the code) is D (in the paper),
J1 is 2mu, I1=lambda+mu=bulk modulus (see auffray and ropars)
note that h and H are NOT given in orthonormal basis, and therefore differ from vianello 1997.
*)
dk[h1_, h2_] := {h1, -h1, Sqrt[2] h2};
Dmatk[H1_, H2_] := {
	{H1, -H1, Sqrt[2] H2},
	{-H1, H1, -Sqrt[2] H2},
	{Sqrt[2] H2, -Sqrt[2] H2, -2 H1}
}
ik = {1, 1, 0}(*second order identity tensor*)
iik = IdentityMatrix[3](*forth order symmetric identity tensor*)
kk = 1/2*Outer[Times, ik, ik](*spheric projector*)
jk = iik - kk(*deviatoric projector*)
tdk[h1_, h2_] := 1/2 (Outer[Times, dk[h1, h2], ik] + Outer[Times, ik, dk[h1, h2]])
tkk[k_] := k (Outer[Times, ik, ik])
tyk[y_] := y*jk
thk := Dmatk[H1, H2] + tdk[h1, h2] + tkk[k] + tyk[y]
thh := turn[thk, ktoh]
distancetostratum[tens_?MatrixQ(*tensor in voigt notation*), key_String] :=
 distancetostratum[harmdecomp[tens], key]
distancetostratum[harmdec : {__Rule}, key_String] :=
 distancetostratum[invariants[harmdec], key]
distancetostratum[harmdec : {__Rule}, key_String] :=
 distancetostratum[invariants[harmdec], key]
distancetostratum[invariants : {__}, key_String] := key /. {
   "O(2)" :> invariants[[3]] + invariants[[4]],
   "D4" :> {invariants[[3]]},
   "D2" :>
    If[AnyTrue[invariants[[3 ;; 4]], (# == 0 &)], 0,
     Min[{invariants[[4]],
       Minimize[
         invariants[[4]]*Sin[x + invariants[[8]]]^2 +
          invariants[[3]]*Sin[x/2]^2, x][[1]]}]]
   }
deductsymmetryclassplane[I2_, J2_, J3_, norm_] := Module[{limit=10^(-5)},
   If[Sqrt[J2/norm^2] < limit,
    If[Sqrt[I2/norm^2] < limit,
     "O(2)",
     "D2"]
    , If[J3<0 ||(J3/norm^3)^(1/3) < limit,
     If[Sqrt[I2/norm^2] < limit,
      "D4",
      "D2"],
     "Z2"]]]
metrictensor[basis:{vecpattern..}]:=basis.Transpose[basis]
(*harmonic decomposition of arbitrary tensor in voigt notation*)
harmdecomp[tens_?MatrixQ/;AnyTrue[Flatten[tens],(!NumericQ[#]&)]] :=(
  Solve[thh == turn[(Kscal*tens), ktoh], {H1, H2, h1, h2, y, k}][[1]])
tv = {{T1111, T1122, T1112}, {T1122, T2222, T1222}, {T1112, T1222,
    T1212}};
tvharmdecomp=harmdecomp[tv];
harmdecomp[tens_?MatrixQ/;MatrixQ[tens,(NumericQ[#]&)]]:=Echo[tvharmdecomp /. {T1111->tens[[1,1]],T1122->tens[[1,2]],T2222->tens[[2,2]],T1222->tens[[2,3]],T1112->tens[[1,3]],T1212->tens[[3,3]]}]
invariants[tens_?MatrixQ] :=invariants[harmdecomp[tens]](*harmonic decomposition of numeric tensor in voigt notation*) 
invariants[harmdec:{__Rule}] :=(*harmdec:harmonic decomposition of numeric tensor in voigt notation*) 
  FullSimplify[{k, y, dk[h1, h2] . dk[h1, h2], 
     Tr[Dmatk[H1, H2] . Dmatk[H1, H2]], 
     dk[h1, h2] . Dmatk[H1, H2] . dk[h1, h2],angle[h1,h2],angle[H1,H2],2*angle[h1,h2]-angle[H1,H2]} /. harmdec];
(*tensor in kelvin system with harmonic composants*)
cauchyschwartz[I2_,J2_,J3_]:=I2^2*J2-2*J3^2
rotmatk[p_] := 
 1/2*{{1 + Cos[2*p], 1 - Cos[2*p], -Sqrt[2]*Sin[2*p]}, {1 - Cos[2*p], 
    1 + Cos[2*p], 
    Sqrt[2]*Sin[2*p]}, {Sqrt[2]*Sin[2*p], -Sqrt[2]*Sin[2*p], 
    2 Cos[2*p]}}
rotmath[p_] := {{Cos[2*p], -Sin[2*p], 0}, {Sin[2*p], Cos[2*p], 0}, {0,0, 1}}
rotmat[p_?NumericQ] := {{Cos[p],-Sin[p]},{Sin[p],Cos[p]}}
End[]

EndPackage[]
