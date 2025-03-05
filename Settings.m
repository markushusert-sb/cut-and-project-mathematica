BeginPackage["Settings`"]
dim=2
vecpattern={Repeated[_?NumericQ]}(* pattern denoting vector of euclidean space to be treated*)
tilingpattern={{{vecpattern..(*single polygon*)}...(*list of polygons (may be empty)*)}..(*multiple lists of polygons*)}(*note that this pattern can be applied to the tiling in both hyper as well as physical space, since in both cases, a vertex is specified by a vector*)
LIshiftpattern={RepeatedNull[_?NumericQ]}(* pattern denoting vector of euclidean space to be treated*)
coloredhypertilepattern={{{Repeated[_?IntegerQ]}..},_?IntegerQ}(* denoting a tile in hyperspace, vector of hyperlatticecoordinates followed by material region as integer*)
matpattern={Repeated[{Repeated[_?NumericQ]}]}
substitutionruleconfigpattern={_?StringQ,LIshiftpattern,polytopepattern,vecpattern}(*String = name of candp, LIshiftpattern = its LI-shift parametrisation, vecpattern and polytopepattern= perpendicularshift and physical window that generates image of substitution rule under generatesubstitutionruleconfig*)
edgeencodedpattern={_?StringQ, _?NumericQ} (*string encoding normal vector and limit associated to normal vector*)
fourierpattern={{_Integer ..},_? NumericQ}
phasonplanepattern={polytopepattern,_?NumericQ,LIshiftpattern}(*list of:
	1: window in phason space
	2: area of window
	3: associated LI-parameters
*)
possiblelocalconfigpattern={{vecpattern..},LIshiftpattern}(* list of
		vertices of polygonal section of phasonplane associated with local config
		LIshift-parameters
	*)
getortbasetonormalvector::usage="given an R^d vector, yields list of (d-1) R^d vector forming an orthonormal base together with input vector"
polytopepattern={halfspacepattern...}
halfspacepattern={vecpattern, _?NumericQ}(*a normal vector n and a limit l*)
reprulepattern={(_Rule | _RuleValue) ...}
(*specify id of corner as well as id of connections linking it to other fundamental regions*)
fundconnectpattern={_Integer, {_Integer ..}}
formsQ::usage="check for nicely structured array of points"
exportpointset::usage=""
removecommondivisor::usage=""
validColorQ::usage="check for color"
vertsorpolyQ::usage=""
sequal::usage="check for symbolic egality of expression"
depthlist::usage="get depth of list without expanding its elements"
RandomnDVector::usage=""
cyclotomicintegerbase::usage="cyclotomicintegerbase[n]:=n x EulerPhi(n) matrix that gives xi_n^i as a linear combination of xi_n^j with 1<=j<=EulerPhi[n]"
colinearQ::usage=""
signedArea::usage="signed area of polygon specified by vertices"
ensureCounterclockwise::usage="takes vertices of polygon and returns same polygon with vertices in counterclockwise order"
cyclicindex::usage="cyclicindex[list,idx] indexes list modulo Length[list]"
vectorangle::usage=""
polygonangle::usage="interior angle at the vertices of a polygon, if any one of them is above pi, we have a concave polygon"
polygonanglesfromedges::usage="same as polygonangle, except that it takes as input the edges (difference vectors between consectutive vertices"
customConvexPolygonQ::usage="removes superfluous vertices and then checks for convexity"
polygonwithoutsuperfluousverts::usage="filters out superflouous vertices that do not change the polygon since they lie on the line connecting their neighbours"
angle::usage="angle[x,y] returns angle to x-axis of point specified by x and y"
diffmod1::usage="difference of 2 numbers modulo 1"
diffmodn::usage="diffmodn[m,n] returns difference of m from 0 on integers modulo n"
(*list that has a consistent depth (all whose element are present on the lowest level) and that has only vecpats on second lowest level*)
diffmod1signed::usage=""
getgeodirforfourieranalysis::usage="gets directory containing patch which is to be fourier analysed for a given name and parameter of tiling"
getgeodirnameforfourieranalysis::usage="gets directoryname (only last part) containing patch which is to be fourier analysed for a given name and parameter of tiling"
FFTcsvfilenames::usage="FFTcsvfilename[intervall,pixel] yields filename containing FFT-values for the frequencies in the center square of length 2*intervall+1, calculated using <pixel> pixel in each dimension"
FFTpngfilename::usage="FFTpngfilename[pixel] yields filename containing image with indeicated number of pixels in each dimension which is used to calculate the FFT"
exportandcreatedir::usage="creates dir and exports to it"
relativediff::usage=""
exportDataToCSV::usage="export to csv while prepending header"
bilinearidx::usage="bilinearidx[array_,idx : vecpattern] indexes into array using floating point index idx by bilinearly interpolating between arrayva&lues around idx"
readidcesfromdict::usage=""
GenerateOrthogonalMatrix::usage=""
identitytrafo[n_:2]:={(0&)/@Range[n], IdentityMatrix[n]}
affineelepattern={vecpattern,matpattern}
FFTdirname="FFT"
polygonintegrationdirname="exact_integration"
pixelintegrationdirname="exact_pixel"
(*https://mathematica.stackexchange.com/questions/38851/what-is-the-proper-way-to-verify-that-two-expressions-are-equal*)
Begin["`Private`"]
GenerateOrthogonalMatrix[n_] := Module[{Q, R},
	 SeedRandom[] ;
  {Q, R} = QRDecomposition[RandomReal[{-1, 1}, {n, n}]];
  If[Det[Q] < 0, Q = -Q];  (* Ensure Q has determinant +1 *)
  Q
]
exportpointset[pointset_(*is supposed to be {vecpattern..}, but pattern matching gives up if we have too many points*),dir:_?StringQ:"."]:=Module[{maxx,maxy,minx,miny,delx,dely},
	maxx=Max[N[pointset[[All,1]]]];
	maxy=Max[N[pointset[[All,2]]]];
	minx=Min[N[pointset[[All,1]]]];
	miny=Min[N[pointset[[All,2]]]];
	delx=(maxx-minx)/20;
	dely=(maxy-miny)/20;
	Export[FileNameJoin[dir,StringJoin[
		"pointset",
		"_minx",
		ToString[minx-delx],
		"_maxx",
		ToString[maxx+delx],
		"_miny",
		ToString[miny-dely],
		"_maxy",
		ToString[maxy+dely],
		".png"]],
	Graphics[ListPlot[pointset,PlotRange->{{minx-delx,maxx+delx},{miny-dely,maxy+dely}},Axes -> False, Frame -> False, GridLines -> None,PlotStyle -> Black,AspectRatio -> Automatic],ImageMargins->0],ImageResolution -> 300
]]

readidcesfromdict[dict_String,idces:{vecpattern..}]:=readidcesfromdict[ToExpression[Import[dict]],idces]
readidcesfromdict[dict_Association,idces:{vecpattern..}]:={dict,Select[idces,(!KeyExistsQ[dict,#]&)]}
exportDataToCSV[fileName_, data_, header_String] := Module[
  {tempFile, stream, csvContent},

  (* Export the data without the header *)
  Export[fileName, data, "CSV"];

  (* Read the CSV content as plain text *)
  csvContent = Import[fileName, "Text"];

  (* Create a temporary file and write the header *)
  tempFile = CreateTemporary[];
  stream = OpenWrite[tempFile];
  WriteLine[stream, header];

  (* Write the original CSV content after the header *)
  WriteString[stream, csvContent];

  (* Close the stream *)
  Close[stream];

  (* Overwrite the original file with the updated content *)
  CopyFile[tempFile, fileName, OverwriteTarget -> True];
  DeleteFile[tempFile];
]
relativediff[x_?NumericQ,y_?NumericQ]:=(x-y)/Max[x,y]
myMFileDirectory = DirectoryName[$InputFileName];
Nextint[x_] := Floor[x] + 1
bilinearidx[array_,idx : vecpattern] :=(*see \
https://en.wikipedia.org/wiki/Bilinear_interpolation*)
(
	 {Nextint[idx[[1]]] - idx[[1]],idx[[1]] -Floor[idx[[1]]]} . (
	{{array[[Floor[idx[[1]]], Floor[idx[[2]]]]],array[[Floor[idx[[1]]], Nextint[idx[[2]]]]]},
{array[[Nextint[idx[[1]]], Floor[idx[[2]]]]],array[[Nextint[idx[[1]]], Nextint[idx[[2]]]]]}}.
{Nextint[idx[[2]]] - idx[[2]], idx[[2]] - Floor[idx[[2]]]})
	 )
exportandcreatedir[file_String,obj_]:=(If[!DirectoryQ[DirectoryName[file]],CreateDirectory[DirectoryName[file],CreateIntermediateDirectories -> True]];Export[file,obj])
FFTcsvfilenames[intervall_Integer,pixel_Integer]:={StringJoin["intervall_",ToString[intervall],"_pixel_",ToString[pixel],"real.csv"],StringJoin["intervall_",ToString[intervall],"_pixel_",ToString[pixel],"image.csv"]}
removecommondivisor[vec:vecpattern]:=vec/(GCD @@vec)
FFTpngfilename[pattern_String,params_:{},pixel_Integer,region_Integer]:=StringJoin[getgeodirnameforfourieranalysis[pattern,params],"FFTbase_region",ToString[region],"_pixel_",ToString[pixel],".png"]
FFTpngfilename[pattern_String,params_:{},pixel_Integer,type_String]:=StringJoin[getgeodirnameforfourieranalysis[pattern,params],"FFTbase_type",type,"_pixel_",ToString[pixel],".png"]
getgeodirnameforfourieranalysis[name_String,params_:{}]:={name,params}/.{
{"p4msquare",{}}->"p4msquarecenter",
{"p6mkites",{}}->"p6m",
{"p6mhexa",{}}->"p6mhexa",
{"pmmtiling",{}}->"pmm",
{"fibonaccirectangles",{}}->"off-centered_fiborect",
{"fibonaccirectangles2",{}}->"off-centered_fiborect2",
{"dodecagonalqp",{0,0}}->"off-centered_dodecagonalqp_0_0",
{"dodecagonalqp",{y_ /; Mod[y, 1] == 0.5, 0}}->"off-centered_dodecagonalqp_0.5_0",
{"dodecagonalqp",{0,x_ /; Mod[x, 1] == 0.5}}->"off-centered_dodecagonalqp_0_0.5",
{"dodecagonalqp",{y_ /; Mod[y, 1] == 0.5,x_ /; Mod[x, 1] == 0.5}}->"off-centered_dodecagonalqp_0.5_0.5",
{"dodecagonalqp",{y_ /; Mod[y, 1] == 0.2,x_ /; Mod[x, 1] == 0.7}}->"off-centered_dodecagonalqp_0.2_0.7",
{"dodecagonalqp",{y_ /; Mod[y, 1] == 0.7,x_ /; Mod[x, 1] == 0.2}}->"off-centered_dodecagonalqp_0.7_0.2",
{"dodecagonalqp",{y_ /; Mod[y, 1] == 0.25,x_ /; Mod[x, 1] == 0.75}}->"off-centered_dodecagonalqp_0.25_0.75",
{"dodecagonalqp",{y_ /; Mod[y, 1] == 0.25,x_ /; Mod[x, 1] == 0.25}}->"off-centered_dodecagonalqp_0.25_0.25",
{"dodecagonalqp",{y_ /; Mod[y, 1] == 0.75,x_ /; Mod[x, 1] == 0.75}}->"off-centered_dodecagonalqp_0.75_0.75",
{"hexagonalqp",{0,0}}->"off-centered_hexagonalqp_0_0",
{"hexagonalqp",{0,x_ /; Mod[x, 1] == 0.5}}->"off-centered_hexagonalqp_0_0.5",
{"hexagonalqp",{y_ /; Mod[y, 1] == 0.5,x_ /; Mod[x, 1] == 0.5}}->"off-centered_hexagonalqp_0.5_0.5",
{"hexagonalqp",{y_ /; Mod[y, 1] == 0.5, 0}}->"off-centered_hexagonalqp_0.5_0",
{"hexagonalqp",{y_ /; Mod[y, 1] == 0.7,x_ /; Mod[x, 1] == 0.2}}->"off-centered_trigonalqp_0.7_0.2",
{"p4msquare",{1}}->"p4msquarerandom",
{"p4msquare",{2}}->"p4msquaremisleading",
{"p4msquare",{3}}->"p4msquaremisleading2",
{"p4gsquare",{Pi/6}}->"snubZ4",
{"p4gsquare",{1}}->"snubD2",
{"p4gsquare",{2}}->"snubrandom",
{"p31mkites",{}}->"p31m",
{"p31mtriangle",{}}->"p31mtriangle",
{"p31mkitesturn",{}}->"p31mturn",
{"p4mill",{}}->"p4mill",
{"p31mkitestriangle",{}}->"p31mtriangle",
{"penrose",{}}->"off-centered_penrose_D10",
{"penrose",{x_ /; Mod[x, 1] == 0}}->"off-centered_penrose_D10_2",
{"penrose",{x_ /; Mod[x, 1] == 0.5}}->"off-centered_penrose_D10",
{"penrose",{x_ /; Mod[x, 1] == 0.7}}->"off-centered_penrose_D5",
{"penroseforgaugegif",{x_ /; MemberQ[Range[0, 0.2, 0.01], x]}}:>FileNameJoin[{"penrose_gamma_varied",StringJoin["shift",ToString[NumberForm[x, {3, 2}]]]}],
{"ammann",{}}->"off-centered_ammann",
{"ammannnonsymmorph",{}}->"off-centered_ammannnonsymmorph",
{"shield",{}}->"off-centered_shield",
{"fibonaccisquares",{}}->"off-centered_fibo",
{"fibonaccisquareshypercube",{}}->"off-centered_fibo_hypercube",
{"goldentriangle",{}}->"triangle",
_:>Throw["unknown tiling",{name,params}]
}
getgeodirforfourieranalysis[name_String,params_:{},size_String:"medium"]:=StringJoin[myMFileDirectory,"../data/samples_for_fourier_calculation/",size,"/geometries/",getgeodirnameforfourieranalysis[name,params]]
angle[x_, y_] := If[x^2 + y^2 > 0,ToPolarCoordinates[{x,y}][[2]],NaN]
cyclicindex[l_List, i_?IntegerQ] := l[[Mod[i - 1, Length[l]] + 1]]
vectorangle[v1_, v2_] :=
 Mod[angle[Sequence @@ v2] - angle[Sequence @@ v1],
  2*Pi](*(If[Cross[Append[v1,0],Append[v2,0]][[3]]>0,2*Pi-#,#]&)[\
VectorAngle[v1,v2]]*)
diffmod1signed[x_?NumericQ, y_?NumericQ] :=MinimalBy[{Mod[x - y, 1], -Mod[y - x, 1]}, Abs][[1]]
diffmod1[x_?NumericQ,
  y_?NumericQ] := (((*Print["difference of ",x," and ",y,
     " is ",#];*)#) &)[
  Min[1 - Abs[Mod[x, 1] - Mod[y, 1]], Abs[Mod[x, 1] - Mod[y, 1]]]]
diffmodn[m_?NumericQ,n_?NumericQ] := Min[n - Abs[Mod[m, n] ], Abs[Mod[m, n]]]
signedArea[vertices : {vecpattern ..}] :=
(*shoelace formula*)
 1/2 Total[
   Table[Quiet[Det[{vertices[[i]],
      vertices[[Mod[i, Length[vertices]] + 1]]}], Det::luc], {i,
     Length[vertices]}]]
ensureCounterclockwise[vertices : {vecpattern ..}] :=
 Module[{area}, area = signedArea[vertices];
  If[area < 0, Reverse[vertices], vertices]]
projecttoplane[toproj : vecpattern, n : vecpattern] :=
 toproj - (toproj . n/Norm[n]^2)*n
getortbasetonormalvector[n : vecpattern] :=
 Select[Orthogonalize[(projecttoplane[#, n] &) /@
    Select[(UnitVector[Length[n], #] &) /@
      Range[Length[n]], (! colinearQ[n, #] &)]], (Norm[#] > 0 &)]
limitcolinearQ=10^(-10)
Options[colinearQ] = {"approximate"->False};
colinearQ[v1:vecpattern, v2:vecpattern,OptionsPattern[]] := Module[{scaledV1, scaledV2},(*If either vector is zero,they are not collinear unless both are zero*)
 If[Norm[v1] == 0 || Norm[v2] == 0, 
  Return[Norm[v1] == 0 && Norm[v2] == 0]];
 (*Scale the vectors by the first non-zero component to avoid division by zero*)
 scaledV1 = v1/First[Select[v1, (Abs[#] > 0.00001 &)]];
 scaledV2 = v2/First[Select[v2, (Abs[#] > 0.00001 &)]];
 (*Check if scaled vectors are equal*)
	If[OptionValue["approximate"],
	Return[(Norm[scaledV1 - scaledV2]<limitcolinearQ || Norm[scaledV1 + scaledV2]<limitcolinearQ)],
	Return[scaledV1 == scaledV2 || scaledV1 == -scaledV2]
	];]
formsQ[list_List]:=Length[Level[list, {Depth[N[list]] - 1}]] == Length[Flatten[list]] && ( MatchQ[Level[list, {Depth[N[list]] - 2}], {vecpattern ..}])
validColorQ[color_] := MatchQ[color, RGBColor[_, _, _] | GrayLevel[_]]
vertsorpolyQ[p_] := MatchQ[p, {vecpattern..} | Polygon[_]]
sequal[a_,b_]:=(True === FullSimplify[a == b])
depthlist[{}] := 2;
depthlist[list_List] := 1 + Max[Map[depthlist, list]];
depthlist[_] := 1;
RandomnDVector[lim_?NumericQ,n_?IntegerQ] := Module[{vec},
  vec = RandomVariate[NormalDistribution[], n];  (* Generate a random 3D vector with normally distributed components *)
  vec = vec/Norm[vec];                           (* Normalize the vector to unit length *)
  lim *vec                                        (* Scale the vector to the specified length *)
]
rowofcyclotomicintegerbase[rowidx_Integer,
  stepsandstarts : {{_?IntegerQ, _?IntegerQ} ..},
  n_Integer] := (Inverse[
    Function[{stepandstart}, (If[
          Mod[# + EulerPhi[n], stepandstart[[1]]] ==
           Mod[stepandstart[[2]], stepandstart[[1]]], 1, 0] &) /@
       Range[Length[stepsandstarts]]] /@
     stepsandstarts] .(*rhsvector*)(If[
       Mod[rowidx, #[[1]]] == Mod[#[[2]], #[[1]]], -1, 0] &) /@
    stepsandstarts)
cyclotomicintegerbase[n_Integer] :=
 Join[IdentityMatrix[EulerPhi[n]],
  If[PrimeQ[n], {(-1 &) /@ Range[EulerPhi[n]]},
   Transpose[((*list of step,start for equation to determine n'th row*)
      rowofcyclotomicintegerbase[#,
        Flatten[Function[{divisor}, ({n/divisor, #} &) /@
             Range[n/divisor]] /@ Divisors[n][[2 ;; -2]],
          1][[1 ;; n - EulerPhi[n]]], n] &) /@ Range[EulerPhi[n]]]]]
polygonangles[poly : {vecpattern ..}] := 
 Module[{edges}, edges = Differences[Append[poly, First[poly]]]; 
  polygonanglesfromedges[edges]]
polygonanglesfromedges[
  edges : {vecpattern ..}] := (Table[
     vectorangle[-cyclicindex[#, i], cyclicindex[#, i - 1]], {i, 
      Length[#]}] &)[edges]
polygonwithoutsuperfluousverts[poly : {vecpattern ..}] := 
 poly[[Flatten[
    Position[polygonangles[poly], _?(Abs[# - Pi] > 0.00001 &)]]]]
customConvexPolygonQ[poly:{vecpattern..}]:=ConvexPolygonQ[Polygon[polygonwithoutsuperfluousverts[poly]]]
End[]
EndPackage[]
