(* ::Package:: *)
BeginPackage["Patterns`"]
(*To add a pattern add the edges of its polygons to formmaster and specify the material regions of each polygon with regionsmaster*)
Needs["Settings`"]
Get["Cutandpro`"]
Needs["Wpgroups`"]
Get["Windows`"]
toindexformpython::usage="uses numpy in order to calculate list of unique vertices of tiling and indices into that list the yield the tiling"
writegeo::usage="writegeo[val,idx,regions]=writes output for gmesh, val=list of points, idx=3rd order list of indices into val,containing groups of shapes, regions=list of integers assigning material region to each group of shapes"
readgeo::usage="reads output of writegeo into list of lists of polygons"
formssnub::usage="formssnub[phi] returns shapes (2x triangles, 1x quadrilateral) in fundamental region of snub square of angle phi between skew square and its bounding square"
formssnubwf::usage="formssnubwf[phi] returns wireframe in fundamental region of snub square of angle phi"
formssnubp4m::usage="formssnub[phi] returns shapes (2x triangles, 1x quadrilateral) in fundamental region of p4g for snub square of angle phi between skew square and its bounding square"
borderedgesoftiling::usage="given a colored tiling (either in in coordinate- or index-form) this function returns a list of the edges at the border, being defined as edges that appear a single time in the tiling (we thereby assume the tiling to be edge-to-edge)"
formsmaster::usage="formsmaster[form:String,param] selects suitable form based on argument form"
patternsmaster::usage="patternsmaster[form:String,params] generates coordinates of point in specified patterns and the corresponding connectivity"
patternsmasterpoly::usage="same as patternsmaster, except for returning lists of polygons specified by edge coordinates instead of val and idx"
unitcellgeo::usage="list of lists of polygon describing periodic unitcell of tiling"
unitcellwindowoftiling::usage="unitcellwindowoftiling[name,n] returns window describing n unitcells for tiling name"
regionsmaster::usage="regionsmaster[form:String] which material region is to be assigned to each shape defined by formsmaster, default returns empty list meaning materials are assigned in order of appearance"
graficsnub::usage="graficsnub[phi] returns grafic object of fundamental region of snub square of angle phi"
graficsnubface::usage="graficsnubspace[x] where x ist list of list of points plots snubspace indicated by forms in x"
graficswireframe::usage="graficsnubspace[x,color] where x ist list of list of points and color is an optional color"
meshgensoutersnub::usage="meshgensoutersnub[phi] returns corners of 2 quad-shells who construct a sunb-square under action of p4g"
extralayer::usage="indicates how many extra layers of unit cells need to be generated for a given periodic pattern to fill the frame"
meshgensoutersnubstar::usage="meshgensoutersnubstar[phi] returns corners of 2 quad-shells who construct a sunb-star under action of p4g"
polywindow::usage="polywindow[polygons] returns window as specified in Windows.m for a polygon given as list of xy coordinates"
groupsdict::usage="groupsdict[patternname] returns its symmetry group as string"
getgeoforfourieranalysis::usage="gets patch which is to be fourier analysed for a given name and parameter of tiling"
iswallpaperpatternQ::usage=""
plottiling::usage=""
Begin["`Private`"]
getgeoforfourieranalysis[name_String,params_:{},size_String:"medium"]:=readgeo[getgeodirforfourieranalysis[name,params,size]]
plottiling[tiling:tilingpattern]:=Flatten[({RandomColor[], EdgeForm[Black], #} &) /@ Map[(Polygon[#] &), tiling, {2}], 1]
nsin[x_] := Sin[x]/(Sin[x] + Cos[x])
ncos[x_] := Cos[x]/(Sin[x] + Cos[x])
outwardsnormals[edges: {vecpattern ..}]:=(({#[[2]], -#[[1]]} &) /@ edges) If[
   edges[[1]][[1]]*edges[[2]][[2]] - edges[[1]][[2]]*edges[[2]][[1]] >
     0, 1, -1];
polywindow[polygon: {vecpattern ..}]:=Module[{edges},
edges = Table[
  polygon[[Mod[i, Length[polygon]] + 1]] - polygon[[i]], {i, 1,
   Length[polygon]}];
	MapThread[({#1,#2}&),{outwardsnormals[edges],(*limit to each normal*)MapThread[((#1 . #2) &), {outwardsnormals[edges], polygon}]}]
]
solveintersectsnub1[
  phi_] := {Cos[phi]/((1 + Cot[phi]) (Cos[phi] + Sin[phi])),
  Cos[phi]/((1 + Cot[phi]) (Cos[phi] + Sin[phi]))}
solveintersectsnub2[
  phi_] := {Cos[phi]/((1 + Cot[phi]) (Cos[phi] + Sin[phi])),
  1 - Cos[phi]/((1 + Cot[phi]) (Cos[phi] + Sin[phi]))}
(*points wrt coordinate system in inner square, which is rotated by 45 degrees*)
pointsinnersnub[phi_] := {
  {0,ncos[phi]},
  solveintersectsnub1[phi],
  solveintersectsnub2[phi]
  }
meshgensinnersnub[phi_] := {{
	{0,ncos[phi]},
  {nsin[phi],0},
	{1,nsin[phi]},
  {ncos[phi],1}},{
	{0,ncos[phi]},
  {nsin[phi],0},
	{0,-ncos[phi]},
  {-nsin[phi],0}}
  }

meshgensinnersnubstar[phi_] := {
  (* {
  {0, ncos[phi]},
  {nsin[phi], 2 nsin[phi]/Sqrt[3]}, 
  {nsin[phi], 0}, 
  {1 - (2 Sin[phi])/(Sqrt[3] (Cos[phi] + Sin[phi])), 1/(1 + Cot[phi])}, 
  {1, nsin[phi]}, 
  {1/(1 + Tan[phi]), 1 - (2 Sin[phi])/(Sqrt[3] (Cos[phi] + Sin[phi]))}, 
  {ncos[phi], 1}, 
  {(2 Sin[phi])/(Sqrt[3] (Cos[phi] + Sin[phi])), 1/(1 + Tan[phi])}
  }, *)
  {{1/2,1/2},
  {nsin[phi], 2 nsin[phi]/Sqrt[3]}, 
  {nsin[phi], 0}, 
  {1 - (2 Sin[phi])/(Sqrt[3] (Cos[phi] + Sin[phi])), 1/(1 + Cot[phi])}},
  {
    {0, ncos[phi]}, 
    {nsin[phi], 2 nsin[phi]/Sqrt[3]}, 
    {nsin[phi], 0}, 
    {-nsin[phi], 0}, 
    {-nsin[phi], 2 nsin[phi]/Sqrt[3]}
    }
    }


meshgensp4msquare[len_:1/2]:={{{1/2-len/2,1/2-len/2},{1/2+len/2,1/2-len/2},{1/2+len/2,1/2+len/2},{1/2-len/2,1/2+len/2}},
{{1/2-len/2,1/2-len/2},{0,0},{1,0},{1/2+len/2,1/2-len/2}}
}
meshgenp4gstar[len_:1/2]:=Module[{r,dr},
r = 1/4 (*length of the square*);
dr =1/20 (*width of the opening*);
Return[Map[({-#[[2]]+1/2,#[[1]]})&,{
	{{len (r + dr), len*r*(1 - dr/(1 - r))}, {len (1 - r), len*r}, {len (1 - r), len (1 - r)}, {len*r, len (1 - r)}, {len*r*(1 - dr/(1 - r)), len (r + dr)}},
	{{0, len}, {0, len (r + dr)}, {len*r*(1 - dr/(1 - r)), len (r + dr)}},
	{{0, 0}, {0, len (r + dr)},{len*r*(1 - dr/(1 - r)), len (r + dr)}, {len (r + dr), len*r*(1 - dr/(1 - r))},{len (r + dr),0}},
	{{len (1-r - dr), len}, {len (1 - r), len (1 - r)}, {len, len}},
	{{0, len}, {len (1 - r), len (1 - r)}, {len (1-r - dr), len}},
	{{0, len}, {len*r*(1 - dr/(1 - r)), len (r + dr)}, {len*r, len (1 - r)}},
	{{0, len}, {len (1 - r), len (1 - r)}, {len*r, len (1 - r)}}},{2}] ]];


meshgenp4gpenta[len_:0.5] := 
  len*Map[{{Cos[-Pi], -Sin[-Pi]}, {Sin[-Pi], Cos[-Pi]}}.# + {0, 
       0} &, {{{0, 0}, {-1, 0}, {0, -1} + 
       Sqrt[2]/4 {Cos[3 Pi/4], Sin[3 Pi/4]}}, {{0, 0}, {0, -1} + 
       Sqrt[2]/4 {Cos[3 Pi/4], Sin[3 Pi/4]}, {0, -1}}}, {2}] ;

meshgenp4mcross[len_:0.5] := Map[{{Cos[Pi/2], -Sin[Pi/2]}, {Sin[Pi/2], Cos[Pi/2]}}.# + {len, 0} &, 
 len {{{0, 0}, {3/4, 1/4}, {1/4, 3/4}}, {{0, 0}, {1, 0}, {3/4, 
     1/4}}, {{0, 0}, {1/4, 3/4}, {0, 1}}}, {2}] ;

meshgensp4msquarecorner[len_:1/2]:={
{{1,1},{1-len,1},{1-len,1-len},{1,1-len}},
{{1-len,1-len},{1,1-len},{1,0},{1-len,0}},
{{1-len,1-len},{1-len,1},{0,1},{0,1-len}},
{{1-len,0},{1-len,1-len},{0,1-len},{0,0}}}
(* Transformation obtained by:
B={{B1,B2},{B3,B4}};
a={a1,a2};
s=Solve[Join[MapThread[Equal,{a+B.{0,0},{1/2,0}}],MapThread[Equal,{a+\
B.{0,1},{0,1/2}}],MapThread[Equal,{a+B.{1/2,1/2},{1/2,1/2}}]],{a1,a2,\
B1,B2,B3,B4}];
B/.s;
a/.s;*)
(*# - (# . {1, 1} - 1/2) & /@ serves to mirror to the right fundamental region*)
inntertooutersnub[x:vecpattern]:=# - (# . {1, 1} - 1/2) & [Simplify[{{1/2, -(1/2)}, {1/2, 1/2}} . x + {1/2, 0}]]
inntertooutersnubp4m[x:vecpattern]:=({{0, 1}, {-1, 0}}.#)+{0,1/2} & [Simplify[{{1/2, -(1/2)}, {1/2, 1/2}} . x + {1/2, 0}]]
simplelaminate[]:={{{0,0},{1,0},{1,1/2},{0,1/2}},{{0,1/2},{1,1/2},{1,1},{0,1}}}

pointsoutersnub[phi_:Pi/6] := inntertooutersnub /@ pointsinnersnub[phi]
pointsoutersnubp4m[phi_:Pi/6] := inntertooutersnubp4m /@ pointsinnersnub[phi](*forms triangle and quadrilateral needed to plot snub square, in normed coordinates*)
tau = (1 + Sqrt[5])/2;
fibonacciline[n_?IntegerQ] := 
 ReplaceRepeated[{tau}, 
  q : {Repeated[_, {0, n}]} :> (q /. {tau -> Sequence[tau, 1], 
      1 -> tau})]
fibonacciarea[n_?IntegerQ] := 
 Flatten[Outer[Function[{x, y}, {x, y}], #, #] &[
   FoldList[Plus, 0, fibonacciline[n][[1 ;; n]]]], 1]
checkerboardconnec[n_?IntegerQ] := {Flatten[Outer[
      Function[{j, i}, 
       If[EvenQ[i + j], {i + (j - 1)*(n + 1), i + 1 + (j - 1)*(n + 1), 
         i + 1 + (j)*(n + 1), i + (j)*(n + 1)}]], #, #] &[Range[n]],1], 
   Flatten[Outer[Function[{j, i}, 
       If[! EvenQ[i + j], {i + (j - 1)*(n + 1), i + 1 + (j - 1)*(n + 1), 
         i + 1 + (j)*(n + 1), i + (j)*(n + 1)}]], #, #] &[Range[n]],1]} /. {Null -> 
    Sequence[]}
generatefibosquare[n_?IntegerQ] := {N[fibonacciarea[n]], checkerboardconnec[n]}
idxtopolygons[val:{vecpattern ..},idx_]:=Map[(val[[#]]&),idx,	{depthlist[idx]-1}]
unitcellgeo[pattern_String,patternparams_List,n_:1]:=patternsmasterpoly[pattern,{unitcellwindowoftiling[pattern,n],n,patternparams,{}}]
unitcellwindowoftiling[pattern_String,n_:1]:=unitcellwindow[{defaultbasevec[wplattice[groupsdict[pattern]]]},n]
wpgroupofpattern[pattern_String]:=creategroup[defaultbasevec[wplattice[groupsdict[pattern]]], groupsdict[pattern]];
iswallpaperpatternQ[pattern_String]:=KeyExistsQ[groupsdict,pattern]
centeratzero[points : {vecpattern ..}] :=
 Module[{meanval},
  meanval = {Mean[MinMax[points[[All, 1]]]],
    Mean[MinMax[points[[All, 2]]]]}; (# - meanval &) /@ points]
toindexformintern[pointslist:{vecpattern..}]:=Module[{vals,indices,invindices},
  Export["points.csv", N[pointslist]];
  Print["python-time",Timing[Echo[RunProcess[{"python3", "-c", "
import numpy as np;
tol="<>ToString[
	NumberForm[N[Wpgroups`Private`limpointmerge], 10, ExponentFunction -> (Null &)]
]<>";# choose tol such that it is significantly smaller than the edges of the polygons (which have about unitary length) BUT significantly greater than the distance of points to be merged
data=np.genfromtxt(\"points.csv\",delimiter=\",\");
idxcum=np.linspace(0,data.shape[0]-1,data.shape[0]).astype(int)
inversecum=np.linspace(0,data.shape[0]-1,data.shape[0]).astype(int)
for offset in [np.array([0,0]),np.array([1/2,1/2])]:
    dataint= np.floor(data[idxcum] / tol+offset).astype(np.int64);
    _, idx,inverse = np.unique(dataint, return_index=True,return_inverse=True,axis=0);
    idxcum=idxcum[idx]
    inversecum=inverse[inversecum]
np.savetxt(\"indices.csv\",idxcum+1,fmt=\"%i\");
np.savetxt(\"inverse.csv\",inversecum+1,fmt=\"%i\");
"}],"pythonoutput"]]];
  indices = Flatten[Import["indices.csv"]];
  invindices = Flatten[Import["inverse.csv"]];
  vals = pointslist[[indices]];
	{vals,invindices}
]
Options[toindexformpython] = {"checkduplicates" -> False};
toindexform[points_?(Length[Flatten[#]] == 0 &)]:={{},{}}
toindexformpython[points_,OptionsPattern[]] :=
 Module[{val,idx, pointslist, dict}, Print["calculating connectivity"];
	Print["Lengths=",{Length[Flatten[points,depthlist[points] - 4]],Length[Flatten[points,depthlist[points] - 4]]}];
  Print["depth=", depthlist[points] - 3];
  pointslist = Flatten[points, depthlist[points] - 3];
  Print["number of vertices=", Length[pointslist]];
	{vals,invindices}=toindexformintern[pointslist];
	idx=Map[DeleteDuplicates(*deleteduplicates filters out several points close to each other in the same polygon*),points /. Thread[pointslist -> invindices],{depthlist[points] - 3}];
	If[OptionValue["checkduplicates"],
		idx=Map[Function[{polys},DeleteDuplicates[polys,(Sort[#1]==Sort[#2]&)]],idx,{depthlist[points] - 4}(*loop over each list of polygons*)];
	];
	idx=Map[Function[{polys},Select[polys,(Length[#]>2&)]
		],
		idx,
		{depthlist[points] - 4}];
  {vals, idx}
]
Options[patternsmaster] = {"hyperpointsfile" -> "","formechanicalcalc"->True,"niter"->Infinity};
patternsmaster[pattern_String,params_:{},shift:vecpattern:{0,0},perpshift:vecpattern:{0,0,0},centertilings: True | False:False,OptionsPattern[]]:=
	({If[centertilings,centeratzero[#[[1]]],#[[1]]],#[[2]]}&)[
		((checkfilledwindow[idxtopolygons[#[[1]],#[[2]]],params[[1]]];#)&)[
			((*returns {val,idx} where val are coordinate positions and idx is connectivity into val*)If[iswallpaperpatternQ[pattern],(
				(*periodic pattern, params[[1]] is window, params[[2]] is number of repetitions of unitcell,params[[3]] is list of additional parameters of pattern,params[[4]] is list of boundary layers to exclude*) ({#[[1]]-(pattern/.extralayer),#[[2]]}&)[
					(toindexformpython[
						quotientgeogenwindow[
							params[[2]]+2*(pattern/.extralayer),params[[2]]+2*(pattern/.extralayer), wpgroupofpattern[pattern], formsmaster[pattern,params[[3]]],Map[({#[[1]],#[[2]]-((*tiling is held constant, instead we apply  negative shift to the window*)shift-1*(pattern/.extralayer)).#[[1]]})&,params[[1]]],params[[4]]
						]
						,"checkduplicates"->True])
					]),
				toindexformpython[
					applyboundarylayers[
						pretreatconcavepolygons[
							N[restricttowindow[params[[1]],
								docandptilingsmasterphys[candpmaster[pattern,params[[3]]],params[[1]],perpshift,"hyperpointsfile"->OptionValue["hyperpointsfile"],"niter"->OptionValue["niter"]]
							]],
							"formechanicalcalc"->OptionValue["formechanicalcalc"]
						]
					,params[[1]],params[[4]]
					]
				]
			])
		]
	]
patternsmasterpoly[args___]:=If[regionsmaster[{args}[[1]]]=={},(#&),((Function[{ll}, Join @@ ll[[All, 2]]]) /@ GatherBy[Transpose[{regionsmaster[{args}[[1]]], #}], First]&)][idxtopolygons @@ patternsmaster @@ {args}]
groupsdict=<|"checkerboard"->"p1","p4gsquare"->"p4g","p4gsquarestar"->"p4g","p4msquare"->"p4m","p4msquarecorner"->"p1","simple_laminate"->"p1","p4gstar"->"p4g","p4gpenta"->"p4g","p4mcross"->"p4m","p31mtriangle"->"p31m","p31mkites"->"p31m","p31mkitesturn"->"p31m","p6mhexa"->"p6m","p6mkites"->"p6m","p4mill"->"p4"|>
extralayer={"p4gsquarestar"->1,_->0}(*some pattern need an extra layer of unit cells to be filled out*)
formsmaster[form_String,params_:{}]:=(form/.{
(*geometries indicated with regards to lattice basis vectors*)
	"snub":>formssnub[],
	"p4mill":>{{{0,0},{8/20,1/2},{1/2,0}},{{0,0},{8/20,1/2},{0,1/2}},{{1/2,0},{8/20,1/2},{1/2,1/2}}},
	"p4gsquare":>meshgensoutersnub[Sequence @@ params],
  "p4gsquarestar":>meshgensoutersnubstar[Sequence @@ params],
	"checkerboard"->{
		{{0,0},{1/2,0},{1/2,1/2},{0,1/2}},
		{{1/2,1/2},{1,1/2},{1,1},{1/2,1}},
		{{1/2,0},{1,0},{1,1/2},{1/2,1/2}},
		{{0,1/2},{1/2,1/2},{1/2,1},{0,1}}
	},
	"p4gstar":>meshgenp4gstar[],
  "p4gpenta":>meshgenp4gpenta[],
	"p4msquare":>meshgensp4msquare[],
  "p4mcross":>meshgenp4mcross[],
  "pmmstar":>{{{0,0},{1/2,0},{0,1/4}},{{1/2,0},{1/2,0},{0,1/4},{0,1/2},{1/2,1/2}}},
	"p4msquarecorner":>meshgensp4msquarecorner[],
	"simple_laminate":>simplelaminate[],
	"snubwf":>formssnubwf[],
	"snubp4m":>formssnubp4m[],
	"p31mkitesturn":>{{{0, 1},{2/3,2/3}, {2/3, 1/3}, {1/3, 1/3}}, {{1, 0},{2/3,2/3}, {2/3, 1/3}, {1/3, 1/3}}},
	"p31mkites":>{{{1, 0},{2/3,2/3}, {1/3, 2/3}, {1/3, 1/3}}, {{0, 1},{2/3,2/3}, {1/3, 2/3}, {1/3, 1/3}}},
	"p31mtriangle":>{{{1/3,0},{1/9,1/9},{0,0}},{{7/9,1/9},{5/6,0},{1,0}},{{1/3,0},{1/9,1/9},{1/3,1/3},{7/9,1/9},{5/6,0}}},
	"p6mkites":>{{{1/3, 1/3}, {1/4, 0}, {0,0}},{{1/3, 1/3}, {1/4, 0}, {1/2,0}} },
	"p6mhexa":>{{{1/6, 1/6}, {3/9, 0}, {0,0}},{{1/6, 1/6}, {3/9, 0}, {1/2,0},{1/3,1/3}} }
})
regionsmaster[form_String]:=form /.{"p4msquarecorner"->{1,2,2,2},"p4gstar"->{1,1,1,1,1,2,2},"p4mcross"->{1,2,2},"p4mill"->{1,2,2},"p31mtriangle"->{1,1,2},"checkerboard"->{1,1,2,2},_->{}}
formssnub[phi_:Pi/6]:={{{0,1/2},#[[1]],#[[3]]},{#[[1]],#[[2]],{0,0},#[[3]]},{#[[1]],#[[2]],{1/2,0}}}&[pointsoutersnub[phi]]
formssnubwf[phi_:Pi/6]:={{#[[2]],#[[1]],#[[3]]}}&[pointsoutersnub[phi]]
formssnubp4m[phi_:Pi/6]:={{{1/2,1/2},#[[1]],#[[3]]},{#[[1]],#[[2]],{1/2,0},#[[3]]},{#[[1]],#[[2]],{0,0}}}&[pointsoutersnubp4m[phi]]
meshgensoutersnub[phi_:Pi/6]:=Map[inntertooutersnub,meshgensinnersnub[phi],{2}]
meshgensoutersnubstar[phi_:Pi/6]:=Map[inntertooutersnub,meshgensinnersnubstar[phi],{2}]
Options[graficsnubface]={colorsq->LightRed,colortr->LightBlue,opts->{EdgeForm[Thickness[Small]]}}
graficsnubface[x:{vecpattern..},OptionsPattern[]]:=(Graphics[Join[OptionValue@opts,{Length[x]/.{3->OptionValue@colortr,4->OptionValue@colorsq},Polygon[x]}]])
graficswireframe[x:{vecpattern..},color: _?validColorQ : RGBColor["green"]]:=Graphics[
	{Thickness[Large],color,Line[x]}
]
graficsnub[phi_:Pi/6]:=graficsnubface[#]&/@ formssnub[phi]
readgeo[dir_ : "."] := Module[{regionsset, idxsorted},
  val = Import[FileNameJoin[{dir, "coords.csv"}]];
  idx = Import[FileNameJoin[{dir, "connec.csv"}]];
  regions = Import[FileNameJoin[{dir, "regions.csv"}]];
  regionsset = Sort[DeleteDuplicates[First /@ regions]];
  idxsorted =
   GroupBy[MapIndexed[({#1, #2[[1]]} &), idx], (regions[[#[[-1]]]] &)];
  Map[(val[[#]] &),
   Function[{region}, First /@ idxsorted[{region}]] /@ regionsset, {3}]
  ]
writegeo[val: {vecpattern..},idx_?(AllTrue[IntegerQ /@ Echo[Flatten[#,2][[{1,-1}]]],Identity]&)(*supposed to be: {{{_Integer..}..}..}, but mathematica pattern matching rule is too expensive, so we only check first and last element*),regions:{_Integer...}(**), dir_String]:=(
		Print["writing geo to ",AbsoluteFileName[dir]," regions=",regions];
		Export[FileNameJoin[{dir,"coords.csv"}],val];
		Export[FileNameJoin[{dir,"connec.csv"}],Flatten[idx,1]];
		Export[FileNameJoin[{dir,"regions.csv"}],Flatten[MapIndexed[(ConstantArray[If[Length[regions]>0,regions[[#2]],#2],Length[#1]])&,idx]]])
writegeo[args___]:=Echo[{args},"writegeo missed, args provided:"]
borderedgesoftiling[tiling : {{{(vecpattern|_Integer) ..} ..} ..}] :=
 Module[{edges},
  edges =
   Flatten[Map[(Sort /@ Transpose[{#, RotateLeft[#]}] &),
     tiling, {2}], 2];
  First /@ Select[Tally[edges], (#[[2]] == 1 &)]
  ]
Options[pretreatconcavepolygons] = {"formechanicalcalc"->True};
pretreatconcavepolygons[listsofpolys:tilingpattern,OptionsPattern[]]:=Module[{intersections,treatedtiling,vals,invindices,idces,intersectionsidcesform,idcesmod},
	(*we split all concave polygons up into convex ones. new intersections created in neighbouring polygons of concave polygons also get added after every loop, until no new intersection exists*)
	If[
		!OptionValue["formechanicalcalc"],
		listsofpolys,
		(
		treatedtiling=listsofpolys;
		Do[(
			intersections={};
			treatedtiling=Map[
				Function[{poly},((If[!NumericQ[#[[-1]]](*if splitconcavepolygon yielded an intersection*),intersections=Append[intersections,#[[-1]]]];Sequence @@ #[[1;;-2]])&)[splitconcavepolygon[poly]]
				],treatedtiling,{2}
			];
			If[intersections=={},Break[]];
			{vals,invindices(*invindices are indices into vals that recontstrcut list of vertices Flatten[treatedtiling,2]*)}=toindexformintern[Flatten[treatedtiling,2]];
			idces=Map[DeleteDuplicates,treatedtiling /. Thread[Flatten[treatedtiling,2] -> invindices],{2}];
			intersectionsidcesform=Select[
				intersections /. Thread[Flatten[treatedtiling,2] -> invindices],
				(MatchQ[#, ({x_, y_} -> z_) /; (x != z && y != z)] &)(*we are selecting those rules who do not place the intersection point exactly on a vertex, as is the case in vertex configuration 2 of Danzer and van-Ophuysen*)
			];
			idcesmodified=Map[
				Function[{polyidcesform},(
					Sequence @@ {
						({cyclicindex[polyidcesform, # - 1],polyidcesform[[#]]}) /.  Append[intersectionsidcesform, (_ -> (Unevaluated@Sequence[]))],(*we apply intersections to be added*)
						polyidcesform[[#]]}&)/@Range[Length[polyidcesform]]
				]
				,idces,{2}];	
			treatedtiling=Map[(vals[[#]]&),idcesmodified,{2}];
		),Infinity];
		treatedtiling
		)
	]
]
End[]
EndPackage[]
