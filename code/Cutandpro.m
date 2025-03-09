BeginPackage["Cutandpro`"]
Needs["Settings`"]
Needs["Wpgroups`"]
Get["Windows`"]
candpmaster::usage="named list of all existing cut-and-project sets"
docandppoints::usage="does cut and project construction of a point set (without projecting selected points to physical space). args are an association descriping the cut and project method, the physical window to fill and the shift in internal space.see docandptilingsphys for doc of the association:
"
checkandfiltertiling::usage=""
periodofapproximant::usage="periodofapproximant[patternname,n,m,i] yields the ith period for approximant patternname with parameters n and m"
getsubstructureforperpshift::usage="getsubstructureforperpshift[tiling,perpshiftw]"
approximantunitcellvertices::usage=""
approximantunitcellwindow::usage="approximantunitcellwindow[candp,factor] denotes the polytopial window corresponding to facter unitcells in x- and y directions for the approximant specified by candp"
approximantunitcelltiling::usage="approximantunitcelltiling[candp,internalshift,factor] tiles factor^2 unitcells with the approximant positioned at internalshift in internal space"
hyperpointstohypertilingcanonical::usage=""
plotprojectedtiling::usage"Graphics[plotprojectedtiling[tiling]]  where  is the output of docandptilingsmasterphys allows to visualise the tiling"
tilingshypertophys::usage="tilingshypertophys[candp_Association,hyperoutput_] sorts hypertilings produced by simple cut and pro algo to real space and according to assignregionfun and projects obtained tiling"
filterhypertilingbysubstructure::usage="filterhypertilingbysubstructure[hypertiling,substructure] sorts hypertiling produced by simple cut and pro algo to two sets of hyperpolygons: those that are subsets of a translate of substructre and the rest"
sampleperpshifts::usage="sampleperpshifts[candp,n] samples n positions in perspendicular space belonging to the LI-class specified by candp"
samplephasonplanes::usage="samplephasonplanes[candp,n] samples n possible position starting positions in the LI-class specified candp, according to the uniformly dense distribution of lattice points"
hypertilingtohyperpoints::usage=""
docandppointssmarthyper::usage="selects hyperspace points to project, this is done in a smart by, by predicting the local configuration and therefore neighbours of each hyperpoint based on its position in the acceptance window
the candp association needs to provide:
hyperdim;
-name: name of pattern (same as entry to homogenise_pattern.py)
projectphysical;
acceptedneighbouroutward: function that takes a hyperpoint and the perpshift and predicts the neigbouring hyperpoints that are also projected and are further from the origin in physical space 
"
generalcutandproject::usage="canonical cut-and-project (canoncial meaning acceptancewindow is internal projection of hyperunitcell) of arbitrary dimensional hypercubic lattice parametrised by orthogonal projection matrix"
generalcandp2d::usage=""
docandppointsphys::usage="same as docandppointssmarthyper, except that selected points are also projected to physical space
the candp association needs to provide on top of requirements for docandppointssmarthyper:
getneighbouringpolygons: function that takes a hyperpoint and the perpshift and returns the list of its neighbour polygons in hyperspace that will be projected"
docandptilingshyper::usage="same as docandptilingsphys but does not project the points" 
docandptilingsphys::usage="does cut and project construction of a tiling. args are an association descriping the cut and project method, the physical window to fill and the shift in internal space. 
the association must contain:
-name: name of pattern (same as entry to homogenise_pattern.py)
-projectphysical: function that projects hyperlattice point (given by integer coordinates) to physical space
-projectperpendicular: function that projects hyperlattice point (given by integer coordinates) to internal space
-isinwindowQ: function that takes a hyperlattice point (given by integer coordinates), the projectperpendicular function, a phason shift and the window and says if the projection of the hyperlatticepoint is inside of the acceptance window
-hyperdim: dimension of the hyperlattice 
-physdim: dimension of the physical space 
-periods: For approximants only
	two vectors belonging to the hyperlattice that, when projected to physical space, are edges of the periodic unit-cell, with (zero,periods[[1]],periods[[1]]+periods[[2]],periods[[2]]) going counterclockwise around the unitcell
-acceptancewindow: window of acceptance in internal space
-assignregion: takes a list of list of vertices of a tile to project (in hyperspace integer coordinates, the first vertex is always less than or equal to all other vertices in all dimensions. returns material region of tile.
"

docandptilingssmarthyper::usage="gives list of tyles to project as hypertyle"
docandptilingssmartphys::usage="projects output of docandptilingssmarthyper and sorts it by region"
docandptilingsbysubwindows::usage="does correct cut and pro method based on name of provided association and classes tilings based on list of given subwindows in internal space (hyperpoints not lying in any subwindow are put last))"
docandptilingsphyssubstructure::usage="does cut and project of a tiling, but sets the region of tiles based on a predefined substructure"
docandptilingsmasterhyper::usage="does correct cut and pro method based on name of provided association"
docandptilingsmastercoronahyper::usage="does correct cut and pro method based on name of provided association"
docandptilingsmasterphys::usage="same as docandptilingsmasterhyper, but also projects results"
penrosecandp::usage="yields cut and project scheme for given gamma (see Ishihara and Yamamoto)"
hexagonalqpcandp::usage="yields cut and project scheme for hexagonal qps, see Hexagonal and trigonal quasiperiodic tilings Coates et al."
dodecagonalqpcandp::usage="yields cut and project scheme for hexagonal qps, see Hexagonal and trigonal quasiperiodic tilings Coates et al."
ammannbeenkercandp::usage="cut and project scheme for amman beenker"
ammannbeenkernonsymmorphcandp::usage="cut and project scheme for nonsymmorph amman beenker"
shieldcandp::usage="yields cut and project scheme for shield tiling"
fibosquarecandp::usage="cut and project scheme for fibo square"
fibosquarehypercubecandp::usage="cut and project scheme for fibo square from a hypercubic lattice"
fibochaincandp::usage="cut and project scheme for fibo chain"
fibochainhypercubecandp::usage="cut and project scheme for fibo chain"
goldentrianglecandp::usage="cut and project scheme for goldentriangle"
goldentrianglehomogcandp::usage="cut and project scheme for goldentriangle with horizontal mirrors"
getneighbourpredictor::usage=""
cleanuppredictor::usage="cleanuppredictor[predictor_Association] is used to clean polygon neighbourhood predictors for tilings that are not edge-to-edge. In this case, the same polygon can appear several times, with and without auxiliary vertices splitting an edge. We want the tile with auxiliary vertices to take precedence over its counterpart"
getpolygonpredictor::usage="getpolygonpredictor[window, multrotinternal,order, internalprojector,hyperrotationmatrix, hypertiles] yields an association with keys being subwindows of window and values the hypervertices of polygons neighbouring a point if it is in a given subwindow."
Begin["`Private`"]
periodofapproximantraw[patternname_String,n_Integer,m_Integer,i_Integer]:={patternname,i}/.{
{"fibonaccisquaresapproximant",1}:>Identity[{Fibonacci[n], Fibonacci[n - 1], 0, 0}],
{"fibonaccisquaresapproximant",2}:>Identity[{0, 0, Fibonacci[m], Fibonacci[m - 1]}],
{"fibonaccirectanglesapproximant",1}:>Identity[{Fibonacci[n], Fibonacci[n - 1], 0, 0}],
{"fibonaccirectanglesapproximant",2}:>Identity[{0, 0, 3*Fibonacci[m], 2*Fibonacci[m - 1]}],
{"penroseapproximant",1}:>Identity[-2*{Fibonacci[n - 1], Fibonacci[n - 1], Fibonacci[n - 1], Fibonacci[n - 1], Fibonacci[n - 1]}-{Fibonacci[n ], Fibonacci[n ], Fibonacci[n ], Fibonacci[n ], Fibonacci[n]}+ 5*{Fibonacci[n], Fibonacci[n - 1], 0, 0, Fibonacci[n - 1]}],
{"penroseapproximant",2}:>Identity[{0, Fibonacci[m], Fibonacci[ m - 1], -Fibonacci[m - 1], -Fibonacci[m]}],
{"ammannapproximantduneau",1}:>Identity[{(pellnumber[n] + pellnumber[n - 1]), pellnumber[n], 0, -pellnumber[n]}],
{"ammannapproximantduneau",2}:>Identity[{0, pellnumber[m], (pellnumber[m] + pellnumber[m - 1]), pellnumber[m]}],
{"hexagonalqpapproximant",1}:>Identity[3*{Fibonacci[n - 1], 0, 0, Fibonacci[n], 0, 0} - {Fibonacci[n - 1], Fibonacci[n - 1], Fibonacci[n - 1], Fibonacci[n], Fibonacci[n], Fibonacci[n]}],
{"hexagonalqpapproximant",2}:>Identity[{0, Fibonacci[m - 1], -Fibonacci[m - 1], 0, Fibonacci[m], -Fibonacci[m]}],
{"dodecagonalqpapproximant",1}:>Identity[(-3*{-approxsqrt3numerator[n], -approxsqrt3denominator[n], 0, 0, 0, approxsqrt3denominator[n]} - {approxsqrt3numerator[n], 0, approxsqrt3numerator[n], 0, approxsqrt3numerator[n], 0})],
{"dodecagonalqpapproximant",2}:>Identity[(-3*{0, 0, approxsqrt3denominator[m], approxsqrt3numerator[m], -approxsqrt3denominator[m], 0} + 1*{0, approxsqrt3numerator[m], 0, approxsqrt3numerator[m], 0, approxsqrt3numerator[m]})],
_:>Throw[StringJoin["unknown approximant: ",ToString[patternname],ToString[i]]]
}
periodofapproximant[patternname_String,n_Integer,m_Integer,i_Integer]:=removecommondivisor[periodofapproximantraw[patternname,n,m,i]]
usewhichcandpQ[patternname_String]:=patternname /.{"shield"->"smart","goldentrianglehomog"->"smart","goldentriangle"->"smart",___->"normal"}(*deprecated*)
samplephasonplanes[candp_Association,n_Integer]:=RandomChoice[candp["phasonplanes"][[All,2]]->candp["phasonplanes"],n]
sampleperpshifts[candp_Association,n_Integer]:=(Join[samplefromwindow[#[[1]]],#[[3]]]&)/@samplephasonplanes[candp,n]
findpolygons[hyperpoints_(* : {vecpattern ..}should be :{vecpattern..}, bu pattern matching fails if we have too many points*)] :=Module[{possiblehypertiles},
	possiblehypertiles = ({ConstantArray[0, Length[hyperpoints[[1]]]], #[[1]], #[[1]] + #[[2]], #[[2]]} &) /@
	({UnitVector[Length[hyperpoints[[1]]], #[[1]]], UnitVector[Length[hyperpoints[[1]]], #[[2]]]} &) /@
     Subsets[Range[Length[hyperpoints[[1]]]], {2}];
  Flatten[
   Outer[Function[{hyperpoint, hypertile},
     Module[{translatedtile},
      translatedtile = Transpose[hyperpoint + Transpose[hypertile]];
      If[SubsetQ[hyperpoints, translatedtile], translatedtile, Unevaluated@Sequence[]]]
		], hyperpoints, possiblehypertiles, 1], 1]
	]
approximantunitcelltiling[candp_Association,internalshift : vecpattern ,factor:_?IntegerQ:1]:=restricttowind[approximantunitcellwindow[candp,factor],docandptilingsmasterphys[candp,approximantunitcellwindow[candp,factor],internalshift]]
approximantunitcellvertices[candp_Association,factor:_?IntegerQ:1]:=(
	If[!KeyExistsQ[candp,"periods"],Throw["unitcells only exist for approximant modell sets"]];
	N[candp["projectphysical"]/@(factor*{
		(0&)/@Range[candp["hyperdim"]],
		candp["periods"][[1]],
		candp["periods"][[1]]+candp["periods"][[2]],
		candp["periods"][[2]]
	})]
)
approximantunitcellwindow[candp_Association,factor:_?IntegerQ:1]:=(
	towindow[approximantunitcellvertices[candp,factor]]
)
numbertohypersspaceunitvec[hyperdim_Integer, number_Integer] :=
 Sign[number]*UnitVector[hyperdim, Abs[number]]
neighbouringpointstile[hyperpoint : vecpattern] := (Transpose[hyperpoint + Transpose[#]] &) /@
  DeleteDuplicates[({numbertohypersspaceunitvec[
        Length[hyperpoint], #[[1]]],
       numbertohypersspaceunitvec[Length[hyperpoint], #[[2]]],
       numbertohypersspaceunitvec[Length[hyperpoint], #[[1]]] +
        numbertohypersspaceunitvec[Length[hyperpoint], #[[2]]]} &) /@
    Flatten[Tuples /@
      Subsets[MapIndexed[(#1*#2[[1]] &), ({1, -1} &) /@
         hyperpoint], {2}], 1]]
neighbouringpointsoutwardtile[hyperpoint :vecpattern] :=(Transpose[hyperpoint+Transpose[#]]&)/@DeleteDuplicates[({
		numbertohypersspaceunitvec[Length[hyperpoint], #[[1]]],
		numbertohypersspaceunitvec[Length[hyperpoint], #[[2]]],
		numbertohypersspaceunitvec[Length[hyperpoint], #[[1]]]+numbertohypersspaceunitvec[Length[hyperpoint], #[[2]]]}
	&)/@ Flatten[Tuples /@ Subsets[MapIndexed[(#1*#2[[1]] &),
    Replace[hyperpoint, {x_Integer /; x > 0 :> {1},
      x_Integer /; x < 0 :> {-1}, 0 -> {1, -1}}, {1}]], {2}], 1]]
neighbouringpointsoutwardedge[ hyperpoint :vecpattern] := (hyperpoint +
     numbertohypersspaceunitvec[Length[hyperpoint], #] &) /@
  Flatten[MapIndexed[(#1*#2[[1]] &),
    Replace[hyperpoint, {x_Integer /; x > 0 :> {1},
      x_Integer /; x < 0 :> {-1}, 0 -> {1, -1}}, {1}]], 1]
neighbouringpointsoutwardfibo[
  hyperpoint : vecpattern] := (hyperpoint + # &) /@
  DeleteDuplicates[
   Flatten[({numbertohypersspaceunitvec[Length[hyperpoint], #[[1]]],
        numbertohypersspaceunitvec[Length[hyperpoint], #[[2]]],
        numbertohypersspaceunitvec[Length[hyperpoint], #[[2]]] +
         numbertohypersspaceunitvec[Length[hyperpoint], #[[1]]]} &) /@
      Flatten[(Tuples[# /. {1 -> {1, 2},
            2 -> {3, 4}, -1 -> {-1, -2}, -2 -> {-3, -4}}] &) /@
       Tuples[MapIndexed[(#1*#2[[1]] &),
         Replace[{Plus @@ hyperpoint[[1 ;; 2]],
           Plus @@ hyperpoint[[3 ;; 4]]}, {x_Integer /; x > 0 :> {1},
           x_Integer /; x < 0 :> {-1}, 0 -> {1, -1}}, {1}]]], 1], 1]]
neighbouringpointsoutwardrhombus[
  hyperpoint : vecpattern] := (hyperpoint + # &) /@
  DeleteDuplicates[
   Flatten[({numbertohypersspaceunitvec[Length[hyperpoint], #[[1]]],
             numbertohypersspaceunitvec[Length[hyperpoint], #[[2]]],
        numbertohypersspaceunitvec[Length[hyperpoint], #[[2]]] +
         numbertohypersspaceunitvec[Length[hyperpoint], #[[1]]]} &) /@
      Flatten[
       Tuples /@
        Subsets[MapIndexed[(#1*#2[[1]] &),
          Replace[hyperpoint, {x_Integer /; x > 0 :> {1},
            x_Integer /; x < 0 :> {-1}, 0 -> {1, -1}}, {1}]], {2}],
       1], 1]]
neighbouringpoints[hyperpoint :vecpattern] := (If[Norm[hyperpoint + #] >= Norm[hyperpoint] && Norm[#] > 0,hyperpoint + #, Unevaluated@Sequence[]] &) /@
  Tuples[{-1, 0, 1}, Length[hyperpoint]]
cleanupnonedgetoedge[hypertiling : {coloredhypertilepattern ..}] :=
 (DeleteDuplicates[SortBy[hypertiling, (-Length[#[[1]]] &)],
  Function[{tile1, tile2},
   Min[Length /@ {Complement[tile1[[1]], tile2[[1]]],
       Complement[tile2[[1]], tile1[[1]]]}] == 0]])
neighbouringpointsoutwardcolumn[
  hyperpoint :
   vecpattern] := (If[Plus @@ Abs /@ # > 0, hyperpoint + #,Unevaluated@Sequence[]] &) /@
  Tuples[Replace[
    hyperpoint, {x_Integer /; x > 0 :> {1, 0},
     x_Integer /; x < 0 :> {-1, 0}, 0 -> {1, 0, -1}}, {1}]]
defaultshiftvector[candp_Association,internalshift_]:=(internalshift /. {Null -> (0 &) /@ Range[candp["hyperdim"]-candp["physdim"]]})
Options[docandppoints] = {"niter"->Infinity};
docandppoints[candp_Association, windowphysical : {}, internalshift : vecpattern : Null,OptionsPattern[]] := (
	If[!candp["isinwindowQ"][(0 &) /@ Range[candp["hyperdim"]], candp["projectperpendicular"], defaultshiftvector[candp,internalshift],candp["acceptancewindow"]],
		Throw["2 choose shift in perpendicularspace that is inside of the acceptance window"]];
	If[Length[windowphysical]>0 && OptionValue["niter"]!=Infinity,Throw["choose either a polytope or the number of iterations, not both"]];
	({{(0 &) /@ Range[candp["hyperdim"]]}, {},OptionValue["niter"]+1} //.
		({hyperlatticepoints : {vecpattern ..}, acceptedpoints : {vecpattern ...},remainingrounds_/;remainingrounds>0}) :>
    	Append[
				Fold[
					Function[{data(*data is list of length 2:{hyperpoints_to_check in next iteration,accepted hyperpoints}*), hyperpoint},
						(If[
							candp["isinwindowQ"][hyperpoint,candp["projectperpendicular"], defaultshiftvector[candp,internalshift],candp["acceptancewindow"]],
							{Join[data[[1]],neighbouringpointsoutwardedge[hyperpoint]], Append[data[[2]], (*candp["projectphysical"]]hyperpoint]*)hyperpoint]},
         			data
						])
					],
					{{}, acceptedpoints},
      		DeleteDuplicates[hyperlatticepoints]],
				remainingrounds-1]
	)[[2]])
docandppoints[candp_Association, windowphysical : polytopepattern, internalshift : vecpattern : Null,OptionsPattern[]] := (Select[hypertilingtohyperpoints[docandptilingsmasterhyper[candp,windowphysical,internalshift,"checkforfilledwindow"->True,"niter"->OptionValue["niter"]]],(isinonwindow[windowphysical, candp["projectphysical"][#]]&)])
Options[docandppointssmarthyper] = {"niter"->Infinity};
docandppointssmarthyper[candp_Association, windowphysical : polytopepattern, internalshift : vecpattern : Null,OptionsPattern[]] := DeleteDuplicates[({{(0 &) /@ Range[candp["hyperdim"]]}, {},OptionValue["niter"]+1} //. ({hyperlatticepoints : {vecpattern ..}, acceptedpoints : {vecpattern ...},remainingrounds_/;remainingrounds>0}) :> 
     Append[Fold[Function[{data, hyperpoint}, (If[isinwindow[windowphysical, candp["projectphysical"][hyperpoint]],
					{Join[data[[1]], 
           candp["acceptedneighbouroutward"][hyperpoint,defaultshiftvector[candp,internalshift]]], Append[data[[2]], hyperpoint]}, 
         data])], {{}, acceptedpoints}, 
      DeleteDuplicates[hyperlatticepoints]],remainingrounds-1])[[2]]]
Options[docandppointsphys] = {"niter"->Infinity};
docandppointsphys[candp_Association, windowphysical : polytopepattern, internalshift : vecpattern : Null,OptionsPattern[]] :=(candp["projectphysical"][#] &) /@ docandppoints[candp,windowphysical,internalshift,"niter"->OptionValue["niter"]]
Options[docandptilingssmarthyper] = {"hyperpointsfile" -> "","niter"->Infinity};
docandptilingssmarthyper[candp_Association, windowphysical : polytopepattern, internalshift : vecpattern : Null,OptionsPattern[]] :=Module[{donepolygonsQ,hypertiles}, 
	donepolygonsQ[___]=False;
hypertiles=(If[KeyExistsQ[candp,"edgetoedge"]&&!candp["edgetoedge"],cleanupnonedgetoedge[#],#]&)[
		({{{(0 &) /@ Range[candp["hyperdim"]],1}}, {},OptionValue["niter"]} //.
			({hyperlatticepoints : {{vecpattern,_?IntegerQ} ..}, projectedpolygons : {coloredhypertilepattern...},remainingrounds_/;remainingrounds>0}) :>{
				Complement[
					DeleteDuplicates[
						SortBy[
							Select[
								Flatten[
									Function[{hyperpoint},
										({#,If[isinonwindow[windowphysical,candp["projectphysical"][hyperpoint[[1]]]],1,(*increase counter by one if hyperpoint is not in physical window*)hyperpoint[[2]]+1]}&)
										/@candp["acceptedneighbouroutward"][hyperpoint[[1]],defaultshiftvector[candp,internalshift]]
									]/@ hyperlatticepoints,1
								] ,(#[[2]]<=2(*1 corresponds to neighbour of vertex in physical window, 2 to neighbour of vertex just one single edge outside*)&)
							],Last(*put point with lowest neighbourhood counter first if point exists several times*)
						],(#1[[1]]==#2[[1]]&)
					],hyperlatticepoints(*exclude points from this generation*)
				],
				(Join[
					projectedpolygons,Select[
						Flatten[
							(candp["getneighbouringpolygons"][#[[1]],defaultshiftvector[candp,internalshift]]&)/@ hyperlatticepoints,1
						],
						(Function[{poly},Module[{toreturn},toreturn=!donepolygonsQ[Sort[poly[[1]]]];donepolygonsQ[Sort[poly[[1]]]]=True;toreturn]])
						]
				]),
				remainingrounds-1
			}
		)[[2]]
	];
((If[OptionValue["hyperpointsfile"]!="",Export[OptionValue["hyperpointsfile"],DeleteDuplicates[Flatten[#,2]]]];#)&)[Map[First,({#[1],#[2]}&)[GroupBy[hypertiles, (#[[2]] &)]],{2}]]
]
tilingshypertophys[candp_Association,hyperoutput:tilingpattern]:=(Map[(candp["projectphysical"]),hyperoutput, {3}])
Options[docandptilingssmartphys] = {"hyperpointsfile" -> "","niter"->Infinity};
docandptilingssmartphys[candp_Association, windowphysical : polytopepattern,
  internalshift : vecpattern : Null,OptionsPattern[]] :=tilingshypertophys[candp,docandptilingssmarthyper[candp,windowphysical, defaultshiftvector[candp,internalshift],"niter"->OptionValue["niter"],"hyperpointsfile"->OptionValue["hyperpointsfile"]]]
nthcorona[hypertiling : tilingpattern, n_?IntegerQ] :=(
Nest[Function[{predecessorcorona},
   	Function[{hyperpolygons},
     		Select[hyperpolygons, (
        Length[Intersection[#, Flatten[predecessorcorona, 2]]] > 0 &)]
     	] /@ hypertiling
   	],
  {{{(0 &) /@ hypertiling[[1, 1, 1]]}}},
  n]
)
docandptilingsmastercoronahyper[candp_Association, internalshift : vecpattern : Null,coronaorder_?IntegerQ]:=Module[{hypertiling},hypertiling=candp["type"]/.{
"canonical":>nthcorona[docandptilingshyper[candp,{},defaultshiftvector[candp,internalshift],"niter"->coronaorder],coronaorder],
"general":>docandptilingssmarthyper[candp,{},defaultshiftvector[candp,internalshift],"niter"->coronaorder]
}]
Options[docandptilingsmasterhyper] = {"niter"->Infinity,"checkforfilledwindow"->True,"minimaltiling"->False,"hyperpointsfile"->"","neighbourhood"->Infinity};
docandptilingsmasterhyper[candp_Association, windowphysical : polytopepattern, internalshift : vecpattern : Null,OptionsPattern[]]:=Module[{phystiling,hypertiling},
hypertiling=(If[OptionValue["niter"]!=Infinity,nthcorona[#,OptionValue["niter"]],#]&)[
	docandptilingshyper[candp,windowphysical,defaultshiftvector[candp,internalshift],"niter"->OptionValue["niter"],"hyperpointsfile"->OptionValue["hyperpointsfile"]]
];
checkandfiltertiling[candp,windowphysical,hypertiling,"checkforfilledwindow"->OptionValue["checkforfilledwindow"],"minimaltiling"->OptionValue["minimaltiling"]]
]
Options[checkandfiltertiling]={"checkforfilledwindow"->True,"minimaltiling"->False};
checkandfiltertiling[candp_Association, windowphysical:polytopepattern,hypertiling:tilingpattern,OptionsPattern[]]:=Module[{phystiling,hypertilingmod,windowphysicalverts},
	If[Length[windowphysical]>0,(
		windowphysicalverts=topolygon[windowphysical];
		hypertilingmod=hypertiling;
		If[(OptionValue["checkforfilledwindow"]||OptionValue["minimaltiling"]),
			(phystiling=tilingshypertophys[candp,hypertiling];
			If[
				OptionValue["minimaltiling"],
				(
				hypertilingmod=Pick[hypertiling,Map[(polywindowintersectQ[N[#],windowphysical,windowphysicalverts]&),phystiling,{2}]]
				)
			];
			If[
				(OptionValue["checkforfilledwindow"]),
				(
				restricttowindfilled[windowphysical,phystiling](*we use restricttowindfilled only to check not to do the restriction*)
				)
			]
		)];
		hypertilingmod),
		hypertiling
	]
]
Options[docandptilingsmasterphys] = {"hyperpointsfile" -> "","neighbourhood"->Infinity,"niter"->Infinity,"checkforfilledwindow"->True,"minimaltiling"->False};
docandptilingsmasterphys[candp_Association, windowphysical : polytopepattern,internalshift : vecpattern : Null,OptionsPattern[]]:=tilingshypertophys[candp,docandptilingsmasterhyper[candp,windowphysical,internalshift,"checkforfilledwindow"->OptionValue["checkforfilledwindow"],"minimaltiling"->OptionValue["minimaltiling"],"neighbourhood"->OptionValue["neighbourhood"],"niter"->OptionValue["niter"],"hyperpointsfile"->OptionValue["hyperpointsfile"]]]
hyperpointstohypertilingcanonical[candp_Association,hyperpoints:{vecpattern..}]:=(
If[!DuplicateFreeQ[hyperpoints],Throw["List of hyperpoints contains duplicates"]];
	(({#[1]/._Missing->{},#[2]/._Missing->{}})&)[
		GroupBy[
			findpolygons[hyperpoints],
			candp["assignregion"]
		]
	]
)
Options[docandptilingshyper] = {"hyperpointsfile" -> "","niter"->Infinity,"neighbourhood"->Infinity};
docandptilingshyper[candp_Association, windowphysical : polytopepattern,internalshift : vecpattern : Null,OptionsPattern[]] := (If[OptionValue["neighbourhood"]!=Infinity && windowphysical=={},nthcorona[#,OptionValue["neighbourhood"]],#]&)[(hyperpointstohypertilingcanonical[candp,
	((If[OptionValue["hyperpointsfile"]!="",Export[OptionValue["hyperpointsfile"],#]];#)&)[
		{windowphysical,OptionValue["niter"],OptionValue["neighbourhood"]}/.{
			{{}, _?NumericQ, _} :> (docandppoints[candp,{},internalshift,"niter"->OptionValue["niter"]]),
			{{}, _, _?NumericQ} :> (docandppoints[candp,{},internalshift,"niter"->OptionValue["neighbourhood"]*2]),
			{_?(Length[#] > 0 &), Infinity, Infinity} :> (docandptilingshyperintern[candp,windowphysical,internalshift]),
			{_, _, _} :> Throw["Wrong combination of parameters:",OptionValue["niter"],OptionValue["neighbourhood"]]
		}
	]
])]
Options[docandptilingshyperintern] = {"niter"->Infinity};
docandptilingshyperintern[candp_Association, windowphysical : polytopepattern,internalshift : vecpattern : Null,OptionsPattern[]] :=Module[{pointtoneighbours,nrneighboursbeyondwindow,newcounter,res},
	(*this function differs from docandppoints in that we may need points situated outside of the window*)
	nrneighboursbeyondwindow=4(*+1 in case window intersects with a polygon but does not contain any vertex,+2 for one case where neighbouringpointsoutwardedge forces us to go outward in hyperspace and therefore not let us complete the polygon*);
	If[!candp["isinwindowQ"][(0 &) /@ Range[candp["hyperdim"]], candp["projectperpendicular"], defaultshiftvector[candp,internalshift],candp["acceptancewindow"]],
		Throw["choose shift in perpendicularspace that is inside of the acceptance window"]];
	res=({
		{{(0 &) /@ Range[candp["hyperdim"]],1}},
		{},
		0
		} //. ({
		hyperlatticepoints : {{vecpattern,_?IntegerQ }..}(*Integer denotes how many edges out of the phyiscal window a given point is*),
		acceptedpoints : {vecpattern ...},
		roundsdone_/;roundsdone<=OptionValue["niter"]
	}) :> (Append[({
					#[[1]],
					#[[2]]
					}&)[
						Fold[
							Function[{data(*data[[1]]=new points to check,data[[2]]list of points to project*), hyperpoint(*hyperpoint[[1]] are coordinates in hyperlattice, hyperpoint[[2]] is distance from cut window*)},
								(If[
									candp["isinwindowQ"][hyperpoint[[1]], candp["projectperpendicular"], defaultshiftvector[candp,internalshift],candp["acceptancewindow"]],
									{
										Join[data[[1]],
											(If[
												isinonwindow[windowphysical,N[candp["projectphysical"][hyperpoint[[1]]]]]||isinonwindow[windowphysical,N[candp["projectphysical"][#]]],
							((*Print[hyperpoint[[1]]," adding ",#," with distance",1];*){#,1}),
							((*Print[hyperpoint[[1]]," adding ",#," with distance",hyperpoint[[2]]+1];*)	{#,hyperpoint[[2]]+1})
											]&)/@ neighbouringpointsoutwardedge[hyperpoint[[1]]]
										],
										Append[(*append hyperpoint to list of points to project*)
											data[[2]],hyperpoint[[1]]
										]
									}
									,(*do nothing if point not in acceptancewindow*) data
								])
							],
							{{}, acceptedpoints},
							DeleteDuplicates[
								Select[
									Sort[hyperlatticepoints,(Sign[Last[#2] - Last[#1]] &)],
									(#[[2]]<=nrneighboursbeyondwindow &)
								],
								(#1[[1]]==#2[[1]]&)
							]
						]
					],
				roundsdone+1])
		)[[2]];
	res
	]
Options[docandptilingsphys] = {"hyperpointsfile" -> "","niter"->Infinity};
docandptilingsphys[candp_Association, windowphysical : polytopepattern,internalshift : vecpattern : Null,OptionsPattern[]] :=(tilingshypertophys[candp,docandptilingshyper[candp,windowphysical,defaultshiftvector[candp,internalshift],"niter"->OptionValue["niter"],"hyperpointsfile"->OptionValue["hyperpointsfile"]]])
filterhypertilingbysubstructure[hypertiling : {{vecpattern ..} ..},subsstructure : {vecpattern ..}] :=
 Module[{selectedpoints, substructurepoints},
  selectedpoints =
   Select[Flatten[hypertiling, 1],
    Function[{hyperpoint},
     AllTrue[(hyperpoint + # &) /@
       subsstructure, (MemberQ[Flatten[hypertiling, 1], #] &)]]];
  substructurepoints =
   Outer[Plus, selectedpoints, subsstructure, 1, 1];
  Reverse[SortBy[(*we assume that SortBy length places polygons not matching substructure in first position, since they are always the clear majority*)
  GatherBy[hypertiling, 
   Function[{hyperpolygon}, 
    Select[Range[
      Length[substructurepoints]], (SubsetQ[substructurepoints[[#]], hyperpolygon] &)]]], Length]]]
Options[docandptilingsphyssubstructure] = {"niter"->Infinity};
docandptilingsphyssubstructure[candp_Association, windowphysical : polytopepattern,substructure:{vecpattern..},internalshift : vecpattern : Null,OptionsPattern[]] :=
Module[{hypertiling},
	({#[[1]],Flatten[#[[2;;]],1]}&)[restricttowind[
		windowphysical,
		Map[(candp["projectphysical"]),
			filterhypertilingbysubstructure[
				Flatten[docandptilingsmasterhyper[candp,windowphysical,defaultshiftvector[candp,internalshift],"niter"->OptionValue["niter"]],1],
				substructure
			],{3}
		]
	]]
]
windowtophasonplane[wind:polytopepattern]:=Sequence[wind,Abs[signedArea[topolygon[wind]]]]
windowtophasonplane[Null]:=Throw["no phason-plane existing"]
(*fibo chain according to aperiodic order*)
fibochainprojector={{1, GoldenRatio}, {1, -GoldenRatio + 1}}
fibochainisinwindowQ[latticepoint:vecpattern, projector_, shift:vecpattern,window:polytopepattern]:=(isinwindow[window[[1;;1]],projector[latticepoint] + shift[[1;;1]]] && isinonwindow[window[[2;;2]], projector[latticepoint] + shift[[1;;1]]])
fibochaincandp=<|"projectphysical" -> ((fibochainprojector[[1;;1]] . #) &),
   "projectperpendicular" -> (fibochainprojector[[2;;2]] . # &),
		"acceptancewindow"->{{{-1}, 1},{{1}, GoldenRatio - 1}},
	"name"->"fibonaccichain",
   "isinwindowQ"->Function[{latticepoint, projector,shift,window},fibochainisinwindowQ[latticepoint, projector,shift,window]],
   "hyperdim" -> 2,
   "phasondim" -> 1,(*dimension of subspace of displacements in internal space that leave tiling indistinguishable*)
   "physdim" -> 1,
   "type" -> "canonical"
	|>
generalcandp2d[projectormatrix:{vecpattern,vecpattern}]:=Module[{corrector={0},window}, 
	window={{{1},Max[(projectormatrix[[2]] . # &) /@ Tuples[{-1/2, 1/2}, 2]]},{{-1},-Min[(projectormatrix[[2]] . # &) /@ Tuples[{-1/2, 1/2}, 2]]}};
<|"projectphysical" -> ((projectormatrix[[1;;1]] . #) &),
   "projectperpendicular" -> (projectormatrix[[2;;2]] . # &),
	"acceptancewindow"->window,
	"name"->"2d cut-and-project",
   "type" -> "canonical",
   "isinwindowQ"->
   Function[{latticepoint, projector, shift, window},
    ((corrector = #[[2]]; #[[1]]) &)[isinonwindowapprox[window, projector[latticepoint] + shift[[1 ;; 1]],corrector]]],
   "hyperdim" -> 2,
   "phasondim" -> 1,(*dimension of subspace of displacements in internal space that leave tiling indistinguishable*)
   "physdim" -> 1
	|>
]

(*fibo chain hypercube*)
fibochainhypercubeprojector={{GoldenRatio/Sqrt[1 + GoldenRatio^2], 1/Sqrt[1 + GoldenRatio^2]}, {-1/Sqrt[1 + GoldenRatio^2], GoldenRatio/Sqrt[1 + GoldenRatio^2]}}
nu=Sqrt[GoldenRatio^2 + 1]
fibochainhypercubecandp[]:=Module[{candp},
candp=generalcandp2d[fibochainhypercubeprojector];
candp["name"]="fibonaccichainhypercube";
candp
]
(*fibo hypercube square*)
fibohypercubeprojector={
	{Sequence @@ fibochainhypercubeprojector[[1]], 0, 0},
	{0, 0, Sequence @@ fibochainhypercubeprojector[[1]]},
	{Sequence @@ fibochainhypercubeprojector[[2]], 0, 0},
	{0,0, Sequence @@ fibochainhypercubeprojector[[2]]}
}
fibosquarecandp[]:=Module[{candp},
	candp=generalcutandproject[fibohypercubeprojector];
  candp["LIshift"]=({}&);
  candp["LIshiftnormed"]=({}&);
  candp["phasondim"]=2;
	candp["basisvectorscolinearinphysicalspace"]={{1,2},{3,4}};
	candp["acceptancewindow"]=removeduplicateedges[candp["acceptancewindow"]];
	candp["phasonplanes"]={{windowtophasonplane[candp["acceptancewindow"]],{}}};
	candp
]
(*fibo hypercube square approximant, selfmade by approximating golden ratio*)
fibosquarehypercubecandpapproximant[n_Integer,m_Integer]:=Module[{candp,projector},
	projector=Join[
		fibohypercubeprojector[[1;;2]],
		{Normalize[{-Fibonacci[n-1],Fibonacci[n],0,0}]},
		{Normalize[{0,0,-Fibonacci[m-1],Fibonacci[m]}]}
	];
	candp=generalcutandproject[projector];
  candp["LIshift"]=({}&);
	candp["basisvectorscolinearinphysicalspace"]={{1,2},{3,4}};
  candp["LIshiftnormed"]=({}&);
  candp["phasondim"]=2;
  candp["periods"]=({
		periodofapproximant["fibonaccisquaresapproximant",n,m,1],
		periodofapproximant["fibonaccisquaresapproximant",n,m,2]
	});
	candp["acceptancewindow"]=removeduplicateedges[candp["acceptancewindow"]];
	candp["phasonplanes"]={{windowtophasonplane[candp["acceptancewindow"]],{}}};
	candp
]
(*fibo hypercube rectangle, the candp in y direction only has half the slope of an actual fibonacci chain*)
fibod2projector = {
{GoldenRatio/Sqrt[1 + GoldenRatio^2], 1/Sqrt[ 1 + GoldenRatio^2], 0, 0},
{0, 0, (GoldenRatio/2)/Sqrt[ 1 + GoldenRatio^2/4], 1/Sqrt[ 1 + GoldenRatio^2/4]},
{-Sqrt[ 1 - GoldenRatio^2/(1 + GoldenRatio^2)], Sqrt[ 1 - 1/(1 + GoldenRatio^2)], 0, 0},
{0, 0, -((GoldenRatio/2)/Sqrt[1 + GoldenRatio^2/4]), 1/Sqrt[ 1 + GoldenRatio^2/4]}
}
fiborectanglehypercubecandp[]:=Module[{candp},
	candp=generalcutandproject[fibod2projector];
	candp["acceptancewindow"]=removeduplicateedges[candp["acceptancewindow"]];
	candp["phasonplanes"]={{windowtophasonplane[candp["acceptancewindow"]],{}}};
  candp["LIshift"]=({}&);
  candp["LIshiftnormed"]=({}&);
	candp["phasondim"]=2;
	candp
]

(*fibo hypercube rectangle, the candp in y direction has 3/2 the slope of an actual fibonacci chain*)
fibod2projector2 = {
{GoldenRatio/Sqrt[1 + GoldenRatio^2], 1/Sqrt[ 1 + GoldenRatio^2], 0, 0},
{0, 0, (GoldenRatio*1.5)/Sqrt[ 1 + GoldenRatio^2*(1.5^2)], 1/Sqrt[ 1 + GoldenRatio^2*(1.5^2)]},
{-Sqrt[ 1 - GoldenRatio^2/(1 + GoldenRatio^2)], Sqrt[ 1 - 1/(1 + GoldenRatio^2)], 0, 0},
{0, 0, -1/Sqrt[1 + GoldenRatio^2*(1.5^2)], (GoldenRatio*1.5)/Sqrt[ 1 + GoldenRatio^2*(1.5^2)]}
}
fiborectangle2hypercubecandp[]:=Module[{candp},
	candp=generalcutandproject[fibod2projector2];
	candp["acceptancewindow"]=removeduplicateedges[candp["acceptancewindow"]];
	candp["basisvectorscolinearinphysicalspace"]={{1,2},{3,4}};
	candp["phasonplanes"]={{windowtophasonplane[candp["acceptancewindow"]],{}}};
  candp["LIshift"]=({}&);
  candp["LIshiftnormed"]=({}&);
	candp["phasondim"]=2;
	candp
]
fiborectanglehypercubecandpapproximant[n_Integer,m_Integer]:=Module[{candp,projector},
	projector=Join[
		fibod2projector2[[1;;2]],
		{Normalize[{-1,Fibonacci[n]/Fibonacci[n-1],0,0}]},
		{Normalize[{0,0,-1,1.5*Fibonacci[m]/Fibonacci[m-1]}]}
	];
	candp=generalcutandproject[projector];
  candp["LIshift"]=({}&);
  candp["LIshiftnormed"]=({}&);
	candp["basisvectorscolinearinphysicalspace"]={{1,2},{3,4}};
  candp["phasondim"]=2;
  candp["periods"]=({
		periodofapproximant["fibonaccirectanglesapproximant",n,m,1],
		periodofapproximant["fibonaccirectanglesapproximant",n,m,2]
	});
	candp["acceptancewindow"]=removeduplicateedges[candp["acceptancewindow"]];
	candp["phasonplanes"]={{windowtophasonplane[candp["acceptancewindow"]],{}}};
	candp
]
getneighbourpredictor[wind : polytopepattern, multrotinternal_Integer,
  order_Integer, internalprojector : {vecpattern ..},
  hyperrotationmatrix : {vecpattern ..}, requiredshifts : {vecpattern ..}(*fundamental domain of set of all possible edges of considered point in hyperspace under action of hyperrotationmatrix*),
  grantedshifts : {vecpattern ..}(*edges along which we proceed afterwards, subset of requiredshifts*)] :=
 AssociationThread @@
  Transpose[
   MapThread[(({#1, #2}) &), {getwindowadmittingshiftssymmetric[wind,
      Transpose[internalprojector . Transpose[requiredshifts]],
      multrotinternal,
      order], (Function[{shift},
          MatrixPower[hyperrotationmatrix, #] . shift] /@
         grantedshifts &) /@ Range[order]}]]
(*remove redundancy from predictor: if hypertiles {a,b,c} and \
{a,b,c,d}, with each letter corresponding to a hypervertex, can be \
predicted, then always predict the most specific one possible, i.e we \
set acceptwindow({a,b,c})<-acceptwindow({a,b,c})\acceptwindow({a,b,c,\
d})*)
cleanuppredictor[predictor_Association] :=
 Module[{inversepredictor},(*Check that *)
  If[Length[DeleteDuplicates[(#[[All, 1]] &) /@ Keys[predictor]]] > 1,
    Throw["predictor has not been generated by getpolygonpredictor, \
the normal vectors of each window do not appear in the same order"]];
  inversepredictor = Association[Reverse /@ Normal[predictor]];
  AssociationThread[
   Sequence @@
    Transpose[
     Function[{key}, {(Fold[
          Function[{currentacceptwindow,
            hyperpoly}, 
            MapThread[({#1[[1]],
                Min[#1[[2]], #2[[2]]]} &), {currentacceptwindow,
              inversepredictor[
               hyperpoly]}](*subtracts acceptance window of \
hyperpoly from currentacceptwindow*)], key,
          Select[(*select all other predictable polygons that are of \
same region as and include predictor[key]*)
           Keys[inversepredictor], (SubsetQ[Sort[#[[1]]],
               Sort[predictor[key][[1]]]] &&
              Not[SameQ[Sort[#[[1]]],
                Sort[predictor[key][[1]]]]] && #[[2]] ==
               predictor[key][[2]] &)]]), predictor[key]}] /@
      Keys[predictor]]]]
getpolygonpredictor[window : polytopepattern(*acceptance window*), multrotinternal_Integer(*at which multiplier a rotation in physical space manifests as rotation in internal space, set to 0 if all possible tiles are given as argument hypertiles*),
  order_Integer(*order of symmetry, set to 1 if all possible tiles are given as argument hypertiles*), internalprojector : {vecpattern ..},
  hyperrotationmatrix : {vecpattern ..}(*set to identitymatrix if all possible tiles are given as hypertiles*), hypertiles : {coloredhypertilepattern ..}(*fundamental domain of set of all possible tiles in hyperspace under action of hyperrotationmatrix*)] :=
  AssociationThread[
  Flatten[(getwindowadmittingshiftssymmetric[window,
       Transpose[internalprojector . Transpose[#[[1]]]],
       multrotinternal, order] &) /@ hypertiles, 1],
  Flatten[Function[{tile}, ({Transpose[
          MatrixPower[hyperrotationmatrix, #] . Transpose[tile[[1]]]],
          tile[[2]]} &) /@ Range[order]] /@ hypertiles, 1]]
(*Penrose approximant_tiling, Scheme 2 of An algorithm for generating quasiperiodic patterns and their approximants, Lord, and Rational approximants to 5, 8 and 7-fold two-dimensional tilings*)
penroseapproximant[n_Integer,m_Integer,a_?NumericQ]:=
 Module[{corrector = {0, 0,0},LIshift,LIshiftnormed,projector,candp},(
	LIshiftnormed=({(#1+a)}&);
	LIshift=(LIshiftnormed[#]/Sqrt[5]&);
	projector={
		projectorpenrose[[1]],
		projectorpenrose[[2]],
		Normalize[{2*Fibonacci[n - 1], -Fibonacci[n], Fibonacci[n] - Fibonacci[n - 1], Fibonacci[n] - Fibonacci[n - 1], -Fibonacci[n]}],
		Normalize[{0, Fibonacci[m - 1], -Fibonacci[m], Fibonacci[m], -Fibonacci[m - 1]}],
(1/Sqrt[5] &) /@ Range[5]
	};
	candp=generalcutandproject[projector];
	candp["LIshift"]=LIshift;
	candp["periods"]={
		periodofapproximant["penroseapproximant",n,m,1],
		periodofapproximant["penroseapproximant",n,m,2]
	};
	candp["LIshiftnormed"]=LIshiftnormed;
	candp["assignregion"]=Function[
	 		{poly},
			(
				(
					Mod[
						FirstPosition[#[[2]], x_Integer /; x != 0][[1]] - FirstPosition[#[[4]], x_Integer /; x != 0][[1]], 5
					]
				&)[
					(# - poly[[1]] &) /@ poly
				] /. {1 -> 1, 4 -> 1, 2 -> 2, 3 -> 2}
			)
		];
 	candp["isinwindowQ"]= Function[{latticepoint, projector,shift,window}, 
		((corrector = #[[2]]; #[[1]]) &)[
      isinonwindowapprox[window,
      	projector[latticepoint] + Join[shift[[1 ;; 2]],LIshift[0]],
       	corrector
			]
		]
	];	
	candp
)]
(*Penrose tiling*)
projectorpenrose =
  Sqrt[2/5]*{(Cos[2*Pi*(# - 1)/5] &) /@ Range[5],
						 (Sin[2*Pi*(# - 1)/5] &) /@ Range[5],
						 (Cos[4*Pi*(# - 1)/5] &) /@ Range[5],
						 (Sin[4*Pi*(# - 1)/5] &) /@ Range[5],
              (1/Sqrt[2] &) /@ Range[5]};
hyperverts = Tuples[({-1/2, 1/2} &) /@ Range[5]];
projectedverts = (projectorpenrose[[3 ;;]] . # &) /@ hyperverts;
hyperfacetsverts =
  Flatten[(Function[{dims}, {#, # +
          UnitVector[Length[#], dims[[1]]], # +
          UnitVector[Length[#], dims[[1]]] +
          UnitVector[Length[#], dims[[2]]], # +
          UnitVector[Length[#], dims[[2]]]}] /@
       Subsets[Flatten[Position[#, -1/2]], {2}] &) /@ hyperverts, 1];
icosahedronfacetsverts = (Transpose[
      projectorpenrose[[3 ;;]] . Transpose[#]] &) /@ hyperfacetsverts;
planesicosahedron = (
	{(*window edge format of normal vector and limit*)
		#[[1]],
		FullSimplify[#[[1]] . #[[ 2]]]
	} &) /@ (
	{
		Cross[#[[2]] - #[[1]], #[[4]] - #[[1]] ],
		#[[1]]
	} &) /@ icosahedronfacetsverts;
lim = 10^(-10)
planesclassifiedicosahedron = (
	{
		Count[
			Function[
				{point},
				point . #[[1]] > #[[2]] + lim
			] /@ projectedverts, True
		],
    Count[
			Function[
				{point},
				point . #[[1]] < #[[2]] - lim
			] /@ projectedverts, (True)
		]
	} &) /@ planesicosahedron;
facetsicosahedron =
  MapThread[
		(#2 /. {{0, _?IntegerQ} :> #1, {_?IntegerQ, 0} :> -#1, ___ :> Sequence[]} &),
		{planesicosahedron, planesclassifiedicosahedron}
	];
facetsicosahedron =
  FullSimplify[({#[[1]]/Norm[#[[1]]], #[[2]]/Norm[#[[1]]]} &) /@
    facetsicosahedron];
penrosecandp[a : _? NumericQ] :=
 Module[{corrector = {0, 0,0},LIshift,LIshiftnormed},(
	LIshiftnormed=({(#1+a)}&);
	LIshift=(LIshiftnormed[#]/Sqrt[5]&);
	<|
	"name"->"penrose",
	"LIshift"->LIshift,
	"LIshiftnormed"->LIshiftnormed,
   "hyperdim" -> 5,
   "phasondim" -> 2,(*dimension of subspace of displacements in internal space that leave tiling indistinguishable*)
   "type" -> "canonical",
   "physdim" -> 2,
   "phasonplanes" -> ({
			windowtophasonplane[projecttosubspace[{Null,Null,LIshift[#][[-1]]},N[facetsicosahedron]]],
			{#}
		}&)/@Select[Range[-5, 5], (-2.5 < # +a < 2.5 &)],
		"getcorrector" :> corrector,
   "projectphysical" -> (projectorpenrose[[1 ;; 2]] . # &),
   "projectperpendicular" -> (projectorpenrose[[3 ;; 5]] . # &),
	 "acceptancewindow"->facetsicosahedron,
   "isinwindowQ" ->
    Function[{latticepoint, projector,shift,window}, ((corrector = #[[2]]; #[[1]]) &)[
      isinonwindowapprox[window,
       projector[latticepoint] +
        Join[shift[[1 ;; 2]],LIshift[0]],
       corrector,2]]],
   "assignregion" ->Function[
	 		{poly},
			(
				(
					Mod[
						FirstPosition[#[[2]], x_Integer /; x != 0][[1]] - FirstPosition[#[[4]], x_Integer /; x != 0][[1]], 5
					]
				&)[
					(# - poly[[1]] &) /@ poly
				] /. {1 -> 1, 4 -> 1, 2 -> 2, 3 -> 2}
			)
		]
	|>)]
penrosecandp[] := penrosecandp[1/2]

(*ammann-beenker from unit hypercubic lattice, the first one i coded has hyperedgelength of Sqrt[2]*)
oktagonwindow = ({{Cos[2*Pi*#/8], Sin[2*Pi*#/8]}, 1/2 (1 + Sqrt[2])/Sqrt[2]} &) /@ Range[8]
ammannbeenkerprojector={
	{1/Sqrt[2], 1/2, 0, -(1/2)},
	{0, 1/2, 1/Sqrt[2], 1/2},
	{1/Sqrt[ 2], -(1/2), 0, 1/2},
	{0, 1/2, -(1/Sqrt[2]), 1/2}
	}
ammannbeenkercandp[]:=Module[{corrector={0,0}} ,
	<|"hyperdim" -> 4,
   "physdim" -> 2,
   "LIshift" -> ({}&),
   "LIshiftnormed" -> ({}&),
	"name" ->"ammann",
   "phasondim" -> 2,(*dimension of subspace of displacements in internal space that leave tiling indistinguishable*)
   "type" -> "canonical",
   "phasonplanes" -> {{windowtophasonplane[oktagonwindow],{}}},
  "projectphysical" -> (ammannbeenkerprojector[[1;;2]] . # &),
  "projectperpendicular" -> (ammannbeenkerprojector[[3;;4]] . # &),
  "acceptancewindow" -> oktagonwindow,
  "isinwindowQ" ->
   Function[{latticepoint, projector, shift, window},
    ((corrector = #[[2]]; #[[1]]) &)[isinonwindowapprox[oktagonwindow, projector[latticepoint] + shift[[1 ;; 2]],corrector]]],
  "assignregion" ->
   Function[{poly}, ((Mod[
          FirstPosition[#[[2]], x_Integer /; x != 0][[1]] -
           FirstPosition[#[[4]], x_Integer /; x != 0][[1]],
          4] &)[(# - poly[[1]] &) /@ poly] /. {1 -> 1, 3 -> 1,2 -> 2})]
	 |>]
(*ammannbeenker-approximant, does not correspond to duneau*)
pellnumber[n_Integer] := Round[((1 + Sqrt[2])^n - (1 - Sqrt[2])^n)/2/Sqrt[2]]
pelllucasnumber[n_Integer] := Round[((1 + Sqrt[2])^n + (1 - Sqrt[2])^n)]
rationalsqrt2[n_Integer] := pelllucasnumber[n]/pellnumber[n]/2
ammannbeenkerapproximant[n_Integer,m_Integer]:=Module[{corrector={0,0},projector,candp},
	projector={Sequence @@ 
		Normalize /@ ammannbeenkerprojector[[1 ;; 2]],
		Normalize[{1, -1/rationalsqrt2[n], 0, 1/rationalsqrt2[n]}],
		Normalize[{0, 1/rationalsqrt2[m], -1, 1/rationalsqrt2[m]}]
 	};
	candp=generalcutandproject[projector];
  candp["LIshift"]=({}&);
  candp["LIshiftnormed"]=({}&);
	candp["assignregion"]=Function[{poly},(
		(Mod[
				FirstPosition[#[[2]], x_Integer /; x != 0][[1]] - FirstPosition[#[[4]], x_Integer /; x != 0][[1]], 4
			] &)[
			(# - poly[[1]] &) /@ poly
		] /. {1 -> 1, 3 -> 1,2 -> 2})];
  candp["phasondim"]=2;
	candp
]
(*ammannbeenker-approximant According to Appendix B of Jagannathan and dunneau, i succesfully compared this to figure 10, it is rotated 90 degrees counterclockwise with regards to figure 15 *)
ammannbeenkerapproximantduneau[n_Integer,m_Integer]:=Module[{corrector={0,0},projector,candp},
	projector={Sequence @@ 
		Normalize /@ ammannbeenkerprojector[[1 ;; 2]],
		Normalize[{2*pellnumber[n], -(pellnumber[n]+pellnumber[n-1]), 0, (pellnumber[n]+pellnumber[n-1])}],
		Normalize[{0, (pellnumber[m]+pellnumber[m-1]), -2*pellnumber[m], (pellnumber[m]+pellnumber[m-1])}]
 	};
	candp=generalcutandproject[projector];
  candp["LIshift"]=({}&);
  candp["LIshiftnormed"]=({}&);
  candp["phasonplanes"]= {{windowtophasonplane[candp["acceptancewindow"]],{}}};
  candp["periods"]=({(*consequtive pell-numbers are coprime up to a reasonable limit so no reason to LCM*)
		periodofapproximant["ammannapproximantduneau",n,m,1],
		periodofapproximant["ammannapproximantduneau",n,m,2]
	});
	candp["assignregion"]=Function[{poly},(
		(Mod[
				FirstPosition[#[[2]], x_Integer /; x != 0][[1]] - FirstPosition[#[[4]], x_Integer /; x != 0][[1]], 4
			] &)[
			(# - poly[[1]] &) /@ poly
		] /. {1 -> 1, 3 -> 1,2 -> 2})];
  candp["phasondim"]=2;
	candp
]
(*ammannbeenker nonsymmorph*)
shiftvectors=0.2*({#[[2]], -#[[1]]} &) /@ Transpose[ammannbeenkerprojector[[1;;2]]](*c^j of Rabson nonsymmorphic*)
ammannbeenkernonsymmorphcandp[]=Module[{corrector={0,0}} ,
	<|"hyperdim" -> 4,
   "physdim" -> 2,
   "LIshift" -> ({}&),
   "phasondim" -> 2,(*dimension of subspace of displacements in internal space that leave tiling indistinguishable*)
	"name" ->"ammann",
   "type" -> "canonical",
(*Projectors according to Example 3.6 of aperiodic order*)
  "projectphysical" -> ((ammannbeenkerprojector[[1;;2]] . #)+(Transpose[shiftvectors].Mod[#,2]) &),
  "projectperpendicular" -> (ammannbeenkerprojector[[3;;4]] . # &),
  "acceptancewindow" -> oktagonwindow,
  "isinwindowQ" ->
   Function[{latticepoint, projector, shift, window},
    ((corrector = #[[2]]; #[[1]]) &)[isinonwindowapprox[oktagonwindow, projector[latticepoint] + shift[[1 ;; 2]],corrector]]],
  "assignregion" ->
   Function[{poly}, ((Mod[
          FirstPosition[#[[2]], x_Integer /; x != 0][[1]] -
           FirstPosition[#[[4]], x_Integer /; x != 0][[1]],
          4] &)[(# - poly[[1]] &) /@ poly] /. {1 -> 1, 3 -> 1,2 -> 2})]
  |>]
(*shield tiling*)
dodecagonwindow = ({{Cos[2*Pi*#/12], Sin[2*Pi*#/12]},
     1/2 (2 + Sqrt[3])} &) /@ Range[12]
shieldprojector = {{1, Sqrt[3]/2, 1/2, 0}, {0, 1/2, Sqrt[3]/2,1},{1, -Sqrt[3]/2, 1/2, 0}, {0, 1/2, -Sqrt[3]/2, 1}}(*Hyperlattice is A2xA2 with A2 being root lattice*)
shieldphysprojector = shieldprojector[[1;;2]]
shieldintprojector = shieldprojector[[3;;4]]
twelfefoldrotationhyperspace =
 Transpose[{{0, 1, 0, 0}, {0, 0, 1, 0}, {0, 0, 0, 1}, {-1, 0, 1, 0}}]
twelfefoldedgeshyperspace = (MatrixPower[
      twelfefoldrotationhyperspace, #] . {-1, 0, 1, -1} &) /@ Range[12]
twelfefoldpredictordiagonalofhex =
  getneighbourpredictor[dodecagonwindow, 5, 12, shieldintprojector,
   twelfefoldrotationhyperspace, {{0, 0, 0, 0}, {1, -1, 0, 1}, {1, -1,1, 0}, {1, -1, 0, 0}, {2, -1, -1, 1}}, {{1, -1, 1,0}, {2, -1, -1, 1}}];
twelfefoldpredictoredges =getneighbourpredictor[dodecagonwindow, 5, 12, shieldintprojector,
   twelfefoldrotationhyperspace, {{0, 0, 0, 0}, {-1, 0, 1, -1}}, {{-1, 0, 1, -1}}];
twelfefoldpredictorneighbours =
  Merge[{twelfefoldpredictoredges, twelfefoldpredictordiagonalofhex},
   First];
shieldtiles = {{Accumulate[(MatrixPower[(*triangle*)
         twelfefoldrotationhyperspace, #] . {1, -1, 0, 1} &) /@ {0, 4,
       8}], 2}, {Accumulate[(MatrixPower[
         twelfefoldrotationhyperspace, #] . {1, -1, 0, 1} &) /@ {0, 3,
       6, 9}],
   1}, {Accumulate[(MatrixPower[
         twelfefoldrotationhyperspace, #] . {1, -1, 0, 1} &) /@ {0, 1,
       4, 5, 8, 9}],
   1}, {Accumulate[(MatrixPower[
         twelfefoldrotationhyperspace, #] . {1, -1, 0, 1} &) /@ {1, 4,
       5, 8, 9, 12}], 1}}
twelfefoldpredictorpolygons = getpolygonpredictor[dodecagonwindow, 5, 12, shieldintprojector,
   twelfefoldrotationhyperspace, shieldtiles];
(*matrix realises 1/12 rotation in real space and 5/12 rotation in \
internal space*)
shieldcandp[] :=
 Module[{corrector = {0, 0}}, <|"hyperdim" -> 4,
   "physdim" -> 2,
   "LIshift" -> ({}&),
   "LIshiftnormed" -> ({}&),
	"name"->"shield",
   "phasondim" -> 2,(*dimension of subspace of displacements in internal space that leave tiling indistinguishable*)
   "projectphysical" -> (shieldphysprojector . # &),
   "projectperpendicular" -> (shieldintprojector . # &),
   "type" -> "general",
   "phasonplanes" -> {{windowtophasonplane[dodecagonwindow],{}}},
   "isinwindowQ" ->
    Function[{latticepoint, projector, shift,
      window}, ((corrector = #[[2]]; #[[1]]) &)[
      isinonwindowapprox[window,
       projector[latticepoint] + shift[[1 ;; 2]], corrector]]],
   "assignregion" -> Function[{poly}, (1)],
   "acceptancewindow" -> dodecagonwindow,
   "neighboursoutward" ->
    Function[{hyperpoint},
     N[Norm[shieldphysprojector . (hyperpoint)]]; (If[
         Norm[shieldphysprojector . (hyperpoint + #)] -
           N[Norm[shieldphysprojector . (hyperpoint)]] > 10^(-5),
         hyperpoint + #, Unevaluated@Sequence[]] &) /@
      twelfefoldedgeshyperspace],
   "getcorrector" :> corrector,
   "acceptedneighbouroutward" ->
    Function[{hyperpoint, shift},
     Select[(hyperpoint + # &) /@
       Join @@ Values[
         KeySelect[twelfefoldpredictorneighbours,
          Function[{window}, ((corrector = #[[2]]; #[[1]]) &)[
            isinonwindowapprox[
             window, (shieldintprojector . hyperpoint) + shift[[1 ;; 2]],
             corrector]]]]], (Norm[
          N[shieldphysprojector . hyperpoint]] <
         Norm[N[shieldphysprojector . #]] &)]],
   "getneighbouringpolygons"->
    Function[{hyperpoint, shift},
     Function[{tile}, {(hyperpoint + # &) /@ tile[[1]],
        tile[[2]]}](*translate each tile by hyperpoint*)/@
      Values[KeySelect[twelfefoldpredictorpolygons,
        Function[{window}, ((corrector = #[[2]]; #[[1]]) &)[
          isinonwindowapprox[
           window, (shieldintprojector . hyperpoint) + shift[[1 ;; 2]],
           corrector]]]]]]
		|>]
(*Goldentriangle*)
v1 = {{{{0, 0, 0, 0}, {2, 2, 1, 1}, {1, 1, 1, 0}, {1, 1, 1, 1}},
    2}, {{{0, 0, 0, 0}, {1, 1, 1, 1}, {1, 1, 0, 1}},
    1}, {{{0, 0, 0, 0}, {1, 1, 0, 1}, {1, 1, 0, 0}, {2, 2, 1, 1}}, 2}};
v2 = {{{{0, 0, 0, 0}, {1, 2, 1, 1}, {1, 1, 1, 1}, {1, 1, 0, 1}},2}, {{{0, 0, 0, 0}, {1, 1, 0, 1}, {1, 1, 0, 0}},2},
{{{0, 0, 0, 0}, {1, 1, 0, 0}, {0, 1, 0, 0}}, 1},
   {{{0, 0, 0, 0}, {0, 1, 0, 0}, {0, 1, 1, 0}, {1, 2, 1, 1}}, 2}};
v3 = {{{{0, 0, 0, 0}, {1, 1, 1, 1}, {1, 1, 0, 1}},
    1}, {{{0, 0, 0, 0}, {1, 1, 0, 1}, {1, 1, 0, 0}},
    2}, {{{0, 0, 0, 0}, {1, 1, 0, 0}, {0, 1, 0, 0}}, 1},
   {{{0, 0, 0, 0}, {0, 1, 0, 0}, {0, 1, 1, 0}}, 2},
   {{{0, 0, 0, 0}, {0, 1, 1, 0}, {0, 0, 1, 0}}, 2},
   {{{0, 0, 0, 0}, {0, 0, 1, 0}, {1, 1, 1, 1}}, 1}};
v4 = {{{{0, 0, 0, 0}, {1, 1, 0, 1}, {1, 1, 0, 0}}, 2},
   {{{0, 0, 0, 0}, {1, 1, 0, 0}, {0, 1, 0, 0}}, 1},
   {{{0, 0, 0, 0}, {0, 1, 0, 0}, {0, 1, 1, 0}}, 2},
   {{{0, 0, 0, 0}, {0, 1, 1, 0}, {0, 0, 1, 0}}, 2},
   {{{0, 0, 0, 0}, {0, 0, 1, 0}, {1, 1, 1, 1}}, 1},
   {{{0, 0, 0, 0}, {1, 1, 1, 1}, {-1, -1, 0, 0}, {0, 0, 0, 1}}, 2},
   {{{0, 0, 0, 0}, {0, 0, 0, 1}, {1, 1, 0, 1}}, 2}};
v5 = {{{{0, 0, 0, 0}, {1, 1, 0, 0}, {0, 1, 0, 0}}, 1},
   {{{0, 0, 0, 0}, {0, 1, 0, 0}, {0, 1, 1, 0}}, 2},
   {{{0, 0, 0, 0}, {0, 1, 1, 0}, {0, 0, 1, 0}}, 2},
   {{{0, 0, 0, 0}, {0, 0, 1, 0}, {1, 1, 1, 1}}, 1},
   {{{0, 0, 0, 0}, {1, 1, 1, 1}, {-1, -1, 0, 0}, {0, 0, 0, 1}}, 2},
   {{{0, 0, 0, 0}, {0, 0, 0, 1}, {1, 1, 0, 1}}, 2},
   {{{0, 0, 0, 0}, {1, 1, 0, 1}, {0, -1, -1, 0}, {0, 0, -1, 0}, {1, 0,
       0, 0}}, 2},
   {{{0, 0, 0, 0}, {1, 0, 0, 0}, {1, 1, 0, 0}}, 1}};
v6 = {{{{0, 0, 0, 0}, {0, 1, 0, 0}, {0, 1, 1, 0}},
    2}, {{{0, 0, 0, 0}, {0, 1, 1, 0}, {0, 0, 1, 0}}, 2},
   {{{0, 0, 0, 0}, {0, 0, 1, 0}, {1, 1, 1, 1}}, 1},
   {{{0, 0, 0, 0}, {1, 1, 1, 1}, {-1, -1, 0, 0}, {0, 0, 0, 1}}, 2},
   {{{0, 0, 0, 0}, {0, 0, 0, 1}, {1, 1, 0, 1}}, 2},
   {{{0, 0, 0, 0}, {1, 1, 0, 1}, {0, -1, -1, 0}, {0, 0, -1, 0}, {1, 0,
       0, 0}}, 2},
   {{{0, 0, 0, 0}, {1, 0, 0, 0}, {1, 1, 0, 0}}, 1},
   {{{0, 0, 0, 0}, {1, 1, 0, 0}, {0, 0, 0, -1}, {0, 1, 0, 0}}, 2}};
fundregpolygons = {v1,
   v2,
   v3,
   v4,
   v5,
   v6};
Hyperidentity = IdentityMatrix[4]
Hypermirror1 =
 Transpose[{{-1, -1, -1, -1}, {1, 1, 0, 1}, {-1, -1, 0, 0}, {0, 1, 1,
    0}}]
Hypermirror2 =
 Transpose[{{1, 1, 1, 1}, {-1, -1, 0, -1}, {1, 1, 0, 0}, {0, -1, -1,
    0}}]
Hyperrotation = Hypermirror1 . Hypermirror2
Hypergroup = {Hyperidentity, Hypermirror1, Hypermirror2, Hyperrotation}
polygons =
  Flatten[Function[{ele},
     Map[({Transpose[ele . Transpose[#[[1]]]], #[[2]]} &),
      fundregpolygons, {2}]] /@ Hypergroup, 1];

(*Definition according to Danzer and van ophuysen*)
goldentriangleprojector = {
	{0, -Sqrt[GoldenRatio], 0, Sqrt[GoldenRatio]^3},
	{1, 0, -GoldenRatio, 0},
	{-Sqrt[GoldenRatio]^3, 1/Sqrt[GoldenRatio], -Sqrt[GoldenRatio], Sqrt[GoldenRatio]^(-3)},
	{1, -GoldenRatio, GoldenRatio^(-1), -1}
}
goldentriangleisinwindowQ[latticepoint : vecpattern, projector_,shift : vecpattern,window :polytopepattern] := (isinwindow[window[[1 ;; 2]],
    projector[latticepoint] + shift[[1 ;; 2]]] &&
   isinonwindow[window[[3 ;; 4]],
    projector[latticepoint] + shift[[1 ;; 2]]])
goldentrianglewindow = {{{-1, 0}, Sqrt[GoldenRatio]^3}, {{0, -1},
   GoldenRatio}, {{1, 0}, Sqrt[GoldenRatio]^3}, {{0, 1},GoldenRatio}}(*for window choose vector a=0, b=1/2 of Eq 30 \
from  Danzer and van ophuysen*)
(*we are adding an extra point to the acceptance window of v5 in \
order to distinguish it from v6*)
extrahyperpointdict = (AssociationThread[#[[1]] -> #[[2]]] \
&)[{Function[{ele},
     Map[({Transpose[ele . Transpose[#[[1]]]], #[[2]]} &), v5, {1}]] /@
     Hypergroup, ({# . {1, 1, 1, 0}} &) /@ Hypergroup}]
goldentrianglepolygonpredictor =(*attention,
  different structure from predictor of shield tiling. In this one,
  the acceptancewindows do not overlapp and contain the entire \
vertexstar.
  in shield tiling the windows do overlap and contain one tile each*)
  AssociationMap[Reverse,
   AssociationMap[(getwindowadmittingshifts[goldentrianglewindow,
       Transpose[
        goldentriangleprojector[[3 ;; 4]] .
         Transpose[
          Join[DeleteDuplicates[Flatten[#[[All, 1]], 1]],
           If[KeyExistsQ[extrahyperpointdict, #],
            extrahyperpointdict[#], {}]]]]] &),
    polygons]];(*see \
/home/markus/work/recherche/sujets/qp/cutandpro/golden_triangle.nb \
for derivation*)
goldentrianglevertpredictor = (DeleteCases[
      Flatten[#[[All, 1]], 1], {0, 0, 0, 0}] &) /@
   goldentrianglepolygonpredictor;
acceptancesubwindows = Keys[goldentrianglepolygonpredictor];
acceptancesubwindowstoarea =
  AssociationMap[(N[RegionMeasure[Polygon[topolygon[#]]]] &),
   acceptancesubwindows];
goldentrianglecandp[] :=
  Module[{corrector = {0, 0}}, <|
    "projectphysical" -> ((goldentriangleprojector[[1 ;; 2]] . #) &),
		"edgetoedge"->False,
   "LIshift" -> ({}&),
   "LIshiftnormed" -> ({}&),
    "projectperpendicular" -> (goldentriangleprojector[[3 ;; 4]] . # &),
    "acceptancewindow" -> goldentrianglewindow,
   "phasonplanes" -> {{windowtophasonplane[goldentrianglewindow],{}}},
    "name" -> "goldentriangle",
   	"type" -> "general",
    "isinwindowQ" ->
     Function[{latticepoint, projector, shift,window},
	((corrector = #[[2]]; #[[1]]) &)[
       isinonwindowapprox[window,
        projector[latticepoint] + shift[[1 ;; 2]], corrector]]],
    "hyperdim" -> 4, "physdim" -> 2,
   "phasondim" -> 2,(*dimension of subspace of displacements in internal space that leave tiling indistinguishable*)
    "getcorrector" :> corrector,
    "acceptedneighbouroutward" ->
     Function[{hyperpoint, shift},
      Select[(hyperpoint + # &) /@
        goldentrianglevertpredictor[
         MinimalBy[
           Keys[KeySelect[goldentrianglevertpredictor,
             Function[{window}, ((corrector = #[[2]]; #[[1]]) &)[
               isinonwindowapprox[
                window, (goldentriangleprojector[[3 ;; 4]] .
                   hyperpoint) + shift[[1 ;; 2]],
                corrector]]]]], (acceptancesubwindowstoarea[#] \
&)][[1]]], (Norm[N[goldentriangleprojector[[1 ;; 2]] . hyperpoint]] <
          Norm[N[goldentriangleprojector[[1 ;; 2]] . #]] &)]],
    "getneighbouringpolygons" ->
     Function[{hyperpoint,shift}, (Function[{tile}, {(hyperpoint + # &) /@ tile[[1]],
          tile[[2]]}](*translate each tile by hyperpoint*)/@
        goldentrianglepolygonpredictor[
         MinimalBy[
           Keys[KeySelect[goldentrianglepolygonpredictor,
             Function[{window}, ((corrector = #[[2]]; #[[1]]) &)[
               isinonwindowapprox[
                window, (goldentriangleprojector[[3 ;; 4]] .
                   hyperpoint) + shift[[1 ;; 2]],
                corrector]]]]], (acceptancesubwindowstoarea[#] &)][[1]]])]|>];
(*we construct goldentrianglehomog to have horizontal mirrors. to this end, a rotation is applied to the projection of the basis vectors into real space such that they align with the mirror axis*)
Hypermirror1 =
 Transpose[{{-1, -1, -1, -1}, {1, 1, 0, 1}, {-1, -1, 0, 0}, {0, 1, 1,
    0}}](*Mirror in 4-dimensional hyperspace*)
resy = goldentriangleprojector[[1 ;; 2]] .Hypermirror1 . {1, 0, 0, 0}(*where first basis vector of hyperlattice is sent to under mirror*)
unitvectoronmirror = FullSimplify[Normalize[Mean[{resy, goldentriangleprojector[[1 ;; 2, 1]]}]]]
goldentrianglehomogprojector = FullSimplify[{
			{unitvectoronmirror[[1]], unitvectoronmirror[[2]], 0, 0},
			{-unitvectoronmirror[[2]], unitvectoronmirror[[1]], 0, 0},
			{0, 0, 1, 0},
			{0, 0, 0, 1}
		} .goldentriangleprojector]
goldentrianglehomogcandp[]:=Module[{res},
	res=goldentrianglecandp[];
	res["name"]="goldentrianglehomog";
	res["projectphysical"]=((goldentrianglehomogprojector[[1;;2]].#)&);
	res
]
hypertilingtohyperpoints[hypertiling:tilingpattern]:=DeleteDuplicates[Flatten[hypertiling,2]]
canonicalacceptancewindowforprojector[projector : {vecpattern ..}]:=
 Module[{hyperdim, hyperfacetsconstantdims, hyperfacetscornerpoints,internalspacenormals, internalspacelimits},
  hyperdim = Length[projector];
	If[(Norm[# - DiagonalMatrix[Diagonal[#]]] &)[projector[[3 ;;]] . Transpose[projector[[3 ;;]]]]>10^(-6),
	Print["WARNING: projectormatrix is not orthonormal; resulting tiling does not correspond to canonical projection since the voronoi cell is only implemented for projections from hypercubic lattices"];
];
	corrector= (0&)/@Range[hyperdim-2];
  (*we are trying to know the bounds of the acceptance window aka the \
internal projection of the hypercube centered at the origin.
  they are of dimension hyperdim-3,
  since we loose 2 dimensions to the physical space and one dimension \
due to taking the boundary.
  To his end, we find all hyperdim-3 facets of the unit cube,
  specified by a contained hypercubevertex and a vector of length 3 \
containing all dimensions which are constant on the hyperfacet*)
  hyperfacetsconstantdims = Subsets[Range[hyperdim], {3}];
  hyperfacetscornerpoints = Outer[
		Function[{dimensions, values}, values . ((UnitVector[hyperdim, #] &) /@ dimensions)],(*all possible combinations of dimensions held constant*)
    hyperfacetsconstantdims,(*for a given tripel of constant \
coordinates, there are 2^3=8 possible constant values*)
    Tuples[{-1/2, 1/2}, 3], 1, 1];
  internalspacenormals =
   Function[{constantdims},
    	NullSpace[(*dimensions other than the 3 hyperfacetsconstantdims \
still keeps us on the same hyperfacet.
       The same must hold after projection into internal space.
       The normal vector in internal space of the facet is therefore \
defined as being normal to the internal projection of the \
hyperunitvectors of all non-constant dimensions*)
				Transpose[
					N[projector[[3 ;;,Complement[Range[hyperdim], constantdims]]]]
				]
			][[1]]] /@
    hyperfacetsconstantdims;
  internalspacelimits =(*Mininum and maximum in each direction of \
internalspacenormals*)
  MapThread[
   	Function[{hyperpoints, internalspacenormal},
    	MinMax[(internalspacenormal . (projector[[3 ;;]] . #) &) /@ hyperpoints]
		], {hyperfacetscornerpoints, internalspacenormals}
	];
   Flatten[MapThread[
     Function[{limits, normal}, {{normal, limits[[2]]}, {-normal, -limits[[1]]}}], {internalspacelimits,
       internalspacenormals}], 1]
 ]
generalcutandproject[projector : {vecpattern ..}](*cut and project scheme for general \
hypercubic lattice of dimension n, specified by n x n orthogonal (an orthogonal matrix corresponds to a hyperrectangular case, orthonormal to hypercubic. Both cases are handeld correctly)
projector matrix. The first 2 rows of projector correspond to the \
physical projection, and the remaining ones to the internal \
projection*):=
 Module[{hyperdim, acceptancewindow,corrector},
  hyperdim = Length[projector];
	If[(Norm[# - DiagonalMatrix[Diagonal[#]]] &)[projector[[3 ;;]] . Transpose[projector[[3 ;;]]]]>10^(-6),
	Print["WARNING: projectormatrix is not orthonormal; resulting tiling does not correspond to canonical projection since the voronoi cell is only implemented for projections from hypercubic lattices"];
];
	corrector= (0&)/@Range[hyperdim-2];
  acceptancewindow =canonicalacceptancewindowforprojector[projector];
  <|
   "hyperdim" -> hyperdim,
   "physdim" -> 2,
	 "acceptancewindow"->acceptancewindow,
   "type" -> "canonical",
   "getcorrector" :> corrector,
   "projectphysical" -> (projector[[1 ;; 2]] . # &),
   "projectperpendicular" -> (projector[[3 ;;]] . # &),
   "isinwindowQ" ->
    Function[{latticepoint, internalprojector, shift,
      window}, ((corrector = #[[2]]; #[[1]]) &)[
      isinonwindowapprox[acceptancewindow,
       internalprojector[latticepoint] + shift[[;; hyperdim - 2]],
       corrector]]],
   "assignregion" ->
    Function[{hyperpoly}, (Mod[
        diffmodn[(Mod[
             FirstPosition[#[[2]], x_Integer /; x != 0][[1]] -
              FirstPosition[#[[4]], x_Integer /; x != 0][[1]],
             hyperdim] &)[(# -(*hyperpoly[[1]] has the lowest values of the \
entire polygon in all hypercoordinates*)hyperpoly[[1]] &) /@
           hyperpoly], hyperdim],
        2(*modulo 2 because we want biphase material*)] + 1)]
   |>
  ]
getsubstructureforperpshift[candp_Association,perpshift:vecpattern,niter:_?IntegerQ]:=Module[{hypertiling},
	hypertiling=docandptilingsmasterhyper[candp,{},perpshift,"niter"->niter];
	DeleteDuplicates[Flatten[hypertiling,2]]
	]
getsubstructureforperpshift[tiling_String,patternparams:{RepeatedNull[_?NumericQ]},perpshift:vecpattern,niter:_?IntegerQ]:=Module[{candp,hypertiling},
	candp=candpmaster[tiling,patternparams];
	getsubstructureforperpshift[candp,perpshift,niter]
	]
getsubstructureforperpshift[tiling_String,perpshift:vecpattern,niter:_?IntegerQ]:=Module[{candp,hypertiling},
	candp=candpmaster[tiling,{}];
	getsubstructureforperpshift[candp,perpshift,niter]
	]
candpmaster[form_String,params_:{}]:=({form,params}/.{
	(*any approximant tilings must have approximant in their name*)
	{"fibonaccisquares",{}}:>fibosquarecandp[],
	{"fibonaccisquaresapproximant",{_,_}}:>fibosquarehypercubecandpapproximant[params[[1]],params[[2]]],
	{"fibonaccirectangles2",{}}:>fiborectangle2hypercubecandp[],
	{"fibonaccirectanglesapproximant",{_,_}}:>fiborectanglehypercubecandpapproximant[params[[1]],params[[2]]],
	{"penroseapproximant",{_,_,_}}:>penroseapproximant[IntegerPart[params[[1]]],IntegerPart[params[[2]]],params[[3]]],
	{"penrose",{}}:>penrosecandp[1/2],
	{"penrose",{_}}:>penrosecandp[Rationalize[params[[1]]]],
	{"hexagonalqp",{}}:>hexagonalqpcandp[0,0],
	{"hexagonalqp",{_, _}}:>hexagonalqpcandp[Rationalize[params[[1]]],Rationalize[params[[2]]]],
	{"hexagonalqpapproximant",{_, _,_,_}}:>hexagonalqpapproximant[IntegerPart[params[[1]]],IntegerPart[params[[2]]],params[[3]],params[[4]]],
	{"dodecagonalqp",{}}:>dodecagonalqpcandp[0,0],
	{"dodecagonalqp",{_, _}}:>dodecagonalqpcandp[Rationalize[params[[1]]],Rationalize[params[[2]]]],
	{"dodecagonalqpapproximant",{_, _,_,_}}:>dodecagonalqpapproximant[IntegerPart[params[[1]]],IntegerPart[params[[2]]],params[[3]],params[[4]]],
	{"ammann",{}}:>ammannbeenkercandp[],
	{"ammannapproximantduneau",{_,_}}:>ammannbeenkerapproximantduneau[IntegerPart[params[[1]]],IntegerPart[params[[2]]]],
	{"ammannapproximant",{_,_}}:>ammannbeenkerapproximant[IntegerPart[params[[1]]],IntegerPart[params[[2]]]],
	{"ammannnonsymmorph",{}}:>ammannbeenkernonsymmorphcandp[],
	{"shield",{}}:>shieldcandp[],
	{"goldentriangle",{}}:>goldentrianglecandp[],
	{"goldentrianglehomog",{}}:>goldentrianglehomogcandp[],
_:>Throw[StringJoin["wrong quasiperiodic tiling: ",ToString[form],ToString[params]]]
})
plotprojectedtiling[tiling:tilingpattern]:=Flatten[({RandomColor[], EdgeForm[Black], #} &) /@ Map[(Polygon[#] &), tiling, {2}], 1]
(*hexagonal quasiperiodic according to: Coates et al. 2023: hexagonal and trigonal quasiperiodic tilings*)
entrylastrows=Sqrt[Sqrt[5]/(2*GoldenRatio)]
hexagonalprojector=FullSimplify[
 Sqrt[(5+Sqrt[5])/15]*{
	{1/GoldenRatio, -1/(2*GoldenRatio), -1/(2*GoldenRatio), 1, -1/2, -1/2},
   {0, Sqrt[3]/(2*GoldenRatio), -Sqrt[3]/(2*GoldenRatio), 0, Sqrt[3]/(2), -Sqrt[3]/(2)},
   {1, -1/2, -1/2, -1/GoldenRatio, 1/(2*GoldenRatio), 1/(2*GoldenRatio)},
   {0, Sqrt[3]/(2), -Sqrt[3]/(2), 0, -Sqrt[3]/(2*GoldenRatio), Sqrt[3]/(2*GoldenRatio)},
   {entrylastrows, entrylastrows, entrylastrows, 0, 0, 0},
	 {0, 0, 0, entrylastrows, entrylastrows, entrylastrows}
	 }]
(*as specfified in Equation 11 of the Coates et al, only scaled vectors to have unit length*)
hexagonalqpcandp[a : _? NumericQ,b : _? NumericQ]:=Module[{candp,LIshift,LIshiftnormed,amod,bmod},
	(*deduct a tiny bit to avoid singular modell set*)
	amod=a-10^(-8);
	bmod=b-10^(-8);
	(*we are using different convention then Coates for our a and b, since we project hypercube centered on origin, and he projects hypercube whose lowest coordinate is on the origin
	The conversion between the our alpha and his is given by Mod[#-0.5,1]
	*);
	LIshiftnormed=({#1+amod,#2+bmod}&);
	LIshift=(LIshiftnormed[#1,#2]/Sqrt[3]&);
	candp=generalcutandproject[hexagonalprojector];
	candp["LIshift"]=LIshift;
	candp["LIshiftnormed"]=LIshiftnormed;
	candp["phasondim"]=2;
	candp["basisvectorscolinearinphysicalspace"]={{1,4},{2,5},{3,6}};
  candp["phasonplanes"]= Select[({windowtophasonplane[projecttosubspace[{Null,Null,Sequence@@LIshift[Sequence@@(#)]},candp["acceptancewindow"]]],#}&)/@Select[
		Tuples[Range[-3, 3], 2], 
		Function[{idxpair}, 
			AllTrue[
				{amod, bmod} + idxpair,
				(-1.5 < # < 1.5 &)
			] && Total[{amod, bmod} + idxpair] < 3
		]
	],(#[[2]]>10^(-10)(*area of the phasonplane*)&)];
	candp["assignregion"] = 
		Function[{poly}, ({FirstPosition[#[[2]], x_Integer /; x != 0][[1]], 
				 FirstPosition[#[[4]], x_Integer /; x != 0][[1]]}(*get directions of edges linked to poly[[1]] in hyperspace*) &)[(# - poly[[1]](*poly[[1]] is the lowest in all hypercoordinates*)&) /@ poly] /. {{x_, y_} /;
				 (Xor (*if we have a hexagonal shape, then either all hyperedges are in 1,2,3 or in 4,5,6*) @@ (MemberQ[{1, 2, 3}, #] &) /@ {x, y}) -> 1, _ -> 2}];
	candp["isinwindowQ"]=Function[{latticepoint, internalprojector, shift,window},
		((candp["getcorrector"] = #[[2]]; #[[1]]) &)[
			isinonwindowapprox[candp["acceptancewindow"],
				 internalprojector[latticepoint]+
	Join[shift[[1 ;; 2]],LIshift[0,0]],
				 candp["getcorrector"],2
			]
		]
	];
	candp
]
hexagonalqpapproximant[n_Integer,m_Integer,a : _? NumericQ,b : _? NumericQ]:=Module[{candp,projector,amod,bmod},
	(*deduct a tiny bit to avoid singular modell set*)
	amod=a-10^(-8);
	bmod=b-10^(-8);
	(*we are using different convention then Coates for our a and b, since we project hypercube centered on origin, and he projects hypercube whose lowest coordinate is on the origin
	The conversion between the our alpha and his is given by Mod[#-0.5,1]
	*);
	LIshiftnormed=({#1+amod,#2+bmod}&);
	LIshift=(LIshiftnormed[#1,#2]/Sqrt[3]&);
	projector=hexagonalprojector;
	projector[[3;;4]]={
    Normalize[{Fibonacci[n], -1/2*Fibonacci[n], -1/2* Fibonacci[n], -Fibonacci[n - 1], 1/(2)*Fibonacci[n - 1], 1/(2)*Fibonacci[n - 1]}],
    Normalize[{0, Sqrt[3]/(2)*Fibonacci[m], -Sqrt[3]/(2)*Fibonacci[m], 0, -Sqrt[3]/(2)*Fibonacci[m - 1], Sqrt[3]/(2)*Fibonacci[m - 1]}]
	};
	candp=generalcutandproject[projector];
	candp["LIshift"]=LIshift;
	candp["basisvectorscolinearinphysicalspace"]={{1,4},{2,5},{3,6}};
	candp["periods"]={
		periodofapproximant["hexagonalqpapproximant",n,m,1],
		periodofapproximant["hexagonalqpapproximant",n,m,2]
	};
	candp["LIshiftnormed"]=LIshiftnormed;
	candp["assignregion"] = 
		Function[{poly}, ({FirstPosition[#[[2]], x_Integer /; x != 0][[1]], 
				 FirstPosition[#[[4]], x_Integer /; x != 0][[1]]}(*get directions of edges linked to poly[[1]] in hyperspace*) &)[(# - poly[[1]](*poly[[1]] is the lowest in all hypercoordinates*)&) /@ poly] /. {{x_, y_} /;
				 (Xor (*if we have a hexagonal shape, then either all hyperedges are in 1,2,3 or in 4,5,6*) @@ (MemberQ[{1, 2, 3}, #] &) /@ {x, y}) -> 1, _ -> 2}];
	candp["isinwindowQ"]=Function[{latticepoint, internalprojector, shift,window},
		((candp["getcorrector"] = #[[2]]; #[[1]]) &)[
			isinonwindowapprox[candp["acceptancewindow"],
				 internalprojector[latticepoint]+
	Join[shift[[1 ;; 2]],LIshift[0,0]],
				 candp["getcorrector"]
			]
		]
	];
	candp
]
(*dodecagonal qp projected from Z6*)
ang = Pi/6
(*row i is vector a_i of eq(2) in Keskiner and Oktel, scaled down by \
1/3 to have vectors of unit length*)
dodecagonalprojector = 1/Sqrt[3]*{
    {1, Cos[ang], -Cos[2*ang], -Cos[3*ang], Cos[4*ang], Cos[5*ang]},
    {0, Sin[ang], -Sin[2*ang], -Sin[3*ang], Sin[4*ang], Sin[5*ang]},
    {1, -Cos[ang], -Cos[2*ang], Cos[3*ang], Cos[4*ang], -Cos[5*ang]},
    {0, Sin[ang], Sin[2*ang], -Sin[3*ang], -Sin[4*ang], Sin[5*ang]},
    {1, 0, 1, 0, 1, 0},
    {0, 1, 0, 1, 0, 1}
    };
dodecagonalqpcandp[a : _? NumericQ,b : _? NumericQ]:=Module[{candp,LIshift,LIshiftnormed},
	amod=a-10^(-8);
	bmod=b-10^(-8);
	candp=generalcutandproject[dodecagonalprojector];
	LIshiftnormed=({#1+amod,#2+bmod}&);
	LIshift=(LIshiftnormed[#1,#2]/Sqrt[3]&);
	candp["phasondim"]=2;
	candp["LIshift"]=LIshift;
	candp["LIshiftnormed"]=LIshiftnormed;
  candp["phasonplanes"]= Select[Function[{idxpair}, {windowtophasonplane[projecttosubspace[{Null,Null,Sequence @@(LIshift[Sequence@@(idxpair)])},candp["acceptancewindow"]]],idxpair}
	]/@Select[
		Tuples[Range[-3, 3], 2], 
		Function[{idxpair}, 
			AllTrue[
				{amod, bmod} + idxpair,
				(-1.5 < # < 1.5 &)
			] && Total[{amod, bmod} + idxpair] < 3
		]
	],(#[[2]]>10^(-10)(*area of the phasonplane*)&)];
	candp["isinwindowQ"]=Function[{latticepoint, internalprojector, shift,window},
		((candp["getcorrector"] = #[[2]]; #[[1]]) &)[
				isinonwindowapprox[candp["acceptancewindow"],
				 internalprojector[latticepoint]+
	Join[shift[[1 ;; 2]],candp["LIshift"][0,0]],
				 candp["getcorrector"],2]]];
	candp
]
numerators = {2, 3, 9, 5, 16, 23, 17, 29, 7, 12, 55, 43, 68, 74, 61,
   54, 31, 47, 81, 87, 50, 40, 69, 88, 113, 107, 73, 126, 145, 106,
   139, 33, 19, 191, 158, 125, 197, 217, 178, 92, 159, 140, 151, 210,
   121, 223, 59, 102, 185, 262, 268, 203, 144, 83, 229, 314, 230, 85,
   147, 211, 281, 275, 196, 339, 307, 111, 64, 248, 301, 137, 237,
   300, 173, 163, 282, 189, 109, 215, 263, 241, 267, 154, 293, 319,
   345, 199, 244, 289, 334, 26, 45, 341, 296, 251, 206, 331, 305, 279,
    161, 253, 277, 227, 201, 116, 175, 303, 324, 187, 149, 258, 329,
   272, 123, 71, 343, 220, 310, 317, 239, 97, 168, 265};
denominators = {1, 2, 5, 3, 9, 13, 10, 17, 4, 7, 32, 25, 39, 43, 35,
   31, 18, 27, 47, 50, 29, 23, 40, 51, 65, 62, 42, 73, 84, 61, 80, 19,
    11, 110, 91, 72, 114, 125, 103, 53, 92, 81, 87, 121, 70, 129, 34,
   59, 107, 151, 155, 117, 83, 48, 132, 181, 133, 49, 85, 122, 162,
   159, 113, 196, 177, 64, 37, 143, 174, 79, 137, 173, 100, 94, 163,
   109, 63, 124, 152, 139, 154, 89, 169, 184, 199, 115, 141, 167, 193,
    15, 26, 197, 171, 145, 119, 191, 176, 161, 93, 146, 160, 131, 116,
    67, 101, 175, 187, 108, 86, 149, 190, 157, 71, 41, 198, 127, 179,
   183, 138, 56, 97, 153};(*for each denominator from 1 to 200, i picked the optimal numerator. Then i sorted the pairs by increasing closeness to sqrt(3)*)
approxsqrt3numerator[n_Integer] := numerators[[n]]
approxsqrt3denominator[n_Integer] := denominators[[n]](*+1 because i start my approximants at parameter n,m=2 and i want that to correspond to 9/5, so totally arbitrary*)
approxsqrt3[n_Integer] := approxsqrt3numerator[n]/approxsqrt3denominator[n]
dodecagonalqpapproximant[n_Integer,m_Integer,a : _? NumericQ,b : _? NumericQ]:=Module[{candp,LIshift,LIshiftnormed,projector},
	amod=a-10^(-8);
	bmod=b-10^(-8);
	projector=dodecagonalprojector;
	projector[[3;;4]]={
		Normalize[{1, -approxsqrt3[n]/2, -1/2, 0, -1/2, approxsqrt3[n]/2}],
		Normalize[{0, 1/2, approxsqrt3[m]/2, -1, -approxsqrt3[m]/2, 1/2}]
	};
	candp=generalcutandproject[projector];
	LIshiftnormed=({#1+amod,#2+bmod}&);
	LIshift=(LIshiftnormed[#1,#2]/Sqrt[3]&);
	candp["phasondim"]=2;
	candp["LIshift"]=LIshift;
	candp["LIshiftnormed"]=LIshiftnormed;
	candp["periods"]={
		periodofapproximant["dodecagonalqpapproximant",n,m,1],
		periodofapproximant["dodecagonalqpapproximant",n,m,2]
	};
  candp["phasonplanes"]= Select[Function[{idxpair}, {windowtophasonplane[projecttosubspace[{Null,Null,Sequence @@(LIshift[Sequence@@(idxpair)])},candp["acceptancewindow"]]],idxpair}
	]/@Select[
		Tuples[Range[-3, 3], 2], 
		Function[{idxpair}, 
			AllTrue[
				{amod, bmod} + idxpair,
				(-1.5 < # < 1.5 &)
			] && Total[{amod, bmod} + idxpair] < 3
		]
	],(#[[2]]>10^(-10)(*area of the phasonplane*)&)];
	candp["isinwindowQ"]=Function[{latticepoint, internalprojector, shift,window},
		((candp["getcorrector"] = #[[2]]; #[[1]]) &)[
				isinonwindowapprox[candp["acceptancewindow"],
				 internalprojector[latticepoint]+
	Join[shift[[1 ;; 2]],candp["LIshift"][0,0]],
				 candp["getcorrector"]]]];
	candp
]
End[]

EndPackage[]
