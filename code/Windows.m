BeginPackage["Windows`"]
(*by window i understand polytopes*)
restricttowindowfilled::usage="like restricttowindow, but also raises error if the window is not completely filled with tiles"
quadraticwindow::usage="quadraticwindow[sidelength] gives polytope of square of specified side length that is centered on the origin"
restricttowindow::usage="restricttowindow[wind : polytopepattern, points_?formsQ] cuts of any parts of polygons described by forms which are outside of the specified window" 
unitwindow::usage="window [0,1]^2" 
unitcellwindow::usage="unitcellwindow[{a,b}] gives unit cell for base vectors a and b" 
sortwindowcounterclockwise::usage=""
windowedgeintersect::usage=""
reciprocalbase::usage="reciprocal base of vectors"
restricttowindowedges::usage=""
parallelogram::usage=""
removeduplicateedges::usage=""
splitconcavepolygon::usage="splitconcavepolygon[poly:{vecpattern..}] returns 2 elements:
	1. a list of convex polygons whose disjoint union is poly
	2. a replacement rule ({p1,p2}->p3) where p3 is a new point created for the split and p1,p2 are counterclockwise vertices of the edge containing p3 in a neighbouring polygon (not the polygon we just split)"
intersection::usage="intersection[edge:halfspacepattern,p1 : vecpattern, p2 : vecpattern] calculates intersection between edge of window and line connecting points p1 and p2"
cutbywindow::usage="cutbywindow[window,forms] cuts all polygons contained in forms by specified window, only keeping the intersection of window with the provided polygons. the window-edges need to be specified in consequtive order, such that 2 consequtive edges are linked by a common vertex)"
cutbywindowpoly::usage=""
applyboundarylayers::usage=""
contractwindow::usage="push window edges inwards by specified amount"
shiftwindow::usage="shiftwindow[wind:polytopepattern,shift:vecpattern] translates wind by vector shift"
samplefromwindow::usage="samplefromwindow[window] samples random point inside of 2-dimensional window"
distancetoedge::usage=""
distanceofedge::usage="euclidean distance of edge from origin"
isinwindow::usage=""
checkfilledwindow::usage=""
isinonwindow::usage=""
isinonwindownum::usage=""
isinonwindowapprox::usage=""
omitedges::usage="omit superflous edges from window"
getwindowadmittingshifts::usage="intersection of window and its translates by -shift for shift in shifts (shifts indicated in internal space)"
getwindowadmittingshiftssymmetric::usage="getwindowadmittingshiftssymmetric[wind,shifts, multrotinternal, order] returns orbit of getwindowadmittingshifts under Z_{order} with adjacent windows being different by 2*Pi*multrotinternal/order"
projecttosubspace::usage="projecttosubspace[subspace,window] calculates the intersection of window with the subspace specified as having free coordinates at values Null and fixed coordinates at numeric values"
removesamewindows::usage="takes list of windows and removes those that are the same except fot their edges being shuffled in a different order"
topolygon::usage="turns 2dimensional window to list of vertices of its polygon"
topolygon3d::usage="same as topolygon except for 3-dimensional polytope"
towindow::usage="turns list of vertices of polygon to 2dimensional window"
selectintersectingpolygons::usage="selectintersectingpolygons[window,tiling] selects those tiles of tiling that intersect with the window"
polywindowintersectQ::usage=""
Get["Settings`"]
Needs["Tensors`"]
eps=10^(-14)
Begin["`Private`"]
limiter=5000
quadraticwindow[sidelength:_?NumericQ]:={{{1,0},sidelength/2},{{0,1},sidelength/2},{{-1,0},sidelength/2},{{0,-1},sidelength/2}}
selectintersectingpolygons[window : polytopepattern,tiling:tilingpattern]:=Module[{windowvertices},
	windowvertices=topolygon[window];
	Select[		
		Function[{tile},
			polywindowintersectQ[tile,window,windowvertices]
		]
	]
	/@tiling
]
polywindowintersectQ[poly:{vecpattern..},window:polytopepattern,windowvertices:{vecpattern..}]:=(
	AnyTrue[poly,(isinwindow[window,#]&)](*either a vertex of the poly is in the window, or*)||
	AnyTrue[windowvertices,(isinwindow[towindow[poly],#]&)](*or a vertex of the window is in the poly*)
)
samplefromwindow[window : polytopepattern] := Module[{rectangle, sample, i},
  If[Length[window[[1, 1]]] > 2,
   Throw["Can only sample from 2-dimensional windows (yet), window \
provided:", window, window]];
  rectangle = MinMax /@ Transpose[topolygon[window]](*list of {{minx,
  maxx},{miny,maxy}}*);
  i = 0;
  While[i < limiter,
   i++;
   sample =RandomReal /@ rectangle;
   If[isinwindow[window, sample],
    Return[sample]
    ];
   If[i == limiter, Throw["failed to sample from window:", window]]
  ]
]
Options[projecttosubspace] = {"omitedges"->True,"checkperpendicular"->True};
projecttosubspace[subspace : {Null ..}, window : polytopepattern,OptionsPattern[]] :=If[Length[window[[1,1]]]==2,omitedges[window],window](*no dimensions restricted, window stays the same*)
projecttosubspace[subspace : {Null .., _?NumericQ ..}, window : polytopepattern,OptionsPattern[]] :=
 Module[{projectedwindow},
  projectedwindow =
   Function[{edge}, {edge[[1]][[Flatten[Position[subspace, Null,{1}]]]],
      edge[[2]] -
       edge[[1]][[Flatten[Position[subspace, _?NumericQ,{1}]]]] .
        subspace[[Flatten[Position[subspace, _?NumericQ,{1}]]]]}] /@
    window;
  If[(Count[
     projectedwindow, {{(0 | 0.), (0 | 0.)},
      x_ /; x < -10^(-10)(*we set limit a teeny tiny bit lower in \
order to handle the singular cut-and-project described by Coates*)}] > 0 && OptionValue["checkperpendicular"]),
   Null,
	 (If[OptionValue["omitedges"],omitedges[#, "approximate" -> True],#]&)[
			removeduplicateedges[
				Select[(*select only those with nonzero component in the target subspace*)
					projectedwindow,
					(Total[Abs[#[[1]]]] > 0 &)
				]
			]
		]
   ]
	]
removeduplicateedges[window:polytopepattern]:=(
		DeleteDuplicates[
			window,
			Function[{edge1,edge2},
				(
					(Norm[#[[1]]-#[[2]]]<10^(-10)&)[((#[[2]]*#[[1]]/Norm[#[[1]]]^2)&)/@N/@{edge1,edge2}] &&
					Norm[Normalize[N[edge1[[1]]]]-Normalize[N[edge2[[1]]]]]<10^(-10)
				)
			]
		]
	)
sortwindowcounterclockwise[wind:polytopepattern]:=SortBy[wind,(angle[#[[1, 1]], #[[1, 2]]] &),Less]
topolygon[wind:polytopepattern]:=(
	If[Length[wind[[1,1]]]!=2,Throw["Function reserved for 3d polytopes"]];
	If[
		Length[wind]>2,
		topolygonsorted[sortwindowcounterclockwise[wind]],
		Throw[StringJoin[{"insufficient length of window ",ToString[Length[wind]]," for polygon generation"}]]
])
towindow[poly : {vecpattern ..}] := ({#[[1]], #[[1]] . #[[2, 1]]} &) /@
  Function[{clockwisevertexpair}, {
     {{0, 1}, {-1, 0}} . (clockwisevertexpair[[2]] -
        clockwisevertexpair[[1]]), clockwisevertexpair
     }
    ] /@
   Transpose[{ensureCounterclockwise[poly],
     RotateLeft[ensureCounterclockwise[poly]]}]
shiftwindow[wind:polytopepattern,shift:vecpattern]:=({#[[1]],#[[2]]+(shift).#[[1]]}&)/@wind
topolygonsorted[wind:polytopepattern]:=MapThread[windowedgeintersect,{wind,RotateLeft[wind]}]
inplaneintersect[plane1 : halfspacepattern,plane2 : halfspacepattern] := (*gets intersection of plane1 and plane2 inside of plane2 and specified in terms of an orthonormal base that complements the normal vector of plane2*){(# . plane1[[1]] &) /@
   getortbasetonormalvector[plane2[[1]]],
  plane1[[2]] - plane2[[2]]*plane2[[1]] . plane1[[1]]}
mymin[x_, y_] :=
 FullSimplify[x - y] /. {r_ :> y /; r > 0, r_ :> x /; r < 0, r_ :> x}
getwindowadmittingshifts[wind : polytopepattern, shifts : {vecpattern ..}] :=(
(*calculates intersection of wind-\ve{shift_i} for \ve{shift_i} in shifts
if wind is set of edges {\ve{n_j},l_j}
then wind+\ve{shift_i} is {\ve{n_j},l_j-\ve{n_j}*\ve{shift_i}}
the intersection of all such shifted windows is then {\ve{n_j},Min_i(l_j-\ve{n_j}*\ve{shift_i})}
*)
(If[
	Length[#[[1,1]]]==2(*two-dimensional case*),
	omitedges[#,"approximate"->True],
	#
	]&)[
		Fold[
			Function[{curwindow, shift},
				MapThread[
					Function[{edge, curedge},
						{edge[[1]], mymin[curedge[[2]], FullSimplify[edge[[2]] - edge[[1]] . shift]]}
					],
					{wind,curwindow}
				]
			],
			wind,
			shifts
		]
	]
)
(*in order to plot 2d window*)
createLine[{{nx_?NumericQ, ny_?NumericQ}, lim_?NumericQ}] := (Module[{x, y, range = 3},
   If[ny != 0,
    ParametricPlot[{x, (lim - nx *x)/ny}, {x, -range, range}],
    Graphics[Line[{{lim/nx, -range}, {lim/nx, range}}]]]]
	)
Options[omitedges] = {"approximate"->False};
omitedges[window : polytopepattern,OptionsPattern[]] :=Module[{windowtouse},
	If[Length[window[[1,1]]]!=2,Throw["only two-dimensional windows are valid, window provided:",window]];
	windowtouse=If[OptionValue["approximate"],N[window],window];
	Pick[window,
		Function[{edge},
			(Count[
				(distancetowind[windowtouse,#] &) /@(
					If[
						OptionValue["approximate"],
						DeleteDuplicates[N[#], (Norm[#1-#2]<=10^(-10)&)],
						DeleteDuplicates[FullSimplify[#]]]
				&)[
					((*intersection of given edge with all other edges*)
						If[colinearQ[edge[[1]], #[[1]],"approximate"->OptionValue["approximate"]],
							Unevaluated@Sequence[],
							(windowedgeintersect[edge, #])
						]
					&) /@ windowtouse]
				,_?(#1<=10^(-10)&)
			] ==2)
		]/@windowtouse
	]
]
getwindowadmittingshiftssymmetric[wind : polytopepattern,
  shifts : {vecpattern ..}, multrotinternal_Integer, order_Integer] :=
 Module[{basewindow},
  basewindow =
   getwindowadmittingshifts[wind,
    shifts]; (Function[{idx}, ({rotmat[
           2*Pi*idx*multrotinternal/order] . #[[1]], #[[2]]} &) /@
      basewindow]) /@ Range[order]]
parallelogram[a:vecpattern, b:vecpattern] :=
 Polygon[{{0, 0}, {0, 1}, {1, 1}, {1, 0}} . {a, b}]
reciprocalbase[vecs:{vecpattern,vecpattern}]:=Transpose[Inverse[vecs]]
unitwindow={{{-1, 0}, 0}, {{0, -1}, 0}, {{1, 0}, 1}, {{0, 1}, 1}}
unitcellwindow[basevecs:{vecpattern,vecpattern}, n_ : 1] := 
 Flatten[({{-#, 0}, {#, n}} &) /@ reciprocalbase[basevecs], 1]
restricttowindow[wind : polytopepattern,points_?formsQ] := ((*Print["restricting to window"];*)
  Fold[restricttoedge[#2[[1]], #2[[2]], #1] &, points, Rationalize[wind]])
checkfilledwindow[wind : polytopepattern, points_?formsQ]:=(
	(*Print["checking for filled window"];*)
  If[
		Abs[relativediff[
   		Total[(Abs[signedArea[#]]&)/@ Flatten[N[points], depthlist[points] - 4]](*area of all tiles*), 
    	Abs[signedArea[topolygon[wind]]](*area of window*)]]> 0.00001,
		(Print[points];Throw["Exception: window not filled"])]
)
restricttowindowfilled[wind : polytopepattern, points_?formsQ] := 
 Module[{result}, (*Print["restricting to window"];*)
  result = 
   Fold[restricttoedge[#2[[1]], #2[[2]], #1] &, points, 
    Rationalize[
     SortBy[wind,(*sort window edges so that they are counterclckwise*)
			(ToPolarCoordinates[#[[1]]][[2]] &)]]];
	checkfilledwindow[wind,result];
  result
  ]
restricttoedge[n : vecpattern, lim_?NumericQ,
  points_?formsQ] := ((*Print["restricting to edge of window", n, lim];*)
  DeleteCases[
   Map[restricttoedgepoly[n, lim, #] &,
    points, {depthlist[points] - 3}], ({} | {vecpattern} | {vecpattern,
      vecpattern}), {depthlist[points] -
     3}](*remove empty polygons (who lie entirely outside of the \
window*))
(*{{{}},{{{1,0}}},{{{1,1},{1,0}}},{{{1,1}}},{{}}}*)
removesamewindows[windows:{polytopepattern..}]:=DeleteDuplicates[(SortBy[#,
     Function[{edge}, angle[edge[[1, 1]], edge[[1, 2]]]]] &) /@ windows]
restricttoedge[n : vecpattern, lim_?NumericQ,
  points_?(Length[Flatten[#]] == 0 &)] := (points)
restricttoedgepoly[n : vecpattern, lim_?NumericQ,
  polygon : {vecpattern ..}] := (Nest[
   Replace[#, {p : vecpattern,
       r : vecpattern ...} :> (treatpoint[n, lim, p, r])] &,
   polygon, Length[polygon]])
treatpoint[n : vecpattern, lim_?NumericQ, p : vecpattern,
  r : vecpattern ...] := ({r,
   If[N[n . p] > lim,
    Sequence @@ {If[Length[{r}] == 0 || N[n . {r}[[-1]]] >= lim,
       Nothing, intersection[n, lim, p, {r}[[-1]]]],
      If[Length[{r}] == 0 || N[n . {r}[[1]]] >= lim, Nothing,
       intersection[n, lim, p, {r}[[1]]]]}, p]})
sortpolygonverts3d[verts : {vecpattern ...}, normal : vecpattern] :=
 Module[{orthonormalbase},
  orthonormalbase =(*vectors that, together with normal,
   form an orthonormal base*)
   Normalize /@
    Select[(# - normal/Norm[normal]^2*(normal . #) &) /@ {{1, 0,
         0}, {0, 1, 0}, {0, 0, 1}}, (Norm[#] > 0.01 &)][[1 ;; 2]];
  orthonormalbase[[2]] =
   Cross[Normalize[normal], orthonormalbase[[1]]];
  SortBy[verts, (ToPolarCoordinates[orthonormalbase . #][[2]] &)]]
topolygon3d[window : polytopepattern] := Select[Function[{edge},
    sortpolygonverts3d[
     DeleteDuplicates[
      Select[((*intersection of the 3 edges*)
         Inverse[{edge[[1]], #[[1, 1]], #[[2,
               1]]}] . {edge[[2]], #[[1, 2]], #[[2, 2]]} &) /@
        (*all pairs of 2 other edges such that all 3 normals are \
linearly independent*)
        Select[Tuples[window,
          2], (NullSpace[
             Append[{#[[1, 1]], #[[2, 1]]}, edge[[1]]]] == {} &)],
       (*select those intersection points that are inside of the \
window*)
       (isinonwindownum[window, #] &)], (Norm[#1 - #2] < 10^(-11) &)],
      edge[[1]]]
    ] /@ window, (Length[#] > 0 &)]
isinonwindownum[wind : polytopepattern, point : vecpattern] :=(
 Count[(N[(#[[1]] . point)-#[[2]]] <= 10^(-9)  &) /@ wind, True] == Length[wind])
isinwindow[wind : polytopepattern, point : vecpattern] :=(
Count[(#[[1]] . point < #[[2]] &) /@ wind, True] == Length[wind]
)
isinonwindow[wind : polytopepattern, point : vecpattern] :=(
 Count[(FullSimplify[(#[[1]] . point)-#[[2]]] <= 0  &) /@ wind, True] == Length[wind])
restricttowindowedges[window : polytopepattern,edges : {{Repeated[{Repeated[_?NumericQ, {2}]}, {2}]} ..}] :=(*edges specified as blocks {{x1,y1},{x2,y2}}, in this context, not edges of a window but of a tiling.*)
 Module[{insideedgeflag},
  Fold[Function[{currentedges,
     windowedge}, (insideedgeflag =
      Map[(windowedge[[1]] . # < windowedge[[2]] &),
       currentedges, {2}]; MapThread[(#2 /. {
          {False, False} :>Unevaluated@Sequence[],
					{True, False} :> {#1[[1]],intersection[windowedge, Sequence @@ #1]},
					{False, True} :> {intersection[windowedge, Sequence @@ #1], #1[[1]]},
					{True, True}:> #1} &), {currentedges, insideedgeflag}])], edges,
   window]]
isinonwindowapprox[window : polytopepattern, point : vecpattern, corrector : vecpattern,correctordims_Integer:0] := 
 Module[{newcorrector,randomdims},(*says if point is in window or not,
  for first point ON window (singular cut and project) pick small \
corrector such that point is in window and return corrector as \
well(corresponds to choice of limit patttern,
  see balke aperiodic order Lemma 7.3)*)
	SeedRandom[29071999];(*set seed to my birthday for reproducibilty of treatment of singular model sets. i.e. we always want to approach singular point from the same direction*)
	If[correctordims==0,randomdims=Length[corrector],randomdims=correctordims];
  newcorrector = corrector;
 (# /. {(x_ :> {True, corrector} /; 
          x < -eps), (x_ :> {False, corrector} /; x > eps), 
       x_ :> (If[Plus @@ Abs /@ corrector != 0, 
					(Print["second correction of singular window required!, point=",point,"corrector=",corrector,"distance to window",x,"eps",eps];Throw[1]), 
          While[! distancetowind[window, point + newcorrector]<-50*eps, 
           newcorrector = Join[RandomnDVector[500*eps,randomdims(*we only set a new point in the irrational dimension of internal space*)],(0&)/@Range[Length[corrector]-randomdims]]]];
			 {True, newcorrector})(*tiebreaker[#,point]*)} &)[
			 	distancetowind[window,point+corrector]]
	]
distancetowind[window:polytopepattern,point:vecpattern]:=Max @@ (distancetoedge[#,point]&) /@ window
distancetoedge[edge:halfspacepattern,point:vecpattern]:=N[((edge[[1]] . point) - edge[[2]])/Norm[edge[[1]]](*divide by Norm to obtain euclidean distance*)]
distanceofedge[edge:halfspacepattern]:=edge[[2]]/Norm[edge[[1]]]
windowedgeintersect[windedge1 : halfspacepattern,windedge2 : halfspacepattern] :=windowedgeintersect[{windedge1,windedge2}]
windowedgeintersect[edges:polytopepattern]:=FullSimplify[Inverse[edges[[All, 1]]] . edges[[All, 2]]]
contractwindow[wind : polytopepattern,boundary : _?NumericQ] := ({#[[1]], #[[2]] - boundary/Norm[#[[1]]]} &) /@ wind
addintersectionpoint[poly_, rules_] :=(*rules are list of replacement rules of form {p1,p2}->intersectionpoint, with each point being a list of length 2. they are created in splitconcavepolygon.*)
(Function[{idx},
   Sequence @@ {({cyclicindex[poly, idx - 1],poly[[idx]]}) /.
      Append[rules, (_ -> (Unevaluated@Sequence[]))],poly[[idx]]}] /@
  Range[Length[poly]])
applyboundarylayers[listsofpolys:{{{vecpattern ..} ..} ..},wind:polytopepattern,boundarylayers:{_?NumericQ ...}]:=Module[
	{intersections={},toreturn},
		toreturn=Fold[
			Function[{polys,width},
				((intersections=Join[intersections,#[[2]]](*;Export[StringJoin[ToString[width],".csv"],#[[1]]]*);filteroutsmallpolygons[#[[1]]])&)[
					cutbywindow[contractwindow[wind, width], polys]
				]
			],listsofpolys, boundarylayers
		];
		Map[(addintersectionpoint[#,intersections]&),N[toreturn],{2}]
	]
applyboundarylayers[points_?formsQ,wind:polytopepattern,boundarylayers:{_?NumericQ ...}]:=(Print["wrong boundary function called",depthlist[points]];Fold[(cutbywindow[contractwindow[wind, #2], #1] &), points, boundarylayers])
removesmalledge[polygon : {vecpattern ..}] :=
 Module[{edgelengths},
  edgelengths =
    EuclideanDistance @@@
     N[Partition[Append[polygon, First[polygon]], 2, 1]];
  polygon[[Select[
     Range[Length[
       polygon]], (edgelengths[[#]] >
        Wpgroups`Private`limpointmerge &)]]]]
filteroutsmallpolygons[polys : {{{vecpattern ..} ..} ..}] :=
 Module[{filtered},
  filtered = (GroupBy[(*first position are polygons to filter out*)#,
       Function[{poly}, (SmallestEdgeLength[poly] < Wpgroups`Private`limpointmerge &&
          Abs[signedArea[poly]] < Wpgroups`Private`limpointmerge^2)]] &) /@ polys;
  Map[(removesmalledge[#]/.({}->Sequence[])&), (#[False] &) /@ filtered, {2}]]
filteroutsmallpolygons[wronginput_]:=
SmallestEdgeLength[polygon : {vecpattern ..}] :=
 Min[EuclideanDistance @@@
    N[Partition[Append[polygon, First[polygon]], 2, 1]]]
intersection[edge:halfspacepattern,p1 : vecpattern, p2 : vecpattern]:=intersection[edge[[1]],edge[[2]],p1,p2]
intersection[n : vecpattern, lim_?NumericQ, p1 : vecpattern, p2 : vecpattern] :=
 (
	FullSimplify[((lim - p2 . n)*
    p1 - (lim - p1 . n)*p2)/(p1 . n - p2 . n)])
SmallestPolygonAngle[polygonPoints : {vecpattern ..}] :=
 Module[{angles, sides, vectors,
   normalizedVectors},(*Create vectors for each edge of the polygon*)
  vectors =
   Differences[Append[polygonPoints, First[polygonPoints]]];
  (*Normalize vectors to unit length*)
  normalizedVectors = Map[Normalize, vectors];
  (*Compute the angles between adjacent vectors*)
  angles =
   Table[VectorAngle[
     normalizedVectors[[i]], -normalizedVectors[[Mod[i,
          Length[polygonPoints]] + 1]]], {i, Length[polygonPoints]}];
  (*Convert angles to degrees and find the smallest one*)Min[angles]]
SmallestPolygonAnglesplitatvertex[polygonPoints : {vecpattern ..},
  idx1_Integer, idx2_Integer] :=Min[SmallestPolygonAngle /@(*split SmallestPolygonAngle  in two at \
vertices specified by idx1 and idx2*){polygonPoints[[Range @@ Sort[{idx1, idx2}]]],
    polygonPoints[[Join[Range[Max[idx1, idx2], Length[polygonPoints]],
        Join[Range[1, Min[idx1, idx2]]]]]]}]
splitconcavepolygon[poly_Polygon] :=splitconcavepolygon[poly[[1]]]
(*concave polygons are not accepted by gmesh, we are presuming that the original tiling contains only convex tiles and each concave tile that we find must therefore stem from the cut by the exlusion strip, which means there is at most a single one convex vertex. We split the polygon at this vertex to create 2 convex ones*)
normvectorlength[vec : vecpattern] := vec/Norm[vec]
intersectlines[x1 : vecpattern, x2 : vecpattern, x3 : vecpattern,
  n : vecpattern] :=(*solves intersection of line connecting x1 and x2, \
with line of form: x3+t*n. first line is parametrised as x1+s(x2-x1)*)
 Module[{s, t}, {s, t} =
   Inverse[{{x2[[1]] - x1[[1]], -n[[1]]}, {x2[[2]] -
        x1[[2]], -n[[2]]}}] . (x3 - x1); x3 + t*n]
splitconcavepolygon[poly : {vecpattern ..}] :=(*returns list of: {poly1,..,polyn,replacementrule} where:
		poly1 and poly2 are 2 polygons and
		replacementrule is 0 if the number of concavevertex is not equal to 1; otherwise it specifies two adjacent vertices of poly as well as the intermediate point that will be inserted between them. See addintersectionpoint for format*)
Module[{polysorted,edges,concavevertex, splitdirectionnormal,verticesseperatedbysplit, intersectionpoint},
	polysorted=ensureCounterclockwise[poly];
	edges = Differences[Append[N[polysorted], N[First[polysorted]]]](*edges[[i]]=polysorted[[i+1]]-polysorted[[i]]*);
  concavevertex = Flatten[Position[polygonanglesfromedges[edges], x_ /; x > Pi-0.000001]];
	If[
		(Length[concavevertex]>1 && AnyTrue[Differences[concavevertex],(#==1&)]),
		Throw["a single polygon contains two or more vertices of the domain-boundary shifted by boundarylayer, choose smaller values for boundarylayers"]
	];
	Length[concavevertex]/.{
		(0:>{polysorted,0}),
		(1:>(	splitdirectionnormal = ({#[[2]], -#[[1]]} &)[
      			Mean[(*edge vectors coinciding at concavevertex pointing inwards*)normvectorlength /@ {
							{{0,-1},{1,0}}.cyclicindex[edges, concavevertex[[1]] - 1], -{{0,1},{-1,0}}.cyclicindex[edges, concavevertex[[1]]]
						}]
					];
    			verticesseperatedbysplit = ({#, Mod[#, Length[polysorted]] + 1} &)[
						SelectFirst[(*vertex after which splitdirectionnormal*t+ poly[[concavevertex]] with parameters 5 intersects polygon*)
							(Mod[concavevertex[[1]] + # - 1, Length[polysorted]] + 1 &) /@ Range[Length[polysorted] - 2],
							(Sign[ polysorted[[#]] . splitdirectionnormal - splitdirectionnormal . polysorted[[concavevertex[[1]]]]] !=
							Sign[cyclicindex[polysorted, # + 1] . splitdirectionnormal - splitdirectionnormal . polysorted[[concavevertex[[1]]]]] &)
						]
					];
    			intersectionpoint =(*point between verticesseperatedbysplit where  both polygons will be split*) intersectlines[
						polysorted[[verticesseperatedbysplit[[1]]]],
						polysorted[[verticesseperatedbysplit[[2]]]],
						polysorted[[concavevertex[[1]]]],
						({-#[[2]], #[[1]]} &)[ splitdirectionnormal]
					];
					{splitconcavepolygon[polysorted, verticesseperatedbysplit[[1]],
     concavevertex[[1]], intersectionpoint],(polysorted[[Reverse[(*reverse because same idces will appear in reversed order in partner polygon*)verticesseperatedbysplit]]]->(intersectionpoint))}
    )
	),
		(2:>{polysorted[[Range @@ concavevertex]],polysorted[[Join[Range[concavevertex[[2]], Length[polysorted]], Range[1, concavevertex[[1]]]]]],0}),
		(_:>{
			Sequence@@(polysorted[[Range @@ #]]&)/@Transpose[{concavevertex, RotateLeft[concavevertex]}][[1 ;; -2]],
			polysorted[[Join[Range[concavevertex[[-1]], Length[polysorted]], Range[1, concavevertex[[1]]]]]],
			polysorted[[concavevertex]],
			0})
}   
]
(*splitconcavepolygon[poly : {vecpattern ..}] :=(*returns list of: {{poly1,..,polyn},replacementrule} where:
		poly1 and poly2 are 2 polygons and
		replacementrule is Sequence[] if the number of concavevertex is not equal to 1; otherwise it specifies two adjacent vertices of poly as well as the intermediate point that will be inserted between them. See addintersectionpoint for format*)
 Module[{concavevertex, splitdirectionnormal,
   verticesseperatedbysplit, intersectionpoint,edges},
  If[ConvexPolygonQ[Polygon[poly]],
   Throw["Polygon must be concave"], (edges = Differences[Append[poly, First[poly]]];
    concavevertex =
     FirstPosition[polygonanglesfromedges[edges], x_ /; x > Pi][[1]];
    splitdirectionnormal = ({#[[2]], -#[[1]]} &)[
      Mean[(*edge vectors coinciding at concavevertex pointing inwards*)normvectorlength /@ {cyclicindex[edges, concavevertex - 1], -cyclicindex[edges, concavevertex]}]];
    verticesseperatedbysplit = ({#, Mod[#, Length[poly]] + 1} &)[
      SelectFirst[(*vertex after which splitdirectionnormal+
       poly[[concavevertex]] may intersect polygon*)(Mod[
            concavevertex + # - 1, Length[poly]] + 1 &) /@
        Range[Length[poly] -
          2], (Sign[
           poly[[#]] . splitdirectionnormal -
            splitdirectionnormal . poly[[concavevertex]]] !=
          Sign[cyclicindex[poly, # + 1] . splitdirectionnormal -
            splitdirectionnormal . poly[[concavevertex]]] &)]];
    intersectionpoint =(*point between verticesseperatedbysplit where \
both polygons will be split*)
     intersectlines[poly[[verticesseperatedbysplit[[1]]]],
      poly[[verticesseperatedbysplit[[2]]]],
      poly[[concavevertex]], ({-#[[2]], #[[1]]} &)[
       splitdirectionnormal]];
		{splitconcavepolygon[poly, verticesseperatedbysplit[[1]],
     concavevertex, intersectionpoint],(poly[[verticesseperatedbysplit]]->(intersectionpoint))}
    )]]*)
splitconcavepolygon[polysorted : {vecpattern ..},
  vertexbeforesplit_Integer, concavevertex_Integer,
  intersectionpoint : vecpattern] := Sequence[
  Join[polysorted[[Select[(Mod[# - 1, Length[polysorted]] + 1 &) /@
       Range[concavevertex,
        concavevertex + Length[polysorted] -
         1], (Mod[# - concavevertex, Length[polysorted]] <=
         Mod[vertexbeforesplit - concavevertex,
          Length[polysorted]] &)]]], {intersectionpoint}],
  Join[{intersectionpoint},
   polysorted[[
      Select[(Mod[# - 1, Length[polysorted]] + 1 &) /@
        Range[vertexbeforesplit + 1, vertexbeforesplit + Length[polysorted]], (
					Mod[vertexbeforesplit - concavevertex, Length[polysorted]] <= Mod[# - 1 - concavevertex, Length[polysorted]] &
				)
			]
		]]
	]
]
cutbywindow[wind : polytopepattern, points_?formsQ] := Module[{intersections={},toreturn},
	toreturn=Map[
		Function[{poly},
			((If[
				NumericQ[(*use numericQ since I use 0 to indicate no intersection is to be reported*)#[[-1]]],
				"donothing",
				intersections=Append[intersections,#[[-1]]]];
				Sequence @@ #[[1;;-2]])&)[
				cutbywindowpoly[wind, poly]
			]
		], 
   points, {depthlist[points] - 3}
	];
	{toreturn,intersections}]
cutbywindowpoly[wind : polytopepattern, polygon : {vecpattern ..}] := (*returns list {p1,..,pn,intersectionrule} where p1,..,pn are new polygons created by the cut through the window and intersection rule is a replacement rule mapping two neighbouring vertices to the intersectionpoint lying on the edge connecting them; see addintersectionpoint for format*)
 Module[{windpoly, meshes,toreturn,intersection=0}, 	
  windpoly = 
  	MapThread[windowedgeintersect, {wind, RotateLeft[wind]}];
  meshes = ConvexHullMesh /@ {polygon, windpoly};
(*deal with concave polygons which gmesh does not like*)
  toreturn=Sequence @@ (Function[{poly},If[
		customConvexPolygonQ[poly[[1]]],
		poly[[1]],
		((intersection=#[[-1]];Sequence@@(#[[1;;-2]]))&)[splitconcavepolygon[poly[[1]]]]
		]])
/@
   Flatten[(MeshPrimitives[#, 2] &) /@ (If[NumericQ[#],Unevaluated@Sequence[],#]&) /@ {Sequence @@ 
       RegionDifference[meshes[[1]], meshes[[2]]], 
      Sequence @@ RegionIntersection[meshes[[1]], meshes[[2]]]}, 1];
	 {toreturn,intersection}]
End[]

EndPackage[]
