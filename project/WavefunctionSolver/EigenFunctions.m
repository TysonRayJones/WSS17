(* ::Package:: *)

Package["WavefunctionSolver`"]

(*
	TODO:
	
	- user specify (as options) number of points / grid-spacing in each dimension in 2D
	- allow AxesLabel \[Rule] option to contain 1D Functions of the quantum number
		
	- adding timing / loadbar stuff (use EchoFunction)
	
	- fix awful private imports
	- non-abs-squared plots for real station states?
	- 2D stationary state finding (GetSpectrum)
	- plotting 2D spectra 
	  hmmm will we need two manipulate variables for two quantum numbers? We'd have to ascertain energy degeneracy ourselves!
	  NAH!
	- functions to plot energy vs angular momentum spectra (like in my honours project)!
	   maybe you can click a E(l) plot point to see the prob density and phase of that mode? hmmm
	   I think this will only be relevant in 2D; what's angular momentum in 1D?
	- overload PlotSpectrum to allow not-passing and not-plotting eigenvalues?
	- use of <> ToString[] is dodgy in PlotSpectrum. E.g. use eigenvalue =1/3 or Exp[2]. Fix! (use StringForm?)
	- add options to time diagonalisation steps
	- FIX USAGE MESSAGES; they're not overloading correctly
	- change Riemann integration in normalisation to something smarter??? (requires updating Usages)
	- PlotSpectrum functions accept 'domain or grid', but don't enforce Length[grid] === Length[eigfuncs[[1]]] say,
	  so (maybe confusingly) would match calls with strange/irrelevant intra-grid values. Fix this?
	- overload functions with British vs American english spelling (s vs z)
	- user specify order of finite difference in laplacian operator (make general laplacian matrix construction
	- error on negative NumberOfPoints or NumberOfModes
	- make numerical methods (e.g. matrix generation) public? add a discretise method which can also discretise a wavefunction?
	- make public functions for converting types (e.g. list to function, symbolic to list, etc)?
	  
	NOTES:
	- hmmm should OutputAsFunctions be renamed to InterpolateResults?
	- PlotSpectrum would be faster if cbar was external to Manipulate, but that's a pain
*)





(* SYMBOL EXPORTS *)

PackageExport[NumberOfModes]
NumberOfModes::usage = "Specifies how many modes GetSpectrum[] should return (must be smaller than NumberOfPoints)";

PackageExport[NumberOfPoints]
NumberOfPoints::usage = "Specifies how many points on which GetSpectrum should discretise the hamiltonian. An odd number larger than one-thousand is reccommended";

PackageExport[AutoNormalize]
AutoNormalize::usage = "Specifies whether to L2-normalize the eigenvectors returned by GetSpectrum";

PackageExport[OutputAsFunctions]
OutputAsFunctions::usage = "Specifies whether to convert the eigenfunction vectors from GetSpectrum to InterpolatingFunctions"





(* FUNCTION EXPORTS *)

PackageExport[GetSpectrum]
PackageExport[PlotSpectrum]
PackageExport[NormalizeWavefunction]





(* FUNCTION USAGE MESSAGES *)

GetSpectrum::usage = "GetSpectrum[potential, domain] returns the family of {eigenvalues, eigenfunctions} of the hamiltonian associated with the given 1D potential, superposed with an infinite potential well.
GetSpectrum[potential, xDomain, yDomain] returns the family of {eigenvalues, eigenfunctions} of the hamiltonian associated with the given 2D potential, superposed with an infinite potential well.";

PlotSpectrum::usage = "PlotSpectrum[potential, domain] calculates and plots the single-particle energy-eigenfunctions of the given 1D potential.
PlotSpectrum[domain/grid, eigvals, eigfuncs] plots the given 1D eigensystem.
PlotSpectrum[{domain/grid, eigvals, eigfuncs}] plots the given 1D eigensystem.
PlotSpectrum[potential, xDomain, yDomain] calculates and plots the single-particle energy-eigenfunctions of the given 2D potential.
PlotSpectrum[xDomain, yDomain, eigvals, eigfuncs] plots the given 2D eigensystem.
PlotSpectrum[gridTensor, eigvals, eigfuncs] plots the given 2D eigensystem specified on the gridTensor of dimensions numXPoints * numYPoints * 2 (a matrix of tuple coordinates).
PlotSpectrum[{gridTensor, eigvals, eigfuncs}] plots the given 2D eigensystem specified on the gridTensor of dimensions numXPoints * numYPoints * 2 (a matrix of tuple coordinates).";

NormalizeWavefunction::usage = "NormalizeWavefunction[const, domain] L2 algebraically normalises a 1D constant wavefunction assumed zero outside of domain, returning a constant.
NormalizeWavefunction[const] returns const, as if the wavefunction vanishes from const outside [0, 1] or [0,1]^2.
NormalizeWavefunction[vector, gridspace] L2 normalises (by Riemann integration) a 1D vectorised wavefunction, with values separated by distance gridspace, returning a vector.
NormalizeWavefunction[vector, grid] L2 normalises (by Riemann integration) a 1D vectorised wavefunction defined on a given grid vector, returning a vector.
NormalizeWavefunction[vector, domain] L2 normalises (by Riemann integration) a 1D vectorised wavefunction defined across the given domain, returning a vector.
NormalizeWavefunction[vector] L2 normalises (by Riemann integration) assuming a grid-spacing of 1, and that the 1D wavefunction vanishes outside [0, Length[vector]].
NormalizeWavefunction[function, domain] numerically L2 normalises a 1D Function/Interpolating function across the given domain, returning a function.
NormalizeWavefunction[function, grid] L2 normalises a 1D Function/Interpolating function by first sampling it on the given grid, then Riemann-integrates. Returns a function.
NormalizeWavefunction[function] numerically L2 normalises a 1D function on (-\[Infinity], \[Infinity]).
NormalizeWavefunction[symb, {x, xL, xR}] symbolically L2 normalises a 1D symbolic expression on x \[Element] [xL, xR] (assuming the wavefunction vanishes outside).
NormalizeWavefunction[symb, x] symbolically L2 normalises a 1D symbolic expression on x \[Element] (-\[Infinity], \[Infinity]).
NormalizeWavefunction[symb, {x}] symbolically L2 normalises a 1D symbolic expression on x \[Element] (-\[Infinity], \[Infinity]).
NormalizeWavefunction[const, xDomain, yDomain] L2 algebraically normalises a 2D constant wavefunction assumed zero outside of domain, returning a constant.
NormalizeWavefunction[matrix, xGridspace, yGridspace] L2 normalises (by Riemann integration) a 2D matrix wavefunction, with values separated by different gridspaces in the x and y directions, returning a matrix.
NormalizeWavefunction[matrix, gridspace] L2 normalises (by Riemann integration) a 2D matrix wavefunction, with values separated by gridspace in both the x and y directions, returning a matrix.
NormalizeWavefunction[matrix, xGrid, yGrid] L2 normalises (by Riemann integration) a 2D matrix wavefunction defined on the given grid vectors, returning a matrix.
NormalizeWavefunction[matrix, grid] L2 normalises (by Riemann integration) a 2D matrix wavefunction defined on the given grid vector in both the x and y directions, returning a matrix.
NormalizeWavefunction[matrix, xDomain, yDomain] L2 normalises a 2D matrix wavefunction assumed zero outside of given domains, returning a matrix.
NormalizeWavefunction[matrix, domain] L2 normalises a 2D matrix wavefunction assumed zero outside of given domain, assumed to apply to both x and y directions, returning a matrix.
NormalizeWavefunction[matrix] L2 normalises (by Riemann integration) assuming a grid-spacing of 1, and that the 2D wavefunction vanishes outside the implied {x, y} \[Element] Dimensions[matrix] domain.
NormalizeWavefunction[function, xDomain, yDomain] numerically L2 normalises a 2D Function/Interpolating function across the given domains, returning a function.
NormalizeWavefunction[function, domain] numerically L2 normalises a 2D Function/Interpolating function across the given domains, assumed to apply in both x and y directions, returning a function.
NormalizeWavefunction[function, xGrid, yGrid] L2 normalises a 2D Function/Interpolating function by first sampling it on an interlacing of the passed grids, then Riemann-integrates. Returns a function.
NormalizeWavefunction[function, grid] L2 normalises a 2D Function/Interpolating function by first sampling it on a 2D grid (symmetric in x and y) then Riemann-integrates. Returns a function.
NormalizeWavefunction[function] numerically L2 normalises the 2D function on x:(-\[Infinity], \[Infinity]), y:(-\[Infinity], \[Infinity]).
NormalizeWavefunction[symb, {x, xL, xR}, {y, yL, yR}] symbolically L2 normalises a 2D symbolic expression on x \[Element] [xL, xR], y \[Element] [yL, yR] (assuming the wavefunction vanishes outside). Returns a symbolic expression.
NormalizeWavefunction[symb, x, y] symbolically L2 normalises a 2D symbolic expression on x \[Element] (-\[Infinity], \[Infinity]), y \[Element] (-\[Infinity], \[Infinity]).
NormalizeWavefunction[symb, {x}, {y}] symbolically L2 normalises a symbolic expression on x \[Element] (-\[Infinity], \[Infinity]), y \[Element] (-\[Infinity], \[Infinity]).";





(* FUNCTION ERROR MESSAGES *)

GetSpectrum::numPointsLessThanTwoError1D = "NumberOfPoints -> `1` given to GetSpectrum is too small; must be at least 2. Using NumberOfPoints -> `2` instead.";
GetSpectrum::numPointsLessThanNumModesError1D = "NumberOfPoints -> `1` given to GetSpectrum is less than NumberOfModes -> `2`. Using NumberOfPoints -> `3` instead.";
GetSpectrum::numPointsError2D = "NumberOfPoints -> `1` given to GetSpectrum contains either too small a number (the number of points must be at least 2 in each dimension), or a number less than NumberOfModes -> `2`, or both. Using NumberOfPoints -> `3` instead.";





(* SYMBOL IMPORTS *)

realNumQ = WavefunctionSolver`PlotFunctions`PackagePrivate`realNumQ;
domainQ = WavefunctionSolver`PlotFunctions`PackagePrivate`domainQ;
domainSymbQ = WavefunctionSolver`PlotFunctions`PackagePrivate`domainSymbQ;
domainOptSymbQ = WavefunctionSolver`PlotFunctions`PackagePrivate`domainOptSymbQ;

isValid1DInput = WavefunctionSolver`PlotFunctions`PackagePrivate`isValid1DInput;
isValid1DPotentialOptions = WavefunctionSolver`PlotFunctions`PackagePrivate`isValid1DPotentialOptions;
isValid2DInput = WavefunctionSolver`PlotFunctions`PackagePrivate`isValid2DInput;
isValid2DPotentialOptions = WavefunctionSolver`PlotFunctions`PackagePrivate`isValid2DPotentialOptions
is1DFunction = WavefunctionSolver`PlotFunctions`PackagePrivate`is1DFunction;
is2DFunction = WavefunctionSolver`PlotFunctions`PackagePrivate`is2DFunction;

convertToFunction = WavefunctionSolver`PlotFunctions`PackagePrivate`convertToFunction;
convertToEndPoints = WavefunctionSolver`PlotFunctions`PackagePrivate`convertToEndPoints;

plotOptionFunctions1D = WavefunctionSolver`PlotFunctions`PackagePrivate`optionFunctions1D;
plotOptionFunctions2D = WavefunctionSolver`PlotFunctions`PackagePrivate`optionFunctions2D;

extractOptions = WavefunctionSolver`PlotFunctions`PackagePrivate`extractOptions;
addDefaultOptions = WavefunctionSolver`PlotFunctions`PackagePrivate`addDefaultOptions;
extractOptionsFromDefaults = WavefunctionSolver`PlotFunctions`PackagePrivate`extractOptionsFromDefaults;

defaultXLabel = WavefunctionSolver`PlotFunctions`PackagePrivate`defaultXLabel;
defaultYLabel = WavefunctionSolver`PlotFunctions`PackagePrivate`defaultYLabel;



(* CONSTANTS *)

default1DNumPoints = 10^4 + 1;    (* odd for pleasant grid spacing *)
default2DNumPoints = 10^2 + 1;    (* in x and y, independently *)
eigenValueLabel = "\!\(\*SubscriptBox[\(E\), \(`1`\)]\) = `2`";
eigenFuncProbLabel = "|\!\(\*SubscriptBox[\(\[Phi]\), \(`1`\)]\)\!\(\*SuperscriptBox[\(|\), \(2\)]\)";
eigenFuncPhaseLabel = "phase(\!\(\*SubscriptBox[\(\[Phi]\), \(`1`\)]\))";

defaultModeLabel = "mode";





(* PRIVATE FUNCTION DEFINITIONS *)

(*
	returns whether argument is a Function/InterpolatingFunction of one or two parameters
*)
func1DQ[func:(_Function|_InterpolatingFunction)] :=
	is1DFunction[func]
func1DQ[Except[(_Function|_InterpolatingFunction)]] :=
	False
	
func2DQ[func:(_Function|_InterpolatingFunction)] :=
	is2DFunction[func]
func2DQ[Except[(_Function|_InterpolatingFunction)]] :=
	False





(*
	returns a numerical grid {x1, x2, ... x_numPoints} and the potential sampled on those grid-points.
	numPoints = Automatic will use the length of list inputs, and default1DNumPoints for other inputs
*)

(* functions *)
discretise1DPotential[
	potential:_?func1DQ,
	domain:{___Symbol, xL_?realNumQ, xR_?realNumQ},
	numPoints:_Integer
] :=
	With[
		{grid = Array[(#&), numPoints, {xL, xR}]},
		{grid, potential /@ grid}
	]
discretise1DPotential[
	potential:_?func1DQ,
	domain:_?domainOptSymbQ,
	Automatic
] :=
	discretise1DPotential[potential, domain, default1DNumPoints]

(* vectors *)
discretise1DPotential[
	potential_/;VectorQ[potential, NumericQ],
	domain:{___Symbol, xL_?realNumQ, xR_?realNumQ},
	numPoints:_Integer
] :=
	With[
		{grid = Array[(#&), numPoints, {xL, xR}]},
		{grid, ListInterpolation[potential, {{xL, xR}}] @ grid}
	]
discretise1DPotential[
	potential_/;VectorQ[potential, NumericQ],
	domain:{___Symbol, xL_?realNumQ, xR_?realNumQ},
	Automatic
] :=
	With[
		{grid = Array[(#&), Length[potential], {xL, xR}]},
		{grid, potential}
	]
	
(* constants *)
discretise1DPotential[
	potential_?NumericQ,
	domain:{___Symbol, xL_?realNumQ, xR_?realNumQ},
	numPoints:_Integer
] :=
	With[
		{grid = Array[(#&), numPoints, {xL, xR}]},
		{grid, ConstantArray[potential, numPoints]}
	]
discretise1DPotential[
	potential_?NumericQ,
	domain:_?domainOptSymbQ,
	Automatic
] :=
	discretise1DPotential[potential, domain, default1DNumPoints]
	
(* symbolic expressions *)
discretise1DPotential[
	potential_,
	domain:{x_Symbol, xL_?realNumQ, xR_?realNumQ},
	numPoints:_Integer
] :=
	With[
		{grid = Array[(#&), numPoints, {xL, xR}]},
		{grid, ReplaceAll[potential, ({x -> #}&) /@ grid]}
	]
discretise1DPotential[
	potential:_,
	domain:_?domainSymbQ,
	Automatic
] :=
	discretise1DPotential[potential, domain, default1DNumPoints]

(* 2D *)

(* functions *)
discretise2DPotential[
	potential:_?func2DQ,
	xDomain:{___Symbol, xL_?realNumQ, xR_?realNumQ},
	yDomain:{___Symbol, yL_?realNumQ, yR_?realNumQ},
	numXPoints:_Integer,
	numYPoints:_Integer
] :=
	Module[
		{xPoints, yPoints, xyTuples, potentialValues},
		xPoints = Array[Identity, numXPoints, {xL, xR}];       (* flat list of x values *)
		yPoints = Array[Identity, numYPoints, {yL, yR}];       (* flat list of y values *)
		xyTuples = Flatten[Outer[List, xPoints, yPoints], 1];  (* flat list of {x, y} tuples *)
		potentialValues = (potential @@ # &) /@ xyTuples;     (* flat list of potential values at {x, y} tuples *)
		
		(* returns *)
		{xPoints, yPoints, xyTuples, potentialValues}
	]
discretise2DPotential[
	potential:_?func2DQ,
	xDomain:_?domainOptSymbQ,
	yDomain:_?domainOptSymbQ,
	numXPoints:Automatic,
	numYPoints:(_Integer|Automatic)
] :=
	discretise2DPotential[potential, xDomain, yDomain, default2DNumPoints, numYPoints]
discretise2DPotential[
	potential:_?func2DQ,
	xDomain:_?domainOptSymbQ,
	yDomain:_?domainOptSymbQ,
	numXPoints:(_Integer|Automatic),
	numYPoints:Automatic
] :=
	discretise2DPotential[potential, xDomain, yDomain, numXPoints, default2DNumPoints]

(* matrices *)
discretise2DPotential[
	potential_/;MatrixQ[potential, NumericQ],
	xDomain:{___Symbol, xL_?realNumQ, xR_?realNumQ},
	yDomain:{___Symbol, yL_?realNumQ, yR_?realNumQ},
	numXPoints:_Integer,
	numYPoints:_Integer
] :=
	discretise2DPotential[
		ListInterpolation[potential, {{xL, xR}, {yL, yR}}],
		xDomain, yDomain, numXPoints, numYPoints
	]
discretise2DPotential[
	potential_/;MatrixQ[potential, NumericQ],
	xDomain:_?domainOptSymbQ,
	yDomain:_?domainOptSymbQ,
	numXPoints:Automatic,
	numYPoints:(_Integer|Automatic)
] :=
	discretise2DPotential[
		potential, xDomain, yDomain,
		Dimensions[potential][[2]],
		numYPoints
	]
discretise2DPotential[
	potential_/;MatrixQ[potential, NumericQ],
	xDomain:_?domainOptSymbQ,
	yDomain:_?domainOptSymbQ,
	numXPoints:(_Integer|Automatic),
	numYPoints:Automatic
] :=
	discretise2DPotential[
		potential, xDomain, yDomain,
		numXPoints,
		Dimensions[potential][[1]]
	]
	
(* constants *)
discretise2DPotential[
	potential_?NumericQ,
	xDomain:_?domainOptSymbQ,
	yDomain:_?domainOptSymbQ,
	numXPoints:(_Integer|Automatic),
	numYPoints:(_Integer|Automatic)
] :=
	discretise2DPotential[
		(potential &),
		xDomain, yDomain, numXPoints, numYPoints
	]
	
(* symbolic expressions *)
discretise2DPotential[
	potential_,
	xDomain:{x_Symbol, _?realNumQ, _?realNumQ},
	yDomain:{y_Symbol, _?realNumQ, _?realNumQ},
	numXPoints:(_Integer|Automatic),
	numYPoints:(_Integer|Automatic)
] :=
	discretise2DPotential[
		Function @@ {{x, y}, potential},
		xDomain, yDomain, numXPoints, numYPoints
	]
	
(* 
	functions for building matrix operators
*)
(* 1D *)

get1DLaplacianMatrix[
	grid_/;VectorQ[grid, realNumQ]
] :=
	With[
		{numPoints = Length[grid],
		 gridSpace = grid[[2]] - grid[[1]]},
		SparseArray[
			{   (* 4th order accuracy of 1D finite difference 2nd-deriv *)
				{i_,i_} :> N @ -5/2,
				{i_,j_} /; Abs[i-j] == 1 :> N @ 4/3,
				{i_,j_} /; Abs[i-j] == 2 :> N @ -1/12 
			},
			{numPoints, numPoints}
		]
		/ gridSpace^2
	]
	
get1DKineticEnergyMatrix[
	grid_/;VectorQ[grid, realNumQ]
] :=
	-(1/2) get1DLaplacianMatrix[grid]
	
get1DPotentialMatrix[
	potentialVector_/;VectorQ[potentialVector, NumericQ]
] :=
	With[
		{endVal = 10^3 Max[potentialVector]},
		DiagonalMatrix @ SparseArray[                                     (* sparse for efficient matrix *)
			Append[Prepend[potentialVector[[2;;-2]], endVal], endVal]    (* big end-points for stability *)
		]
	]
	
get1DHamiltonianMatrix[
	grid_/;VectorQ[grid, realNumQ],
	potentialVector_/;VectorQ[potentialVector, NumericQ]
] :=
	get1DKineticEnergyMatrix[grid] + get1DPotentialMatrix[potentialVector]	
	
get1DGridAndHamiltonian[
	potential:_,                      (* has passed isValid1DInput *)
	domain:_?domainOptSymbQ,
	numPoints:(_Integer|Automatic)
] :=
	Module[
		{grid, potentialVector},
		{grid, potentialVector} = discretise1DPotential[potential, domain, numPoints];
		{grid, get1DHamiltonianMatrix[grid, potentialVector]}
	]
	
(* 2D *)
get2DLaplacianMatrix[   (* via kronecker sum *)
	xPoints_/;VectorQ[xPoints, realNumQ],
	yPoints_/;VectorQ[yPoints, realNumQ]     (* I DON'T THINK THIS IS RIGHT, WHEN xPoints \[NotEqual] yPoints *)
] :=
	KroneckerProduct[
		get1DLaplacianMatrix[yPoints],
		IdentityMatrix @ Length[xPoints]
	] + 
	KroneckerProduct[
		IdentityMatrix @ Length[yPoints], 
		get1DLaplacianMatrix[xPoints]
	]

get2DKineticEnergyMatrix[
	xPoints_/;VectorQ[xPoints, realNumQ],
	yPoints_/;VectorQ[yPoints, realNumQ]
] :=
	-(1/2) get2DLaplacianMatrix[xPoints, yPoints]

get2DPotentialMatrix[
	potentialVector_/;VectorQ[potentialVector, NumericQ]
] :=
	get1DPotentialMatrix[potentialVector]
	
get2DHamiltonianMatrix[
	xPoints_/;VectorQ[xPoints, realNumQ],
	yPoints_/;VectorQ[yPoints, realNumQ],
	potentialVector_/;VectorQ[potentialVector, NumericQ]
] :=
	get2DKineticEnergyMatrix[xPoints, yPoints] + 
	get2DPotentialMatrix[potentialVector]	

get2DGridAndHamiltonian[
	potential:_,                      (* has passed isValid2DInput *)
	xDomain:_?domainOptSymbQ,
	yDomain:_?domainOptSymbQ,
	numXPoints:(_Integer|Automatic),
	numYPoints:(_Integer|Automatic)
] :=
	Module[
		{xPoints, yPoints, xyTuples, potentialVector},
		{xPoints, yPoints, xyTuples, potentialVector} = 
			discretise2DPotential[potential, xDomain, yDomain, numXPoints, numYPoints];
			
		(* return *)
		{
			{Length[xPoints], Length[yPoints], xyTuples}, 
			get2DHamiltonianMatrix[xPoints, yPoints, potentialVector]
		}
	]
	


(* 1D and 2D *)
getGridAndHamiltonian[
	potential:_,                      (* has passed isValid1DInput *)
	domain:_?domainOptSymbQ,
	numPoints:(_Integer|Automatic)
] :=
	get1DGridAndHamiltonian[potential, domain, numPoints]
getGridAndHamiltonian[
	potential:_,                      (* has passed isValid2DInput *)
	xDomain:_?domainOptSymbQ,
	yDomain:_?domainOptSymbQ,
	numXPoints:(_Integer|Automatic),
	numYPoints:(_Integer|Automatic)
] :=
	get2DGridAndHamiltonian[potential, xDomain, yDomain, numXPoints, numYPoints]





(* 1D and 2D *)

getEigenmodes[
	potential_,
	domains:Repeated[_?domainOptSymbQ, {1, 2}],       
	numPoints:Repeated[(_Integer|Automatic), {1, 2}], 
	numModes:_Integer,
	normalise:(True|False),    (* whether to L2 normalise eigenfunctions *)
	interpolate:(True|False)   (* whether to interpolate eigenfunctions vectors to InterpolatingFunctions *)
] /; (
	Length[{domains}] === Length[{numPoints}]  (* eliminates numPoints|numModes ambiguity *)
) :=
	Module[
		{grid, hamiltonian, eigvals, eigfuncs},
			
		(* get grid vector and hamiltonian matrix *)
		{grid, hamiltonian} = getGridAndHamiltonian[
			potential, 
			Evaluate @ domains, 
			Evaluate @ numPoints
		];
		
		(* diagonalise hamiltonian *)
		{eigvals, eigfuncs} = Eigensystem[hamiltonian, -numModes];
		
		(* sort eigenfunctions by increasing eigenvalue *)
		{eigvals, eigfuncs} = {eigvals[[#]], eigfuncs[[#]]}& @ Ordering[eigvals];
		
		(* restore flat grid and convert vectors to matrices if we're working in 2D *)
		{grid, eigfuncs} = If[
			Length[{domains}] === 1,
			{
				grid, 
				eigfuncs
			},
			{
				(* transform grid from tuple array into tuple matrix *)
				Transpose @ Table[
					grid[[3, j + (i-1)grid[[2]]]],
					{i, grid[[1]]}, {j, grid[[2]]}
				],
				(ArrayReshape[#, Reverse @ grid[[;;2]]]&) /@ eigfuncs
			}
		];
		
		(* optionally L2-normalise eigenfunctions *)
		eigfuncs = If[
			normalise,
			NormalizeWavefunction[#, Evaluate @ domains]& /@ eigfuncs,
			eigfuncs
		];
		
		(* optionally interpolate eigenfunctions *)
		eigfuncs = If[
			interpolate,
			convertToFunction[#, Evaluate @ domains]& /@ eigfuncs,
			eigfuncs
		];
		
		(* return *)
		{grid, eigvals, eigfuncs}
	]



(* 
	ensures NumberOfPoints option to GetSpectrum is of correct dimensionality and size 
*)

(* 1D *)
fix1DNumPointsParam[
	numPoints:_Integer /; numPoints < 2,
	numModes:_Integer
] := (
	Message[
		GetSpectrum::numPointsLessThanTwoError1D, 
		numPoints,
		default1DNumPoints
	];
	default1DNumPoints
)

fix1DNumPointsParam[
	numPoints:_Integer?Positive,
	numModes:_Integer
] /; (
	numPoints < Abs @ numModes
) := 
	With[
		{substitute = Max[default1DNumPoints, Abs @ numModes]},
		(
			Message[
				GetSpectrum::numPointsLessThanNumModesError1D, 
				numPoints, 
				Abs @ numModes, 
				substitute
			];
			substitute
		)
	]

fix1DNumPointsParam[
	numPoints:Automatic|(_Integer?Positive),
	numModes:_Integer
] :=
	numPoints

fix1DNumPointsParam[
	tuple:{Repeated[Automatic|(_Integer?Positive), {1,2}]},
	numModes:_Integer
] :=
	fix1DNumPointsParam[tuple[[1]], numModes]
	


monkeyhackConstrainNumPoints[
	tuple:{numXPoints_Integer, numYPoints_Integer}
] /; (
	numXPoints === numYPoints
) :=
	tuple
monkeyhackConstrainNumPoints[
	{numXPoints_Integer, numYPoints:(_Integer|Automatic)}
] /; (
	numXPoints != numYPoints
) := (
	Echo @ Style[
		StringForm[
			"GetSpectrum currently cannot accept a different number of points in the X and Y directions. Proceeding with NumberOfPoints -> {`1`, `1`}", 
			numXPoints
		],
		Orange
	];
	{numXPoints, numXPoints}
)
monkeyhackConstrainNumPoints[
	tuple:{Automatic, Automatic}
] :=
	tuple

monkeyhackConstrainDomains[
	xDomain:_?domainOptSymbQ,
	yDomain:_?domainOptSymbQ
] /; (
	convertToEndPoints[xDomain] === convertToEndPoints[yDomain]
) :=
	Sequence @@ {xDomain, yDomain}

monkeyhackConstrainDomains[
	{x:Repeated[_Symbol, {0,1}], xL:_?realNumQ, xR_?realNumQ},
	{y:Repeated[_Symbol, {0,1}], yL:_?realNumQ, yR_?realNumQ}
] /; (
	{xL, xR} != {yL, yR}
) := (
	Echo @ Style[
		StringForm[
			"GetSpectrum currently cannot accept different end-points in the X and Y directions. Proceeding using domains x: `1` and y:`1`",
			{xL, xR}
		],
		Orange
	];
	Sequence @@ {
		{x, xL, xR},
		{y, xL, xR}
	}
)



(* 2D *)

fix2DNumPointsParam[
	tuple:{
		numXPoints:(_Integer ? (# > 1 &))|Automatic,
		numYPoints:(_Integer ? (# > 1 &))|Automatic
	},
	numModes:_Integer
] /; (
	(numXPoints === Automatic || numXPoints >= Abs @ numModes) &&
	(numYPoints === Automatic || numYPoints >= Abs @ numModes)
) :=
	monkeyhackConstrainNumPoints @ tuple (************************ MONKEY PATCH **************************)
	
fix2DNumPointsParam[
	tuple:{
		numXPoints:(_Integer|Automatic),
		numYPoints:(_Integer|Automatic)
	},
	numModes_Integer
] :=
	With[
		{subtuple = {
			If[
				(numXPoints === Automatic) || (numXPoints >= Max[2, Abs @ numModes]), 
				numXPoints, 
				default2DNumPoints
			],
			If[
				(numYPoints === Automatic) || (numYPoints >= Max[2, Abs @ numModes]), 
				numYPoints, 
				default2DNumPoints
			]
		}},
		(
			Message[
				GetSpectrum::numPointsError2D,
				tuple, 
				Abs @ numModes, 
				subtuple
			];
			monkeyhackConstrainNumPoints @ subtuple  (************************ MONKEY PATCH **************************)
		)
	]
	
fix2DNumPointsParam[
	numPoints:(_Integer|Automatic),
	numModes_Integer
] :=
	fix2DNumPointsParam[{numPoints, numPoints}, numModes]
	
fix2DNumPointsParam[
	{numPoints:(_Integer|Automatic)},
	numModes_Integer
] :=
	fix2DNumPointsParam[{numPoints, numPoints}, numModes]
	
	
	


(* PUBLIC FUNCTION DEFINITIONS *)

Options[GetSpectrum] = {
	NumberOfModes -> 10,
	NumberOfPoints -> Automatic,  (* this can additionally be a {x, y} tuple in 2D *)
	AutoNormalize -> True,
	OutputAsFunctions -> False
};

(*
	Calculates {grid, {energy eigenvalues}, {energy eigenfunctions}} associated with a given potential
*)

(* 1D *)
GetSpectrum[
	potential:_,
	domain:_?domainOptSymbQ,
	options:OptionsPattern[GetSpectrum]
] /; (
	isValid1DInput[potential, domain]
) :=
	Module[
		(* unpack options once to throw at-most one error on unknown options *)
		{optNumberOfPoints, optNumberOfModes, optAutoNormalize, optOutputAsFunctions},
		{optNumberOfPoints, optNumberOfModes, optAutoNormalize, optOutputAsFunctions} =
			OptionValue @ {NumberOfPoints, NumberOfModes, AutoNormalize, OutputAsFunctions};
			
		getEigenmodes[
			potential,
			domain,
			fix1DNumPointsParam[optNumberOfPoints, optNumberOfModes],
			optNumberOfModes, optAutoNormalize, optOutputAsFunctions
		]
	]

(* 2D *)
GetSpectrum[
	potential:_,
	xDomain:_?domainOptSymbQ,
	yDomain:_?domainOptSymbQ,
	options:OptionsPattern[GetSpectrum]
] /; (
	isValid2DInput[potential, xDomain, yDomain]
) := 
	Module[
		(* unpack options once to throw at-most one error on unknown options *)
		{optNumberOfPoints, optNumberOfModes, optAutoNormalize, optOutputAsFunctions},
		{optNumberOfPoints, optNumberOfModes, optAutoNormalize, optOutputAsFunctions} =
			OptionValue @ {NumberOfPoints, NumberOfModes, AutoNormalize, OutputAsFunctions};

		getEigenmodes[
			potential,
			monkeyhackConstrainDomains @@ {xDomain, yDomain},
			Sequence @@ fix2DNumPointsParam[optNumberOfPoints, optNumberOfModes],
			optNumberOfModes, optAutoNormalize, optOutputAsFunctions
		]
	]
	
	
(* 
	L2 normalizes a given wavefunction, preserving its input form (besdies InterpolatingFunction \[Rule] Function).
	If no explicit domain is provided, symbolic/function integrations will assume {-\[Infinity], \[Infinity]} while
	constant/vector integrations will assume a grid-length/grid-spacing of 1
*)

(*
	1D
*)



symbolicQ := 
	MatchQ @ Except[_?NumericQ|_?(VectorQ[#, NumericQ]&)|_?(MatrixQ[#, NumericQ]&)|_?func1DQ|_?func2DQ];
	
(* constants *)
NormalizeWavefunction[
	wavefuncConst_?NumericQ,
	domain:{___Symbol, xL:_?realNumQ, xR_?realNumQ}
] :=
	wavefuncConst / (Abs[wavefuncConst] Sqrt[xR - xL])
	
NormalizeWavefunction[
	_?NumericQ,
	{___Symbol, (_?realNumQ|-\[Infinity]|\[Infinity]), (_?realNumQ|-\[Infinity]|\[Infinity])}
] :=
	0
	
NormalizeWavefunction[
	wavefuncConst_?NumericQ
] :=
	wavefuncConst

(* vectors *)
NormalizeWavefunction[
	wavefuncVector_/;VectorQ[wavefuncVector, NumericQ],           (* assumed 0 outside grid *)
	gridSpace_/;(realNumQ[gridSpace] && Positive[gridSpace])
] :=
	wavefuncVector / Sqrt[gridSpace Total[ Abs[wavefuncVector]^2 ]]	

NormalizeWavefunction[
	wavefuncVector_/;VectorQ[wavefuncVector, NumericQ],   (* assumed 0 outside grid *)
	domain:{___Symbol, xL_?realNumQ, xR_?realNumQ}
] :=
	NormalizeWavefunction[wavefuncVector, (xR - xL)/(Length[wavefuncVector] - 1)]
	
NormalizeWavefunction[
	wavefuncVector_/;VectorQ[wavefuncVector, NumericQ],   
	{___Symbol, (_?realNumQ|-\[Infinity]|\[Infinity]), (_?realNumQ|-\[Infinity]|\[Infinity])}
] :=
	ConstantArray[0, Length[wavefuncVector]]
	
NormalizeWavefunction[
	wavefuncVector_/;VectorQ[wavefuncVector, NumericQ],   
	grid_/;VectorQ[grid, realNumQ]
] /; (
	Length[grid] === Length[wavefuncVector]
) :=
    NormalizeWavefunction[wavefuncVector, grid[[2]] - grid[[1]]]
	
NormalizeWavefunction[
	wavefuncVector_/;VectorQ[wavefuncVector, NumericQ]
] :=
	NormalizeWavefunction[wavefuncVector, 1]
	
(* functions *)
NormalizeWavefunction[                         
	wavefuncFunc:_?func1DQ,                          (* converts InterpolatingFunction to Function *)
	domain:{___Symbol, xL:(_?realNumQ|-\[Infinity]), xR:(_?realNumQ|\[Infinity])}
] :=
	With[
		{norm = NIntegrate[
			Abs[wavefuncFunc[x]]^2, {x, xL, xR}, 
			Method -> {Automatic, "SymbolicProcessing" -> 0}
		]},
		Sqrt[norm] // Function[{n}, (wavefuncFunc[#]/n&)]
	]

NormalizeWavefunction[
	wavefuncFunc:_?func1DQ,                          (* converts InterpolatingFunction to Function *)                      
	grid:{xL_?realNumQ, ___?realNumQ, xR_?realNumQ}
] :=
	With[
		{norm = (grid[[2]] - grid[[1]]) Total[Abs[ wavefuncFunc /@ grid ]^2]},
		Sqrt[norm] // Function[{n}, (wavefuncFunc[#]/n&)]
	]
	
NormalizeWavefunction[
	wavefuncFunc:_?func1DQ
] :=
	NormalizeWavefunction[wavefuncFunc, {-\[Infinity], \[Infinity]}]
	
(* symbolic expression *)
NormalizeWavefunction[
	wavefuncSymbolic_?symbolicQ,
	domain:{_Symbol, (_?realNumQ|-\[Infinity]), (_?realNumQ|\[Infinity])}
] :=
	With[
		(*
		{norm = NIntegrate[
			wavefuncSymbolic, domain, 
			Method -> {Automatic, "SymbolicProcessing" -> 0}
		]},
		*)
		{norm = Integrate[Abs[wavefuncSymbolic]^2, domain]},          (* DANGEROUS SYMBOLIC INTERGRATION?!?!! *)
		wavefuncSymbolic / Sqrt[norm]
	]
	
NormalizeWavefunction[
	wavefuncSymbolic:_?symbolicQ,
	x_Symbol
] :=
	NormalizeWavefunction[wavefuncSymbolic, {x, -\[Infinity], \[Infinity]}]

NormalizeWavefunction[
	wavefuncSymbolic:_?symbolicQ,
	{x_Symbol}
] :=
	NormalizeWavefunction[wavefuncSymbolic, x]	
	
	
	
(* 
	2D
 *)
 
(* constants *)
NormalizeWavefunction[
	wavefuncConst_?NumericQ,
	{___Symbol, xL:_?realNumQ, xR_?realNumQ},
	{___Symbol, yL:_?realNumQ, yR_?realNumQ}
] :=
	wavefuncConst / (Abs[wavefuncConst] Sqrt[xR - xL] Sqrt[yR - yL])
	
NormalizeWavefunction[
	_?NumericQ,
	{___Symbol, (_?realNumQ|-\[Infinity]|\[Infinity]), (_?realNumQ|-\[Infinity]|\[Infinity])},
	{___Symbol, (_?realNumQ|-\[Infinity]|\[Infinity]), (_?realNumQ|-\[Infinity]|\[Infinity])}
] :=
	0

(* matrix *)
NormalizeWavefunction[
	wavefuncMatrix_/;MatrixQ[wavefuncMatrix, NumericQ],           (* assumed 0 outside grid *) 
	xGridSpace_/;(realNumQ[xGridSpace] && Positive[xGridSpace]),
	yGridSpace_/;(realNumQ[yGridSpace] && Positive[yGridSpace])
] :=
	wavefuncMatrix / Sqrt[xGridSpace yGridSpace Total[Abs[wavefuncMatrix]^2, 2]]

NormalizeWavefunction[
	wavefuncMatrix_/;MatrixQ[wavefuncMatrix, NumericQ],           (* assumed 0 outside grid *)
	gridSpace_/;(realNumQ[gridSpace] && Positive[gridSpace])
] := 
	wavefuncMatrix / Sqrt[gridSpace^2 Total[Abs[wavefuncMatrix]^2, 2]]	
    
NormalizeWavefunction[
	wavefuncMatrix_/;MatrixQ[wavefuncMatrix, NumericQ],   (* assumed 0 outside grid *)
	xDomain:{___Symbol, xL_?realNumQ, xR_?realNumQ},
	yDomain:{___Symbol, yL_?realNumQ, yR_?realNumQ}
] :=
	NormalizeWavefunction[
		wavefuncMatrix, 
		(xR - xL)/(Dimensions[wavefuncMatrix][[2]] - 1),
		(yR - yL)/(Dimensions[wavefuncMatrix][[1]] - 1)
	]
	
NormalizeWavefunction[
	wavefuncMatrix_/;MatrixQ[wavefuncMatrix, NumericQ],   (* assumed 0 outside grid *)
	endpoints:{xyL_?realNumQ, xyR_?realNumQ}
] :=
	NormalizeWavefunction[
		wavefuncMatrix, 
		(xyR - xyL)/(Dimensions[wavefuncMatrix][[2]] - 1),
		(xyR - xyL)/(Dimensions[wavefuncMatrix][[1]] - 1)
	]
	
NormalizeWavefunction[
	wavefuncMatrix_/;MatrixQ[wavefuncMatrix, NumericQ],    
	{___Symbol, (_?realNumQ|-\[Infinity]|\[Infinity]), (_?realNumQ|-\[Infinity]|\[Infinity])},
	{___Symbol, (_?realNumQ|-\[Infinity]|\[Infinity]), (_?realNumQ|-\[Infinity]|\[Infinity])}
] :=
	ConstantArray[0, Dimensions[wavefuncMatrix]]
	
NormalizeWavefunction[
	wavefuncMatrix_/;MatrixQ[wavefuncMatrix, NumericQ],    
	{___Symbol, (_?realNumQ|-\[Infinity]|\[Infinity]), (_?realNumQ|-\[Infinity]|\[Infinity])}
] :=
	ConstantArray[0, Dimensions[wavefuncMatrix]]
	
NormalizeWavefunction[
	wavefuncMatrix_/;MatrixQ[wavefuncMatrix, NumericQ],  
	xGrid_/;VectorQ[xGrid, realNumQ],
	yGrid_/;VectorQ[yGrid, realNumQ]
] /; (
	Length[xGrid] === Dimensions[wavefuncMatrix][[2]] &&
	Length[yGrid] === Dimensions[wavefuncMatrix][[1]]
) :=
    NormalizeWavefunction[wavefuncMatrix, xGrid[[2]] - xGrid[[1]], yGrid[[2]] - yGrid[[1]]]
    
NormalizeWavefunction[
	wavefuncMatrix_/;MatrixQ[wavefuncMatrix, NumericQ], 
	grid_/;VectorQ[grid, realNumQ]
] /; (
	Length[grid] === Dimensions[wavefuncMatrix][[1]] === Dimensions[wavefuncMatrix][[2]]
) :=
    NormalizeWavefunction[wavefuncMatrix, grid[[2]] - grid[[1]]]
	
NormalizeWavefunction[
	wavefuncMatrix_/;MatrixQ[wavefuncMatrix, NumericQ] 
] :=
	NormalizeWavefunction[wavefuncMatrix, 1]
	
(* functions *)
NormalizeWavefunction[                         
	wavefuncFunc:_?func2DQ,                          (* converts InterpolatingFunction to Function *)
	xDomain:{___Symbol, xL:(_?realNumQ|-\[Infinity]), xR:(_?realNumQ|\[Infinity])},
	yDomain:{___Symbol, yL:(_?realNumQ|-\[Infinity]), yR:(_?realNumQ|\[Infinity])}
] :=
	With[
		{norm = NIntegrate[
			Abs[wavefuncFunc[x, y]]^2, 
			{x, xL, xR}, 
			{y, yL, yR},
			Method -> {Automatic, "SymbolicProcessing" -> 0}
		]},
		Sqrt[norm] // Function[{n}, (wavefuncFunc[#1, #2]/n &)]
	]
	
NormalizeWavefunction[                         
	wavefuncFunc_?func2DQ,                          (* converts InterpolatingFunction to Function *)
	domain:{___Symbol, (_?realNumQ|-\[Infinity]), (_?realNumQ|\[Infinity])}
] :=
	NormalizeWavefunction[
		wavefuncFunc,
		domain,
		domain
	]

NormalizeWavefunction[
	wavefuncFunc:_?func2DQ,                          (* converts InterpolatingFunction to Function *)                      
	xGrid:{xL_?realNumQ, ___?realNumQ, xR_?realNumQ},
	yGrid:{yL_?realNumQ, ___?realNumQ, yR_?realNumQ}   
] :=
	Module[
		{xSpacing, ySpacing, samples, norm},
		xSpacing = xGrid[[2]] - xGrid[[1]];
		ySpacing = yGrid[[2]] - yGrid[[1]];
		samples = (wavefuncFunc @@ # &) /@ Flatten[Outer[List, xGrid, yGrid], 1];
		norm = N @ xSpacing ySpacing Total[Abs[samples]^2] ;
		Sqrt[norm] // Function[{n}, (wavefuncFunc[#1, #2]/n &)]
	]
	
NormalizeWavefunction[
	wavefuncFunc:_?func2DQ,                          (* converts InterpolatingFunction to Function *)                      
	grid:{xyL_?realNumQ, ___?realNumQ, xyR_?realNumQ}
] :=
	NormalizeWavefunction[
		wavefuncFunc,
		grid,
		grid
	]
	
NormalizeWavefunction[
	wavefuncFunc:_?func2DQ
] :=
	NormalizeWavefunction[wavefuncFunc, {-\[Infinity], \[Infinity]}, {-\[Infinity], \[Infinity]}]
	
(* symbolic expression *)
NormalizeWavefunction[
	wavefuncSymbolic_?symbolicQ,
	xDomain:{_Symbol, (_?realNumQ|-\[Infinity]), (_?realNumQ|\[Infinity])},
	yDomain:{_Symbol, (_?realNumQ|-\[Infinity]), (_?realNumQ|\[Infinity])}
] :=
	With[
		(*
		{norm = NIntegrate[
			wavefuncSymbolic, domain, 
			Method -> {Automatic, "SymbolicProcessing" -> 0}
		]},
		*)
		{norm = Integrate[Abs[wavefuncSymbolic]^2, xDomain, yDomain]},          (* DANGEROUS SYMBOLIC INTERGRATION?!?!! *)
		wavefuncSymbolic / Sqrt[norm]
	]
	
NormalizeWavefunction[
	wavefuncSymbolic:_?symbolicQ,
	x_Symbol,
	y_Symbol
] :=
	NormalizeWavefunction[wavefuncSymbolic, {x, -\[Infinity], \[Infinity]}, {y, -\[Infinity], \[Infinity]}]

NormalizeWavefunction[
	wavefuncSymbolic:_?symbolicQ,
	{x_Symbol},
	{y_Symbol}
] :=
	NormalizeWavefunction[wavefuncSymbolic, x, y]	

	
	


(*
	Plots a given family of eigenvalues and eigenfunctions (in vector or function form), 
	otherwise calculating then plotting the family for a given potential
*)

(* 
	1D
*)

(* passing a domain and a pre-computed eigensystem *)
PlotSpectrum[
	domain_?domainOptSymbQ,
	eigvals_List,            (* eigvals don't actually need to be numbers! *)
	eigfuncs_List,           (* eigfuncs can have mixed type *)
	options:OptionsPattern[{plotOptionFunctions1D, Manipulate} // Flatten]
] /; (
	VectorQ[eigfuncs, (isValid1DInput[#, domain]&)] &&
	Length[eigvals] === Length[eigfuncs] &&
	isValid1DPotentialOptions[options, domain]
) := (
	(* fires a single error when invalid options are passed *)
	OptionValue[{}];

	(* calls PlotWavefunction on the nth eigenfunction *)
	Manipulate[
		PlotWavefunction[
			eigfuncs[[n+1]],
			domain,
			
			(* apply over-riding user plot-options *)
			Evaluate[Sequence @@ FilterRules[{options}, Options /@ plotOptionFunctions1D]],
			
			(* otherwise apply this default style *)
			AxesLabel -> {
				(* use domain symbol or default string for xlabel *)
				If[MatchQ[domain[[1]], _Symbol], domain[[1]], defaultXLabel], 
				StringForm[eigenFuncProbLabel, n]
			},
			LegendLabel -> StringForm[eigenFuncPhaseLabel, n],
			PlotRange -> All,
			PlotLabel -> StringForm[eigenValueLabel, n, eigvals[[n+1]]]
		],
		{{n, 0, defaultModeLabel}, 0, Length[eigfuncs]-1, 1},
		
		(* apply over-riding user Manipulate options *)
		Evaluate[Sequence @@ FilterRules[{options}, Options[Manipulate]]],
		
		(* otherwise apply this default style *)
		Paneled -> False
	]
)


interpolate1DEigenfunction[
	eigfunc_/;VectorQ[eigfunc, NumericQ],
	grid_/;VectorQ[grid, NumericQ]
] /; (
	Length[eigfunc] === Length[grid]
) :=
	Interpolation[
		Partition[Riffle[grid, eigfunc], 2]
	]
	
interpolate1DEigenfunction[
	eigfunc_, domainOrGrid_
] :=
	eigfunc


(* passing a grid and a pre-computed eigensystem *)
PlotSpectrum[
	{symb:Repeated[_Symbol, {0, 1}], points:__?realNumQ},
	eigvals_List,   
	eigfuncs_List,
	options:OptionsPattern[{plotOptionFunctions1D, Manipulate} // Flatten]
] /; (
	Length[{points}] > 1 &&
	VectorQ[eigfuncs, (isValid1DInput[#, {symb, 0, 0}]&)] &&   (* numerical vals don't matter *)
	Length[eigvals] === Length[eigfuncs] &&
	isValid1DPotentialOptions[options, {symb, 0, 0}]
) :=
	(* inner call will handle invalid option errors *)
	PlotSpectrum[
		{symb, Min @ {points}, Max @ {points}}, 
		eigvals, 
		(interpolate1DEigenfunction[#, {points}]&) /@ eigfuncs, 
		options
	]    

(* passing a grid and a pre-computed eigensystem in a vector; e.g. output of EigenSystem *)
PlotSpectrum[
	{
		domainOrGrid:{x:Repeated[_Symbol, {0, 1}], points:__?realNumQ},
		eigvals_List,
		eigfuncs_List
	},
	options:OptionsPattern[{plotOptionFunctions1D, Manipulate} // Flatten]
] /; (
	Length[{points}] > 1 &&
	VectorQ[eigfuncs, (isValid1DInput[#, {x, 0, 0}]&)] &&  (* numerical vals don't matter *)
	Length[eigvals] === Length[eigfuncs] &&
	isValid1DPotentialOptions[options, {x, 0, 0}]
) :=
	PlotSpectrum[domainOrGrid, eigvals, eigfuncs, options]    (* inner call will handle invalid option errors *)
	
(* passing a potential, to have the eigensystem computed then plotted *)
PlotSpectrum[
	potential:_,
	domain:_?domainOptSymbQ,
	options:OptionsPattern[{plotOptionFunctions1D, GetSpectrum, Manipulate} // Flatten]
] /; (
	isValid1DInput[potential, domain] &&
	isValid1DPotentialOptions[options, domain]
) :=
	Module[
		{eigsys},    (* avoids garbage collection *)
			
		(* fires a single error when invalid options are passed *)
		OptionValue[{}];
			
		(* compute the eigenmodes *)
		eigsys = GetSpectrum[
			potential, domain, 
			Evaluate @ FilterRules[{options}, Options[GetSpectrum]]
		];
		
		(* if domain contained a symbol, inject it back into eigsys' grid (for x labeling) *)
		If[
			MatchQ[domain[[1]], _Symbol],
			PrependTo[eigsys[[1]], domain[[1]]]
		];
			
		(* plot vectorised system *)
		PlotSpectrum[
			eigsys,
			
			(* if the Potential option was passed, strip it of variables *)
			Sequence @@ If[
				Quiet @ OptionValue[Potential] === None, 
				{}, 
				{Potential -> convertToFunction[Quiet @ OptionValue[Potential], domain]}
			],
			
			(* apply overriding user-given plot options *)
			Evaluate @ extractOptions[options, {Manipulate, plotOptionFunctions1D}],
			
			(* plot the potential used for numerics, if no other Potential option was passed *)
			Potential -> convertToFunction[potential, domain]
		]
	]
	
	
	
	
(* 
	2D
*)

(* passing a domain and a pre-computed eigensystem *)
PlotSpectrum[
	xDomain_?domainOptSymbQ,
	yDomain_?domainOptSymbQ,
	eigvals_List,            (* eigvals don't actually need to be numbers! *)
	eigfuncs_List,
	options:OptionsPattern[{plotOptionFunctions2D, Manipulate} // Flatten]
] /; (
	VectorQ[eigfuncs, (isValid2DInput[#, xDomain, yDomain]&)] &&
	Length[eigvals] === Length[eigfuncs] &&
	isValid2DPotentialOptions[options, xDomain, yDomain]
) := (
	(* fires a single error when invalid options are passed *)
	OptionValue[{}];

	(* calls PlotWavefunction on the nth eigenfunction *)
	Manipulate[
		PlotWavefunction[
			eigfuncs[[n+1]],
			xDomain,
			yDomain,
			
			(* apply over-riding user plot-options *)
			Evaluate[Sequence @@ FilterRules[{options}, Options /@ plotOptionFunctions2D]],
			
			(* otherwise apply this default style *)
			AxesLabel -> {
				(* use domain symbol or default string for xlabel *)
				If[MatchQ[xDomain[[1]], _Symbol], xDomain[[1]], defaultXLabel], 
				If[MatchQ[yDomain[[1]], _Symbol], yDomain[[1]], defaultYLabel], 
				StringForm[eigenFuncProbLabel, n]
			},
			LegendLabel -> StringForm[eigenFuncPhaseLabel, n],
			PlotRange -> All,
			PlotLabel -> StringForm[eigenValueLabel, n, eigvals[[n+1]]]
		],
		{{n, 0, defaultModeLabel}, 0, Length[eigfuncs]-1, 1},
		
		(* apply over-riding user Manipulate options *)
		Evaluate[Sequence @@ FilterRules[{options}, Options[Manipulate]]],
		
		(* otherwise apply this default style *)
		Paneled -> False
	]
)

interpolate2DEigenfunction[
	eigfunc_/;MatrixQ[eigfunc, NumericQ],
	grid_/;TensorQ[grid, realNumQ]
] /; (
	Dimensions[eigfunc] === Dimensions[grid][[;;2]]
) :=
	Interpolation[
		Partition[
			Riffle[
				Flatten[grid, {2, 1}], 
				Flatten[eigfunc, {2, 1}]
			], 2
		]
	]
	
interpolate2DEigenfunction[
	eigfunc_, domainOrGrid_
] :=
	eigfunc






(* passing a grid and a pre-computed eigensystem *)
PlotSpectrum[
	grid_/;TensorQ[grid, realNumQ],
	eigvals_List,   
	eigfuncs_List,
	options:OptionsPattern[{plotOptionFunctions2D, Manipulate} // Flatten]
] /; (
	VectorQ[eigfuncs, (isValid2DInput[#, {0, 0}, {0, 0}]&)] &&   (* numerical vals don't matter *)
	Length[eigvals] === Length[eigfuncs] &&
	isValid2DPotentialOptions[options, {0, 0}, {0, 0}]
) := 
	With[
		{gridDomains = Sequence @@ {
			{
				Min @ Flatten[grid][[1;; ;;2]],
				Max @ Flatten[grid][[1;; ;;2]]
			}, 
			{
				Min @ Flatten[grid][[2;; ;;2]],
				Max @ Flatten[grid][[2;; ;;2]]
			}}
		},
		PlotSpectrum[
			monkeyhackConstrainDomains[gridDomains], 
			eigvals, 
			(interpolate2DEigenfunction[#, grid]&) /@ eigfuncs, 
			options
		]  
	]  

(* passing a grid and a pre-computed eigensystem in a vector; e.g. output of EigenSystem *)
PlotSpectrum[
	{
		grid_/;TensorQ[grid, realNumQ],
		eigvals_List,   
		eigfuncs_List
	},
	options:OptionsPattern[{plotOptionFunctions2D, Manipulate} // Flatten]
] /; (
	VectorQ[eigfuncs, (isValid2DInput[#, {0, 0}, {0, 0}]&)] &&   (* numerical vals don't matter *)
	Length[eigvals] === Length[eigfuncs] &&
	isValid2DPotentialOptions[options, {0, 0}, {0, 0}]
) := 
	PlotSpectrum[grid, eigvals, eigfuncs, options]



(* passing a potential, to have the eigensystem computed then plotted *)
PlotSpectrum[
	potential:_,
	xDomain:_?domainOptSymbQ,
	yDomain:_?domainOptSymbQ,
	options:OptionsPattern[{plotOptionFunctions2D, GetSpectrum, Manipulate} // Flatten]
] /; (
	isValid2DInput[potential, xDomain, yDomain] &&
	isValid2DPotentialOptions[options, xDomain, yDomain]
) :=
	Module[
		{eigvals, eigfuncs, monkeyDomains},    (* avoids garbage collection *)
			
		(* fires a single error when invalid options are passed *)
		OptionValue[{}];
		
		monkeyDomains = monkeyhackConstrainDomains[xDomain, yDomain];
			
		(* compute the eigenmodes *)
		{eigvals, eigfuncs} = (GetSpectrum[
			potential, monkeyDomains,
			Evaluate @ FilterRules[{options}, Options[GetSpectrum]]
		][[2;;]]);
			
		(* plot vectorised system *)
		PlotSpectrum[
			monkeyDomains,
			eigvals, eigfuncs,
			
			(* if the Potential option was passed, strip it of variables *)
			Sequence @@ If[
				Quiet @ OptionValue[Potential] === None, 
				{}, 
				{Potential -> convertToFunction[Quiet @ OptionValue[Potential], xDomain, yDomain]}
			],
			
			(* apply overriding user-given plot options *)
			Evaluate @ extractOptions[options, {Manipulate, plotOptionFunctions2D}],
			
			(* plot the potential used for numerics, if no other Potential option was passed *)
			Potential -> convertToFunction[potential, xDomain, yDomain]
		]
	]
