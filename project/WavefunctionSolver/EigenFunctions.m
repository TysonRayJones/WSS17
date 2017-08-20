(* ::Package:: *)

Package["WavefunctionSolver`"]

(*
	TODO:
	- user specify (as options) number of points / grid-spacing in each dimension in 2D
	- fix awful private imports
	- non-abs-squared plots for real station states?
	- 2D stationary state finding (GetEigenmodes)
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
	- warnings for too few NumberOfPoints
	- error on negative NumberOfPoints or NumberOfModes
	- make numerical methods (e.g. matrix generation) public? add a discretise method which can also discretise a wavefunction?
	- make public functions for converting types (e.g. list to function, symbolic to list, etc)?
	  
	NOTES:
	- hmmm should OutputAsFunctions be renamed to InterpolateResults?
	- PlotSpectrum would be faster if cbar was external to Manipulate, but that's a pain
*)





(* SYMBOL EXPORTS *)

PackageExport[NumberOfModes]
NumberOfModes::usage = "Specifies how many modes GetEigenmodes[] should return (must be smaller than NumberOfPoints)";

PackageExport[NumberOfPoints]
NumberOfPoints::usage = "Specifies how many points on which GetEigenmodes should discretise the hamiltonian. An odd number larger than one-thousand is reccommended";

PackageExport[AutoNormalize]
AutoNormalize::usage = "Specifies whether to L2-normalize the eigenvectors returned by GetEigenmodes";

PackageExport[OutputAsFunctions]
OutputAsFunctions::usage = "Specifies whether to convert the eigenfunction vectors from GetEigenmodes to InterpolatingFunctions"





(* FUNCTION EXPORTS *)

PackageExport[GetEigenmodes]
PackageExport[PlotSpectrum]
PackageExport[NormalizeWavefunction]





(* FUNCTION USAGE MESSAGES *)

GetEigenmodes::usage = "GetEigenmodes[potential, domain] returns the family of {eigenvalues, eigenfunctions} of the hamiltonian associated with the given potential, superposed with an infinite potential well";

PlotSpectrum::usage = "PlotSpectrum[potential, domain] calculates and plots the single-particle energy-eigenfunctions of the given potential";
PlotSpectrum::usage = "PlotSpectrum[domain/grid, eigvals, eigfuncs] plots the given eigensystem";
PlotSpectrum::usage = "PlotSpectrum[{domain/grid, eigvals, eigfuncs}] plots the given eigensystem";

NormalizeWavefunction::usage = "NormalizeWavefunction[const, domain] L2 algebraically normalises a constant wavefunction assumed zero outside of domain, returning a constant";
NormalizeWavefunction::usage = "NormalizeWavefunction[const] returns const, as if the wavefunction vanishes from const outside [0, 1]";
NormalizeWavefunction::usage = "NormalizeWavefunction[vector, gridspace] L2 normalises (by Riemann integration) a vectorised wavefunction, with values separated by distance gridspace, returning a vector";
NormalizeWavefunction::usage = "NormalizeWavefunction[vector, grid] L2 normalises (by Riemann integration) a vectorised wavefunction defined on a given grid vector, returning a vector";
NormalizeWavefunction::usage = "NormalizeWavefunction[vector, domain] L2 normalises (by Riemann integration) a vectorised wavefunction defined across the given domain, returning a vector";
NormalizeWavefunction::usage = "NormalizeWavefunction[vector] L2 normalises (by Riemann integration) assuming a grid-spacing of 1, and that the wavefunction vanishes outside [0, Length[vector]]";
NormalizeWavefunction::usage = "NormalizeWavefunction[function, domain] numerically L2 normalises a Function/Interpolating function across the given domain, returning a function";
NormalizeWavefunction::Usage = "NormalizeWavefunction[function, grid] L2 normalises a Function/Interpolating function by first sampling it on the given grid, then Riemann-integrates. Returns a function";
NormalizeWavefunction::usage = "NormalizeWavefunction[function] numerically L2 normalises the function on [-\[Infinity], \[Infinity]]";
NormalizeWavefunction::usage = "NormalizeWavefunction[symb, {x, xL, xR}] symbolically L2 normalises a symbolic expression on x \[Element] [xL, xR] (assuming the wavefunction vanishes outside)";
NormalizeWavefunction::usage = "NormalizeWavefunction[symb, x] symbolically L2 normalises a symbolic expression on x \[Element] (-\[Infinity], \[Infinity])";
NormalizeWavefunction::usage = "NormalizeWavefunction[symb, {x}] symbolically L2 normalises a symbolic expression on x \[Element] (-\[Infinity], \[Infinity])";





(* FUNCTION ERROR MESSAGES *)

GetEigenmodes::numPointsLessThanTwoError = "NumberOfPoints given to GetEigenmodes (`1`) too small; must be at least 2. Using NumberOfPoints -> `2` instead.";
GetEigenmodes::numPointsLessThanNumModesError = "NumberOfPoints given to GetEigenmodes (`1`) is less than NumberOfModes (`2`). Using NumberOfPoints -> `3` instead.";





(* SYMBOL IMPORTS *)

realNumQ = WavefunctionSolver`PlotFunctions`PackagePrivate`realNumQ;
domainQ = WavefunctionSolver`PlotFunctions`PackagePrivate`domainQ;
domainSymbQ = WavefunctionSolver`PlotFunctions`PackagePrivate`domainSymbQ;
domainOptSymbQ = WavefunctionSolver`PlotFunctions`PackagePrivate`domainOptSymbQ;

isValid1DInput = WavefunctionSolver`PlotFunctions`PackagePrivate`isValid1DInput;
isValid1DPotentialOptions = WavefunctionSolver`PlotFunctions`PackagePrivate`isValid1DPotentialOptions
is1DFunction = WavefunctionSolver`PlotFunctions`PackagePrivate`is1DFunction;
is2DFunction = WavefunctionSolver`PlotFunctions`PackagePrivate`is2DFunction;

convertToFunction = WavefunctionSolver`PlotFunctions`PackagePrivate`convertToFunction;

plotOptionFunctions1D = WavefunctionSolver`PlotFunctions`PackagePrivate`optionFunctions1D;

extractOptions = WavefunctionSolver`PlotFunctions`PackagePrivate`extractOptions;
addDefaultOptions = WavefunctionSolver`PlotFunctions`PackagePrivate`addDefaultOptions;
extractOptionsFromDefaults = WavefunctionSolver`PlotFunctions`PackagePrivate`extractOptionsFromDefaults;





(* CONSTANTS *)

defaultNumPoints = 10^4 + 1;    (* odd for pleasant grid spacing *)





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
	numPoints = Automatic will use the length of list inputs, and defaultNumPoints for other inputs
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
	discretise1DPotential[potential, domain, defaultNumPoints]

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
	discretise1DPotential[potential, domain, defaultNumPoints]
	
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
	discretise1DPotential[potential, domain, defaultNumPoints]
	
	
	
(* 
	functions for building matrix operators
*)

get1DLaplacianMatrix[
	grid_/;VectorQ[grid, realNumQ]
] :=
	With[
		{numPoints = Length[grid],
		 gridSpace = grid[[2]] - grid[[1]]},
		SparseArray[
			{   (* 4th order accuracy of 1D finite difference 2nd-deriv *)
				{i_,i_} :> -5/2,
				{i_,j_} /; Abs[i-j] == 1 :> 4/3,
				{i_,j_} /; Abs[i-j] == 2 :> -1/12 
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
	


(*
	functions for building strings in wavefunction plots
*)
getEigvalString[n_Integer, eigvals_List] :=
	"\!\(\*SubscriptBox[\(E\), \(" <> ToString[n] <> "\)]\) = " <> ToString[eigvals[[n+1]]]
getEigfuncProbString[n_Integer] :=
	"|\!\(\*SubscriptBox[\(\[Phi]\), \(" <> ToString[n] <> "\)]\)\!\(\*SuperscriptBox[\(|\), \(2\)]\)"
getEigfuncPhaseString[n_Integer] :=
	"phase(\!\(\*SubscriptBox[\(\[Phi]\), \(" <> ToString[n] <> "\)]\))"
	
	
	


(* PUBLIC FUNCTION DEFINITIONS *)

Options[GetEigenmodes] = {
	NumberOfModes -> 10,
	NumberOfPoints -> Automatic,
	AutoNormalize -> True,
	OutputAsFunctions -> False
};

(*
	Calculates {grid, {energy eigenvalues}, {energy eigenfunctions}} associated with a given potential
*)
GetEigenmodes[
	potential:_,
	domain:_?domainOptSymbQ,
	options:OptionsPattern[GetEigenmodes]
] /; (
	isValid1DInput[potential, domain]
) :=
	Module[
		{  (* local vars *)
			grid, hamiltonian, eigvals, eigfuncs, 
			
			(* extracted options *)
			numPoints, numModes, autoNorm, outputAsFuncs
		},
		
		(* extract once to fire a single error when an invalid option is passed *)
		{numPoints, numModes, autoNorm, outputAsFuncs} = OptionValue[
			{NumberOfPoints, NumberOfModes, AutoNormalize, OutputAsFunctions}
		];
		
		(* correct for numPoints being too small *)
		numPoints = If[
			NumberQ[numPoints] && numPoints < 2,
			( 
				Message[
					GetEigenmodes::numPointsLessThanTwoError, 
					numPoints, 
					defaultNumPoints
				];
				defaultNumPoints
			),
			numPoints
		];		
		
		(* correct for numPoints being less than numModes *)
		numPoints = If[
			NumberQ[numPoints] && numPoints < Abs[numModes],
			( 
				Message[
					GetEigenmodes::numPointsLessThanNumModesError, 
					numPoints, 
					Abs[numModes], 
					Max[defaultNumPoints, numModes]
				];
				Max[defaultNumPoints, numModes]
			),
			numPoints
		];	
		
		(* get grid vector and hamiltonian matrix *)
		{grid, hamiltonian} = N @ get1DGridAndHamiltonian[potential, domain, numPoints];
		
		(* diagonalise hamiltonian *)
		{eigvals, eigfuncs} = Eigensystem[hamiltonian, -numModes];
		
		(* sort eigenfunctions by increasing eigenvalue *)
		{eigvals, eigfuncs} = {eigvals[[#]], eigfuncs[[#]]}& @ Ordering[eigvals];
		
		(* optionally L2-normalise eigenfunctions *)
		{eigvals, eigfuncs} = If[
			autoNorm,
			{eigvals, NormalizeWavefunction[#, grid]& /@ eigfuncs},
			{eigvals, eigfuncs}
		];
		
		(* optionally interpolate eigenfunctions *)
		{eigvals, eigfuncs} = If[
			outputAsFuncs,
			{eigvals, convertToFunction[#, domain]& /@ eigfuncs},
			{eigvals, eigfuncs}
		];
		
		(* return *)
		{grid, eigvals, eigfuncs}
	]
	
	
(* 
	L2 normalizes a given wavefunction, preserving its input form (besdies InterpolatingFunction \[Rule] Function).
	If no explicit domain is provided, symbolic/function integrations will assume {-\[Infinity], \[Infinity]} while
	constant/vector integrations will assume a grid-length/grid-spacing of 1
*)

(*
	1D
*)

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
	wavefuncVector_/;VectorQ[wavefuncVector, NumericQ],   
	grid_/;VectorQ[grid, realNumQ]
] /; (
	Length[grid] === Length[wavefuncVector]
) :=
    NormalizeWavefunction[wavefuncVector, grid[[2]] - grid[[1]]]

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
		(wavefuncFunc[#]/Sqrt[norm] &)
	]

NormalizeWavefunction[
	wavefuncFunc:_?func1DQ,                          (* converts InterpolatingFunction to Function *)                      
	grid:{xL_?realNumQ, ___?realNumQ, xR_?realNumQ}
] :=
	With[
		{norm = (grid[[2]] - grid[[1]]) Total[Abs[ wavefuncFunc /@ grid ]^2]},
		(wavefuncFunc[#]/Sqrt[norm] &)
	]
	
NormalizeWavefunction[
	wavefuncFunc:_?func1DQ
] :=
	NormalizeWavefunction[wavefuncFunc, {-\[Infinity], \[Infinity]}]
	
(* symbolic expression *)
NormalizeWavefunction[
	wavefuncSymbolic_,
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
	wavefuncSymbolic_,
	x_Symbol
] :=
	NormalizeWavefunction[wavefuncSymbolic, {x, -\[Infinity], \[Infinity]}]

NormalizeWavefunction[
	wavefuncSymbolic_,
	{x_Symbol}
] :=
	NormalizeWavefunction[wavefuncSymbolic, x]	
	
	
	
(* 
	2D
 *)



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
	eigfuncs_List,
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
			AxesLabel -> {"x", getEigfuncProbString[n]},
			LegendLabel -> getEigfuncPhaseString[n],
			PlotRange -> All,
			PlotLabel -> getEigvalString[n, eigvals]
		],
		{{n, 0, "mode"}, 0, Length[eigfuncs]-1, 1},
		
		(* apply over-riding user Manipulate options *)
		Evaluate[Sequence @@ FilterRules[{options}, Options[Manipulate]]],
		
		(* otherwise apply this default style *)
		Paneled -> False
	]
)

(* passing a grid and a pre-computed eigensystem *)
PlotSpectrum[
	grid:{xL_?realNumQ, ___?realNumQ, xR_?realNumQ},
	eigvals_List,   
	eigfuncs_List,
	options:OptionsPattern[{plotOptionFunctions1D, Manipulate} // Flatten]
] /; (
	VectorQ[eigfuncs, (isValid1DInput[#, {xL, xR}]&)] &&
	Length[eigvals] === Length[eigfuncs] &&
	isValid1DPotentialOptions[options, {xL, xR}]
) :=
	PlotSpectrum[{xL, xR}, eigvals, eigfuncs, options]    (* will handle invalid option errors *)

(* passing a grid and a pre-computed eigensystem in a vector; e..g output of EigenSystem *)
PlotSpectrum[
	{
		domainOrGrid:{___Symbol, xL_?realNumQ, ___?realNumQ, xR_?realNumQ},
		eigvals_List,
		eigfuncs_List
	},
	options:OptionsPattern[{plotOptionFunctions1D, Manipulate} // Flatten]
] /; (
	VectorQ[eigfuncs, (isValid1DInput[#, {xL, xR}]&)] &&
	Length[eigvals] === Length[eigfuncs] &&
	isValid1DPotentialOptions[options, {xL, xR}]
) :=
	PlotSpectrum[domainOrGrid, eigvals, eigfuncs, options]    (* will handle invalid option errors *)
	
(* passing a potential, to have the eigensystem computed then plotted *)
PlotSpectrum[
	potential:_,
	domain:_?domainOptSymbQ,
	options:OptionsPattern[{plotOptionFunctions1D, GetEigenmodes, Manipulate} // Flatten]
] /; (
	isValid1DInput[potential, domain] &&
	isValid1DPotentialOptions[options, domain]
) :=
	DynamicModule[
		{eigsys},    (* avoids garbage collection *)
			
		(* fires a single error when invalid options are passed *)
		OptionValue[{}];
			
		(* compute the eigenmodes *)
		eigsys = 
			GetEigenmodes[
				potential, domain, 
				Evaluate @ FilterRules[{options}, Options[GetEigenmodes]]
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


