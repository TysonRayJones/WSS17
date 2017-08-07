(* ::Package:: *)

Package["WavefunctionSolver`"]

(*
	TODO:
	- add function for adding a colorbar to any plot (e.g. to wrap dynamic plots)
	- automatic plotrange only considers probability density;
	  can we make it the union of the individual automatic plotranges of probability density and potential?
	  this would require (perhaps expensively) replotting both after finding the automatic ranges
	- "Ticks" and "TickLabels" aren't captured by BarLegend options, so can't be passed to PlotWavefunction. How can we fix this?
	- give PlotWavefunction a PotentialPlotOptions \[Rule] {options to customise potential plot};
	  it may be tricky to validate passed PotentialPlotOptions (should we match only valid, or error on invalid?)
	- non-square domain in Plot3D causes non-uniform axis scaling; should we fix by default?
	- should this paclet have a function for time-caching/animating any wavefunction (w. time) function?
*)

(* 
	NOTES:
	- a 1D potential transformation function disallows scaling higher dimensional potentials in certain directions
	- plotting the colorbar (by ShowBar \[Rule] True) makes animations very laggy; avoid in dynamic plots
*)





(* SYMBOL EXPORTS *)

PackageExport[Potential]
Potential::usage = "Specify the external potential as a function/list/number/symbolic expression";

PackageExport[PotentialTransform]
PotentialTransform::usage = "Specify a function by which to transform the external potential when plotting";

PackageExport[ShowBar]
ShowBar::usage = "Whether to display a phase-colorbar in the plot. Dynamic plots should set to False";

PackageExport[ColorScheme]
ColorScheme::usage = "The name of the color scheme to use for the phase in wavefunction plots";





(* FUNCTION EXPORTS *)

PackageExport[PlotWavefunction]
PlotWavefunction::usage = "PlotWavefunction[wavef, {xL, xR}] plots a 1D functional/list/numerical wavefunction between xL and xR";
PlotWavefunction::usage = "PlotWavefunction[wavef, {xL, xR}, {yL, yR}] plots a 2D functional/matrix/numerical wavefunction on x \[Element] [xL, xR], y \[Element] [yL, yR]";
PlotWavefunction::usage = "PlotWavefunction[wavef, {x, xL, xR}] plots a 1D symbolic (in x) wavefunction between xL and xR";
PlotWavefunction::usage = "PlotWavefunction[wavef, {x, xL, xR}, {y, yL, yR}] plots a 2D symbolic (in x and y) wavefunction on x \[Element] [xL, xR], y \[Element] [yL, yR]";





(* PRIVATE FUNCTION DEFINITIONS *)

(* 
	pattern matching for {xL, xR}, {x, xL, xR} and both 
*)
realNumQ = Internal`RealValuedNumericQ;
domainQ = MatchQ[{_?realNumQ, _?realNumQ}];
domainSymbQ = MatchQ[{_Symbol, _?realNumQ, _?realNumQ}];
domainOptSymbQ = MatchQ[{Repeated[_Symbol, {0,1}], _?realNumQ, _?realNumQ}];





(* 
	(silently) checks whether func has correct number of args
*)
is1DFunction[func:(_Function|_InterpolatingFunction)] :=
	Quiet @ Not[Check[func[dummyval], None] === None]
is2DFunction[func:(_Function|_InterpolatingFunction)] :=
	Quiet @ Not[Check[func[dummyval1, dummyval2], None] === None]



(* 
	checks input and domain types are compatible/valid
*)
isValid1DInput[cons_?NumericQ, _?domainOptSymbQ] := 
	True
isValid1DInput[list_/;VectorQ[list, NumericQ], _?domainOptSymbQ] := 
	True
isValid1DInput[symb:Except[_Function|_InterpolatingFunction], _?domainSymbQ] :=
	True
isValid1DInput[func:(_Function|_InterpolatingFunction), _?domainOptSymbQ] :=
	is1DFunction[func]
isValid1DInput[_, _?domainOptSymbQ] := 
	False
	


isValid2DInput[cons_?NumericQ, _?domainOptSymbQ, _?domainOptSymbQ] := 
	True
isValid2DInput[list_/;MatrixQ[list, NumericQ], _?domainOptSymbQ, _?domainOptSymbQ] := 
	True
isValid2DInput[symb:Except[_Function|_InterpolatingFunction], _?domainSymbQ, _?domainSymbQ] :=
	True
isValid2DInput[func:(_Function|_InterpolatingFunction), _?domainOptSymbQ, _?domainOptSymbQ] :=
	is2DFunction[func]
isValid2DInput[_, _?domainOptSymbQ, _?domainOptSymbQ] :=
	False
	
	

(* 
	checks a function given to PotentialTransform is valid
*)
isValidTransform[transform:(_Function|_InterpolatingFunction)] :=
	is1DFunction[transform]
	
	

(* 
	checks PlotWavefunction's OptionPattern features valid Potential and PotentialTransform 
	values (compatibl with the passed domain), if they're passed
*)
isValid1DOptions[
	options:OptionsPattern[PlotWavefunction], 
	endpoints:_?domainQ
] :=
	(
		Not[MemberQ[{options}, Potential -> _]] || 
		MemberQ[{options}, Potential -> potential_ /; isValid1DInput[potential, endpoints]] 
	) && (
		Not[MemberQ[{options}, PotentialTransform -> _]] || 
		MemberQ[{options}, PotentialTransform -> _?isValidTransform] 
	)
	
isValid1DOptions[
	options:OptionsPattern[PlotWavefunction], 
	domain:_?domainSymbQ
] :=
	(
		Not[MemberQ[{options}, Potential -> _]] || 
		MemberQ[{options}, Potential -> potential_ /; isValid1DInput[potential, domain[[2;;]]]] ||
		MemberQ[{options}, Potential -> potential_ /; isValid1DInput[potential, domain]] 	
	) && (
		Not[MemberQ[{options}, PotentialTransform -> _]] || 
		MemberQ[{options}, PotentialTransform -> _?isValidTransform] 
	)



isValid2DOptions[
	options:OptionsPattern[PlotWavefunction],   (* non-symbolic potential *)
	endpoints1:_?domainQ, 
	endpoints2:_?domainQ
] :=
	(
		Not[MemberQ[{options}, Potential -> _]] || 
		MemberQ[{options}, Potential -> potential_ /; isValid2DInput[potential, endpoints1, endpoints2]] 
	) && (
		Not[MemberQ[{options}, PotentialTransform -> _]] || 
		MemberQ[{options}, PotentialTransform -> _?isValidTransform] 
	)

isValid2DOptions[
	options:OptionsPattern[PlotWavefunction],   (* possibly symbolic potential *)
	domain1:_?domainSymbQ, 
	domain2:_?domainSymbQ
] :=
	(
		Not[MemberQ[{options}, Potential -> _]] || 
		MemberQ[{options}, Potential -> potential_ /; isValid2DInput[potential, domain1[[2;;]], domain2[[2;;]]]] ||
		MemberQ[{options}, Potential -> potential_ /; isValid2DInput[potential, domain1, domain2]] 
	) && (
		Not[MemberQ[{options}, PotentialTransform -> _]] || 
		MemberQ[{options}, PotentialTransform -> _?isValidTransform] 
	)
	
isValid2DOptions[
	options:OptionsPattern[PlotWavefunction], 
	domain1:_?domainSymbQ,     (* a mix of non-symbolic and symbolic domain (treat both as non-symbolic) *)
	domain2:_?domainQ
] :=
	isValid2DOptions[options, domain1[[2;;]], domain2]

isValid2DOptions[
	options:OptionsPattern[PlotWavefunction], 
	domain1:_?domainQ, 
	domain2:_?domainSymbQ       (* a mix of non-symbolic and symbolic domain (treat both as non-symbolic) *)
] :=
	isValid2DOptions[options, domain1, domain2[[2;;]]]
	
	
	
(*
	converts numeric vectors, matrices and constants to functions,
	and keeps functions, interpolating functions and None unchanged
*)
convertToFunction[
	None, 
	Repeated[_?domainOptSymbQ, {1,2}]
] :=
	None

convertToFunction[
	const_?NumericQ, 
	Repeated[_?domainOptSymbQ, {1,2}]
] :=
	(const &)
	
convertToFunction[
	func:(_Function|_InterpolatingFunction),
	Repeated[_?domainOptSymbQ, {1,2}]
] :=
	func
	
convertToFunction[
	list_/;VectorQ[list, NumericQ], 
	{___Symbol, xL_?NumericQ, xR_?NumericQ}
] :=
	ListInterpolation[list, {{xL, xR}}]
	
convertToFunction[
	matrix_/;MatrixQ[matrix, NumericQ], 
	{___Symbol, xL_?NumericQ, xR_?NumericQ},
	{___Symbol, yL_?NumericQ, yR_?NumericQ}
] :=
	ListInterpolation[matrix, {{xL, xR}, {yL, yR}}]
	
convertToFunction[
	symbolic_,
	{symb_Symbol, _?NumericQ, _?NumericQ}
] :=
	Function @@ {symb, symbolic}
	
convertToFunction[
	symbolic_,
	{symb1_Symbol, _?NumericQ, _?NumericQ},
	{symb2_Symbol, _?NumericQ, _?NumericQ}
] :=
	Function @@ {{symb1, symb2}, symbolic}



(*
	extracts end-points from any domain format
*)
convertToEndPoints[
	{___Symbol, xL_?NumericQ, xR_?NumericQ}
] := 
	{xL, xR}
	
	

(*
	options that apply to plotting both 1D and 2D wavefunctions
*)
Options[PlotWavefunction] = {

	(* exported *)
	Potential -> None,
	PotentialTransform -> (#&),
	ShowBar -> True,
	ColorScheme -> "Rainbow"
};

(*
	options compatible only with 1D wavefunctions
*)
Options[plot1DWavefunction] = {
	AxesLabel -> {"x", "Abs[\[Psi]]^2"},
	LegendLabel -> "Phase[\[Psi]]"
};

(*
	options compatible only with 2D wavefunctions
*)
Options[plot2DWavefunction] = {
	AxesLabel -> {"x", "y", "Abs[\[Psi]]^2"},
	LegendLabel -> "Phase[\[Psi]]"
};





(*
	functions of which these package-functions can accept the options;
	used for matching options to internal functions
*)
optionFunctions1D = {PlotWavefunction, plot1DWavefunction, BarLegend, Plot};
optionFunctions2D = {PlotWavefunction, plot2DWavefunction, BarLegend, Plot3D};

(*
	merges passed options with the defaults of these package-functions;
	avoids the clutter of passing defaults of external (BarLegend, Plot, Plot3D)
	to these package-functions
*)
optionFilter1D[
	options:OptionsPattern[optionFunctions1D]
] :=
	{options, Sequence @@ Options[PlotWavefunction], Sequence @@ Options[plot1DWavefunction]};
	
optionFilter2D[
	options:OptionsPattern[optionFunctions2D]
] :=
	{options, Sequence @@ Options[PlotWavefunction], Sequence @@ Options[plot2DWavefunction]};
	
	
	



(*
	plots a functional wavefunction, combined with a potential 
	(if not None) and a colorbar (if ShowBar \[Rule] True in options)
*)
plot1DWavefunction[
	wavef:(_Function|_InterpolatingFunction),
	potential:(_Function|_InterpolatingFunction|None),
	domain:{xL_?NumericQ, xR_?NumericQ},
	options:OptionsPattern[optionFunctions1D]
] :=
	Module[
		{probabilityPlot, potentialPlot, colorBar},
		probabilityPlot = plot1DProbabilityDensity[wavef, domain, options];
		
		(******* THE OVERRIDING OF POTENTIAL PLOTRANGE BELOW SHOULD BE UNDONE BY PASSING OF AN EXPLICIT POTENTIAL PLOTRANGE LATER *******)
		(* (though I suspect it will be automatic within plot1DPotential, if we apply eval options before our own styling) *)
		potentialPlot = plot1DPotential[potential, domain, PlotRange[probabilityPlot][[2]], options];
		colorBar = plotColorBar[
			OptionValue[optionFunctions1D, {options}, ColorScheme],
			OptionValue[optionFunctions1D, {options}, ShowBar],
			Evaluate @ FilterRules[optionFilter1D[options], Options[BarLegend]]
		];
		
		combineGraphics[probabilityPlot, potentialPlot, colorBar]
	]
	
plot2DWavefunction[
	wavef:(_Function|_InterpolatingFunction),
	potential:(_Function|_InterpolatingFunction|None),
	xDomain:{xL_?NumericQ, xR_?NumericQ},
	yDomain:{yL_?NumericQ, yR_?NumericQ},
	options:OptionsPattern[optionFunctions2D]
] :=
	Module[
		{probabilityPlot, potentialPlot, colorBar},
		
		probabilityPlot = plot2DProbabilityDensity[wavef, xDomain, yDomain, options];
		
		(******* THE OVERRIDING OF POTENTIAL PLOTRANGE BELOW SHOULD BE UNDONE BY PASSING OF AN EXPLICIT POTENTIAL PLOTRANGE LATER *******)
		(* (though I suspect it will be automatic within plot1DPotential, if we apply eval options before our own styling) *)
		potentialPlot = plot2DPotential[potential, xDomain, yDomain, PlotRange[probabilityPlot][[3]], options];
		colorBar = plotColorBar[
			OptionValue[optionFunctions2D, {options}, ColorScheme],
			OptionValue[optionFunctions2D, {options}, ShowBar],
			Evaluate @ FilterRules[optionFilter2D[options], Options[BarLegend]]
		];
		
		combineGraphics[probabilityPlot, potentialPlot, colorBar]
	]
	
	
	
(*
	plots the probability distribution of a given wavefunction,
	accepting external plotting function options, which override
	those explicit here
*)
plot1DProbabilityDensity[
	wavef:(_Function|_InterpolatingFunction),
	{xL_?NumericQ, xR_?NumericQ},
	options:OptionsPattern[optionFunctions1D]
] :=
	ReplaceAll[
		Plot[
			Abs[wavef[x]]^2,
			{x, xL, xR},
			
			(* apply (overridding) user-given Plot options first *)
			Evaluate @ FilterRules[optionFilter1D[options], Options[Plot]],
			
			(* otherwise apply this default styling ... *)
			Filling -> Axis,
			ColorFunctionScaling -> False,
			ColorFunction -> 
				Function[x, 
					ColorData[OptionValue[optionFunctions1D, {options}, ColorScheme]][
						Rescale[Arg[wavef[x]], {-\[Pi], \[Pi]}]
					]
				]
		],
		(* make plot outline black (not the ColorFunction) *)
		Line[pts_, _] :> {Black, Line[pts]}
	]

plot2DProbabilityDensity[
	wavef:(_Function|_InterpolatingFunction),
	{xL_?NumericQ, xR_?NumericQ},
	{yL_?NumericQ, yR_?NumericQ},
	options:OptionsPattern[optionFunctions2D]
] :=
	Plot3D[
		Abs[wavef[x, y]]^2,
		{x, xL, xR},
		{y, yL, yR},
		
		(* apply (overridding) user-given Plot3D options first *)
		Evaluate @ FilterRules[optionFilter2D[options], Options[Plot3D]],
		
		(* otherwise apply this default styling ... *)
		Mesh -> None,
		Exclusions -> None,
		ColorFunctionScaling -> False,
		ColorFunction -> 
			Function[{x, y}, 
				ColorData[OptionValue[optionFunctions2D, {options}, ColorScheme]][
					Rescale[Arg[wavef[x,y]], {-\[Pi], \[Pi]}]
				]
			]
	]




(*
	plots a potential if not None, otherwise returns None
*)
plot1DPotential[
	None,
	_?domainQ,  (* domain *)
	_?domainQ,  (* range *)
	OptionsPattern[optionFunctions1D]
] :=
	None

plot1DPotential[
	potential:(_Function|_InterpolatingFunction),
	domain:{xL_?NumericQ, xR_?NumericQ},
	range:{_?NumericQ, _?NumericQ},
	options:OptionsPattern[optionFunctions1D]
] :=
	Plot[
		Composition[
			OptionValue[optionFunctions1D, {options}, PotentialTransform],
			potential
		][x],
		{x, xL, xR},
		PlotRange -> range,
		
		(* for now; do not use the Plot options *)   (* TODO: an PlotWavefunction option for Potential plot options! *)
		(* ^^^ update above doc when implemented *)
		
		Exclusions -> None,
		Filling -> Axis,
		PlotStyle -> {Thick, Red}
	]



plot2DPotential[
	None,
	_?domainQ, (* x domain *)
	_?domainQ, (* y domain *)
	_?domainQ, (* range *)
	OptionsPattern[optionFunctions2D]
] :=
	None	

plot2DPotential[
	potential:(_Function|_InterpolatingFunction),
	{xL_?NumericQ, xR_?NumericQ},
	{yL_?NumericQ, yR_?NumericQ},
	{zL_?NumericQ, zR_?NumericQ},
	options:OptionsPattern[optionFunctions2D]
] :=
	Plot3D[
		Composition[
			OptionValue[optionFunctions2D, {options}, PotentialTransform],
			potential
		][x, y],
		{x, xL, xR},
		{y, yL, yR},
		PlotRange -> {zL, zR},
		
		(* for now; do not use the Plot options *)   (* TODO: an PlotWavefunction option for Potential plot options! *)
		(* ^^^ update above doc when implemented *)

		ClippingStyle -> None,
		Exclusions -> None,
		PlotStyle -> {{Opacity[.3], Red}},
		Mesh -> 5,
		MeshStyle -> {{Red}}
	]





(*
	plots a color bar, accepting options for BarLegend
*)
plotColorBar[
	_String,
	False,
	OptionsPattern[BarLegend]
] :=
	None
	
plotColorBar[
	scheme_String,
	True,
	options:OptionsPattern[BarLegend]
] :=
	BarLegend[
		{scheme, {-\[Pi], \[Pi]}}, 
		"Ticks" -> N[{-\[Pi]+.01, -\[Pi]/2, 0, \[Pi]/2, \[Pi]-.01}],
		"TickLabels" -> {"-\[Pi]", "-\[Pi]/2", "0", "\[Pi]/2", "\[Pi]"},
		options
	]
	
	
	


(*
	combines a probability density with a potential plot and a 
	color bar (each of which may be None)
*)
combineGraphics[
	probabilityPlot:(_Graphics|_Graphics3D),
	potentialPlot:(_Graphics|_Graphics3D),
	colorBar:_BarLegend
] :=
	Legended[
		Show[
			probabilityPlot,
			potentialPlot
		],
		colorBar
	]
combineGraphics[
	probabilityPlot:(_Graphics|_Graphics3D),
	potentialPlot:(_Graphics|_Graphics3D),
	None
] :=
	Show[
		probabilityPlot,
		potentialPlot
	]
combineGraphics[
	probabilityPlot:(_Graphics|_Graphics3D),
	None,
	colorBar:_BarLegend
] :=
	Legended[
		probabilityPlot,
		colorBar
	]
combineGraphics[
	probabilityPlot:(_Graphics|_Graphics3D),
	None,
	None
] :=
	probabilityPlot

		
		


(* PUBLIC FUNCTION DEFINITIONS *)

(*
	Plots a 1D or 2D wavefunction, with optional potential.
	A 1D wavefunction can be specified as a quantity, vector
	(of numeric quantities), function, interpolating function 
	or a symbolic expression (requiring a symbol is passed 
	within the domain).
	A 2D wavefunction can be specified as a quantity, matrix
	(of numeric quantities), function, interpolating function
	or a symbolic expression (requiring symbols are passed
	within the two domains)
*)
PlotWavefunction[
	wavef_,
	domain_?domainOptSymbQ,
	options:OptionsPattern[optionFunctions1D]
] /; (
	isValid1DInput[wavef, domain] &&
	isValid1DOptions[options, domain]
) :=
	plot1DWavefunction[
		convertToFunction[wavef, domain],
		convertToFunction[OptionValue[optionFunctions1D, {options}, Potential], domain],
		convertToEndPoints[domain],
		options
	]
	
PlotWavefunction[
	wavef_, 
	xDomain_?domainOptSymbQ, 
	yDomain_?domainOptSymbQ, 
	options:OptionsPattern[optionFunctions2D]
] /; (
	isValid2DInput[wavef, xDomain, yDomain] &&
	isValid2DOptions[options, xDomain, yDomain]
) := 
	plot2DWavefunction[
		convertToFunction[wavef, xDomain, yDomain],
		convertToFunction[OptionValue[optionFunctions2D, {options}, Potential], xDomain, yDomain],
		convertToEndPoints[xDomain],
		convertToEndPoints[yDomain],
		options
	]
