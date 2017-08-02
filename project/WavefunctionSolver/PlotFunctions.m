(* ::Package:: *)

Package["WavefunctionSolver`"]





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

domainPattern = MatchQ[{_?NumericQ, _?NumericQ}];
domainSymbPattern = MatchQ[{_Symbol, _?NumericQ, _?NumericQ}];
domainOptSymbPattern = MatchQ[{Repeated[_Symbol, {0,1}], _?NumericQ, _?NumericQ}];



valid1DFunction[cons_?NumericQ, _?domainOptSymbPattern] := 
	True
valid1DFunction[list_/;VectorQ[list, NumericQ], _?domainOptSymbPattern] := 
	True
valid1DFunction[symb:Except[_Function|_InterpolatingFunction], _?domainSymbPattern] :=
	True
valid1DFunction[func:(_Function|_InterpolatingFunction), _?domainOptSymbPattern] :=
	Quiet @ Not[Check[func[dummyval], None] === None]
valid1DFunction[_, _?domainOptSymbPattern] := 
	False
	


valid2DFunction[cons_?NumericQ, _?domainOptSymbPattern, _?domainOptSymbPattern] := 
	True
valid2DFunction[list_/;MatrixQ[list, NumericQ], _?domainOptSymbPattern, _?domainOptSymbPattern] := 
	True
valid2DFunction[symb:Except[_Function|_InterpolatingFunction], _?domainSymbPattern, _?domainSymbPattern] :=
	True
valid2DFunction[func:(_Function|_InterpolatingFunction), _?domainOptSymbPattern, _?domainOptSymbPattern] :=
	Quiet @ Not[Check[func[dummyval1, dummyval2], None] === None]
valid2DFunction[_, _?domainOptSymbPattern, _?domainOptSymbPattern] :=
	False
	
	

valid1DOptions[options:OptionsPattern[PlotWavefunction], endpoints:_?domainPattern] :=
	Not[MemberQ[{options}, Potential -> _]] || 
	MemberQ[{options}, Potential -> potential_ /; valid1DFunction[potential, endpoints]] 
	
valid1DOptions[options:OptionsPattern[PlotWavefunction], domain:_?domainSymbPattern] :=
	Not[MemberQ[{options}, Potential -> _]] || 
	MemberQ[{options}, Potential -> potential_ /; valid1DFunction[potential, domain[[2;;]]]] ||
	MemberQ[{options}, Potential -> potential_ /; valid1DFunction[potential, domain]] 	



(* non-symbolic potential *)
valid2DOptions[
	options:OptionsPattern[PlotWavefunction], 
	endpoints1:_?domainPattern, 
	endpoints2:_?domainPattern
] :=
	Not[MemberQ[{options}, Potential -> _]] || 
	MemberQ[{options}, Potential -> potential_ /; valid2DFunction[potential, endpoints1, endpoints2]] 

(* possibly symbolic potential *)
valid2DOptions[
	options:OptionsPattern[PlotWavefunction], 
	domain1:_?domainSymbPattern, 
	domain2:_?domainSymbPattern
] :=
	Not[MemberQ[{options}, Potential -> _]] || 
	MemberQ[{options}, Potential -> potential_ /; valid2DFunction[potential, domain1[[2;;]], domain2[[2;;]]]] ||
	MemberQ[{options}, Potential -> potential_ /; valid2DFunction[potential, domain1, domain2]] 
	
(* a mix of non-symbolic and symbolic domain (treat both as non-symbolic) *)
valid2DOptions[
	options:OptionsPattern[PlotWavefunction], 
	domain1:_?domainSymbPattern, 
	domain2:_?domainPattern
] :=
	valid2DOptions[options, domain1[[2;;]], domain2]

valid2DOptions[
	options:OptionsPattern[PlotWavefunction], 
	domain1:_?domainPattern, 
	domain2:_?domainSymbPattern
] :=
	valid2DOptions[options, domain1, domain2[[2;;]]]
	
	
	
convertToFunction[
	None, 
	Repeated[_?domainOptSymbPattern, {1,2}]
] :=
	None

convertToFunction[
	const_?NumericQ, 
	Repeated[_?domainOptSymbPattern, {1,2}]
] :=
	(const &)
	
convertToFunction[
	func:(_Function|_InterpolatingFunction),
	Repeated[_?domainOptSymbPattern, {1,2}]
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



convertToEndPoints[
	{___Symbol, xL_?NumericQ, xR_?NumericQ}
] := 
	{xL, xR}
	
	

Options[PlotWavefunction] = {

	(* exported *)
	Potential -> None,
	PotentialTransform -> (#&),
	ShowBar -> True,
	ColorScheme -> "Rainbow",
	
	(* pre-existing *)
	PlotRange -> {0, 1},
	LegendLabel -> "\[Theta]"
};

Options[plot1DWavefunction] = {
	AxesLabel -> {"x", "Abs[\[Psi]]^2"}
};

Options[plot2DWavefunction] = {
	AxesLabel -> {"x", "y", "Abs[\[Psi]]^2"}
};




(* for accepting options to internal functions *)
optionFunctions1D = {PlotWavefunction, plot1DWavefunction, BarLegend, Plot};
optionFunctions2D = {PlotWavefunction, plot2DWavefunction, BarLegend, Plot3D};

(* for generating default options to functions defined here *)
optionFilter1D[
	options:OptionsPattern[optionFunctions1D]
] :=
	{options, Sequence @@ Options[PlotWavefunction], Sequence @@ Options[plot1DWavefunction]};
	
optionFilter2D[
	options:OptionsPattern[optionFunctions2D]
] :=
	{options, Sequence @@ Options[PlotWavefunction], Sequence @@ Options[plot2DWavefunction]};
	
	
	


plot1DWavefunction[
	wavef:(_Function|_InterpolatingFunction),
	potential:(_Function|_InterpolatingFunction|None),
	domain:{xL_?NumericQ, xR_?NumericQ},
	options:OptionsPattern[optionFunctions1D]
] :=
	Module[
		{probabilityPlot, potentialPlot, colorBar},
		probabilityPlot = plot1DProbabilityDensity[wavef, domain, options];
		potentialPlot = plot1DPotential[potential, domain, options];
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
		potentialPlot = plot2DPotential[potential, xDomain, yDomain, options];
		colorBar = plotColorBar[
			OptionValue[optionFunctions2D, {options}, ColorScheme],
			OptionValue[optionFunctions2D, {options}, ShowBar],
			Evaluate @ FilterRules[optionFilter2D[options], Options[BarLegend]]
		];
		
		combineGraphics[probabilityPlot, potentialPlot, colorBar]
	]
	
	
	
plot1DProbabilityDensity[
	wavef:(_Function|_InterpolatingFunction),
	{xL_?NumericQ, xR_?NumericQ},
	options:OptionsPattern[optionFunctions1D]
] :=
	ReplaceAll[
		Plot[
			Abs[wavef[x]]^2,
			{x, xL, xR},
			
			Filling -> Axis,
			ColorFunctionScaling -> False,
			ColorFunction -> 
				Function[x, 
					ColorData[OptionValue[optionFunctions1D, {options}, ColorScheme]][
						Rescale[Arg[wavef[x]], {-\[Pi], \[Pi]}]
					]
				],
			
			Evaluate @ FilterRules[optionFilter1D[options], Options[Plot]]
		],
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
		
		Mesh -> None,
		Exclusions -> None,
		ColorFunctionScaling -> False,
		ColorFunction -> 
			Function[{x, y}, 
				ColorData[OptionValue[optionFunctions2D, {options}, ColorScheme]][
					Rescale[Arg[wavef[x,y]], {-\[Pi], \[Pi]}]
				]
			],
		
		Evaluate @ FilterRules[optionFilter2D[options], Options[Plot3D]]
	]



plot1DPotential[
	None,
	_?domainPattern,
	OptionsPattern[optionFunctions1D]
] :=
	None

plot1DPotential[
	potential:(_Function|_InterpolatingFunction),
	{xL_?NumericQ, xR_?NumericQ},
	options:OptionsPattern[optionFunctions1D]
] :=
	Plot[
		Composition[
			OptionValue[optionFunctions1D, {options}, PotentialTransform],
			potential
		][x],
		{x, xL, xR},
		
		(* for now; do not use the Plot options *)   (* TODO: an PlotWavefunction option for Potential plot options! *)
		
		Exclusions -> None,
		Filling -> Axis,
		PlotStyle -> {Thick, Red}
	]



plot2DPotential[
	None,
	_?domainPattern,
	_?domainPattern,
	OptionsPattern[optionFunctions2D]
] :=
	None	

plot2DPotential[
	potential:(_Function|_InterpolatingFunction),
	{xL_?NumericQ, xR_?NumericQ},
	{yL_?NumericQ, yR_?NumericQ},
	options:OptionsPattern[optionFunctions2D]
] :=
	Plot3D[
		Composition[
			OptionValue[optionFunctions2D, {options}, PotentialTransform],
			potential
		][x, y],
		{x, xL, xR},
		{y, yL, yR},
		
		(* for now; do not use the Plot options *)   (* TODO: an PlotWavefunction option for Potential plot options! *)

		ClippingStyle -> None,
		Exclusions -> None,
		PlotStyle -> {{Opacity[.3], Red}},
		Mesh -> 5,
		MeshStyle -> {{Red}}
	]



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
	


combineGraphics[
	probabilityPlot:(_Graphics|_Graphics3D),
	potentialPlot:(_Graphics|_Graphics3D|None),
	colorBar:(_BarLegend|None)
] :=
	Module[
		{combinedPlot},
		
		combinedPlot = 
			If[
				potentialPlot === None,
				probabilityPlot,
				Show[
					probabilityPlot,
					potentialPlot
				]
			];
		
		combinedPlot = 
			If[
				colorBar === None,
				combinedPlot,
				Legended[
					combinedPlot,
					colorBar
				]
			];
		
		combinedPlot
	]
		
		
		


(* PUBLIC FUNCTION DEFINITIONS *)

PlotWavefunction[
	wavef_,
	domain_?domainOptSymbPattern,
	options:OptionsPattern[optionFunctions1D]
] /; (
	valid1DFunction[wavef, domain] &&
	valid1DOptions[options, domain]
) :=
	plot1DWavefunction[
		convertToFunction[wavef, domain],
		convertToFunction[OptionValue[optionFunctions1D, {options}, Potential], domain],
		convertToEndPoints[domain],
		options
	]
	
PlotWavefunction[
	wavef_, 
	xDomain_?domainOptSymbPattern, 
	yDomain_?domainOptSymbPattern, 
	options:OptionsPattern[optionFunctions2D]
] /; (
	valid2DFunction[wavef, xDomain, yDomain] &&
	valid2DOptions[options, xDomain, yDomain]
) := 
	plot2DWavefunction[
		convertToFunction[wavef, xDomain, yDomain],
		convertToFunction[OptionValue[optionFunctions2D, {options}, Potential], xDomain, yDomain],
		convertToEndPoints[xDomain],
		convertToEndPoints[yDomain],
		options
	]
