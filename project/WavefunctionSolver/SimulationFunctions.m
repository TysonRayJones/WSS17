(* ::Package:: *)

Package["WavefunctionSolver`"]

(*
	TODO:
	- function to plot a symbolic, analytic wavefunction (has a time dependence), 
		in 1D space and 2D space
	- same ^ but with function wavefunctions
	- function to actually calc evolution of a static wavefunction
	- options should offer pre-caching
*)





(* SYMBOL EXPORTS *)





(* FUNCTION EXPORTS *)

PackageExport[PlotEvolution]




(* FUNCTION USAGE MESSAGES *)





(* FUNCTION ERROR MESSAGES *)





(* SYMBOL IMPORTS *)

realNumQ = WavefunctionSolver`PlotFunctions`PackagePrivate`realNumQ;
domainQ = WavefunctionSolver`PlotFunctions`PackagePrivate`domainQ;
domainSymbQ = WavefunctionSolver`PlotFunctions`PackagePrivate`domainSymbQ;
domainOptSymbQ = WavefunctionSolver`PlotFunctions`PackagePrivate`domainOptSymbQ;

func1DQ = WavefunctionSolver`EigenFunctions`PackagePrivate`func1DQ;
func2DQ = WavefunctionSolver`EigenFunctions`PackagePrivate`func2DQ;

convertToFunction = WavefunctionSolver`PlotFunctions`PackagePrivate`convertToFunction;
convertToEndPoints = WavefunctionSolver`PlotFunctions`PackagePrivate`convertToEndPoints;

plotOptionFunctions1D = WavefunctionSolver`PlotFunctions`PackagePrivate`optionFunctions1D;

extractOptions = WavefunctionSolver`PlotFunctions`PackagePrivate`extractOptions;
addDefaultOptions = WavefunctionSolver`PlotFunctions`PackagePrivate`addDefaultOptions;

defaultProbDensityLabel = WavefunctionSolver`PlotFunctions`PackagePrivate`defaultProbDensityLabel;
defaultXLabel = WavefunctionSolver`PlotFunctions`PackagePrivate`defaultXLabel;
defaultYLabel = WavefunctionSolver`PlotFunctions`PackagePrivate`defaultYLabel;





(* CONSTANTS *)

defaultTimeLabel = "time";




(* PRIVATE FUNCTION DEFINITIONS *)

(* extend convertToFunction to handle a third (time) dimension *)
convertToFunction[
	symbolic_,
	{symb1_Symbol, _?realNumQ, _?realNumQ},
	{symb2_Symbol, _?realNumQ, _?realNumQ},
	{symb3_Symbol, _?realNumQ, _?realNumQ}
] :=
	Function @@ {{symb1, symb2, symb3}, symbolic}




plot1DEvolution[
	wavef:(_Function|_InterpolatingFunction),           (* 2D: [x, t] *)
	potential:(_Function|_InterpolatingFunction|None),  (* 1D: [x]    *)   (* overriden by Potential option *)
	{xL_?realNumQ, xR_?realNumQ},
	{t0_?realNumQ, t1_?realNumQ},
	options:OptionsPattern[{plotOptionFunctions1D, Animate} // Flatten]
] :=
	plot1DEvolution[
		wavef,
		potential,
		
		(* use an internal symbol ...*)
		{xInternal, xL, xR},
		{defaultTimeLabel, t0, t1},
		
		(* and the default x label (unless AxesLabel already user-specified *)
		options,
		AxesLabel -> {defaultXLabel, defaultProbDenistyLabel}
	]
		

plot1DEvolution[
	wavef:(_Function|_InterpolatingFunction),           (* 2D: [x, t] *)
	potential:(_Function|_InterpolatingFunction|None),  (* 1D: [x]    *)   (* overriden by Potential option *)
	{xLabel:_Symbol, xL_?realNumQ, xR_?realNumQ},
	{tLabel:(_Symbol|_String), t0_?realNumQ, t1_?realNumQ},
	options:OptionsPattern[{plotOptionFunctions1D, Animate} // Flatten]
] :=
	Animate[
	
		PlotWavefunction[
			wavef[xLabel, tVar],
			{xLabel, xL, xR},
			
			(* apply over-riding user-given plot options *)
			Evaluate @ extractOptions[options, plotOptionFunctions1D],
			
			(* otherwise apply this default style *)
			Potential -> potential
		],
		
		{{tVar, t0, tLabel}, t0, t1},
		
		(* apply over-riding user-given Animate options *)
		Evaluate @ extractOptions[options, Animate]
	]





(* PUBLIC FUNCTION DEFINITIONS *)

Options[PlotEvolution] = {};


PlotEvolution[
	wavef_,     (* time-dependent, symbolic, one-dimensional*)
	xDomain:{x_Symbol, _?realNumQ, _?realNumQ},
	tDomain:{t_Symbol, _?realNumQ, _?realNumQ},
	options:OptionsPattern[{PlotEvolution, Animate, plotOptionFunctions1D} // Flatten]
] :=
	plot1DEvolution[
		convertToFunction[wavef, xDomain, tDomain],
		convertToFunction[OptionValue[Potential], xDomain],
		xDomain,
		tDomain,
		
		(* apply over-riding user-given plot-options *)
		Evaluate @ extractOptions[options, {Animate, plotOptionFunctions1D}]
	]
	

