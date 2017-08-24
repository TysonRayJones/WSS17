(* ::Package:: *)

Package["WavefunctionSolver`"]

(* 2D PLOTEVOLUTION of INTERPOLATING FUNCTION without CACHING always ABORTS: some random over-sampling of interpolation???? *)


(*
	TODO:
	- check and fix warnings for invalid options
	- loading bar for NDSOlve? (https://groups.google.com/forum/#!topic/comp.soft-sys.math.mathematica/aFxHw8gkUEU)
	     will have to stop using the dot unassignable symobls tho
	     (https://www.wolfram.com/mathematica/new-in-9/gauges/monitor-evaluation-progress.html)
	- resizing mid ListAnimation still goofs up! All properties need to be dynamic! (https://mathematica.stackexchange.com/questions/16765/creating-an-animation-illustrating-the-time-evolution-of-a-pre-computed-orbit)
	- ListAnimate doesn't display tLabel (Manipulate much slower) 
	- options for exporting animation to file (https://mathematica.stackexchange.com/questions/1428/is-it-possible-to-prerender-animation-in-wolfram-mathematica)
	- is it possible to warn user attempting to save expensive cached simulation?
	- is it possible to have a loading bar for rendering of cached frames?
	- implement AutoNormalise
	- implement Verbose or TimeSteps (use EchoFunction)
	- extend time-dependent plotting to lists and matrix inputs and what not
	
	NOTES:
	- ControlActive upperbound in PlotPoints affects render-time of the lowerbound; WTH? 
	   Occurs for both Animate and Manipuulate
*)





(* SYMBOL EXPORTS *)

PackageExport[NumberOfFrames]
PackageExport[ShowCacheProgress]

PackageExport[EvolutionEquation]




(* FUNCTION EXPORTS *)

PackageExport[PlotEvolution]
PackageExport[FindEvolution]
PackageExport[GetSchrodingerEquation]





(* FUNCTION USAGE MESSAGES *)

PlotEvolution::usage = "PlotEvolution[wavefunction[x, t], {x, xL, xR}, {t, t0, t1}] animates the evolution of a symbolic time-dependent wavefunction";
FindEvolution::usage = "TODO";
GetSchrodingerEquation::usage = "TODO";





(* FUNCTION ERROR MESSAGES *)





(* PRIVATE SYMBOL IMPORTS *)

(* patterns for accepting domains *)
realNumQ = WavefunctionSolver`PlotFunctions`PackagePrivate`realNumQ;
domainQ = WavefunctionSolver`PlotFunctions`PackagePrivate`domainQ;
domainSymbQ = WavefunctionSolver`PlotFunctions`PackagePrivate`domainSymbQ;
domainOptSymbQ = WavefunctionSolver`PlotFunctions`PackagePrivate`domainOptSymbQ;

(* testers for functions *)
is2DFunction = WavefunctionSolver`PlotFunctions`PackagePrivate`is2DFunction;
func1DQ = WavefunctionSolver`EigenFunctions`PackagePrivate`func1DQ;
func2DQ = WavefunctionSolver`EigenFunctions`PackagePrivate`func2DQ;

(* for manipulating accepted inputs into functional form *)
convertToFunction = WavefunctionSolver`PlotFunctions`PackagePrivate`convertToFunction;
convertToEndPoints = WavefunctionSolver`PlotFunctions`PackagePrivate`convertToEndPoints;

(* functions for filtering options *)
extractOptions = WavefunctionSolver`PlotFunctions`PackagePrivate`extractOptions;
addDefaultOptions = WavefunctionSolver`PlotFunctions`PackagePrivate`addDefaultOptions;
extractOptionsFromDefaults = WavefunctionSolver`PlotFunctions`PackagePrivate`extractOptionsFromDefaults;

(* to ensure we use the same default axes labels as PlotFunctions, when we're forced to pass them *)
defaultProbDensityLabel = WavefunctionSolver`PlotFunctions`PackagePrivate`defaultProbDensityLabel;
defaultXLabel = WavefunctionSolver`PlotFunctions`PackagePrivate`defaultXLabel;
defaultYLabel = WavefunctionSolver`PlotFunctions`PackagePrivate`defaultYLabel;

(* for accessing default plot options *)
plotOptionFunctions1D = WavefunctionSolver`PlotFunctions`PackagePrivate`optionFunctions1D;
plotOptionFunctions2D = WavefunctionSolver`PlotFunctions`PackagePrivate`optionFunctions2D;
plot1DWavefunction = WavefunctionSolver`PlotFunctions`PackagePrivate`plot1DWavefunction;
plot2DWavefunction = WavefunctionSolver`PlotFunctions`PackagePrivate`plot2DWavefunction;

(* for plotting potentials and colorbars *)
plot1DPotential = WavefunctionSolver`PlotFunctions`PackagePrivate`plot1DPotential;
plot2DPotential = WavefunctionSolver`PlotFunctions`PackagePrivate`plot2DPotential;
addColorBar = WavefunctionSolver`PlotFunctions`PackagePrivate`addColorBar;

(* functions for validating input wavefunctions and potentials *)
isValid1DInput = WavefunctionSolver`PlotFunctions`PackagePrivate`isValid1DInput;
isValid2DInput = WavefunctionSolver`PlotFunctions`PackagePrivate`isValid2DInput;

(* for validating the Potential in options passed to PlotEvolution *)
isValid1DPotentialOptions = WavefunctionSolver`PlotFunctions`PackagePrivate`isValid1DPotentialOptions;
isValid2DPotentialOptions = WavefunctionSolver`PlotFunctions`PackagePrivate`isValid2DPotentialOptions;





(* CONSTANTS *)

(* display labels *)
defaultTimeLabel = "time";
cachingFramesLabel = " caching frame `1`/`2`";

(* symbols used internally for numerical solving *)
internalWavef = \[FormalCapitalY];
internalSpace1 = \[FormalX];
internalSpace2 = \[FormalY];
internalTime = \[FormalT];





(* PRIVATE FUNCTION DEFINITIONS *)

is3DFunction[function:(_Function|_InterpolatingFunction)] :=
	Quiet @ Not[Check[func[dummyval1, dummyval2, dummyval3], None] === None]





(* 1D *)
convertToCurriedFunction[
	function_?func2DQ,
	_?domainOptSymbQ,    (* space *)
	_?domainOptSymbQ     (* time *)
] :=
	Function @@ {t, Function @@ {x, function[x, t]}}

convertToCurriedFunction[
	symbolic_,
	xDomain:_?domainSymbQ,                    (* space *)
	{tSymb_Symbol, _?realNumQ, _?realNumQ}    (* time *)
] :=
	Function @@ {tSymb, convertToFunction @@ {symbolic, xDomain}}
	
(* 2D *)
convertToCurriedFunction[
	function_?func2DQ,
	_?domainOptSymbQ,    (* space *)
	_?domainOptSymbQ,    (* space *)
	_?domainOptSymbQ     (* time *)
] :=
	Function @@ {t, Function @@ {{x, y}, function[x, y, t]}}

convertToCurriedFunction[
	symbolic_,
	xDomain:_?domainSymbQ,                    (* space *)
	yDomain:_?domainSymbQ,                    (* space *)  
	{tSymb_Symbol, _?realNumQ, _?realNumQ}   (* time *)
] :=
	Function @@ {tSymb, convertToFunction @@ {symbolic, xDomain, yDomain}}





(* 
	directly animates a held PlotWavefunction combined with a potential plot.
	can produce a plot at any time-stamp in between the given bounds
*)
animateEvolution[
	probPotPlot:_Hold,
	{tSymb_Symbol, tLabel:(_Symbol|_String), t0_?realNumQ, t1_?realNumQ},
	numFrames:0,
	_Symbol,  (* show loading bar irrelevant *)
	options:OptionsPattern[Manipulate]
] :=
	(* using Manipulate over Animate, since more efficient somehow *)
	Manipulate[
		ReleaseHold[probPotPlot], 
		{{tSymb, t0, tLabel}, t0, t1},
		
		(* apply over-riding user-given Animate options *)
		options,
		
		(* otherwise apply this styling *)
		Paneled -> False
	]

(*
	collects a number of frames at fixed time instances from a held 
	PlotWavefunction combined with a potential plot, and produces a ListAnimation
*)
animateEvolution[
	probPotPlot:_Hold,
	tDomain:{tSymb_Symbol, tLabel:(_Symbol|_String), t0_?realNumQ, t1_?realNumQ},
	numFrames:_Integer?Positive,
	showLoadingBar:_Symbol,
	options:OptionsPattern[ListAnimate]
] :=
	DynamicModule[
		{frames, times, viewA, viewP, viewV},  (* view params in module to isolate separate 3D plots *)
		
		(* collect frames, with optional loading bar *)
		frames = cacheFrames[
			probPotPlot,
			tDomain,
			numFrames,
			showLoadingBar
		];
			
		(* animate frames *)
		ListAnimate[
			
			frames,
			
			(* display time-stamp of frames IS TOO SLOW! *)
			(*
			frames[[Round[N[1] + (tSymb - t0)(numFrames - 1)/(t1 - t0)]]],
			{{tSymb, t0, tLabel}, t0, t1, (N @ t1-t0)/(numFrames-1)},
			*)
			
			(* apply over-riding user-given Animate options *)
			options,
		
			(* otherwise apply this styling *)    (* MMA BUG; THIS OVER-RIDES! GRRR *)
			Paneled -> False,
			
			(* initialise dynamic view-angle, to enable mid-animation rotation *)
			Initialization :> (		
				viewA = 60 Degree; viewP = {1, 1, 1}; viewV = {0, 0, 2.0};
			)
		]
	]





cacheFrames[
	probPotPlot:_Hold,
	{tSymb_Symbol, tLabel:(_Symbol|_String), t0_?realNumQ, t1_?realNumQ},
	numFrames:_Integer?Positive,
	showLoadingBar:False
] :=
	Quiet @ Table[
		ReleaseHold[probPotPlot],
		{tSymb, t0, t1, (t1-t0)/(numFrames-1)}    (* harmless /0 error suppressed *)
	]
	
cacheFrames[
	probPotPlot:_Hold,
	tDomain:{tSymb_Symbol, tLabel:(_Symbol|_String), t0_?realNumQ, t1_?realNumQ},
	numFrames:_Integer?Positive,
	showLoadingBar:True
] :=
	Monitor[
		cacheFrames[probPotPlot, tDomain, numFrames, False],
		Row[{
			ProgressIndicator[tSymb, {t0, t1}],
			StringForm[
				cachingFramesLabel, 
				1 + (tSymb - t0)(numFrames - 1)/(t1 - t0),
				numFrames
			]
		}]
	]





(*
    combines a held wavefunction plot and a potential plot
    (which may be None) in a Show
*)
combineProbPotentialPlots[
	probPlot:_Hold,
	potentialPlot:None
] :=
	probPlot
	
combineProbPotentialPlots[
	probPlot:_Hold,
	potentialPlot:(_Graphics|_Graphics3D)
] :=
	Hold @ Show[
		ReleaseHold @ probPlot,
		potentialPlot
	]





statusAlert[message_String] := 
	PrintTemporary @ Style[">> " <> message, Orange]





plot1DEvolution[
	wavef:(_Function|_InterpolatingFunction),           (* 2D: [x][t] (curried) *)
	potential:(_Function|_InterpolatingFunction|None),  (* 1D: [x] *)
	{xLabel:(_Symbol|_String):defaultXLabel,    xL_?realNumQ, xR_?realNumQ},
	{tLabel:(_Symbol|_String):defaultTimeLabel, t0_?realNumQ, t1_?realNumQ},
	options:OptionsPattern[{plotOptionFunctions1D, Manipulate, ListAnimate, PlotEvolution} // Flatten]
] :=
	Module[
		{probabilityPlot, potentialPlot, rawAnimation, cbarAnimation},
		
		statusAlert @ "preparing probability density plot";
		
		(* prepare (held) probability without sources of slow animation (we'll restore externally if desired *)
		probabilityPlot = Hold @ PlotWavefunction[
			 (* evals to a 1D-in-space function *)
			wavef[tDummyVar],  
			{xL, xR},  
			(* kill features which ruin performance (restore externally if user desires) *)
			Potential -> None, ShowColorBar -> False,
			(* pass user-given over-riding options *)
			Evaluate @ extractOptions[options, plotOptionFunctions1D],	
			(* otherwise update the axes labels *)
			AxesLabel -> {xLabel, defaultProbDensityLabel}
		];
		
		statusAlert @ "combining probability density plot with potential plot";
		
		(* plot static potential with automatic range (it'll use Show range); may return none *)
		potentialPlot = plot1DPotential[
			potential, {xL, xR}, Automatic, 
			Evaluate @ extractOptions[options, plotOptionFunctions1D]
		];
		
		statusAlert @ "animating plots";
		
		(* merge probability and potential (into a held Show), then animate *)
		rawAnimation = animateEvolution[
			combineProbPotentialPlots[probabilityPlot, potentialPlot],
			{tDummyVar, tLabel, t0, t1}, 
			
			(* decide whether to directly animate, or cache *)
			OptionValue[NumberOfFrames],
			OptionValue[ShowCacheProgress],
			Evaluate @ extractOptions[options, Manipulate]
		];
		
		statusAlert @ "adding a colorbar to plots";
		
		(* add an external colorbar *)
		cbarAnimation = addColorBar[
			rawAnimation,
			OptionValue[ColorScheme],
			OptionValue[ShowColorBar],
			Evaluate @ extractOptionsFromDefaults[options, {PlotWavefunction, plot1DWavefunction}, BarLegend]
		];
		
		statusAlert @ "rendering plots";
		
		cbarAnimation
	]





plot2DEvolution[
	wavef:(_Function|_InterpolatingFunction),           (* 3D: [x, y][t] (curried) *)
	potential:(_Function|_InterpolatingFunction|None),  (* 2D: [x, y] *)
	{xLabel:(_Symbol|_String):defaultXLabel,    xL_?realNumQ, xR_?realNumQ},
	{yLabel:(_Symbol|_String):defaultYLabel,    yL_?realNumQ, yR_?realNumQ},
	{tLabel:(_Symbol|_String):defaultTimeLabel, t0_?realNumQ, t1_?realNumQ},
	options:OptionsPattern[{plotOptionFunctions2D, Manipulate, ListAnimate, PlotEvolution} // Flatten]
] :=
	Module[
		{probabilityPlot, potentialPlot, rawAnimation, cbarAnimation},
		
		statusAlert @ "preparing probability density plot";
		
		(* prepare (held) probability without sources of slow animation (we'll restore externally if desired *)
		probabilityPlot = Hold @ PlotWavefunction[
			 (* evals to a 2D-in-space function *)
			wavef[tInternalVar],  
			{xL, xR}, {yL, yR},
			(* kill features which ruin performance (restore externally if user desires) *)
			Potential -> None, 
			ShowColorBar -> False,
			(* pass user-given over-riding options *)
			Evaluate @ extractOptions[options, plotOptionFunctions2D],	
			(* otherwise update the axes labels *)
			AxesLabel -> {xLabel, yLabel, defaultProbDensityLabel},	
			(* use dynamic view params (to enable mid-cached animation rotation) if caching *)
			Evaluate[Sequence @@ If[
				OptionValue[NumberOfFrames] === 0, {},
				{ViewAngle :> Dynamic@viewA, ViewPoint :> Dynamic@viewP, ViewVertical :> Dynamic@viewV}
			]]
		];
		
		statusAlert @ "combining probability density plot with potential plot";
		
		(* plot static potential with automatic range (it'll use Show range); may return none *)
		potentialPlot = plot2DPotential[
			potential, {xL, xR}, {yL, yR}, Automatic, 
			Evaluate @ extractOptions[options, plotOptionFunctions2D]
		];
		
		statusAlert @ "animating plots";
		
		(* merge probability and potential (into a held Show), then animate *)
		rawAnimation = animateEvolution[
			combineProbPotentialPlots[probabilityPlot, potentialPlot],
			{tInternalVar, tLabel, t0, t1}, 
			
			(* decide whether to directly animate, or cache *)
			OptionValue[NumberOfFrames],
			OptionValue[ShowCacheProgress],
			
			(* pass over-riding user Animate options *)
			Evaluate @ extractOptions[options, Manipulate],
			
			(* otherwise pause 2D animation (can be laggy) *)
			AnimationRunning -> False
		];
		
		statusAlert @ "adding a colorbar to plots";
		
		(* add an external colorbar *)
		cbarAnimation = addColorBar[
			rawAnimation,
			OptionValue[ColorScheme],
			OptionValue[ShowColorBar],
			Evaluate @ extractOptionsFromDefaults[options, {PlotWavefunction, plot2DWavefunction}, BarLegend]
		];
		
		statusAlert @ "rendering plots";
		
		cbarAnimation
	]






isValid1DTimeDependentWavef[
	symb:Except[(_Function|_InterpolatingFunction)], 
	Repeated[_?domainSymbQ, 2]
] := 
	True
	
isValid1DTimeDependentWavef[
	function:(_Function|_InterpolatingFunction), 
	Repeated[_?domainOptSymbQ, 2]
] := 
	is2DFunction[function]
	
isValid1DTimeDependentWavef[
	else_, 
	Repeated[_?domainOptSymbQ, 2]
] :=
	False
	
	

isValid2DTimeDependentWavef[
	symb:Except[(_Function|_InterpolatingFunction)], 
	Repeated[_?domainSymbQ, 3]
] := 
	True
	
isValid2DTimeDependentWavef[
	function:(_Function|_InterpolatingFunction), 
	Repeated[_?domainOptSymbQ, 3]
] := 
	is3DFunction[function]
	
isValid2DTimeDependentWavef[
	else_, 
	Repeated[_?domainOptSymbQ, 3]
] :=
	False






getBoundaryEquations[
	wavefSymbol_Symbol,
	timeSymbol_Symbol,
	spaceSymbols_List,
	endpoints_List
] :=
	With[
		{substitutions = 
			Partition[
				Flatten @ Table[{ 
					spaceSymbols[[i]] -> endpoints[[i, 1]],
					spaceSymbols[[i]] -> endpoints[[i, 2]]}, 
					{i, Length[spaceSymbols]}
				], 1]
		},
		Sequence @@ Map[
			wavefSymbol[Sequence @@ #] == 0 &,
			Append[spaceSymbols, timeSymbol] /. substitutions
		]
	]



(* 1D *)
getEquations[
	initWavef_Function,
	potential_Function,
	evolutionEq:(_Function|_Symbol),
	endpoints_?domainQ
] :=
	{
		evolutionEq[internalWavef, potential, internalTime, internalSpace1],              (* evolution equation *)
		getBoundaryEquations[internalWavef, internalTime, {internalSpace1}, {endpoints}],   (* boundary conditions *)
		internalWavef[internalSpace1, 0] == initWavef[internalSpace1]                       (* initial condition *)
	}

(* 2D *)
getEquations[
	initWavef_Function,
	potential_Function,
	evolutionEq:(_Function|_Symbol),
	xEndpoints_?domainQ,
	yEndpoints_?domainQ
] :=
	{
		evolutionEq[internalWavef, potential, internalTime, internalSpace1, internalSpace2], 
		getBoundaryEquations[internalWavef, internalTime, {internalSpace1, internalSpace2}, {xEndpoints, yEndpoints}],  
		internalWavef[internalSpace1, internalSpace2, 0] == initWavef[internalSpace1, internalSpace2]                
	}
	
	
	
evolutionOptionFunctions = {PlotEvolution, Manipulate, ListAnimate};


	


(* PUBLIC FUNCTION DEFINITIONS *)

GetSchrodingerEquation[
	wavef_Symbol,
	potential_Function,
	time_Symbol,
	space__Symbol
] :=
	I D[wavef[space, time], time] ==
	-1/2 Laplacian[wavef[space, time], {space}] + 
	wavef[space, time] potential[space]
	
	

Options[FindEvolution] = {

	EvolutionEquation -> GetSchrodingerEquation
	
}

(* 1D *)
FindEvolution[
	wavef_,     (* one space dimension *)
	potential_, (* one space dimension *)
	domain_?domainOptSymbQ,
	duration_/;(NumericQ[duration] && Positive[duration]),
	options:OptionsPattern[{FindEvolution, NDSolveValue}]
] /; (
	isValid1DInput[wavef, domain] &&
	isValid1DInput[potential, domain]
) := 
	NDSolveValue[
		getEquations[
			convertToFunction[wavef, domain],
			convertToFunction[potential, domain],
			OptionValue[EvolutionEquation], 
			convertToEndPoints[domain]
		],
		internalWavef,
		{internalSpace1, Sequence @@ convertToEndPoints[domain]},
		{internalTime, 0, duration},
		
		Evaluate @ extractOptions[options, NDSolveValue]
	]
			
(* 2D *)
FindEvolution[
	wavef_,     (* two space dimensions *)
	potential_, (* two space dimensions *)
	xDomain_?domainOptSymbQ,
	yDomain_?domainOptSymbQ,
	duration_/;(NumericQ[duration] && Positive[duration]),
	options:OptionsPattern[{FindEvolution, NDSolveValue}]
] /; (
	isValid2DInput[wavef, xDomain, yDomain] &&
	isValid2DInput[potential, xDomain, yDomain]
) := 
	NDSolveValue[
		getEquations[
			convertToFunction[wavef, xDomain, yDomain],
			convertToFunction[potential, xDomain, yDomain],
			OptionValue[EvolutionEquation], 
			convertToEndPoints[xDomain],
			convertToEndPoints[yDomain]
		],
		internalWavef,
		{internalSpace1, Sequence @@ convertToEndPoints[xDomain]},
		{internalSpace2, Sequence @@ convertToEndPoints[yDomain]},
		{internalTime, 0, duration},
		
		Evaluate @ extractOptions[options, NDSolveValue]
	]



(*
	PUT INFO ABOUT PLOTEVOLUTOIN HERE 
*)
Options[PlotEvolution] = {

	NumberOfFrames -> 0,
	ShowCacheProgress -> True

	(* AutoNormalize -> False *)   (* TODO *)
};

(* 1D *)

(*
	passing time-dependent
*)
PlotEvolution[
	wavef_,     (* time-dependent, symbolic, one space dimension *)
	xDomain_?domainOptSymbQ,
	tDomain_?domainOptSymbQ, 
	options:OptionsPattern[{evolutionOptionFunctions, plotOptionFunctions1D} // Flatten]
] /; (
	isValid1DTimeDependentWavef[wavef, xDomain, tDomain] &&
	isValid1DPotentialOptions[options, xDomain]
) := 
	plot1DEvolution[
		convertToCurriedFunction[wavef, xDomain, tDomain],
		convertToFunction[OptionValue[Potential], xDomain],   (* throws an alert on bad options (once) *)
		xDomain, tDomain,
		options
	]

(*
	passing initial-state
*)
PlotEvolution[
	wavef_,     (* one space dimension *)
	potential_, (* one space dimension *)
	domain_?domainOptSymbQ,
	duration_/;(NumericQ[duration] && Positive[duration]),
	options:OptionsPattern[{FindEvolution, NDSolveValue, evolutionOptionFunctions, plotOptionFunctions1D} // Flatten]
] /; (
	isValid1DInput[wavef, domain] &&
	isValid1DInput[potential, domain]
) := 
	PlotEvolution[
		FindEvolution[
			wavef, 
			potential, 
			domain, 
			duration, 
			Evaluate @ extractOptions[options, {FindEvolution, NDSolveValue}]
		],
		domain,
		{0, duration},
		
		(* pass user-given over-riding options *)
		Evaluate @ extractOptions[options, {evolutionOptionFunctions, plotOptionFunctions1D}],
		
		(* otherwise plot potential *)
		Potential -> potential
	]
	



(* 2D *)

(* 2D PLOTEVOLUTION of INTERPOLATING FUNCTION without CACHING always ABORTS: some random over-sampling of interpolation???? *)


(*
	passing time-dependent
*)
PlotEvolution[
	wavef_,     (* time-dependent, symbolic, two space dimensions *)
	xDomain_?domainOptSymbQ,
	yDomain_?domainOptSymbQ,
	tDomain_?domainOptSymbQ, 
	options:OptionsPattern[{evolutionOptionFunctions, plotOptionFunctions2D} // Flatten]
] /; (
	isValid2DTimeDependentWavef[wavef, xDomain, yDomain, tDomain] &&
	isValid2DPotentialOptions[options, xDomain, yDomain]
) := 
	plot2DEvolution[
		convertToCurriedFunction[wavef, xDomain, yDomain, tDomain],
		convertToFunction[OptionValue[Potential], xDomain, yDomain],   (* throws an alert on bad options (once) *)
		xDomain, yDomain, tDomain,
		options
	]
	
(*
	passing initial-state
*)
PlotEvolution[
	wavef_,     (* two space dimensions *)
	potential_, (* two space dimensions *)
	xDomain_?domainOptSymbQ,
	yDomain_?domainOptSymbQ,
	duration_/;(NumericQ[duration] && Positive[duration]),
	options:OptionsPattern[{FindEvolution, NDSolveValue, evolutionOptionFunctions, plotOptionFunctions2D} // Flatten]
] /; (
	isValid2DInput[wavef, xDomain, yDomain] &&
	isValid2DInput[potential, xDomain, yDomain]
) := 
	PlotEvolution[
		FindEvolution[
			wavef, 
			potential, 
			xDomain,
			yDomain,
			duration, 
			Evaluate @ extractOptions[options, {FindEvolution, NDSolveValue}]
		],
		xDomain,
		yDomain,
		{0, duration},
		
		(* pass user-given over-riding options *)
		Evaluate @ extractOptions[options, {evolutionOptionFunctions, plotOptionFunctions2D}],
		
		(* otherwise plot potential *)
		Potential -> potential
	]
