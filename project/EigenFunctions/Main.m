(* ::Package:: *)

Package["EigenFunctions`", 
	
	(* needs (for ShowSpectrum) *)
	{"PlotFunctions`"}
]


(* symbol exports *)

PackageExport[NumberOfModes]
PackageExport[NumberOfPoints]
PackageExport[TimeSteps]


(* function exports *)

PackageExport[ShowSpectrum]
PackageExport[NormaliseWavefunction]
PackageExport[GetLaplacianMatrix]
PackageExport[GetPotentialMatrix]
PackageExport[GetHamiltonianMatrix]
PackageExport[GetEigenmodes]
PackageExport[Discretise]


(* public functions *)

ShowSpectrum::usage = 
	"ShowSpectrum[potential, domain] finds and displays the eigenfunctions of the given potential"

NormaliseWavefunction::usage =
	"NormaliseWavefunction[psi, grid] L2 normalises list psi over grid"
	
NormaliseWavefunction::usage =
	"NormaliseWavefunction[psi, {xL, xR}] L2 normalises list psi over domain x \[Element] [xL, xR]"

GetLaplacianMatrix::usage =
	"GetLaplacianMatrix[grid] returns a 1D, finite-difference, Laplacian matrix with the dimensionality/spacing of grid"

GetPotentialMatrix::usage =
	"GetPotentialMatrix[potential] restructures a discretised potential list into a diagonal matrix"
			
GetHamiltonianMatrix::usage =
	"GetHamiltonianMatrix[potential, grid] returns potential's corresponding Hamiltonian matrix, on grid"

GetEigenmodes::usage =
	"GetEigenmodes[potential, domain] returns the family of {eigenvals, eigenfunctions} associated with the given potential"	

Discretise::usage = 
	"Discretise[potential, domain] returns discretised {grid, potential} lists with NumberOfPoints grid-points"


(* function definitions *)


(* times and prints an expression's evaluation *)
TimeEigFlag = False;
SetAttributes[EchoTiming, HoldFirst]
EchoTiming[expr_, label_:"Timing"] := 
	If[
		TimeEigFlag,
		Last @ Echo[AbsoluteTiming @ expr, label <> " ", First],
		expr
	]



NormaliseWavefunction[psi_List, grid_] :=
	With[
		{gridSpace = grid[[2]] - grid[[1]]},
		psi / Sqrt[gridSpace Total[ Abs[psi]^2 ]]
	]
	
NormaliseWavefunction[psi_List, {xL_, xR_}] :=
	With[
		{gridSpace = (xR - xL)/(Length[psi]-1)},
		psi / Sqrt[gridSpace Total[ Abs[psi]^2 ]]
	]


ShowSpectrum[potential_, domain_, args___] :=

	DynamicModule[
		{\[Lambda], \[Phi], eigArgs, plotArgs},
		
		(* distinguish options between eig and plot functions *)
		eigArgs = Sequence[FilterRules[{args}, Options[GetEigenmodes]]];
		plotArgs = FilterRules[{args}, Options[PlotWavefunction]];
		plotArgs = Sequence[If[
		
			(* if plot options doesn't already specify a Potential ... *)
			MemberQ[plotArgs, Potential -> _],
			plotArgs,
			Join[plotArgs, {Potential -> 
				If[
					(* set Potential to the passed non-symbolic potential *)
					NumericQ[domain[[1]]],
					potential,
					
					(* replace symbolic dependence with pure function *)
					Function @@ {domain[[1]], potential}
				]}
			]
		]];
		
		(* diagonalise ... *)
		{\[Lambda], \[Phi]} = GetEigenmodes[potential, domain, eigArgs];
		
		(* and plot eigenmodes *)
		Manipulate[
			PlotWavefunction[
				\[Phi][[n]],
				
				(* filtering out any variables in domain *)
				If[
					Length[domain] === 3,
					{domain[[2]], domain[[3]]},
					{domain[[1]], domain[[-1]]}
				],
				plotArgs
			],
			{n, 1, Length[\[Lambda]], 1}
		]
	]


GetLaplacianMatrix[grid_] :=

	With[
		{
			numPoints = Length[grid],
			gridSpace = grid[[2]] - grid[[1]]
		},
		SparseArray[
			{
				(* 4th order accuracy of 1D finite difference 2nd-deriv *)
				{i_,i_} :> -5/2,
				{i_,j_} /; Abs[i-j] == 1 :> 4/3,
				{i_,j_} /; Abs[i-j] == 2 :> -1/12 
			},
			{numPoints, numPoints}
		]
		/ gridSpace^2
	]
		 

GetPotentialMatrix[potential_] :=

	(* scalar potentials are diagonal *)
	With[
		{numPoints = Length[potential]},
		SparseArray[
			{i_, i_} :> potential[[i]],
			{numPoints, numPoints}
		]
	]
	(* shouldn't this function just be a call to Diagonal? *)

GetHamiltonianMatrix[potential_, grid_] :=

	(* single-particle, non-dimensional, inertial, kinetic energy + potential *)
	-(1/2) GetLaplacianMatrix[grid] +  GetPotentialMatrix[potential]



Options[GetEigenmodes] = {

	"NumberOfModes" -> 10,
	"NumberOfPoints" -> 100,   (* overrides Discretise default *)
	
	"TimeSteps" -> False

}

GetEigenmodes[potential_, domain_, OptionsPattern[]] :=

	(* will assume NumberOfPoints < numerical points in grid *)
	Module[
		{x, H, V, \[Lambda], \[Phi]},
		
		(* re-instate timing and printing (if true) *)
		TimeEigFlag = OptionValue["TimeSteps"];
		
		(* get discretised grid and potential lists *)
		{x, V} = EchoTiming[
			Discretise[
				potential, 
				domain, 
				NumberOfPoints -> OptionValue["NumberOfPoints"]
			],
			"discretising potential"];
		
		(* fabricate high potential at end-points to enhance stability *)
		V[[1]] = V[[-1]] = 10^5;
		
		(* build the Hamiltonian matrix *)
		H = EchoTiming[
			GetHamiltonianMatrix[V, x], 
			"building H matrix"];
		
		(* diagonalise the matrix *)
		{\[Lambda], \[Phi]} = EchoTiming[
			Eigensystem[N[H], -OptionValue["NumberOfModes"]],
			"diagonalising H matrix"];

		(* reorder eigenfunctions by increasing energy eigenvalue *)
		{\[Lambda], \[Phi]} = {\[Lambda][[#]], \[Phi][[#]]}& @ Ordering[\[Lambda]];
		
		(* normalise eigenfunction lists *)
		{\[Lambda], \[Phi]} = EchoTiming[
			{\[Lambda], NormaliseWavefunction[#, x]& /@ \[Phi]},
			"normalising eigenfunctions"];
		
		(* disable timing and printing *)
		TimeEigFlag = False;
		
		(* return *)
		{\[Lambda], \[Phi]}
	]
	
	
	
Options[Discretise] = {

	"NumberOfPoints" -> 100
}

Discretise[potential_, domain_, OptionsPattern[]] :=

	(* method of discretisation depends on given potential and domain format *)
	Which[
		Head[potential] === InterpolatingFunction || Head[potential] === Function,
		processFunctionalPotential[potential, domain, OptionValue["NumberOfPoints"]],
			
		Head[potential] === List,
		processListPotential[potential, domain, OptionValue["NumberOfPoints"]],
				
		True,
		processSymbolicPotential[potential, domain, OptionValue["NumberOfPoints"]]
	]

		
processFunctionalPotential[potential_, {xL_, ___, xR_}, numPoints_] :=

	(* apply function over grid *)
	With[
		{grid = Array[(#&), numPoints, {xL, xR}]},
		{grid, potential /@ grid}
	]
	
	
processListPotential[potential_, {xL_, ___, xR_}, numPoints_] :=

	Module[
		{grid, gridV},
		grid = Array[#&, numPoints, {xL, xR}];
		gridV =
			If[
				(* skip if we've already got a numPoints array *)
				Length[potential] === numPoints,
				potential,
		
				(* otherwise interpolate and resample list *)
				ListInterpolation[potential, {{xL, xR}}] @ grid
			];
		{grid, gridV}
	]
	
	
		
processSymbolicPotential[potential_, {x_, xL_, xR_}, numPoints_] :=

	(* change symbol into function *)
	processFunctionalPotential[
		Function @@ {x, potential},
		{xL, xR},
		numPoints
	]

processSymbolicPotential[potential_, {xL_, xR_}, numPoints_] :=	

	(* not supplying a var is valid, when potential is a constant *)
	processSymbolicPotential[potential, {dummyvar, xL, xR}, numPoints]

	
