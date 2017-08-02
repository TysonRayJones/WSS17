(* ::Package:: *)

BeginPackage["EigenFunctions`"]
EndPackage[]

(* All loading of the paclet's Wolfram Language code should go through this file. *)

(* Developer maintains this list of symbols.
   autoloadSymbols must agree with the symbols listed in the Kernel extension in the PacletInfo.m file.
*)

Map[
    (Unprotect[#];ClearAll[#]) &, {
        "EigenFunctions`*",
        "EigenFunctions`*`*",
        "EigenFunctions`*`*`*"
    }
]

PacletManager`Package`loadWolframLanguageCode[
    "EigenFunctions", 
    "EigenFunctions`", 
    DirectoryName[$InputFileName], 
    "Main.m",
    "AutoUpdate" -> True,
    "ForceMX" -> False, 
    "Lock" -> False,
    "AutoloadSymbols" -> {},
    "HiddenImports" -> {}
]
