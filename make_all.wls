#!/usr/bin/env wolframscript
(* Rebuilds everything: cached data, the notebook and the PNG snapshots.
   Run from the repository root:   wolframscript -file make_all.wls *)
root = DirectoryName[$InputFileName];
If[root === "", root = Directory[]];
SetDirectory[root];
Get["SMEFTADMExplorer.m"];

data = BuildADMData["Input files/SMEFT_beta_functions.m", "ADMData.wxf"];
(* explicit flavour at mu = M_t (needs Input files/ADM; about 80 s) *)
flavor = If[DirectoryQ["Input files/ADM"], BuildFlavorADMData["Input files/ADM", "FlavorADMData.wxf"], LoadFlavorADMData["FlavorADMData.wxf"]];

(* notebook that loads SMEFTADMExplorer.m and ADMData.wxf from its own folder *)
MakeExplorerNotebook["SMEFT_ADM_Explorer.nb", "EmbedData" -> False, "StartOperator" -> "cH"];

(* snapshots for the README *)
If[! DirectoryQ["snapshots"], CreateDirectory["snapshots"]];
views = {
  {"cH_generated_by_direct", "cH", "Incoming", "Direct"},
  {"cuH_generated_by_direct_56_neighbours", "cuH", "Incoming", "Direct"},
  {"cHWB_runs_into_one_loop_squared", "cHWB", "Outgoing", "OneLoopSquared"},
  {"cHWtB_runs_into_direct_unchecked_entries", "cHWtB", "Outgoing", "Direct"}};
UsingFrontEnd[
 Do[
  Module[{g = ExplorerGraphData[data, v[[2]], v[[3]], v[[4]], "ShowSelfRunning" -> True]},
   Export[FileNameJoin["snapshots", v[[1]] <> ".png"],
    SMEFTADMExplorer`Private`explorerGraphics[data, g, "Radial", "ShowSelf" -> True], ImageResolution -> 90]], {v, views}];
 (* side panel with a one-loop and a two-loop entry *)
 Export[FileNameJoin["snapshots", "entry_panel_cHq2_to_cHq1.png"],
  Rasterize[Panel[SMEFTADMExplorer`Private`entryPanel[data, ExplorerGraphData[data, "cHq1", "Incoming", "Direct"], {"cHq2", "cHq1", 1}, False], ImageSize -> 440], ImageResolution -> 100]];
 (* the same for an entry whose two-loop part is not cross-checked (Table 3 of the paper) *)
 SMEFTADMExplorer`Private`$explorerData = data;
 Export[FileNameJoin["snapshots", "entry_panel_cuG_to_cHGt_unchecked.png"],
  Rasterize[Panel[SMEFTADMExplorer`Private`entryPanel[data, ExplorerGraphData[data, "cuG", "Outgoing", "Direct"], {"cuG", "cHGt", 1}, False], ImageSize -> 440], ImageResolution -> 100]];
 (* explicit flavour: weighted family graph, focus view, flavour panels *)
 If[AssociationQ[flavor],
  SMEFTADMExplorer`Private`$flavorData = flavor;
  Export[FileNameJoin["snapshots", "cuH_generated_by_weighted_by_flavour.png"],
   SMEFTADMExplorer`Private`explorerGraphics[data, SMEFTADMExplorer`Private`weightFamilyEdges[ExplorerGraphData[data, "cuH", "Incoming", "Direct", "ShowSelfRunning" -> True], flavor, "FullUp", "Sum", 10.^-8], "Radial", "ShowSelf" -> True], ImageResolution -> 90];
  Export[FileNameJoin["snapshots", "cuH33_flavour_focus.png"],
   SMEFTADMExplorer`Private`explorerGraphics[data, SMEFTADMExplorer`Private`flavorFocusGraph[flavor, "cuH", {3, 3}, "Incoming", "FullUp", "Sum", 20, 10.^-8], "Radial", "ShowSelf" -> True], ImageResolution -> 90];
  Export[FileNameJoin["snapshots", "flavour_panel_cHG_to_cuH.png"],
   Rasterize[Panel[SMEFTADMExplorer`Private`flavorBlockPanel[flavor, "cHG", "cuH", "FullUp", "Sum", "", False, (Null &), {flavor["RowsOf"]["cuH"][[-1]], flavor["ColsOf"]["cHG"][[1]]}], ImageSize -> 440], ImageResolution -> 100]];
  Export[FileNameJoin["snapshots", "flavour_panel_cqu1_to_cuH.png"],
   Rasterize[Panel[SMEFTADMExplorer`Private`flavorBlockPanel[flavor, "cqu1", "cuH", "FullUp", "Sum", "", False, (Null &), None], ImageSize -> 440], ImageResolution -> 100]]]];
Print["done"];
