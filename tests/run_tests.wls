#!/usr/bin/env wolframscript
(* Acceptance tests for the SMEFT ADM Explorer (roadmap Phase 7, the parts that can run headlessly).
   Run from the repository root:   wolframscript -file tests/run_tests.wls
   Needs ADMData.wxf (run BuildADMData first). Exit code 1 if any check fails. *)

root = DirectoryName[$InputFileName, 2];
If[root === "", root = Directory[]];
Get[FileNameJoin[{root, "SMEFTADMExplorer.m"}]];
rawFile = FileNameJoin[{root, "Input files", "SMEFT_beta_functions.m"}];
dataFile = FileNameJoin[{root, "ADMData.wxf"}];
tmp = CreateDirectory[];

failures = 0;
check[name_String, ok_, detail_ : ""] := (
  Print[If[TrueQ[ok], "[ OK ] ", "[FAIL] "], name, If[detail =!= "", "   " <> ToString[detail], ""]];
  If[! TrueQ[ok], failures++]);
P[name_String] := Symbol["SMEFTADMExplorer`Private`" <> name];

(* ---------------------------------------------------------------- loading *)
tLoad = First[AbsoluteTiming[d = SMEFTADMExplorer`LoadADMData[dataFile]]];
check["cache loads, < 2 s", AssociationQ[d] && tLoad < 2., Round[tLoad, 0.01]];
ops = d["Operators"];
check["59 operators", Length[ops] === 59, Length[ops]];
check["exactness check of the build passed", d["Checks", "ExactnessOK"]];
check["129 non-linear (Weinberg^2) terms excluded, none without WC", d["Excluded", "NonLinearTerms"] === 129 && d["Excluded", "PureSMTerms"] === 0];

(* ---------------------------------------------------------------- independent reading of the raw file *)
raw = Block[{$Context = "TestRaw`", $ContextPath = {"System`"}}, Get[rawFile]];
nm[s_] := SymbolName[s];
rawEdges = <|1 -> {}, 2 -> {}|>;
Do[
 With[{k = First[Select[Keys[raw], Head[#] =!= Power && SymbolName[Head[#]] === "Coupling" && nm[#[[1]]] === ci &]]},
  Do[
   Module[{wcs = Union[Cases[t, c_ /; MatchQ[Head[c], _Symbol] && SymbolName[Head[c]] === "Coupling" && MemberQ[ops, nm[c[[1]]]] :> nm[c[[1]]], {0, Infinity}]], L = Exponent[t, Symbol["TestRaw`hbar"]]},
    If[Length[wcs] === 1 && wcs[[1]] =!= ci && MemberQ[{1, 2}, L], AppendTo[rawEdges[L], {wcs[[1]], ci}]]],
   {t, If[Head[raw[k]] === Plus, List @@ raw[k], {raw[k]}]}]],
 {ci, ops}];
rawEdges = Map[Union, rawEdges];
check["edge sets equal an independent read of the raw file (1L)", rawEdges[1] === Sort[Keys[Select[d["Edges"], #["L1"] &]]], {Length[rawEdges[1]]}];
check["edge sets equal an independent read of the raw file (2L)", rawEdges[2] === Sort[Keys[Select[d["Edges"], #["L2"] &]]], {Length[rawEdges[2]]}];

(* ---------------------------------------------------------------- graph logic vs raw data, all operators *)
out1 = GroupBy[rawEdges[1], First -> Last]; in1 = GroupBy[rawEdges[1], Last -> First];
out12 = GroupBy[Union[rawEdges[1], rawEdges[2]], First -> Last]; in12 = GroupBy[Union[rawEdges[1], rawEdges[2]], Last -> First];
nbr[a_Association, op_] := Sort[Lookup[a, op, {}]];
graphOK = True; modeColorOK = True; oneLoopGreen = 0; newOK = True; nCfg = 0;
Do[
 Do[
  Module[{g1, g12, g2, expected1, expected12, A, second, exp2edges, newExp},
   g1 = ExplorerGraphData[d, op, dir, "OneLoop"];
   g12 = ExplorerGraphData[d, op, dir, "Direct"];
   g2 = ExplorerGraphData[d, op, dir, "OneLoopSquared"];
   nCfg += 3;
   expected1 = If[dir === "Outgoing", {op, #} & /@ nbr[out1, op], {#, op} & /@ nbr[in1, op]];
   expected12 = If[dir === "Outgoing", {op, #} & /@ nbr[out12, op], {#, op} & /@ nbr[in12, op]];
   graphOK = graphOK && Sort[{#["From"], #["To"]} & /@ g1["Edges"]] === Sort[expected1] && Sort[{#["From"], #["To"]} & /@ g12["Edges"]] === Sort[expected12];
   oneLoopGreen += Count[g1["Edges"], e_ /; e["Color"] === "Green"];
   modeColorOK = modeColorOK && AllTrue[g12["Edges"], (#["Color"] === "Red") === #["L1"] && (#["Color"] === "Green") === (! #["L1"] && #["L2"]) &];
   (* one-loop squared: two hops of one-loop edges; the trivial chain op -> X -> op is dropped *)
   A = If[dir === "Outgoing", nbr[out1, op], nbr[in1, op]];
   second = Flatten[Table[If[dir === "Outgoing", {a, #} & /@ nbr[out1, a], {#, a} & /@ nbr[in1, a]], {a, A}], 1];
   second = DeleteCases[second, e_ /; If[dir === "Outgoing", e[[2]], e[[1]]] === op];
   exp2edges = Union[expected1, second];
   graphOK = graphOK && Sort[{#["From"], #["To"]} & /@ g2["Edges"]] === Sort[exp2edges];
   newExp = Complement[Union[If[dir === "Outgoing", #[[2]] & /@ second, #[[1]] & /@ second]], A, {op}];
   newOK = newOK && Sort[g2["NewEndpoints"]] === Sort[newExp] && Intersection[g2["NewEndpoints"], A] === {}],
  {dir, {"Outgoing", "Incoming"}}], {op, ops}];
check["edge lists = direct queries on the raw data (3 modes x 2 directions x 59 operators = " <> ToString[nCfg] <> " graphs)", graphOK];
check["colouring agrees with the stored L1/L2 flags (red = one-loop, green = two-loop only)", modeColorOK];
check["1-loop mode never shows a green edge", oneLoopGreen === 0];
check["highlighted chain endpoints are absent among the direct one-loop neighbours", newOK];

(* ---------------------------------------------------------------- layouts: no collisions, no overlaps, timing *)
overlaps[cl_] := Module[{pos = cl["Layout"]["Pos"], sz = cl["Routing"]["Sizes"], o = Keys[cl["Layout"]["Pos"]], c = 0},
  Do[If[i < j, With[{a = pos[o[[i]]], b = pos[o[[j]]], sa = sz[o[[i]]], sb = sz[o[[j]]]},
     If[Abs[a[[1]] - b[[1]]] < (sa[[1]] + sb[[1]])/2 + 2 && Abs[a[[2]] - b[[2]]] < (sa[[2]] + sb[[2]])/2 + 2, c++]]], {i, Length[o]}, {j, Length[o]}]; c];
viol = 0; ovl = 0; times = {}; renderMsgs = 0; nRender = 0;
Do[
 Do[
  Do[
   Module[{g = ExplorerGraphData[d, op, dir, mode, "ShowSelfRunning" -> True], cl, t, r},
    t = First[AbsoluteTiming[cl = P["computeLayout"][d, g, "Radial"]]];
    AppendTo[times, {mode, t}];
    viol += cl["Routing"]["Violations"]; ovl += overlaps[cl];
    r = Quiet[Check[P["renderGraphics"][d, g, "Radial", "ShowSelf" -> True], $Failed, All::stop], All::stop];
    nRender++; If[r === $Failed || Head[r] =!= Association, renderMsgs++]],
   {mode, {"OneLoop", "Direct", "OneLoopSquared"}}], {dir, {"Outgoing", "Incoming"}}], {op, ops}];
check["every operator renders in all modes/directions without messages (" <> ToString[nRender] <> " views)", renderMsgs === 0, renderMsgs];
check["no arrow touches a node it does not start or end at (radial layout)", viol === 0, viol];
check["no two nodes overlap (radial layout)", ovl === 0, ovl];
Print["       layout time per view [s]: max one-loop ", Round[Max[Select[times, #[[1]] === "OneLoop" &][[All, 2]]], 0.01],
  ", max direct ", Round[Max[Select[times, #[[1]] === "Direct" &][[All, 2]]], 0.01],
  ", max chains ", Round[Max[Select[times, #[[1]] === "OneLoopSquared" &][[All, 2]]], 0.01],
  ", median chains ", Round[Median[Select[times, #[[1]] === "OneLoopSquared" &][[All, 2]]], 0.01]];
check["switching is fast: single-hop views < 0.5 s", Max[Select[times, #[[1]] =!= "OneLoopSquared" &][[All, 2]]] < 0.5];
(* spring layout on a few of the biggest graphs *)
springOK = True;
Do[Module[{g = ExplorerGraphData[d, op, dir, mode], cl}, cl = P["computeLayout"][d, g, "Spring"]; springOK = springOK && overlaps[cl] === 0],
 {op, {"cuH", "cdB", "cH"}}, {dir, {"Outgoing", "Incoming"}}, {mode, {"Direct", "OneLoopSquared"}}];
check["spring layout: no node overlaps on the largest graphs", springOK];
(* layouts are deterministic *)
g = ExplorerGraphData[d, "cHq1", "Outgoing", "Direct"];
a1 = P["springLayout"][g, P["getPrep"][d]]; a2 = P["springLayout"][g, P["getPrep"][d]];
check["layouts are deterministic", a1 === a2];

(* ---------------------------------------------------------------- spot checks of 5 edges against the raw beta functions *)
spot = {{"cHWB", "cHW", 1}, {"cHq2", "cHq1", 1}, {"cuW", "cuB", 1}, {"cdH", "cHD1", 2}, {"cquqd2", "cqq2", 2}};
spotOK = True;
Do[
 Module[{k = First[Select[Keys[raw], Head[#] =!= Power && SymbolName[Head[#]] === "Coupling" && nm[#[[1]]] === s[[2]] &]], terms, cnt},
  terms = If[Head[raw[k]] === Plus, List @@ raw[k], {raw[k]}];
  cnt = Count[terms, t_ /; Exponent[t, Symbol["TestRaw`hbar"]] === s[[3]] && ! FreeQ[t, c_ /; MatchQ[Head[c], _Symbol] && SymbolName[Head[c]] === "Coupling" && nm[c[[1]]] === s[[1]]]];
  Print["       raw beta of ", s[[2]], " has ", cnt, " loop-", s[[3]], " terms containing ", s[[1]], "; cache: ", d["Edges", {s[[1]], s[[2]]}, "NTerms" <> ToString[s[[3]]]]];
  spotOK = spotOK && cnt === d["Edges", {s[[1]], s[[2]]}, "NTerms" <> ToString[s[[3]]]]],
 {s, spot}];
check["5 spot-checked edges agree with the raw beta functions (term counts)", spotOK];

(* ---------------------------------------------------------------- notation: LaTeX compiles *)
allEdges = Keys[d["Edges"]];
SeedRandom[7];
sample = Join[RandomSample[allEdges, 60], {{"cquqd2", "cqq2"}, {"cuH", "cuH"}}];
texs = Flatten[Table[If[e[[1]] === e[[2]], {d["SelfRunning", e[[1]], "TeX1"], d["SelfRunning", e[[1]], "TeX2"]}, {d["Edges", e, "TeX1"], d["Edges", e, "TeX2"]}], {e, sample}]];
texs = Select[texs, StringQ];
doc = "\\documentclass[10pt]{article}\n\\usepackage{amsmath,geometry}\n\\geometry{a4paper,landscape,margin=0.5cm}\n\\begin{document}\n" <>
   StringRiffle["\\[ " <> # <> " \\]" & /@ texs, "\n"] <> "\n\\end{document}\n";
Export[FileNameJoin[{tmp, "t.tex"}], doc, "Text"];
If[Run["which pdflatex > /dev/null 2>&1"] === 0,
 Module[{rc = Run["cd " <> tmp <> " && pdflatex -interaction=nonstopmode -halt-on-error t.tex > t.log 2>&1"]},
  check["LaTeX of " <> ToString[Length[texs]] <> " sampled entries compiles with pdflatex", rc === 0 && FileExistsQ[FileNameJoin[{tmp, "t.pdf"}]]]],
 Print["[SKIP] pdflatex not available"]];
(* the TeX is also reproducible from Matchete-notation input *)
inp = d["Edges", {"cHWB", "cHW"}, "Input1"];
parsed = Block[{$Context = "SMEFTADMRaw`", $ContextPath = {"System`", "SMEFTADMRaw`"}}, ToExpression[inp, InputForm, HoldComplete]];
check["InputForm strings parse back to expressions (and give the same TeX)",
 ToTeXString[ReleaseHold[parsed]] === d["Edges", {"cHWB", "cHW"}, "TeX1"]];

(* ---------------------------------------------------------------- largest entry does not freeze the panel *)
big = Last[SortBy[Normal[d["Edges"]], #[[2, "NTerms2"]] &]];
g = ExplorerGraphData[d, big[[1, 2]], "Incoming", "Direct"];
tPanel = First[AbsoluteTiming[P["entryPanel"][d, g, {big[[1, 1]], big[[1, 2]], 1}, True]]];
check["largest entry (" <> ToString[big[[2, "NTerms2"]]] <> " terms, full expression) builds in < 1 s", tPanel < 1., Round[tPanel, 0.01]];

(* ---------------------------------------------------------------- click handlers must be self-contained *)
(* Regression test: handlers that refer to Module-local symbols stop working in the front end (the
   temporary symbols are removed once only the front end holds the graphic). *)
Module[{ui, gr, rules, tmpSyms, ok1, ok2, edgeRule, nodeRule, S},
 ui = SMEFTADMExplorer`SMEFTADMExplorer[d, "cH"];
 ui[[1]];
 gr = ReleaseHold[First[Select[Cases[ui, HoldPattern[Dynamic[a_, ___]] :> Hold[a], Infinity], MatchQ[#, Hold[_Module]] &]]];
 rules = Cases[gr, HoldPattern[RuleDelayed["MouseClicked", rhs_]] :> Hold[rhs], Infinity];
 tmpSyms = Union[Cases[rules, s_Symbol /; StringMatchQ[SymbolName[s], __ ~~ "$" ~~ DigitCharacter ..] && Context[s] === "SMEFTADMExplorer`Private`", Infinity]];
 tmpSyms = Select[tmpSyms, ! MemberQ[{"op$", "key$"}, StringReplace[SymbolName[#], DigitCharacter .. ~~ EndOfString -> ""]] &];
 check["click handlers contain no Module-local temporary symbols (" <> ToString[Length[rules]] <> " handlers)", Length[rules] > 0 && tmpSyms === {}, tmpSyms];
 S[n_] := Symbol["SMEFTADMExplorer`Private`" <> n];
 edgeRule = First[Select[rules, ! FreeQ[#, {_String, _String, _Integer}] &]];
 ReleaseHold[edgeRule];
 check["clicking an arrow selects its edge", MatchQ[S["selected"], {_String, _String, _Integer}], S["selected"]];
 nodeRule = First[Select[rules, FreeQ[#, {_String, _String, _Integer}] && ! MatchQ[#, Hold[_["cH"]]] &]];
 ReleaseHold[nodeRule];
 check["clicking an operator recenters the graph and records history", S["center"] =!= "cH" && S["history"] === {"cH"} && S["selected"] === None, S["center"]]];

(* ---------------------------------------------------------------- packaging *)
dd = FileNameJoin[{tmp, "ADMData.wxf"}]; CopyFile[dataFile, dd];
nbEmb = SMEFTADMExplorer`MakeExplorerNotebook[FileNameJoin[{tmp, "emb.nb"}], "EmbedData" -> True, "DataFile" -> dd];
nbLin = SMEFTADMExplorer`MakeExplorerNotebook[FileNameJoin[{tmp, "lnk.nb"}], "EmbedData" -> False, "DataFile" -> dd];
check["notebooks are generated (embedded " <> ToString[Round[FileByteCount[nbEmb]/10.^6, 0.01]] <> " MB, linked " <> ToString[Round[FileByteCount[nbLin]/10.^3, 1]] <> " kB)", FileExistsQ[nbEmb] && FileExistsQ[nbLin]];
(* run the loader of the embedded notebook in a fresh kernel *)
nbExpr = Import[nbEmb, "NB"];
loadSrc = First[Cases[nbExpr, Cell[s_String, "Input", ___] /; StringContainsQ[s, "Uncompress"] :> s, Infinity]];
Export[FileNameJoin[{tmp, "load.wl"}], loadSrc <> "\nPrint[Length[SMEFTADMExplorer`Private`$notebookData[\"Operators\"]]];", "Text"];
out = RunProcess[{"wolframscript", "-file", FileNameJoin[{tmp, "load.wl"}]}];
check["embedded loader works in a fresh kernel (no package file needed)", StringTrim[out["StandardOutput"]] === "59", StringTrim[out["StandardOutput"]]];

Print["\n", If[failures === 0, "ALL CHECKS PASSED", ToString[failures] <> " CHECK(S) FAILED"]];
DeleteDirectory[tmp, DeleteContents -> True];
Exit[If[failures === 0, 0, 1]];
