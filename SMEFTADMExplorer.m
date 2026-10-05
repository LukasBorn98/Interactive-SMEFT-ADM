(* ::Package:: *)

(* SMEFT ADM Explorer
   Interactive graph explorer for the anomalous dimension matrix of the dimension-six SMEFT
   in the Mainz basis (Born, Fuentes-Martin, Thomsen, "Next-to-Leading Order Running in the SMEFT").

   Public entry points
     BuildADMData[betaFile, outFile]          one-time preprocessing of SMEFT_beta_functions.m
     SMEFTADMExplorer[dataFile, startOp]      interactive explorer (needs the Mathematica front end)
     MakeExplorerNotebook[nbFile, opts]       generate the stand-alone notebook

   The package does not need Matchete. The raw beta functions are read in a private context, and the
   explorer only ever touches the cached data (ADMData.wxf).

   Conventions
     * beta = dC/d(log mu); hbar = 1/(16 Pi^2); one-loop terms ~ hbar, two-loop terms ~ hbar^2.
     * Operator IDs are the Matchete keys of the input file ("cHq1", "cG", ...); the labels follow the
       Mainz basis (parallel / crossed contractions are written (||) and (x)).
     * Edge Cj -> Ci exists if beta_Ci contains a term linear in Cj or in its conjugate.
*)

BeginPackage["SMEFTADMExplorer`"];

BuildADMData::usage = "BuildADMData[betaFile, outFile] reads the SMEFT beta functions (Matchete notation) and writes the cached ADM data to outFile (WXF). Returns the data association. Without outFile only the association is returned.";
SMEFTADMExplorer::usage = "SMEFTADMExplorer[dataFile] returns the interactive ADM explorer. SMEFTADMExplorer[dataFile, op] starts centered on operator op (e.g. \"cH\"). The first argument may also be the data association itself.";
MakeExplorerNotebook::usage = "MakeExplorerNotebook[nbFile, \"EmbedData\" -> True|False] generates the stand-alone explorer notebook.";
LoadADMData::usage = "LoadADMData[file] reads a cached ADM data file.";
ToCleanExpression::usage = "ToCleanExpression[expr] converts a beta-function expression in Matchete notation into a typeset (TraditionalForm-ready) expression.";
ToTeXString::usage = "ToTeXString[expr] converts a beta-function expression in Matchete notation into a LaTeX string.";
ADMDataSummary::usage = "ADMDataSummary[data] prints the sanity-check summary of the cached data.";
ExplorerGraphData::usage = "ExplorerGraphData[data, center, direction, mode, opts] returns vertices, edges and metadata of the explorer graph (no UI).";
EdgeExpressionHeld::usage = "EdgeExpressionHeld[data, {src, tgt}, L] reconstructs the raw (Matchete notation) contribution as HoldComplete expression in the private context SMEFTADMRaw`.";
$SMEFTADMExplorerDirectory::usage = "Directory of the package file.";

Begin["`Private`"];

$SMEFTADMExplorerDirectory = If[StringQ[$InputFileName] && $InputFileName =!= "", DirectoryName[$InputFileName], Directory[]];

(* ================================================================================================
   1. Operator table (Mainz basis)
   ================================================================================================ *)

(* {ID (= Matchete key), class, subscript spec, superscript spec}
   subscript spec: one character per symbol, "~X" = X with tilde, "l" = script ell
   superscript spec: None | "par" (parallel contraction) | "cross" (crossed contraction)            *)
$operatorSpecs = {
  {"cG", "X3", "G", None}, {"cGt", "X3", "~G", None}, {"cW", "X3", "W", None}, {"cWt", "X3", "~W", None},
  {"cH", "H6", "H", None},
  {"cHD1", "H4D2", "HD", "par"}, {"cHD2", "H4D2", "HD", "cross"},
  {"ceH", "psi2H3", "eH", None}, {"cuH", "psi2H3", "uH", None}, {"cdH", "psi2H3", "dH", None},
  {"cHG", "X2H2", "HG", None}, {"cHGt", "X2H2", "H~G", None}, {"cHW", "X2H2", "HW", None},
  {"cHWt", "X2H2", "H~W", None}, {"cHB", "X2H2", "HB", None}, {"cHBt", "X2H2", "H~B", None},
  {"cHWB", "X2H2", "HWB", None}, {"cHWtB", "X2H2", "H~WB", None},
  {"ceW", "psi2XH", "eW", None}, {"ceB", "psi2XH", "eB", None}, {"cuG", "psi2XH", "uG", None},
  {"cuW", "psi2XH", "uW", None}, {"cuB", "psi2XH", "uB", None}, {"cdG", "psi2XH", "dG", None},
  {"cdW", "psi2XH", "dW", None}, {"cdB", "psi2XH", "dB", None},
  {"cHl1", "psi2H2D", "Hl", "par"}, {"cHl2", "psi2H2D", "Hl", "cross"}, {"cHe", "psi2H2D", "He", None},
  {"cHq1", "psi2H2D", "Hq", "par"}, {"cHq2", "psi2H2D", "Hq", "cross"}, {"cHu", "psi2H2D", "Hu", None},
  {"cHd", "psi2H2D", "Hd", None}, {"cHud", "psi2H2D", "Hud", None},
  {"cll", "LLLL", "ll", None}, {"cqq1", "LLLL", "qq", "par"}, {"cqq2", "LLLL", "qq", "cross"},
  {"clq1", "LLLL", "lq", "par"}, {"clq2", "LLLL", "lq", "cross"},
  {"cee", "RRRR", "ee", None}, {"cuu", "RRRR", "uu", None}, {"cdd", "RRRR", "dd", None},
  {"ceu", "RRRR", "eu", None}, {"ced", "RRRR", "ed", None}, {"cud1", "RRRR", "ud", "par"},
  {"cud2", "RRRR", "ud", "cross"},
  {"cle", "LLRR", "le", None}, {"clu", "LLRR", "lu", None}, {"cld", "LLRR", "ld", None},
  {"cqe", "LLRR", "qe", None}, {"cqu1", "LLRR", "qu", "par"}, {"cqu2", "LLRR", "qu", "cross"},
  {"cqd1", "LLRR", "qd", "par"}, {"cqd2", "LLRR", "qd", "cross"},
  {"cledq", "LRRL", "ledq", None}, {"cquqd1", "LRRL", "quqd", "par"}, {"cquqd2", "LRRL", "quqd", "cross"},
  {"clequ", "LRRL", "lequ", None}, {"cluqe", "LRRL", "luqe", None}};

(* {class ID, plain-text name, TeX name} in the order used for layouts and legends *)
$classTable = {
  {"X3", "X^3", "X^3"}, {"H6", "H^6", "H^6"}, {"H4D2", "H^4 D^2", "H^4D^2"},
  {"psi2H3", "\[Psi]^2 H^3", "\\psi^2H^3"}, {"X2H2", "X^2 H^2", "X^2H^2"},
  {"psi2XH", "\[Psi]^2 X H", "\\psi^2XH"}, {"psi2H2D", "\[Psi]^2 H^2 D", "\\psi^2H^2D"},
  {"LLLL", "(LL)(LL)", "(\\bar LL)(\\bar LL)"}, {"RRRR", "(RR)(RR)", "(\\bar RR)(\\bar RR)"},
  {"LLRR", "(LL)(RR)", "(\\bar LL)(\\bar RR)"},
  {"LRRL", "(LR)(RL)+(LR)(LR)", "(\\bar LR)(\\bar RL)+(\\bar LR)(\\bar LR)"}};

$operatorIDs = $operatorSpecs[[All, 1]];

(* tokens of a subscript spec *)
subTokens[spec_String] := StringCases[spec, ("~" ~~ _) | _];

subTokenTeX[tok_String] := Which[tok === "l", "\\ell ", StringStartsQ[tok, "~"], "\\tilde{" <> StringDrop[tok, 1] <> "}", True, tok];
subTokenBox[tok_String] := Which[tok === "l", "\[ScriptL]", StringStartsQ[tok, "~"], OverscriptBox[StringDrop[tok, 1], "~"], True, tok];

subTeX[spec_String] := StringJoin[subTokenTeX /@ subTokens[spec]];
subBox[spec_String] := Module[{groups = Split[subTokens[spec], ! (StringStartsQ[#1, "~"] || StringStartsQ[#2, "~"]) &], parts},
  parts = Map[If[StringStartsQ[First[#], "~"], subTokenBox[First[#]], StringJoin[subTokenBox /@ #]] &, groups];
  StyleBox[If[Length[parts] === 1, First[parts], RowBox[parts]], FontSlant -> "Plain", SingleLetterItalics -> False]];
supTeX[None] := "";
supTeX["par"] = "(\\parallel)";
supTeX["cross"] = "(\\times)";
supBox[None] := None;
supBox["par"] = "(\[DoubleVerticalBar])";
supBox["cross"] = "(\[Times])";

$opSpec = Association[#[[1]] -> # & /@ $operatorSpecs];
$opTeX = Association[#[[1]] -> ("C_{" <> subTeX[#[[3]]] <> "}" <> If[#[[4]] === None, "", "^{" <> supTeX[#[[4]]] <> "}"]) & /@ $operatorSpecs];

opLabelBoxes[id_String] := Module[{s = $opSpec[id], c = StyleBox["C", FontSlant -> "Italic"]},
  If[s[[4]] === None, SubscriptBox[c, subBox[s[[3]]]], SubsuperscriptBox[c, subBox[s[[3]]], supBox[s[[4]]]]]];

(* the label that is stored in the cache: TraditionalForm-ready *)
opLabelExpr[id_String] := RawBoxes[opLabelBoxes[id]];

(* ================================================================================================
   2. Reading the raw beta functions (isolated context, no Matchete needed)
   ================================================================================================ *)

$rawContext = "SMEFTADMRaw`";

readRawBetas[file_String] := Block[{$Context = $rawContext, $ContextPath = {"System`"}, $Messages = {}},
  Get[file]];

(* ================================================================================================
   3. Context-free representation (IR) of a beta-function term
   ================================================================================================
   A term is an Association
     <|"Coeff" -> number, "Hbar" -> loop order, "Factors" -> {factor, ...}|>
   with factors
     <|"Type" -> "WC"|"Gauge"|"Yukawa"|"Quartic"|"Mass"|"Delta", "Name" -> string, "Pow" -> integer,
       "Idx" -> {flavor index numbers}, "Bar" -> True|False|>
   Everything is keyed on symbol *names*, so it does not matter in which context the symbols live
   (Global`, Matchete`, SMEFTADMRaw`).                                                                  *)

symName[s_Symbol] := SymbolName[s];
symName[_] := None;

$gaugeNames = {"gY", "gL", "gs"};
$yukawaNames = {"Yu", "Yd", "Ye"};

couplingType[nm_String] := Which[
  MemberQ[$gaugeNames, nm], "Gauge",
  MemberQ[$yukawaNames, nm], "Yukawa",
  nm === "\[Lambda]", "Quartic",
  nm === "\[Mu]2", "Mass",
  KeyExistsQ[$opSpec, nm] || nm === "cllHH", "WC",
  True, "Other"];

idxNum[e_] := If[Length[e] === 2 && symName[Head[e]] === "Index" && symName[e[[1]]] =!= None,
  ToExpression[StringDelete[symName[e[[1]]], "d$$"]], Throw[$Failed, "parse"]];

parseFactor[f_] := Module[{base = f, pow = 1, bar = False, h, nm},
  If[Head[base] === Power && IntegerQ[base[[2]]], pow = base[[2]]; base = base[[1]]];
  If[symName[Head[base]] === "Bar" && Length[base] === 1, bar = True; base = base[[1]]];
  h = symName[Head[base]];
  Which[
   h === "Coupling" && Length[base] === 3 && symName[base[[1]]] =!= None,
   nm = symName[base[[1]]];
   <|"Type" -> couplingType[nm], "Name" -> nm, "Pow" -> pow, "Idx" -> (idxNum /@ base[[2]]), "Bar" -> bar|>,
   h === "Delta" && !bar,
   <|"Type" -> "Delta", "Name" -> "Delta", "Pow" -> pow, "Idx" -> (idxNum /@ List @@ base), "Bar" -> False|>,
   True, Throw[$Failed, "parse"]]];

factorSortKey[fc_] := {
  Switch[fc["Type"], "Gauge", 1, "Quartic", 2, "Mass", 2, "Yukawa", 3, "Delta", 4, "WC", 5, _, 6],
  Switch[fc["Name"], "gY", 1, "gL", 2, "gs", 3, "Yu", 1, "Yd", 2, "Ye", 3, _, 4],
  If[fc["Bar"], 0, 1], fc["Idx"]};

(* returns the IR term, or $Failed *)
parseTerm[t_] := Catch[Module[{facs = If[Head[t] === Times, List @@ t, {t}], coeff = 1, hb = 0, parsed = {}},
   Do[
    Which[
     NumberQ[f], coeff *= f,
     symName[f] === "hbar", hb += 1,
     Head[f] === Power && symName[f[[1]]] === "hbar" && IntegerQ[f[[2]]], hb += f[[2]],
     True, AppendTo[parsed, parseFactor[f]]],
    {f, facs}];
   <|"Coeff" -> coeff, "Hbar" -> hb, "Factors" -> SortBy[parsed, factorSortKey]|>], "parse"];

wcPower[term_Association] := Total[#["Pow"] & /@ Select[term["Factors"], #["Type"] === "WC" &]];

(* SM-coupling monomial of a term: sorted {name, bar, power} triples, flavor indices stripped *)
termMonomial[term_Association] := Cases[term["Factors"],
   fc_ /; MemberQ[{"Gauge", "Quartic", "Yukawa", "Mass"}, fc["Type"]] :> {fc["Name"], fc["Bar"], fc["Pow"]}];

termSortKey[term_Association] := {
  Count[term["Factors"], fc_ /; fc["Type"] === "Yukawa"],
  termMonomial[term],
  Cases[term["Factors"], fc_ /; fc["Type"] === "WC" :> {fc["Name"], fc["Bar"], fc["Idx"]}],
  Cases[term["Factors"], fc_ /; fc["Type"] === "Delta" :> fc["Idx"]],
  term["Coeff"] // ReIm};

(* ================================================================================================
   4. Notation layer: LaTeX and typeset (boxes) output
   ================================================================================================ *)

$freeLetters = {"p", "r", "s", "t"};
$dummyLetters = {"u", "v", "w", "x", "y", "z"};

(* free indices occur once in a term, summed (dummy) indices twice *)
indexNames[factors_List] := Module[{tally, free, dummy},
  tally = Tally[Flatten[#["Idx"] & /@ factors]];
  free = Sort[Select[tally, #[[2]] === 1 &][[All, 1]]];
  dummy = Sort[Select[tally, #[[2]] =!= 1 &][[All, 1]]];
  Association[
   MapIndexed[#1 -> If[First[#2] <= 4, $freeLetters[[First[#2]]], "f" <> ToString[First[#2]]] &, free],
   MapIndexed[#1 -> If[First[#2] <= 6, $dummyLetters[[First[#2]]], "k" <> ToString[First[#2]]] &, dummy]]];

(* --- coefficient --- *)
(* returns {sign (+1/-1), magnitude (Rational/Integer, or complex magnitude info), isImaginary} *)
coeffParts[c_] := Which[
  Head[c] === Complex && PossibleZeroQ[Re[c]], {Sign[Im[c]], Abs[Im[c]], "imag"},
  Head[c] === Complex, {1, c, "general"},
  True, {Sign[c], Abs[c], "real"}];

coeffTeX[mag_, kind_, hasFactors_] := Which[
  kind === "general", "\\left(" <> ToString[Re[mag]] <> If[Im[mag] >= 0, "+", "-"] <> ToString[Abs[Im[mag]]] <> "\\,i\\right)",
  kind === "imag",
   If[mag === 1, "i",
    If[IntegerQ[mag], ToString[mag] <> "\\,i", "\\frac{" <> ToString[Numerator[mag]] <> "\\,i}{" <> ToString[Denominator[mag]] <> "}"]],
  mag === 1, If[hasFactors, "", "1"],
  IntegerQ[mag], ToString[mag],
  True, "\\frac{" <> ToString[Numerator[mag]] <> "}{" <> ToString[Denominator[mag]] <> "}"];

coeffBox[mag_, kind_, hasFactors_] := Which[
  kind === "general", RowBox[{"(", ToString[Re[mag]], If[Im[mag] >= 0, "+", "-"], ToString[Abs[Im[mag]]], StyleBox["i", FontSlant -> "Italic"], ")"}],
  kind === "imag",
   With[{ii = StyleBox["i", FontSlant -> "Italic"]},
    If[mag === 1, ii,
     If[IntegerQ[mag], RowBox[{ToString[mag], ii}],
      FractionBox[RowBox[{ToString[Numerator[mag]], ii}], ToString[Denominator[mag]]]]]],
  mag === 1, If[hasFactors, None, "1"],
  IntegerQ[mag], ToString[mag],
  True, FractionBox[ToString[Numerator[mag]], ToString[Denominator[mag]]]];

(* --- factors --- *)
idxString[idx_List, names_Association] := StringJoin[Lookup[names, idx, "?"]];

joinSup[parts_List] := StringRiffle[Select[parts, # =!= "" &], "\\,"];

factorTeX[fc_Association, names_Association] := Module[{pw = fc["Pow"], idx = idxString[fc["Idx"], names], star = If[fc["Bar"], "*", ""], nm = fc["Name"], base, pstr = ""},
  If[pw =!= 1, pstr = "^{" <> ToString[pw] <> "}"];
  Switch[fc["Type"],
   "Gauge", "g_{" <> StringDrop[nm, 1] <> "}" <> pstr,
   "Quartic", "\\lambda" <> pstr,
   "Mass", "\\mu^2" <> pstr,
   "Yukawa",
   base = "Y_{" <> StringDrop[nm, 1] <> "}^{" <> joinSup[{star, idx}] <> "}";
   If[pw === 1, base, "\\left(" <> base <> "\\right)" <> pstr],
   "WC",
   base = If[KeyExistsQ[$opSpec, nm],
     With[{s = $opSpec[nm]}, "C_{" <> subTeX[s[[3]]] <> "}^{" <> joinSup[{supTeX[s[[4]]], star, idx}] <> "}"],
     "C_{\\mathrm{" <> nm <> "}}^{" <> joinSup[{star, idx}] <> "}"];
   base = StringReplace[base, "^{}" -> ""];
   If[pw === 1, base, "\\left(" <> base <> "\\right)" <> pstr],
   "Delta", "\\delta^{" <> idx <> "}" <> pstr,
   _, "\\mathrm{" <> nm <> "}"]];

factorBox[fc_Association, names_Association] := Module[{pw = fc["Pow"], idx = idxString[fc["Idx"], names], star = If[fc["Bar"], "*", ""], nm = fc["Name"], base, ital, supParts},
  ital[s_String] := StyleBox[s, FontSlant -> "Italic"];
  supParts[lst_List] := With[{l = Select[lst, # =!= "" && # =!= None &]}, Switch[Length[l], 0, None, 1, First[l], _, RowBox[Riffle[l, "\[ThinSpace]"]]]];
  base = Switch[fc["Type"],
    "Gauge", SubscriptBox[ital["g"], ital[StringDrop[nm, 1]]],
    "Quartic", ital["\[Lambda]"],
    "Mass", SuperscriptBox[ital["\[Mu]"], "2"],
    "Yukawa", With[{sp = supParts[{star, ital[idx]}]}, SubsuperscriptBox[ital["Y"], ital[StringDrop[nm, 1]], sp]],
    "WC", With[{s = Lookup[$opSpec, nm, None]},
     With[{sp = supParts[{If[s === None, None, supBox[s[[4]]]], star, ital[idx]}],
       sb = If[s === None, nm, subBox[s[[3]]]]},
      If[sp === None, SubscriptBox[ital["C"], sb], SubsuperscriptBox[ital["C"], sb, sp]]]],
    "Delta", SuperscriptBox["\[Delta]", ital[idx]],
    _, nm];
  If[pw === 1, base,
   If[MemberQ[{"Gauge", "Quartic", "Mass"}, fc["Type"]],
    Switch[Head[base], SubscriptBox, SubsuperscriptBox[base[[1]], base[[2]], ToString[pw]], _, SuperscriptBox[base, ToString[pw]]],
    SuperscriptBox[RowBox[{"(", base, ")"}], ToString[pw]]]]];

(* --- single terms: {sign, body string} / {sign, body box} --- *)
termTeXParts[term_Association] := Module[{names = indexNames[term["Factors"]], facs, sgn, mag, kind, cs, body},
  {sgn, mag, kind} = coeffParts[term["Coeff"]];
  facs = StringRiffle[factorTeX[#, names] & /@ term["Factors"], " "];
  cs = coeffTeX[mag, kind, term["Factors"] =!= {}];
  body = StringRiffle[Select[{cs, facs}, # =!= "" &], " "];
  {sgn, If[body === "", "1", body]}];

termBoxParts[term_Association, skip_List : {}] := Module[{names = indexNames[term["Factors"]], sgn, mag, kind, cb, fb, shownFactors},
  {sgn, mag, kind} = coeffParts[term["Coeff"]];
  shownFactors = Select[term["Factors"], ! MemberQ[skip, #["Type"]] &];
  cb = coeffBox[mag, kind, shownFactors =!= {}];
  fb = factorBox[#, names] & /@ shownFactors;
  {sgn, With[{items = Select[Join[{cb}, fb], # =!= None &]},
    If[items === {}, "1", RowBox[Riffle[items, "\[ThinSpace]"]]]]}];

loopPrefactorTeX[L_Integer] := If[L === 1, "\\frac{1}{16\\pi^2}", "\\frac{1}{(16\\pi^2)^{" <> ToString[L] <> "}}"];
loopPrefactorBox[L_Integer] := FractionBox["1", If[L === 1, RowBox[{"16", SuperscriptBox["\[Pi]", "2"]}],
    SuperscriptBox[RowBox[{"(", RowBox[{"16", SuperscriptBox["\[Pi]", "2"]}], ")"}], ToString[L]]]];

sortedTerms[terms_List] := SortBy[terms, termSortKey];

(* LaTeX of an entry: prefactor (16 pi^2)^(-L) pulled out front, the hbar^L stripped from the terms *)
entryTeX[terms_List, L_Integer] := Module[{ts = sortedTerms[terms], parts, strs, lines, cur = "", all, single},
  parts = termTeXParts /@ ts;
  strs = MapIndexed[
    Function[{p, i}, If[First[i] === 1, If[p[[1]] < 0, "-", ""], If[p[[1]] < 0, " - ", " + "]] <> p[[2]]], parts];
  single = StringJoin[strs];
  If[Length[ts] === 0, Return["0"]];
  If[StringLength[single] <= 110,
   loopPrefactorTeX[L] <> "\\left(" <> single <> "\\right)",
   (* break into lines of ~100 characters *)
   lines = {}; cur = "";
   Do[If[cur =!= "" && StringLength[cur] + StringLength[s] > 100, AppendTo[lines, cur]; cur = s, cur = cur <> s], {s, strs}];
   AppendTo[lines, cur];
   "\\begin{aligned}\n& " <> loopPrefactorTeX[L] <> "\\Big(" <> StringRiffle[lines, "\\\\\n& \\quad "] <> "\\Big)\n\\end{aligned}"]];

(* typeset lines of an entry: a list of RowBoxes (to be stacked in a Column) *)
entryBoxLines[terms_List, L_Integer, termsPerLine_Integer : 3] := Module[{ts = sortedTerms[terms], parts, items, chunks, n},
  If[Length[ts] === 0, Return[{"0"}]];
  parts = termBoxParts /@ ts;
  items = MapIndexed[
    Function[{p, i}, Sequence @@ If[First[i] === 1, If[p[[1]] < 0, {"-", p[[2]]}, {p[[2]]}], {If[p[[1]] < 0, "-", "+"], p[[2]]}]], parts];
  (* chunk by terms *)
  chunks = Partition[Table[
     Flatten[{If[i === 1, If[parts[[i, 1]] < 0, {"-"}, {}], {If[parts[[i, 1]] < 0, "-", "+"]}], {parts[[i, 2]]}}, 1], {i, Length[parts]}],
    UpTo[termsPerLine]];
  n = Length[chunks];
  MapIndexed[
   Function[{ch, i}, Module[{row = Riffle[Flatten[ch, 1], "\[ThinSpace]"]},
     RowBox[Join[
       If[First[i] === 1, {loopPrefactorBox[L], "(", "\[ThinSpace]"}, {"\[ThinSpace]"}],
       row,
       If[First[i] === n, {"\[ThinSpace]", ")"}, {}]]]]],
   chunks]];

(* --- public converters (accept Matchete-notation expressions in any context) --- *)
toTerms[expr_] := Module[{ts = If[Head[expr] === Plus, List @@ expr, {expr}], parsed},
  parsed = parseTerm /@ ts;
  If[MemberQ[parsed, $Failed], Message[ToTeXString::parse]; $Failed, parsed]];
ToTeXString::parse = "The expression contains objects other than Matchete couplings, Bar and Delta.";

inferLoop[terms_List] := Module[{l = Union[#["Hbar"] & /@ terms]}, If[Length[l] === 1 && l[[1]] >= 1, l[[1]], 1]];

ToTeXString[expr_] := Module[{ts = If[ListQ[expr] && AllTrue[expr, AssociationQ], expr, toTerms[expr]]},
  If[ts === $Failed, $Failed, entryTeX[ts, inferLoop[ts]]]];

ToCleanExpression[expr_] := Module[{ts = If[ListQ[expr] && AllTrue[expr, AssociationQ], expr, toTerms[expr]], L, lines},
  If[ts === $Failed, Return[$Failed]];
  L = inferLoop[ts];
  lines = entryBoxLines[ts, L];
  Style[If[Length[lines] === 1, RawBoxes[First[lines]], Column[RawBoxes /@ lines, Left, 0.4]], SingleLetterItalics -> False]];

(* coupling monomials: list of {name, bar, power} -> typeset / TeX *)
monomialBoxes[mono_List] := If[mono === {}, "1",
   RowBox[Riffle[factorBox[<|"Type" -> couplingType[#[[1]]], "Name" -> #[[1]], "Pow" -> #[[3]], "Idx" -> {}, "Bar" -> False|>, <||>] & /@ mono, "\[ThinSpace]"]]];

monomialDisplayBoxes[mono_List] := If[mono === {}, "1",
   RowBox[Riffle[
     Map[Function[m, Module[{nm = m[[1]], pw = m[[3]], core},
        core = Switch[couplingType[nm],
          "Gauge", SubscriptBox[StyleBox["g", FontSlant -> "Italic"], StyleBox[StringDrop[nm, 1], FontSlant -> "Italic"]],
          "Quartic", StyleBox["\[Lambda]", FontSlant -> "Italic"],
          "Yukawa", If[m[[2]], SubsuperscriptBox[StyleBox["Y", FontSlant -> "Italic"], StyleBox[StringDrop[nm, 1], FontSlant -> "Italic"], "\[Dagger]"],
            SubscriptBox[StyleBox["Y", FontSlant -> "Italic"], StyleBox[StringDrop[nm, 1], FontSlant -> "Italic"]]],
          _, nm];
        If[pw === 1, core,
         Switch[Head[core], SubscriptBox, SubsuperscriptBox[core[[1]], core[[2]], ToString[pw]],
          SubsuperscriptBox, SubsuperscriptBox[core[[1]], core[[2]], RowBox[{"\[Dagger]", ToString[pw]}]], _, SuperscriptBox[core, ToString[pw]]]]]],
      mono], "\[ThinSpace]"]]];

monomialTeX[mono_List] := If[mono === {}, "1",
   StringRiffle[Map[Function[m, Module[{nm = m[[1]], pw = m[[3]], core},
        core = Switch[couplingType[nm],
          "Gauge", "g_{" <> StringDrop[nm, 1] <> "}", "Quartic", "\\lambda",
          "Yukawa", "Y_{" <> StringDrop[nm, 1] <> "}" <> If[m[[2]], "^{\\dagger}", ""], _, nm];
        If[pw === 1, core, If[m[[2]], StringReplace[core, "^{\\dagger}" -> "^{\\dagger " <> ToString[pw] <> "}"],
           If[StringContainsQ[core, "_"], core <> "^{" <> ToString[pw] <> "}", core <> "^{" <> ToString[pw] <> "}"]]]]], mono], " "]];

(* ================================================================================================
   5. BuildADMData
   ================================================================================================ *)

rawKeyName[k_] := Which[
  symName[Head[k]] === "Coupling", symName[k[[1]]],
  symName[Head[k]] === "Bar" && symName[Head[k[[1]]]] === "Coupling", symName[k[[1, 1]]],
  Head[k] === Power && symName[Head[k[[1]]]] === "Coupling", symName[k[[1, 1]]] <> "^" <> ToString[k[[2]]],
  True, "?"];
rawKeyIsBar[k_] := symName[Head[k]] === "Bar";

rawInputString[expr_] := Block[{$Context = $rawContext, $ContextPath = {"System`", $rawContext}},
  ToString[expr, InputForm, CharacterEncoding -> "ASCII", PageWidth -> Infinity]];

Options[BuildADMData] = {"Verbose" -> True};

BuildADMData[betaFile_String, outFile_ : None, OptionsPattern[]] := Module[
  {raw, keys, res, ops = $operatorIDs, byName, nonWCKeys, missing, groups = <||>, selfGroups = <||>, excludedByOp = <||>,
   pureSM = 0, nonLinear = 0, parseFailed = 0, edges = <||>, self = <||>, opInfo, nonHermitian, nIdx, data, t0 = AbsoluteTime[],
   unknownNames = {}, checkResults = {}, mkEntry, verbose = OptionValue["Verbose"], termsOf, srcFile, hash},
  srcFile = FileNameTake[betaFile];
  If[verbose, Print["Reading ", betaFile, " ..."]];
  raw = readRawBetas[betaFile];
  If[!AssociationQ[raw], Message[BuildADMData::input, betaFile]; Return[$Failed]];
  keys = Keys[raw];
  byName = Association[Cases[keys, k_ /; !rawKeyIsBar[k] :> (rawKeyName[k] -> k)]];
  missing = Complement[ops, Keys[byName]];
  If[missing =!= {}, Message[BuildADMData::missing, missing]; Return[$Failed]];
  nonHermitian = Intersection[ops, rawKeyName /@ Select[keys, rawKeyIsBar]];
  nonWCKeys = rawInputString /@ Select[keys, ! MemberQ[ops, rawKeyName[#]] || rawKeyIsBar[#] &];
  termsOf[b_] := If[Head[b] === Plus, List @@ b, {b}];
  (* ---- term selection *)
  Do[
   Module[{ts = termsOf[raw[byName[ci]]]},
    Scan[
     Function[t,
      Module[{ir = parseTerm[t], nwc, src, L},
       If[ir === $Failed, parseFailed++; Return[Null, Module]];
       nwc = wcPower[ir];
       Which[
        nwc === 0, pureSM++,
        nwc >= 2, nonLinear++; excludedByOp[ci] = Lookup[excludedByOp, ci, 0] + 1,
        True,
        src = First[Select[ir["Factors"], #["Type"] === "WC" &]];
        L = ir["Hbar"];
        If[! MemberQ[{1, 2}, L], Message[BuildADMData::loop, ci, L]; Return[Null, Module]];
        If[! KeyExistsQ[$opSpec, src["Name"]], (* e.g. the Weinberg operator appearing linearly *)
         unknownNames = Union[unknownNames, {src["Name"]}]; Return[Null, Module]];
        With[{key = {src["Name"], ci, L}},
         groups[key] = Append[Lookup[groups, Key[key], {}], <|"IR" -> ir, "Raw" -> t, "Bar" -> src["Bar"]|>]]]]],
     ts]],
   {ci, ops}];
  If[parseFailed > 0, Message[BuildADMData::parse, parseFailed]; Return[$Failed]];
  If[unknownNames =!= {}, Message[BuildADMData::unknown, unknownNames]];
  (* ---- assemble per-edge records *)
  mkEntry[grp_List] := Module[{irs = grp[[All, "IR"]], rawTerms = grp[[All, "Raw"]], L, sorted, bars, rawSum},
    sorted = sortedTerms[irs];
    rawSum = Plus @@ rawTerms;
    <|"Terms" -> sorted, "NTerms" -> Length[sorted], "Input" -> rawInputString[rawSum], "Size" -> LeafCount[rawSum],
      "Couplings" -> SortBy[DeleteDuplicates[termMonomial /@ sorted], {Length[#], #} &],
      "Bars" -> DeleteDuplicates[grp[[All, "Bar"]]]|>];
  Module[{pairs = DeleteDuplicates[{#[[1]], #[[2]]} & /@ Keys[groups]], rec},
   Do[
    Module[{g1 = Lookup[groups, Key[{pr[[1]], pr[[2]], 1}], {}], g2 = Lookup[groups, Key[{pr[[1]], pr[[2]], 2}], {}], e1, e2, allBars, via, entry},
     e1 = If[g1 === {}, None, mkEntry[g1]];
     e2 = If[g2 === {}, None, mkEntry[g2]];
     allBars = Union[Flatten[{If[e1 === None, {}, e1["Bars"]], If[e2 === None, {}, e2["Bars"]]}]];
     via = Switch[allBars, {False}, "No", {True}, "Yes", _, "Both"];
     entry = <|
       "L1" -> (e1 =!= None), "L2" -> (e2 =!= None),
       "Terms1" -> If[e1 === None, Missing[], e1["Terms"]], "Terms2" -> If[e2 === None, Missing[], e2["Terms"]],
       "Input1" -> If[e1 === None, Missing[], e1["Input"]], "Input2" -> If[e2 === None, Missing[], e2["Input"]],
       "TeX1" -> If[e1 === None, Missing[], entryTeX[e1["Terms"], 1]], "TeX2" -> If[e2 === None, Missing[], entryTeX[e2["Terms"], 2]],
       "Couplings1" -> If[e1 === None, {}, e1["Couplings"]], "Couplings2" -> If[e2 === None, {}, e2["Couplings"]],
       "ViaConjugate" -> via,
       "ViaConjugate1" -> If[e1 === None, Missing[], Switch[e1["Bars"], {False}, "No", {True}, "Yes", _, "Both"]],
       "ViaConjugate2" -> If[e2 === None, Missing[], Switch[e2["Bars"], {False}, "No", {True}, "Yes", _, "Both"]],
       "Size1" -> If[e1 === None, 0, e1["Size"]], "Size2" -> If[e2 === None, 0, e2["Size"]],
       "NTerms1" -> If[e1 === None, 0, e1["NTerms"]], "NTerms2" -> If[e2 === None, 0, e2["NTerms"]]|>;
     If[pr[[1]] === pr[[2]], self[pr[[1]]] = entry, edges[pr] = entry]],
    {pr, pairs}]];
  (* operators without any self-running term still get a record *)
  Do[If[! KeyExistsQ[self, op], self[op] = <|"L1" -> False, "L2" -> False|>], {op, ops}];
  (* ---- exactness check: extracted + excluded terms reproduce beta_i *)
  checkResults = Table[
    Module[{orig = termsOf[raw[byName[ci]]], extracted, nExcl, irNL},
     extracted = Flatten[Lookup[groups, Key[{#, ci, #2}], {}] & @@@ Tuples[{ops, {1, 2}}]];
     irNL = Select[orig, With[{ir = parseTerm[#]}, ir =!= $Failed && wcPower[ir] =!= 1] &];
     {ci, Sort[Join[extracted[[All, "Raw"]], irNL]] === Sort[orig], Length[orig], Length[irNL]}],
    {ci, ops}];
  (* ---- operator info *)
  opInfo = Association[Table[
     With[{s = $opSpec[op]},
      op -> <|"Label" -> opLabelExpr[op], "TeX" -> $opTeX[op], "Class" -> s[[2]], "MatcheteSymbol" -> op,
        "Hermitian" -> ! MemberQ[nonHermitian, op], "NIndices" -> 0|>],
     {op, ops}]];
  (* number of flavor indices from the key *)
  Do[opInfo[op, "NIndices"] = If[symName[Head[byName[op]]] === "Coupling", Length[byName[op][[2]]], 0], {op, ops}];
  data = <|
    "Meta" -> <|"Source" -> srcFile, "BuildDate" -> DateString[{"Year", "-", "Month", "-", "Day"}],
      "Convention" -> "beta = dC/d(log mu); hbar = 1/(16 Pi^2) per loop; Mainz basis (Matchete model SMEFT_Mainz); flavor indices explicit, repeated index = sum",
      "LoopMarker" -> "hbar", "Basis" -> "Mainz", "FormatVersion" -> 1,
      "SourceHash" -> IntegerString[Hash[ReadByteArray[betaFile], "SHA256"], 16],
      "BuildSeconds" -> Round[AbsoluteTime[] - t0, 0.1]|>,
    "Operators" -> ops,
    "Classes" -> $classTable,
    "OperatorInfo" -> opInfo,
    "Edges" -> edges,
    "SelfRunning" -> self,
    "Excluded" -> <|"NonLinearTerms" -> nonLinear, "NonLinearByOperator" -> excludedByOp, "PureSMTerms" -> pureSM,
      "NonWCKeys" -> nonWCKeys|>,
    "Checks" -> <|"ExactnessOK" -> AllTrue[checkResults, #[[2]] &], "ExactnessFailures" -> Select[checkResults, ! #[[2]] &][[All, 1]]|>|>;
  If[outFile =!= None,
   Module[{stream = OpenWrite[outFile, BinaryFormat -> True]},
    BinaryWrite[stream, Normal[BinarySerialize[data, PerformanceGoal -> "Size"]]];
    Close[stream]];
   If[verbose, Print["Wrote ", outFile, " (", ToString[NumberForm[FileByteCount[outFile]/10.^6, {4, 2}]], " MB)"]]];
  If[verbose, ADMDataSummary[data]];
  data];

BuildADMData::input = "Could not read an Association of beta functions from `1`.";
BuildADMData::missing = "Operators missing in the input file: `1`";
BuildADMData::parse = "`1` terms could not be parsed (unknown objects).";
BuildADMData::loop = "Operator `1`: term with loop order `2` (expected 1 or 2).";
BuildADMData::unknown = "Terms linear in unexpected couplings were ignored: `1`";

LoadADMData[file_String] := BinaryDeserialize[ReadByteArray[file]];

EdgeExpressionHeld[data_Association, {src_String, tgt_String}, L_Integer] := Module[{inp},
  inp = If[src === tgt, data["SelfRunning", tgt, "Input" <> ToString[L]], data["Edges", {src, tgt}, "Input" <> ToString[L]]];
  If[MissingQ[inp], Missing[], Block[{$Context = $rawContext, $ContextPath = {"System`", $rawContext}}, ToExpression[inp, InputForm, HoldComplete]]]];

(* ---- summary / sanity checks ---- *)
ADMDataSummary[data_Association] := Module[{ed = data["Edges"], e1, e2, sr = data["SelfRunning"], ex = data["Excluded"]},
  e1 = Select[ed, #["L1"] &]; e2 = Select[ed, #["L2"] &];
  Print["---- ADM data summary ----"];
  Print["Source: ", data["Meta", "Source"], "   built ", data["Meta", "BuildDate"], "   (", data["Meta", "BuildSeconds"], " s)"];
  Print["Nodes (operators): ", Length[data["Operators"]]];
  Print["Off-diagonal edges: one-loop ", Length[e1], ", two-loop ", Length[e2], ", two-loop only ", Count[ed, e_ /; ! e["L1"] && e["L2"]],
    ", one-loop only ", Count[ed, e_ /; e["L1"] && ! e["L2"]]];
  Print["Self-running (diagonal) nonzero: one-loop ", Count[sr, s_ /; s["L1"]], ", two-loop ", Count[sr, s_ /; s["L2"]]];
  Print["Excluded: non-linear terms ", ex["NonLinearTerms"], " (by operator: ", Length[ex["NonLinearByOperator"]], " betas), terms without WC ",
    ex["PureSMTerms"], ", non-operator keys ", Length[ex["NonWCKeys"]]];
  Print["Exactness check (extracted + excluded = original beta, all operators): ", If[data["Checks", "ExactnessOK"], "PASSED", "FAILED for " <> ToString[data["Checks", "ExactnessFailures"]]]];
  Print["Largest entry: ", Last[SortBy[Normal[ed], #[[2, "NTerms2"]] &]][[1]], " with ", Max[#["NTerms2"] & /@ Values[ed]], " two-loop terms"];
  ];


(* ================================================================================================
   6. Graph logic (pure functions, no UI)
   ================================================================================================ *)

$prepCache = <||>;

getPrep[data_Association] := Module[{h = data["Meta", "SourceHash"] <> data["Meta", "BuildDate"]},
  If[! KeyExistsQ[$prepCache, h], $prepCache[h] = buildPrep[data]];
  $prepCache[h]];

buildPrep[data_Association] := Module[{ed = data["Edges"], ops = data["Operators"], l1, l12, classes = data["Classes"][[All, 1]]},
  l1 = Keys[Select[ed, #["L1"] &]];
  l12 = Keys[ed];
  <|"Ops" -> ops, "OpPos" -> AssociationThread[ops, Range[Length[ops]]],
   "Class" -> Association[# -> data["OperatorInfo", #, "Class"] & /@ ops],
   "ClassPos" -> AssociationThread[classes, Range[Length[classes]]],
   "Out1" -> Association[# -> Cases[l1, {#, t_} :> t] & /@ ops],
   "In1" -> Association[# -> Cases[l1, {s_, #} :> s] & /@ ops],
   "Out12" -> Association[# -> Cases[l12, {#, t_} :> t] & /@ ops],
   "In12" -> Association[# -> Cases[l12, {s_, #} :> s] & /@ ops]|>];

$graphCache = <||>;

Options[ExplorerGraphData] = {"ShowSelfRunning" -> False, "ExcludeTrivialChains" -> True, "HiddenClasses" -> {}};

(* Operator IDs may be given as "cHq1" or "CHq1" *)
normalizeOpID[data_Association, op_String] := Which[
  MemberQ[data["Operators"], op], op,
  StringLength[op] > 0 && MemberQ[data["Operators"], "c" <> StringDrop[op, 1]] && StringTake[op, 1] === "C", "c" <> StringDrop[op, 1],
  True, Message[ExplorerGraphData::op, op]; Throw[$Failed, "badop"]];
ExplorerGraphData::op = "Unknown operator `1`.";

ExplorerGraphData[data_Association, center0_String, direction_String, mode_String, OptionsPattern[]] :=
 Catch[Module[{center = normalizeOpID[data, center0], prep = getPrep[data], hidden = Sort[OptionValue["HiddenClasses"]],
    excl = TrueQ[OptionValue["ExcludeTrivialChains"]], key, res},
   key = {data["Meta", "SourceHash"], center, direction, mode, excl, hidden};
   If[! KeyExistsQ[$graphCache, key], $graphCache[key] = computeGraph[data, prep, center, direction, mode, excl, hidden]];
   res = $graphCache[key];
   If[TrueQ[OptionValue["ShowSelfRunning"]],
    Append[res, "Self" -> data["SelfRunning", center]], Append[res, "Self" -> None]]], "badop"];

computeGraph[data_, prep_, center_, dir_, mode_, excl_, hidden_] := Module[
  {outgoing = (dir === "Outgoing"), keepQ, hop, farEnd, first, second = {}, A, B, newEnd = {}, edgeList, mkEdge, verts, depth, edges, classPos = prep["ClassPos"], opPos = prep["OpPos"], cls = prep["Class"]},
  keepQ[op_] := op === center || ! MemberQ[hidden, cls[op]];
  farEnd[e_] := If[outgoing, e[[2]], e[[1]]];
  hop[op_, which_] := Select[
    If[outgoing, {op, #} & /@ prep["Out" <> which, op], {#, op} & /@ prep["In" <> which, op]], keepQ[farEnd[#]] &];
  Switch[mode,
   "OneLoop", first = hop[center, "1"],
   "Direct", first = hop[center, "12"],
   "OneLoopSquared",
   first = hop[center, "1"];
   A = farEnd /@ first;
   second = Flatten[hop[#, "1"] & /@ A, 1];
   If[excl, second = DeleteCases[second, e_ /; farEnd[e] === center]];
   B = DeleteDuplicates[farEnd /@ second];
   newEnd = Complement[B, A, {center}];
   newEnd = SortBy[newEnd, {classPos[cls[#]], opPos[#]} &],
   _, Message[ExplorerGraphData::mode, mode]; Throw[$Failed, "badop"]];
  mkEdge[{f_, t_}, hopNo_] := With[{ed = data["Edges", {f, t}]},
    <|"From" -> f, "To" -> t, "L1" -> ed["L1"], "L2" -> ed["L2"], "Hop" -> hopNo,
     "Color" -> If[mode === "Direct", If[ed["L1"], "Red", "Green"], "Red"]|>];
  edges = DeleteDuplicatesBy[Join[mkEdge[#, 1] & /@ first, mkEdge[#, 2] & /@ second], {#["From"], #["To"]} &];
  A = DeleteDuplicates[farEnd /@ first];
  depth = Association[center -> 0];
  Scan[(depth[#] = 1) &, A];
  Scan[If[! KeyExistsQ[depth, #], depth[#] = 2] &, DeleteDuplicates[Flatten[{#["From"], #["To"]} & /@ edges]]];
  verts = SortBy[Keys[depth], {depth[#], classPos[cls[#]], opPos[#]} &];
  <|"Center" -> center, "Direction" -> dir, "Mode" -> mode, "Vertices" -> verts, "Depth" -> depth,
   "Edges" -> SortBy[edges, {#["Hop"], classPos[cls[farEnd[#]]], opPos[farEnd[#]], opPos[#["From"]]} &],
   "NewEndpoints" -> newEnd, "NRed" -> Count[edges, e_ /; e["Color"] === "Red"], "NGreen" -> Count[edges, e_ /; e["Color"] === "Green"]|>];
ExplorerGraphData::mode = "Unknown mode `1`.";

(* ================================================================================================
   7. Layout and edge routing (pure geometry)
   ================================================================================================ *)

$nodeSize = {58., 26.};
$centerSize = {104., 52.};
$nodeMargin = 6.;

boxOf[p_, {w_, h_}, m_ : 0.] := {p[[1]] - w/2 - m, p[[2]] - h/2 - m, p[[1]] + w/2 + m, p[[2]] + h/2 + m};

angDiff[a_, b_] := Abs[Mod[a - b + Pi, 2 Pi] - Pi];

samplePolyline[pts_, step_ : 6.] := Module[{segs = Partition[pts, 2, 1]},
  Append[Flatten[Table[
     With[{a = s[[1]], b = s[[2]]}, With[{n = Max[1, Ceiling[Norm[b - a]/step]]}, Table[a + (b - a) t/n, {t, 0, n - 1}]]], {s, segs}], 1], Last[pts]]];

catmullRom[pts_, nPer_ : 14] := Module[{n = Length[pts], m},
  m = Table[Which[i == 1, pts[[2]] - pts[[1]], i == n, pts[[n]] - pts[[n - 1]], True, (pts[[i + 1]] - pts[[i - 1]])/2], {i, n}];
  Append[Flatten[Table[
     Table[With[{t = k/nPer}, (2 t^3 - 3 t^2 + 1) pts[[i]] + (t^3 - 2 t^2 + t) m[[i]] + (-2 t^3 + 3 t^2) pts[[i + 1]] + (t^3 - t^2) m[[i + 1]]], {k, 0, nPer - 1}],
     {i, n - 1}], 1], Last[pts]]];

bezierPts[a_, b_, k_, n_ : 24] := Module[{d = b - a, len = Norm[b - a], ctrl},
  ctrl = (a + b)/2 + k len {-d[[2]], d[[1]]}/len;
  Table[With[{t = i/n}, (1 - t)^2 a + 2 (1 - t) t ctrl + t^2 b], {i, 0, n}]];

(* number of (sample point, box) overlaps; boxes: {{x1,y1,x2,y2}, ...} *)
hitCount[samp_, bm_] := If[bm === {} || samp === {}, 0, Module[{x = samp[[All, 1]], y = samp[[All, 2]]},
    Total[UnitStep[Outer[Subtract, x, bm[[All, 1]]]] UnitStep[Outer[Plus, -x, bm[[All, 3]]]] UnitStep[Outer[Subtract, y, bm[[All, 2]]]] UnitStep[Outer[Plus, -y, bm[[All, 4]]]], 2]]];

(* --- ring counts and radial layout --- *)
apportion[n_, w_] := Module[{c = Round[n w/Total[w]]}, c[[-1]] += n - Total[c]; c];
ringCounts[n_Integer] := Which[n <= 0, {}, n <= 14, {n}, n <= 30, apportion[n, {1, 2}], True, apportion[n, {1, 2, 3}]];

(* distribute an ordered list over rings so that every ring gets a proportional share of every class *)
interleave[sorted_List, counts_List] := Module[{assigned = ConstantArray[0, Length[counts]], rings = Table[{}, Length[counts]], k},
  Do[
   k = First[Ordering[Table[If[assigned[[m]] >= counts[[m]], Infinity, (assigned[[m]] + 0.5)/counts[[m]]], {m, Length[counts]}]]];
   AppendTo[rings[[k]], node]; assigned[[k]]++, {node, sorted}];
  rings];

radialLayout[graph_Association, prep_Association] := Module[
  {center = graph["Center"], verts = graph["Vertices"], depth = graph["Depth"], classPos = prep["ClassPos"], cls = prep["Class"], opPos = prep["OpPos"],
   bands, allRings = {}, nRings, radii = {}, prev = 0., sp, r, pos = <||>, ringOf = <||>, angleOf = <||>, sorter},
  sorter[op_] := {classPos[cls[op]], opPos[op]};
  bands = Table[SortBy[Select[verts, depth[#] === d &], sorter], {d, 1, 2}];
  Do[If[band =!= {}, allRings = Join[allRings, interleave[band, ringCounts[Length[band]]]]], {band, bands}];
  nRings = Length[allRings];
  Do[
   sp = If[nRings === 1, 80., If[k < nRings, 96., 76.]];
   r = Max[If[k === 1, 130., prev + 88.], Length[allRings[[k]]] sp/(2 Pi)];
   AppendTo[radii, r]; prev = r, {k, nRings}];
  pos[center] = {0., 0.};
  Do[
   With[{n = Length[allRings[[k]]], off = If[EvenQ[k], Pi/Length[allRings[[k]]], 0.]},
    Do[
     With[{op = allRings[[k, j]], th = Pi/2 - 2 Pi (j - 0.5)/n - off},
      pos[op] = radii[[k]] {Cos[th], Sin[th]}; ringOf[op] = k; angleOf[op] = Mod[th, 2 Pi]], {j, n}]], {k, nRings}];
  <|"Pos" -> pos, "RingOf" -> ringOf, "AngleOf" -> angleOf, "Radii" -> radii, "RingNodes" -> allRings|>];

(* --- Fruchterman-Reingold ("Spring") layout, deterministic: starts from the radial layout --- *)
springLayout[graph_Association, prep_Association] := Module[
  {rad = radialLayout[graph, prep], verts = graph["Vertices"], center = graph["Center"], n, ci, p, idx, edges, k = 105., iter = 160,
   X, Y, dx, dy, d2, disp, delta, dist, f, t, ovl, dirx, diry, norm},
  n = Length[verts];
  ci = First[First[Position[verts, center]]];
  idx = AssociationThread[verts, Range[n]];
  p = N[Lookup[rad["Pos"], verts]] 0.8;
  edges = Map[{idx[#["From"]], idx[#["To"]]} &, graph["Edges"]];
  Do[
   t = 40. (1 - it/iter) + 1.;
   X = p[[All, 1]]; Y = p[[All, 2]];
   dx = Outer[Subtract, X, X]; dy = Outer[Subtract, Y, Y];
   d2 = dx^2 + dy^2 + IdentityMatrix[n];
   disp = Transpose[{Total[dx k^2/d2, {2}], Total[dy k^2/d2, {2}]}];
   Do[
    delta = p[[e[[1]]]] - p[[e[[2]]]]; dist = Max[Norm[delta], 0.01];
    f = dist^2/k/6.;
    disp[[e[[1]]]] = disp[[e[[1]]]] - delta/dist f; disp[[e[[2]]]] = disp[[e[[2]]]] + delta/dist f, {e, edges}];
   Do[
    dist = Max[Norm[disp[[i]]], 0.01];
    p[[i]] = p[[i]] + disp[[i]]/dist Min[dist, t], {i, n}];
   p = p - ConstantArray[p[[ci]], n], {it, iter}];
  (* push overlapping boxes apart *)
  Do[
   X = p[[All, 1]]; Y = p[[All, 2]];
   dx = Outer[Subtract, X, X]; dy = Outer[Subtract, Y, Y];
   ovl = UnitStep[$nodeSize[[1]] + 10. - Abs[dx]] UnitStep[$nodeSize[[2]] + 10. - Abs[dy]] (1. - IdentityMatrix[n]);
   norm = Sqrt[dx^2 + dy^2] + IdentityMatrix[n];
   dirx = dx/norm + 0.3 (1. - IdentityMatrix[n]) Sign[Outer[Subtract, Range[n], Range[n]]] UnitStep[0.1 - Sqrt[dx^2 + dy^2]];
   diry = dy/norm;
   p[[All, 1]] = X + 4. Total[ovl dirx, {2}];
   p[[All, 2]] = Y + 4. Total[ovl diry, {2}], {pass, 60}];
  p = p - ConstantArray[p[[ci]], n];
  <|"Pos" -> AssociationThread[verts, p], "RingOf" -> <||>, "AngleOf" -> <||>, "Radii" -> {}, "RingNodes" -> {}|>];

(* --- edge routing --- *)
exitPoint[p_, q_, {x1_, y1_, x2_, y2_}] := Module[{d = q - p, ts = {}},
  If[d[[1]] > 0, AppendTo[ts, (x2 - p[[1]])/d[[1]]], If[d[[1]] < 0, AppendTo[ts, (x1 - p[[1]])/d[[1]]]]];
  If[d[[2]] > 0, AppendTo[ts, (y2 - p[[2]])/d[[2]]], If[d[[2]] < 0, AppendTo[ts, (y1 - p[[2]])/d[[2]]]]];
  p + Min[ts] d];

insideQ[p_, {x1_, y1_, x2_, y2_}] := x1 <= p[[1]] <= x2 && y1 <= p[[2]] <= y2;

(* remove the part of the path inside the start box *)
clipStart[pts_, box_] := Module[{k = LengthWhile[pts, insideQ[#, box] &] + 1},
  If[k > Length[pts] || k === 1, pts, Prepend[Drop[pts, k - 1], exitPoint[pts[[k - 1]], pts[[k]], box]]]];
clipPath[pts_, boxU_, boxV_] := Reverse[clipStart[Reverse[clipStart[pts, boxU]], boxV]];

(* ---- compiled geometry kernels (WVM byte code, no external C compiler needed) ---- *)
(* number of sample points of a polyline lying in a (masked-in) box *)
cfPolyHits = Compile[{{pts, _Real, 2}, {bm, _Real, 2}, {mask, _Real, 1}},
   Module[{n = Length[pts], m = Length[bm], cnt = 0, ax = 0., ay = 0., bx = 0., by = 0., len = 0., k = 1, tt = 0., x = 0., y = 0.},
    Do[
     ax = pts[[i, 1]]; ay = pts[[i, 2]]; bx = pts[[i + 1, 1]]; by = pts[[i + 1, 2]];
     len = Sqrt[(bx - ax)^2 + (by - ay)^2];
     k = Max[1, Ceiling[len/6.]];
     Do[
      tt = (s - 1.)/k; x = ax + (bx - ax) tt; y = ay + (by - ay) tt;
      Do[If[mask[[j]] > 0. && x >= bm[[j, 1]] && x <= bm[[j, 3]] && y >= bm[[j, 2]] && y <= bm[[j, 4]], cnt++], {j, m}],
      {s, 1, k}], {i, n - 1}];
    x = pts[[n, 1]]; y = pts[[n, 2]];
    Do[If[mask[[j]] > 0. && x >= bm[[j, 1]] && x <= bm[[j, 3]] && y >= bm[[j, 2]] && y <= bm[[j, 4]], cnt++], {j, m}];
    cnt]];

(* does the segment (px,py)-(qx,qy) hit a masked-in box? (slab method) *)
cfSegBlocked = Compile[{{px, _Real}, {py, _Real}, {qx, _Real}, {qy, _Real}, {bm, _Real, 2}, {mask, _Real, 1}},
   Module[{m = Length[bm], dx = qx - px, dy = qy - py, hit = False, b = 1, tmin = 0., tmax = 1., t1 = 0., t2 = 0., tt = 0., ok = True},
    While[b <= m && ! hit,
     If[mask[[b]] > 0. && Max[px, qx] >= bm[[b, 1]] && Min[px, qx] <= bm[[b, 3]] && Max[py, qy] >= bm[[b, 2]] && Min[py, qy] <= bm[[b, 4]],
      tmin = 0.; tmax = 1.; ok = True;
      If[Abs[dx] < 1.*^-12, If[px < bm[[b, 1]] || px > bm[[b, 3]], ok = False],
       t1 = (bm[[b, 1]] - px)/dx; t2 = (bm[[b, 3]] - px)/dx; If[t1 > t2, tt = t1; t1 = t2; t2 = tt]; tmin = Max[tmin, t1]; tmax = Min[tmax, t2]];
      If[Abs[dy] < 1.*^-12, If[py < bm[[b, 2]] || py > bm[[b, 4]], ok = False],
       t1 = (bm[[b, 2]] - py)/dy; t2 = (bm[[b, 4]] - py)/dy; If[t1 > t2, tt = t1; t1 = t2; t2 = tt]; tmin = Max[tmin, t1]; tmax = Min[tmax, t2]];
      If[ok && tmin <= tmax, hit = True]];
     b++];
    hit]];

(* distance matrix of the visibility graph (1.*^9 = not visible) *)
cfVisMatrix = Compile[{{pts, _Real, 2}, {bm, _Real, 2}, {mask, _Real, 1}},
   Module[{n = Length[pts], D = ConstantArray[1.*^9, {Length[pts], Length[pts]}], blocked = False},
    Do[
     D[[i, i]] = 0.;
     Do[
      blocked = cfSegBlocked[pts[[i, 1]], pts[[i, 2]], pts[[j, 1]], pts[[j, 2]], bm, mask];
      If[! blocked, D[[i, j]] = Sqrt[(pts[[i, 1]] - pts[[j, 1]])^2 + (pts[[i, 2]] - pts[[j, 2]])^2]; D[[j, i]] = D[[i, j]]],
      {j, i + 1, n}], {i, n}];
    D]];

(* distances from a point to all points it can see (1.*^9 otherwise) *)
cfVisPoint = Compile[{{a, _Real, 1}, {pts, _Real, 2}, {bm, _Real, 2}, {mask, _Real, 1}},
   Table[If[cfSegBlocked[a[[1]], a[[2]], pts[[j, 1]], pts[[j, 2]], bm, mask], 1.*^9, Sqrt[(a[[1]] - pts[[j, 1]])^2 + (a[[2]] - pts[[j, 2]])^2]], {j, Length[pts]}]];

(* dense Dijkstra on the visibility graph; init = distance from the source to every corner;
   returns {dist, pred} with pred 0 = reached directly from the source *)
cfDijkstra = Compile[{{D, _Real, 2}, {init, _Real, 1}},
   Module[{n = Length[init], dist = init, pred = ConstantArray[0., Length[init]], done = ConstantArray[0, Length[init]], u = 0, best = 0., alt = 0.},
    Do[
     u = 0; best = 1.*^20;
     Do[If[done[[i]] == 0 && dist[[i]] < best, best = dist[[i]]; u = i], {i, n}];
     If[u > 0 && best < 1.*^8,
      done[[u]] = 1;
      Do[alt = dist[[u]] + D[[u, j]]; If[done[[j]] == 0 && alt < dist[[j]], dist[[j]] = alt; pred[[j]] = u], {j, n}]],
     {step, n}];
    {dist, pred}]];

(* segments P_i -> Q_i against boxes {x1,y1,x2,y2}: matrix (segments x boxes) of 0/1 (slab method) *)
minE[a_, b_] := (a + b - Abs[a - b])/2;
maxE[a_, b_] := (a + b + Abs[a - b])/2;
segBoxHits[P_, Q_, bm_] := Module[{px = P[[All, 1]], py = P[[All, 2]], dx, dy, tx1, tx2, ty1, ty2, tmin, tmax},
  dx = Q[[All, 1]] - px; dy = Q[[All, 2]] - py;
  dx = dx + 1.*^-9 (2 UnitStep[dx] - 1); dy = dy + 1.*^-9 (2 UnitStep[dy] - 1);
  tx1 = Outer[Plus, -px, bm[[All, 1]]]/dx; tx2 = Outer[Plus, -px, bm[[All, 3]]]/dx;
  ty1 = Outer[Plus, -py, bm[[All, 2]]]/dy; ty2 = Outer[Plus, -py, bm[[All, 4]]]/dy;
  tmin = maxE[maxE[0., minE[tx1, tx2]], minE[ty1, ty2]];
  tmax = minE[minE[1., maxE[tx1, tx2]], maxE[ty1, ty2]];
  UnitStep[tmax - tmin]];

(* Route all edges of a graph. Returns the paths (list of points per edge index) and the number of
   edges that still touch a node box (should be 0). *)
routeEdges[graph_Association, layout_Association] := Module[
  {pos = layout["Pos"], sizes, boxesM, boxesPlain, center = graph["Center"], edges = graph["Edges"], paths = <||>, violations = 0,
   opsList, opIdx, bmMat, evalHits, edgeSet, starIdx, otherIdx, ringOf = layout["RingOf"], angleOf = layout["AngleOf"], radii = layout["Radii"],
   ringNodes = layout["RingNodes"], onesVec, maskExcl, generic, vgData, buildVG, visVec, dijk, routeVG, shiftPath, corr, gatePts, routeStar, nodeOf, starRes = <||>, members = <||>, offsetsOf = <||>, centerPos},
  opsList = Keys[pos];
  opIdx = AssociationThread[opsList, Range[Length[opsList]]];
  sizes = Association[# -> If[# === center, $centerSize, $nodeSize] & /@ opsList];
  boxesM = Association[# -> boxOf[pos[#], sizes[#], $nodeMargin] & /@ opsList];
  boxesPlain = Association[# -> boxOf[pos[#], sizes[#], 1.5] & /@ opsList];
  bmMat = Developer`ToPackedArray[N[Values[boxesM]]];
  centerPos = pos[center];
  (* collisions of a polyline with all node boxes except those in excl *)
  onesVec = ConstantArray[1., Length[bmMat]];
  maskExcl[excl_List] := ReplacePart[onesVec, Thread[Lookup[opIdx, excl] -> 0.]];
  evalHits[pts_, excl_List] := cfPolyHits[Developer`ToPackedArray[N[pts]], bmMat, maskExcl[excl]];
  edgeSet = Association[({#["From"], #["To"]} -> True) & /@ edges];
  nodeOf[i_] := If[edges[[i, "From"]] === center, edges[[i, "To"]], edges[[i, "From"]]];
  (* ---- obstacle-avoiding router for all edges that do not start/end at the center:
     visibility graph over the (inflated) box corners, all-pairs shortest paths computed once ---- *)
  vgData = None;
  buildVG[] := Module[{boxesBig, cands, x, y, inside, valid, n},
    boxesBig = Table[boxOf[pos[op], sizes[op], $nodeMargin + 5.], {op, opsList}];
    cands = Flatten[Map[{{#[[1]], #[[2]]}, {#[[3]], #[[2]]}, {#[[1]], #[[4]]}, {#[[3]], #[[4]]}} &, boxesBig], 1];
    cands = Developer`ToPackedArray[N[cands]];
    x = cands[[All, 1]]; y = cands[[All, 2]];
    inside = Total[UnitStep[Outer[Subtract, x, bmMat[[All, 1]]]] UnitStep[Outer[Plus, -x, bmMat[[All, 3]]]] UnitStep[Outer[Subtract, y, bmMat[[All, 2]]]] UnitStep[Outer[Plus, -y, bmMat[[All, 4]]]], {2}];
    valid = Developer`ToPackedArray[N[Pick[cands, inside, 0]]];
    n = Length[valid];
    <|"Pts" -> valid, "D" -> cfVisMatrix[valid, bmMat, onesVec], "N" -> n|>];
  visVec[u_] := visVec[u] = cfVisPoint[pos[u], vgData["Pts"], bmMat, maskExcl[{u}]];
  dijk[u_] := dijk[u] = cfDijkstra[vgData["D"], visVec[u]];
  routeVG[u_, v_] := Module[{dp = dijk[u], tb = visVec[v], tot, j, path, cur, guard = 0},
    tot = dp[[1]] + tb;
    j = First[Ordering[tot, 1]];
    If[tot[[j]] >= 1.*^8, Return[None]];
    path = {j}; cur = j;
    While[dp[[2, cur]] > 0. && guard < 400, cur = Round[dp[[2, cur]]]; PrependTo[path, cur]; guard++];
    Join[{pos[u]}, vgData["Pts"][[path]], {pos[v]}]];
  (* shift a path sideways (to the left of its direction) so that opposite edges do not overlap *)
  shiftPath[pts_, sh_] := Module[{n = Length[pts], tang, nrm},
    tang = Table[Normalize[If[i === 1, pts[[2]] - pts[[1]], If[i === n, pts[[n]] - pts[[n - 1]], pts[[i + 1]] - pts[[i - 1]]]]], {i, n}];
    nrm = {-#[[2]], #[[1]]} & /@ tang;
    pts + sh nrm];
  generic[u_, v_, pair_] := Catch[Module[{a = pos[u], b = pos[v], pts, sm, ks, best, hits},
    If[! cfSegBlocked[a[[1]], a[[2]], b[[1]], b[[2]], bmMat, maskExcl[{u, v}]],
     pts = {a, b},
     If[vgData === None, $rtDebug["buildVG"] = First[AbsoluteTiming[vgData = buildVG[]]]];
     pts = routeVG[u, v];
     If[pts === None,
      ks = {0.1, -0.1, 0.2, -0.2, 0.32, -0.32, 0.5, -0.5, 0.75, -0.75}; best = {Infinity, {a, b}};
      Do[With[{pp = bezierPts[a, b, k, 16]}, hits = evalHits[pp, {u, v}]; If[hits < best[[1]], best = {hits, pp}]], {k, ks}];
      Throw[best[[2]], "route"]]];
    If[pair,
     Module[{done = False},
      Do[If[! done,
        With[{cand = shiftPath[pts, sh]},
         With[{fin = If[Length[cand] > 2, catmullRom[cand, 8], cand]},
          If[evalHits[fin, {u, v}] === 0, pts = fin; done = True,
           If[evalHits[cand, {u, v}] === 0, pts = cand; done = True]]]]], {sh, {3.5, 2.5, 1.5}}];
      If[! done,
       Do[If[! done, With[{pp = bezierPts[a, b, k, 16]}, If[evalHits[pp, {u, v}] === 0, pts = pp; done = True]]], {k, {0.1, 0.18, 0.28}}]]];
     Throw[pts, "route"]];
    If[Length[pts] > 2, sm = catmullRom[pts, 8]; If[evalHits[sm, {u, v}] === 0, pts = sm]];
    pts], "route"];
  (* corridors: angles half-way between neighbouring nodes of ring k *)
  corr = Table[
    Module[{angs = Sort[angleOf[#] & /@ ringNodes[[k]]], n},
     n = Length[angs];
     If[n === 1, {Mod[First[angs] + Pi, 2 Pi]},
      Table[Mod[(angs[[i]] + If[i < n, angs[[i + 1]], angs[[1]] + 2 Pi])/2, 2 Pi], {i, n}]]], {k, Length[ringNodes]}];
  gatePts[k_, alpha_, off_] := radii[[k]] {Cos[alpha], Sin[alpha]} + off {-Sin[alpha], Cos[alpha]};
  (* arrow from the center to node v, leaving through corridors of the inner rings; the corridor
     combination with the smallest angular detour that avoids all nodes is taken *)
  routeStar[v_, offsets_ : <||>, fixedAl_ : None] := Module[{a = centerPos, b = pos[v], kv = ringOf[v], phi = angleOf[v], best, cand, combos, hits, pts},
    best = <|"Score" -> Infinity, "Pts" -> {a, b}, "Al" -> {}|>;
    If[fixedAl === None && evalHits[{a, b}, {center, v}] === 0, Return[<|"Score" -> 0., "Pts" -> {a, b}, "Al" -> {}|>]];
    If[kv > 1,
     If[fixedAl =!= None, combos = {fixedAl},
      cand = Table[Take[SortBy[corr[[j]], angDiff[#, phi] &], UpTo[3]], {j, kv - 1}];
      combos = SortBy[Tuples[cand], Total[angDiff[#, phi] & /@ #] &]];
     Catch[
      Do[
       pts = Join[{a}, Table[gatePts[j, al[[j]], Lookup[offsets, Key[{j, al[[j]]}], 0.]], {j, kv - 1}], {b}];
       Do[
        With[{pp = If[smooth, catmullRom[pts, 8], pts]},
         hits = evalHits[pp, {center, v}];
         If[hits === 0, best = <|"Score" -> Total[angDiff[#, phi] & /@ al], "Pts" -> pp, "Al" -> al|>; Throw[Null]];
         If[best["Score"] === Infinity, best = <|"Score" -> 1000. hits, "Pts" -> pp, "Al" -> al|>]],
        {smooth, {True, False}}], {al, combos}]]];
    If[best["Score"] === Infinity, best["Score"] = 5000.];
    best];
  starIdx = Select[Range[Length[edges]], edges[[#, "From"]] === center || edges[[#, "To"]] === center &];
  otherIdx = Complement[Range[Length[edges]], starIdx];
  If[Length[ringNodes] === 0,
   Do[paths[i] = generic[center, nodeOf[i], False], {i, starIdx}],
   Do[starRes[i] = routeStar[nodeOf[i]], {i, starIdx}];
   (* arrows that share a corridor of an inner ring are spread out sideways *)
   Do[Do[With[{g = {j, starRes[i]["Al"][[j]]}}, members[g] = Append[Lookup[members, Key[g], {}], i]], {j, Length[starRes[i]["Al"]]}], {i, starIdx}];
   Do[
    With[{m = Length[members[g]]},
     If[m > 1,
      With[{ids = SortBy[members[g], angleOf[nodeOf[#]] &],
        step = Clip[(radii[[g[[1]]]] 2 Pi/Max[Length[ringNodes[[g[[1]]]]], 1] - 56.)/(m + 1), {0.5, 7.}]},
       Do[offsetsOf[ids[[q]]] = Append[Lookup[offsetsOf, Key[ids[[q]]], <||>], g -> (q - (m + 1)/2.) step], {q, m}]]]],
    {g, Keys[members]}];
   Do[
    With[{new = routeStar[nodeOf[i], KeyMap[Key, offsetsOf[i]], starRes[i]["Al"]]},
     If[new["Score"] <= starRes[i]["Score"] + 0.001, starRes[i] = new]], {i, Keys[offsetsOf]}];
   Do[paths[i] = starRes[i]["Pts"], {i, starIdx}]];
  Do[If[edges[[i, "To"]] === center, paths[i] = Reverse[paths[i]]], {i, starIdx}];
  Do[
   With[{e = edges[[i]]}, paths[i] = generic[e["From"], e["To"], KeyExistsQ[edgeSet, {e["To"], e["From"]}]]], {i, otherIdx}];
  violations = Count[Range[Length[edges]], i_ /; evalHits[paths[i], {edges[[i, "From"]], edges[[i, "To"]]}] > 0];
  (* clip at the node boundaries *)
  Do[With[{e = edges[[i]]}, paths[i] = clipPath[paths[i], boxesPlain[e["From"]], boxesPlain[e["To"]]]], {i, Range[Length[edges]]}];
  <|"Paths" -> paths, "Violations" -> violations, "Sizes" -> sizes,
   "ViolatingEdges" -> Select[Range[Length[edges]], evalHits[paths[#], {edges[[#, "From"]], edges[[#, "To"]]}] > 0 &]|>];

(* bounding radius of a layout (half the side of the square canvas) *)
layoutRadius[layout_Association, routing_Association] := Max[Join[
    Table[Max[Abs[layout["Pos"][op]] + routing["Sizes"][op]/2], {op, Keys[layout["Pos"]]}], {120.}]] + 22.;

$rtDebug = <||>;
$layoutCache = <||>;
computeLayout[data_Association, graph_Association, layoutName_String] := Module[{prep = getPrep[data], key, lay, rt},
  key = {data["Meta", "SourceHash"], graph["Center"], graph["Direction"], graph["Mode"], Hash[graph["Vertices"]], Hash[graph["Edges"]], layoutName};
  If[! KeyExistsQ[$layoutCache, key],
   lay = If[layoutName === "Spring", springLayout[graph, prep], radialLayout[graph, prep]];
   rt = routeEdges[graph, lay];
   $layoutCache[key] = <|"Layout" -> lay, "Routing" -> rt, "Radius" -> layoutRadius[lay, rt]|>];
  $layoutCache[key]];

(* ================================================================================================
   8. Rendering
   ================================================================================================ *)

$classColors = <|
   "X3" -> RGBColor["#A6CEE3"], "H6" -> RGBColor["#CAB2D6"], "H4D2" -> RGBColor["#FB9A99"],
   "psi2H3" -> RGBColor["#B2DF8A"], "X2H2" -> RGBColor["#FDBF6F"], "psi2XH" -> RGBColor["#F3EE8C"],
   "psi2H2D" -> RGBColor["#8FD3C8"], "LLLL" -> RGBColor["#7EB6E0"], "RRRR" -> RGBColor["#E8A0BF"],
   "LLRR" -> RGBColor["#C9B79C"], "LRRL" -> RGBColor["#BDBDBD"]|>;
$edgeRed = RGBColor["#C62828"];
$edgeGreen = RGBColor["#2E7D32"];
$haloColor = RGBColor["#0D2B6B"];
$uiFont = "Arial";

(* typeset class names for the UI *)
classLabel[cid_String] := With[{sp = Function[{b, e}, Superscript[b, e]]}, Switch[cid,
   "X3", sp["X", "3"], "H6", sp["H", "6"], "H4D2", Row[{sp["H", "4"], " ", sp["D", "2"]}],
   "psi2H3", Row[{sp["\[Psi]", "2"], " ", sp["H", "3"]}], "X2H2", Row[{sp["X", "2"], " ", sp["H", "2"]}],
   "psi2XH", Row[{sp["\[Psi]", "2"], " X H"}], "psi2H2D", Row[{sp["\[Psi]", "2"], " ", sp["H", "2"], " D"}],
   "LLLL", Row[{"(", OverBar["L"], "L)(", OverBar["L"], "L)"}], "RRRR", Row[{"(", OverBar["R"], "R)(", OverBar["R"], "R)"}],
   "LLRR", Row[{"(", OverBar["L"], "L)(", OverBar["R"], "R)"}],
   "LRRL", Row[{"(", OverBar["L"], "R)(", OverBar["R"], "L)+(", OverBar["L"], "R)(", OverBar["L"], "R)"}], _, cid]];

classDisplayName[data_Association, cls_String] := Last[Select[data["Classes"], First[#] === cls &]][[2]];

opLabel[data_Association, op_String, size_ : 13] := Style[data["OperatorInfo", op, "Label"], FontFamily -> "Times", FontSize -> size];

selfBadgeString[rec_Association] := "self: 1L " <> If[rec["L1"], "\[Checkmark]", "\[Dash]"] <> "  2L " <> If[rec["L2"], "\[Checkmark]", "\[Dash]"];

(* typeset list of coupling monomials, truncated *)
couplingsRow[tag_String, monos_List, maxN_ : 6] := If[monos === {}, Nothing,
   Row[{Style[tag <> ": ", Bold, 11], Row[Riffle[RawBoxes[monomialDisplayBoxes[#]] & /@ Take[monos, UpTo[maxN]], Style[",  ", 11]]],
     If[Length[monos] > maxN, Style["  \[Ellipsis] (+" <> ToString[Length[monos] - maxN] <> ")", 11], ""]}]];

newOrderText[] := Row[{"new at order ", Superscript[Row[{"(", Superscript["\[Gamma]", "(0)"], ")"}], "2"], ": reached only via a chain"}];

viaConjText[via_String] := Switch[via, "Yes", "via the conjugate C*", "Both", "via C and its conjugate C*", _, None];

edgeTooltip[data_Association, e_Association] := Module[{f = e["From"], t = e["To"], ed = data["Edges", {e["From"], e["To"]}], loops, via},
  loops = Which[ed["L1"] && ed["L2"], "1-loop + 2-loop", ed["L1"], "1-loop only", True, "2-loop only"];
  via = viaConjText[ed["ViaConjugate"]];
  Framed[Column[Join[{
      Row[{opLabel[data, f, 14], Style["  \[RightArrow]  ", 14], opLabel[data, t, 14]}],
      Style[loops <> If[e["Hop"] === 2, "   (second leg of a one-loop chain)", ""], 11, FontFamily -> $uiFont],
      couplingsRow["1-loop", ed["Couplings1"]], couplingsRow["2-loop", ed["Couplings2"]]},
     If[via === None, {}, {Style[via, 11, FontFamily -> $uiFont]}],
     {Style["click for the full entry", Gray, 10, FontFamily -> $uiFont]}], Left, 0.35],
   FrameStyle -> GrayLevel[0.7], Background -> White, FrameMargins -> 6]];

nodeTooltip[data_Association, graph_Association, op_String] := Framed[Column[{
     opLabel[data, op, 15],
     Style[classDisplayName[data, data["OperatorInfo", op, "Class"]] <> " operator   (" <> op <> ")", 11, FontFamily -> $uiFont],
     If[MemberQ[graph["NewEndpoints"], op], Style[newOrderText[], 11, FontFamily -> $uiFont, FontColor -> $haloColor], Nothing],
     Style[If[op === graph["Center"], "current center", "click to recenter on this operator"], Gray, 10, FontFamily -> $uiFont]}, Left, 0.3],
   FrameStyle -> GrayLevel[0.7], Background -> White, FrameMargins -> 6];

Options[renderGraphics] = {"EdgeThickness" -> 1.8, "CanvasSize" -> 880, "OnNodeClick" -> (None &), "OnEdgeClick" -> (None &), "ShowSelf" -> False};

(* Builds the primitives of the explorer graph. Returns an Association with everything the UI needs. *)
renderGraphics[data_Association, graph_Association, layoutName_String, OptionsPattern[]] := Module[
  {cl, lay, rt, R, z, canvas, pos, sizes, cls, newEnd, th, ahSize, onNode, onEdge, edges, edgePrims, nodePrims, badge, center, selfRec, paths, fs},
  cl = computeLayout[data, graph, layoutName];
  lay = cl["Layout"]; rt = cl["Routing"];
  canvas = OptionValue["CanvasSize"];
  R = cl["Radius"];
  z = Min[1.2, canvas/(2 R)];
  center = graph["Center"];
  pos = lay["Pos"]; sizes = rt["Sizes"];
  edges = graph["Edges"];
  newEnd = graph["NewEndpoints"];
  th = OptionValue["EdgeThickness"];
  onNode = OptionValue["OnNodeClick"]; onEdge = OptionValue["OnEdgeClick"];
  ahSize = Clip[13./(2 R z), {0.008, 0.03}];
  paths = Association[Table[i -> z rt["Paths"][i], {i, Length[edges]}]];
  (* draw the long, green edges first so that the red ones stay on top *)
  edgePrims = Table[
    With[{e = edges[[i]], pts = paths[i]},
     With[{col = If[e["Color"] === "Red", $edgeRed, $edgeGreen], dash = If[e["Color"] === "Green", AbsoluteDashing[{7, 4}], Dashing[None]],
       key = {e["From"], e["To"], e["Hop"]}},
      EventHandler[
       Tooltip[
        Mouseover[
         {{col, AbsoluteThickness[th], dash, Arrowheads[{{ahSize, 1}}], Arrow[Line[pts]]}, {White, Opacity[0.01], AbsoluteThickness[11], Line[pts]}},
         {{Darker[col, 0.25], AbsoluteThickness[th + 2.2], dash, Arrowheads[{{ahSize 1.25, 1}}], Arrow[Line[pts]]}, {White, Opacity[0.01], AbsoluteThickness[11], Line[pts]}}],
        edgeTooltip[data, e]],
       {"MouseClicked" :> onEdge[key]}]]], {i, Length[edges]}];
  edgePrims = edgePrims[[Ordering[If[#["Color"] === "Green", 0, 1] & /@ edges, All, Less]]];
  nodePrims = Table[
    With[{op = op0, p = z pos[op0], sz = z sizes[op0]},
     With[{isC = (op === center), fill = $classColors[data["OperatorInfo", op, "Class"]], isNew = MemberQ[newEnd, op],
       fsz = If[op === center, 17., 13.] z},
      With[{rect = Rectangle[p - sz/2, p + sz/2, RoundingRadius -> 6 z],
        halo = If[isNew, {EdgeForm[{AbsoluteThickness[2.6], $haloColor}], FaceForm[None], Rectangle[p - sz/2 - 4 z, p + sz/2 + 4 z, RoundingRadius -> 9 z],
           EdgeForm[{AbsoluteThickness[1.2], Lighter[$haloColor, 0.55]}], Rectangle[p - sz/2 - 8 z, p + sz/2 + 8 z, RoundingRadius -> 12 z]}, {}],
        lab = Inset[Style[data["OperatorInfo", op, "Label"], FontFamily -> "Times", FontSize -> fsz, FontColor -> GrayLevel[0.08]],
          p + If[isC && OptionValue["ShowSelf"], {0, 9 z}, {0, 0}], Center],
        selfTxt = If[isC && OptionValue["ShowSelf"] && graph["Self"] =!= None,
          Text[Style[selfBadgeString[graph["Self"]], FontFamily -> $uiFont, FontSize -> 9.5 z, FontColor -> GrayLevel[0.15]], p + {0, -14 z}], {}]},
       EventHandler[
        Tooltip[
         Mouseover[
          {halo, EdgeForm[{AbsoluteThickness[If[isC, 2.4, 1]], If[isC, Black, GrayLevel[0.35]]}], FaceForm[fill], rect, lab, selfTxt},
          {halo, EdgeForm[{AbsoluteThickness[2.2], RGBColor["#1565C0"]}], FaceForm[Lighter[fill, 0.25]], rect, lab, selfTxt}],
         nodeTooltip[data, graph, op]],
        {"MouseClicked" :> onNode[op]}]]]], {op0, graph["Vertices"]}];
  <|"Primitives" -> {edgePrims, nodePrims}, "PlotRange" -> R z {{-1, 1}, {-1, 1}}, "ImageSize" -> 2 R z, "Zoom" -> z,
    "Paths" -> paths, "Positions" -> Association[# -> z pos[#] & /@ Keys[pos]], "Violations" -> rt["Violations"], "Radius" -> R|>];

(* a static Graphics (no hooks needed) *)
explorerGraphics[data_Association, graph_Association, layoutName_String, opts___] := Module[{r = renderGraphics[data, graph, layoutName, opts]},
  Graphics[r["Primitives"], PlotRange -> r["PlotRange"], ImageSize -> r["ImageSize"], PlotRangePadding -> 0, Background -> White, ImagePadding -> 2]];

(* highlight under the currently selected edge *)
selectedEdgeOverlay[paths_Association, edges_List, sel_] := Module[{i},
  If[sel === None, Return[{}]];
  i = First[Flatten[Position[edges, e_ /; {e["From"], e["To"], e["Hop"]} === sel, 1, 1]], None];
  If[i === None || ! KeyExistsQ[paths, i], {}, {RGBColor[1., 0.82, 0.1, 0.55], AbsoluteThickness[9], CapForm["Round"], Line[paths[i]]}]];

(* legend: edge styles and highlights *)
legendEdgeStyles[mode_String] := Grid[{
    {Graphics[{$edgeRed, AbsoluteThickness[2], Arrowheads[{{0.22, 1}}], Arrow[{{0, 0}, {1, 0}}]}, ImageSize -> 46, PlotRange -> {{-0.05, 1.05}, {-0.3, 0.3}}, AspectRatio -> 0.35],
     Style["nonzero at one loop", 11, FontFamily -> $uiFont]},
    If[mode === "Direct",
     {Graphics[{$edgeGreen, AbsoluteThickness[2], AbsoluteDashing[{6, 3}], Arrowheads[{{0.22, 1}}], Arrow[{{0, 0}, {1, 0}}]}, ImageSize -> 46, PlotRange -> {{-0.05, 1.05}, {-0.3, 0.3}}, AspectRatio -> 0.35],
      Style["two-loop only (dashed)", 11, FontFamily -> $uiFont]}, Nothing],
    If[mode === "OneLoopSquared",
     {Graphics[{EdgeForm[{AbsoluteThickness[2.2], $haloColor}], FaceForm[None], Rectangle[{0, 0}, {1, 0.5}, RoundingRadius -> 0.15], EdgeForm[{AbsoluteThickness[1], Lighter[$haloColor, 0.55]}], Rectangle[{-0.12, -0.12}, {1.12, 0.62}, RoundingRadius -> 0.2]}, ImageSize -> 46, PlotRange -> {{-0.2, 1.2}, {-0.2, 0.7}}],
      Style[newOrderText[], 11, FontFamily -> $uiFont]}, Nothing]},
   Alignment -> {Left, Center}, Spacings -> {0.6, 0.5}];

(* ================================================================================================
   9. Side panel: entries of the selected edge
   ================================================================================================ *)

(* an entry collected by the gauge/quartic coupling monomial, truncated to maxTerms terms *)
collectedBoxLines[terms_List, L_Integer, maxTerms_Integer : 24] := Module[
  {groups, shown = 0, lines = {}, names, total = Length[terms], first = True, grp, inner},
  groups = GatherBy[sortedTerms[terms], Cases[#["Factors"], f_ /; MemberQ[{"Gauge", "Quartic"}, f["Type"]], {1}] &];
  groups = SortBy[groups, {Length[#[[1]]["Factors"]], #[[1]]["Factors"][[1, "Name"]]} &];
  Catch[
   Do[
    inner = Map[termBoxParts[#, {"Gauge", "Quartic"}] &, grp];
    AppendTo[lines, RowBox[{If[first, loopPrefactorBox[L], "\[ThinSpace]"], "\[ThinSpace]",
       monomialDisplayBoxes[Cases[termMonomial[grp[[1]]], m_ /; MemberQ[{"Gauge", "Quartic"}, couplingType[m[[1]]]]]], "\[ThinSpace]", "\[Times]", "\[ThinSpace]", "["}]];
    first = False;
    Do[
     If[shown >= maxTerms, Throw[Null]];
     AppendTo[lines, RowBox[{"\[ThinSpace]\[ThinSpace]\[ThinSpace]", If[inner[[q, 1]] < 0, "-", "+"], "\[ThinSpace]", inner[[q, 2]]}]];
     shown++, {q, Length[inner]}];
    AppendTo[lines, "]"], {grp, groups}]];
  If[shown < total, AppendTo[lines, StyleBox["\[Ellipsis]  (" <> ToString[total - shown] <> " more terms hidden; tick \"Show full expression\")", FontColor -> GrayLevel[0.4], FontSlant -> "Italic"]]];
  lines];

entryDisplay[terms_List, L_Integer, full_] := Module[{n = Length[terms], lines},
  lines = If[n <= 12 || TrueQ[full], entryBoxLines[terms, L, 1], collectedBoxLines[terms, L]];
  Style[Column[RawBoxes /@ lines, Left, 0.55], SingleLetterItalics -> False, FontSize -> 13]];

legendKeyText[e_Association] := Which[e["L1"] && e["L2"], "1-loop + 2-loop", e["L1"], "1-loop only", True, "2-loop only"];

(* the legs to display for a clicked edge key {from, to, hop} *)
legsForEdge[graph_Association, key_List] := Module[{c = graph["Center"], dir = graph["Direction"]},
  If[graph["Mode"] === "OneLoopSquared" && key[[3]] === 2,
   If[dir === "Outgoing", {{c, key[[1]], 1}, key}, {key, {key[[2]], c, 1}}], {key}]];

entryPanelSection[data_Association, key_List, showFull_] := Module[{f = key[[1]], t = key[[2]], ed = data["Edges", {key[[1]], key[[2]]}], blocks, via},
  via = viaConjText[ed["ViaConjugate"]];
  blocks = Table[
    With[{tag = ToString[L], ln = If[L === 1, "One-loop entry", "Two-loop entry"]},
     If[! ed["L" <> tag], Nothing,
      Column[{
        Row[{Style[ln, Bold, 13, FontFamily -> $uiFont], Style["   (" <> ToString[ed["NTerms" <> tag]] <> " terms)", Gray, 11, FontFamily -> $uiFont]}],
        entryDisplay[ed["Terms" <> tag], L, showFull],
        Row[{Button["Copy LaTeX", CopyToClipboard[ed["TeX" <> tag]], ImageSize -> {110, 24}], "  ",
          Button["Copy InputForm", CopyToClipboard[ed["Input" <> tag]], ImageSize -> {120, 24}]}]}, Left, 0.8]]], {L, {1, 2}}];
  Column[Join[{
     Row[{opLabel[data, f, 18], Style["   \[RightArrow]   ", 18], opLabel[data, t, 18]}],
     Style[legendKeyText[ed] <> If[via === None, "", "; " <> via] <> ". The entry lists all terms of the beta function of the target that contain the source.", 11, FontFamily -> $uiFont, GrayLevel[0.35]]},
    blocks], Left, 1.1]];

entryPanel[data_Association, graph_Association, key_, showFull_] := Module[{legs},
  If[key === None,
   Return[Style["Click an arrow to show its entry here.\nClick an operator to recenter the graph on it.", Gray, 13, FontFamily -> $uiFont]]];
  legs = legsForEdge[graph, key];
  Column[Riffle[
    Table[entryPanelSection[data, leg, showFull], {leg, legs}],
    Style[Row[{"\[DownArrow]  second leg of the chain"}], Gray, 12, FontFamily -> $uiFont]], Left, 1.6]];

infoLine[graph_Association] := Style[Row[{
    "Showing ", Length[graph["Edges"]], " edges between ", Length[graph["Vertices"]], " operators:  ",
    Style[graph["NRed"], $edgeRed, Bold], " red (one-loop)",
    If[graph["Mode"] === "Direct", Row[{",  ", Style[graph["NGreen"], $edgeGreen, Bold], " green (two-loop only)"}], ""],
    If[graph["Mode"] === "OneLoopSquared", Row[{",  ", Style[Length[graph["NewEndpoints"]], $haloColor, Bold], " new endpoints"}], ""]}], 12, FontFamily -> $uiFont];

(* search field: operators whose ID contains the query *)
searchOps[data_Association, q_String] := Module[{qq = ToLowerCase[StringTrim[q]]},
  If[qq === "", {}, Select[data["Operators"], StringContainsQ[ToLowerCase[#], StringReplace[qq, {"c_" -> "c", "{" -> "", "}" -> "", "\\" -> "", "^" -> "", "(" -> "", ")" -> ""}]] &]]];

(* ================================================================================================
   10. The explorer UI
   ================================================================================================ *)

$explorerData = None;

SMEFTADMExplorer::data = "Could not read ADM data from `1`.";

SMEFTADMExplorer[src_, startOp_String : "cH"] := Module[{data = If[AssociationQ[src], src, LoadADMData[src]], start},
  If[! AssociationQ[data], Message[SMEFTADMExplorer::data, src]; Return[$Failed]];
  $explorerData = data;
  start = Catch[normalizeOpID[data, startOp], "badop"];
  If[start === $Failed, Return[$Failed]];
  explorerUI[data, start]];

explorerUI[data_Association, start_String] := With[{
   classIDs = data["Classes"][[All, 1]], opsAll = data["Operators"],
   classRows = Table[With[{cid = cr[[1]]},
      Row[{Checkbox[Dynamic[shown[cid]]],
        Graphics[{$classColors[cid], EdgeForm[GrayLevel[0.4]], Rectangle[{0, 0}, {1, 0.55}, RoundingRadius -> 0.15]}, ImageSize -> {26, 15}, PlotRange -> {{-0.05, 1.05}, {-0.05, 0.6}}, AspectRatio -> Full],
        Style[classLabel[cr[[1]]], 12, FontFamily -> $uiFont]}, "  "]], {cr, data["Classes"]}]},
  DynamicModule[{center = start, history = {}, direction = "Incoming", mode = "Direct", layoutName = "Radial", showSelf = True,
    excl = True, shown = Association[# -> True & /@ classIDs], thick = 1.8, selected = None, showFull = False, query = "", browseClass = classIDs[[1]]},
   Grid[{{
      (* ---------------- left column: controls ---------------- *)
      Panel[Column[{
         Style["SMEFT ADM Explorer", Bold, 18, FontFamily -> $uiFont],
         Style["dimension-six SMEFT, Mainz basis, one and two loops", Gray, 11, FontFamily -> $uiFont],
         Delimiter,
         Style["Center operator", Bold, 12, FontFamily -> $uiFont],
         Dynamic[Row[{Style[opLabel[$explorerData, center, 24], Bold], "  ", Style["(" <> center <> ")", Gray, 11]}]],
         Row[{Button["\[LeftTriangle] Back",
             center = Last[history]; history = Most[history]; selected = None,
             Enabled -> Dynamic[history =!= {}], ImageSize -> {80, 24}]}],
         InputField[Dynamic[query], String, FieldHint -> "search operator, e.g. Hq, HWB, quqd", ImageSize -> {240, 24}],
         Dynamic[With[{res = Take[searchOps[$explorerData, query], UpTo[8]]},
           If[res === {}, Spacer[1],
            Row[Table[With[{op = op0}, Button[opLabel[$explorerData, op, 13], If[op =!= center, history = Append[history, center]; center = op; selected = None; query = ""],
                 ImageSize -> {70, 26}, Appearance -> "Palette"]], {op0, res}], " ", BaselinePosition -> Baseline]]]],
         Row[{PopupMenu[Dynamic[browseClass], Table[cr[[1]] -> Style[classLabel[cr[[1]]], 12, FontFamily -> $uiFont], {cr, $explorerData["Classes"]}], ImageSize -> {120, 24}], " ",
           Dynamic[PopupMenu[Dynamic[center, Function[op, If[op =!= center, history = Append[history, center]]; center = op; selected = None]],
             Table[op -> opLabel[$explorerData, op, 13], {op, Select[opsAll, $explorerData["OperatorInfo", #, "Class"] === browseClass &]}], ImageSize -> {110, 24}]]}],
         Delimiter,
         Style["Direction", Bold, 12, FontFamily -> $uiFont],
         RadioButtonBar[Dynamic[direction, (direction = #; selected = None) &], {"Outgoing" -> "Runs into (center \[RightArrow] X)", "Incoming" -> "Generated by (X \[RightArrow] center)"}, Appearance -> "Vertical"],
         Style["Mode", Bold, 12, FontFamily -> $uiFont],
         RadioButtonBar[Dynamic[mode, (mode = #; selected = None) &], {"OneLoop" -> "1-loop only", "Direct" -> "1-loop + 2-loop (direct)", "OneLoopSquared" -> "1-loop squared (chains)"}, Appearance -> "Vertical"],
         Style["Layout", Bold, 12, FontFamily -> $uiFont],
         RadioButtonBar[Dynamic[layoutName, (layoutName = #; selected = None) &], {"Radial" -> "Radial (rings)", "Spring" -> "Spring"}],
         Delimiter,
         Style["Options", Bold, 12, FontFamily -> $uiFont],
         Row[{Checkbox[Dynamic[showSelf]], "  show self-running badge"}],
         Row[{Checkbox[Dynamic[excl], Enabled -> Dynamic[mode === "OneLoopSquared"]], "  exclude trivial chains"}],
         Row[{"edge thickness  ", Slider[Dynamic[thick], {0.8, 4., 0.2}, ImageSize -> 110]}],
         Style["Show operator classes", Bold, 12, FontFamily -> $uiFont],
         Column[classRows, Left, 0.3],
         Delimiter,
         Dynamic[legendEdgeStyles[mode]]}, Left, 0.9], ImageSize -> {290, All}],
      (* ---------------- middle: the graph ---------------- *)
      Column[{
        Dynamic[infoLine[ExplorerGraphData[$explorerData, center, direction, mode, "ExcludeTrivialChains" -> excl, "HiddenClasses" -> Keys[Select[shown, ! # &]]]],
         TrackedSymbols :> {center, direction, mode, excl, shown}],
        Dynamic[
         Module[{graph, r},
          graph = ExplorerGraphData[$explorerData, center, direction, mode, "ShowSelfRunning" -> showSelf, "ExcludeTrivialChains" -> excl,
            "HiddenClasses" -> Keys[Select[shown, ! # &]]];
          r = renderGraphics[$explorerData, graph, layoutName, "EdgeThickness" -> thick, "ShowSelf" -> showSelf,
            "OnNodeClick" -> Function[op, If[op =!= center, history = Append[history, center]; center = op; selected = None]],
            "OnEdgeClick" -> Function[key, selected = key]];
          With[{paths = r["Paths"], es = graph["Edges"]},
           Graphics[{r["Primitives"], Dynamic[selectedEdgeOverlay[paths, es, selected], TrackedSymbols :> {selected}]},
            PlotRange -> r["PlotRange"], ImageSize -> r["ImageSize"], PlotRangePadding -> 0, Background -> White, ImagePadding -> 2]]],
         TrackedSymbols :> {center, direction, mode, layoutName, showSelf, excl, shown, thick}]}, Left, 0.5],
      (* ---------------- right: side panel ---------------- *)
      Panel[Column[{
         Style["Entry", Bold, 14, FontFamily -> $uiFont],
         Row[{Checkbox[Dynamic[showFull]], "  show full expression (long entries are otherwise collected and truncated)"}],
         Delimiter,
         Dynamic[Pane[
           entryPanel[$explorerData, ExplorerGraphData[$explorerData, center, direction, mode, "ExcludeTrivialChains" -> excl, "HiddenClasses" -> Keys[Select[shown, ! # &]]], selected, showFull],
           {420, 700}, Scrollbars -> {Automatic, Automatic}, AppearanceElements -> None], TrackedSymbols :> {selected, showFull, center, direction, mode, excl, shown}]}, Left, 0.8],
         ImageSize -> {460, All}]
     }}, Alignment -> {Left, Top}, Spacings -> {1, 0}],
   SaveDefinitions -> False]];

(* ================================================================================================
   11. Packaging: stand-alone notebook
   ================================================================================================ *)

Options[MakeExplorerNotebook] = {"EmbedData" -> True, "DataFile" -> "ADMData.wxf", "PackageFile" -> Automatic, "StartOperator" -> "cH"};

MakeExplorerNotebook[nbFile_String, OptionsPattern[]] := Module[{embed = TrueQ[OptionValue["EmbedData"]], dataFile = OptionValue["DataFile"],
    pkg = OptionValue["PackageFile"], start = OptionValue["StartOperator"], cells, loadCode, launch, nb, bytes, src},
  If[pkg === Automatic, pkg = FileNameJoin[{$SMEFTADMExplorerDirectory, "SMEFTADMExplorer.m"}]];
  If[! FileExistsQ[dataFile], Message[MakeExplorerNotebook::nodata, dataFile]; Return[$Failed]];
  launch = "SMEFTADMExplorer`SMEFTADMExplorer[SMEFTADMExplorer`Private`$notebookData, \"" <> start <> "\"]";
  If[embed,
   src = Import[pkg, "Text"];
   bytes = BinarySerialize[LoadADMData[dataFile], PerformanceGoal -> "Size"];
   loadCode = "(* stand-alone: the package source and the cached ADM data are embedded in this notebook *)\n" <>
     "Get[StringToStream[Uncompress[\"" <> Compress[src] <> "\"]]];\n" <>
     "SMEFTADMExplorer`Private`$notebookData = BinaryDeserialize[BaseDecode[\"" <> BaseEncode[bytes] <> "\"]];",
   loadCode = "(* loads SMEFTADMExplorer.m and ADMData.wxf from the folder of this notebook *)\n" <>
     "Get[FileNameJoin[{NotebookDirectory[], \"SMEFTADMExplorer.m\"}]];\n" <>
     "SMEFTADMExplorer`Private`$notebookData = SMEFTADMExplorer`LoadADMData[FileNameJoin[{NotebookDirectory[], \"" <> FileNameTake[dataFile] <> "\"}]];"];
  cells = {
    Cell["SMEFT ADM Explorer", "Title"],
    Cell["Interactive graph of the one- and two-loop anomalous dimension matrix of the dimension-six SMEFT (Mainz basis). Evaluate the two input cells below (or accept the automatic evaluation of the initialization cells). Click an arrow for its entry, click an operator to recenter the graph.", "Text"],
    Cell[CellGroupData[{Cell["Loader (package source and data)", "Section"], Cell[loadCode, "Input", InitializationCell -> True, CellLabel -> "load"]}, Closed]],
    Cell[launch, "Input", InitializationCell -> True, CellLabel -> "launch"]};
  nb = Notebook[cells, CellGrouping -> Manual, WindowSize -> {1500, 900}, WindowTitle -> "SMEFT ADM Explorer"];
  Export[nbFile, nb, "NB"];
  If[FileExistsQ[nbFile], Print["Wrote ", nbFile, " (", ToString[NumberForm[FileByteCount[nbFile]/10.^6, {4, 2}]], " MB, embedded data: ", embed, ")"]];
  nbFile];
MakeExplorerNotebook::nodata = "Data file `1` not found; run BuildADMData first.";

(* @@INSERT@@ *)

End[];
EndPackage[];
