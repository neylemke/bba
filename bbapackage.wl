(* ::Package:: *)

(* ::Section:: *)
(*Fun\[CCedilla]\[OTilde]es Hash e Convers\[OTilde]es*)


hashlsd["RELIGI"]={{0,"N\[ATilde]o tem"},
{1,"Cat\[OAcute]lica"},
{2,"Evang\[EAcute]lica/protestante"},
{3,"Esp\[IAcute]rita"},
{4,"Judaica"},
{5,"Afro-brasileira"},
{6,"Orientais/budismo"},
{7,"Outra"},
{9,"Prejudicado/n\[ATilde]o sabe"}};

hashlsd["RELIGIPR"]={{0, "N\[ATilde]o tenho religi\[ATilde]o"},
{1,"N\[ATilde]o freq\[UDoubleDot]ento,por\[EAcute]m acredito"},
{2,"Freq\[UDoubleDot]ento menos que 1x/m\[EHat]s"},
{3,"Freq\[UDoubleDot]ento pelo menos 2x/m\[EHat]s"},
{4,"Freq\[UDoubleDot]ento 1x/semana"},
{6,"Freq\[UDoubleDot]ento 2x/semana ou mais"}};

hashlsd["RACA"]={{1,"Branca"},
{2,"Preta"},
{3,"Parda"},
{4,"Amarela"},
{5,"Ind\[IAcute]gena"},
{6,"Outros"}};

hashlsd["CLASSE"]={{"E","E"},
{"D","D"},
{"C2","C2"},
{"C1","C1"},
{"B2","B2"},
{"B1","B1"},{"A2","A2"},
{"A1","A1"}};

hashlsd["SEXOS"]={{"M","Homem"},{"F","Mulher"}};

hashlsd["AMBINGE"]={{0,"Nenhum"},{1,"Poucos"},{2,"Muitos"},{3,"Todos"}};
hashlsd["FAMILAL"]={{0,"N\[ATilde]o"},{1,"Sim"}};


hashlsd["INSTRCHE"]={{0,"At\[EAcute] 3\.aa S\[EAcute]rie Fundamental"},
{1,"4\.aa S\[EAcute]rie Fundamental"},
{2,"Fundamental completo"},
{3,"M\[EAcute]dio completo"},
{4,"Superior completo"}};

hashlsd["AUDIT11"]={
{0,"Nenhuma"} ,
{1,"Uma ou menos de uma vez por me\:0302s"},
{2, "2 a 4 vezes por me\:0302s"},
{3,"2 a 3 vezes por semana"},
{4,"4 ou mais vezes por semana"}};

hashlsd["CATIDADEBEB"]={
{0,"$<$13"} ,
{1,"$[13,14]$"},
{2, "$>$ 14"}};

hashaoc[0]="0 doses";
hashaoc[1]="1 ou 2 doses";
hashaoc[2]="3 ou 4 doses";
hashaoc[3]="5 ou mais doses";


hashGrupos[1]="BR";
hashGrupos[2]="C";
hashGrupos[3]="I";

hashvarname["RELIGI"]="Prefer\[EHat]ncia Religiosa";
hashvarname["RELIGIPR"]="Pr\[AAcute]tica da Religi\[ATilde]o";
hashvarname["RACA"]="Ra\[CCedilla]a";
hashvarname["INSTRCHE"]="Instru\[CCedilla]\[ATilde]o do Chefe";
hashvarname["CLASSE"]="Classe Social";

assocvarnamesTex=<|
"SEXO"->"Sexo",
"SEXOS"->"Sexos",
"AUDITC"-> "AUDITC",
"AUDITESC"->"AUDIT",
"CATIDADEBEB"-> "Idade que iniciou a beber",
"DROGAS12"-> "Drogas no Ano",
"RELIGI"->"Prefer\\^encia Religiosa",
"RELIGIPR"->"Pr\\'atica da Religi\\~ao",
"RACA"->"Ra\\c{c}a",
"IDADEBEB"-> "Idade Primeiro Consumo",
"FAMILAL"->"Fam\\'{\\i}lia com Consumo de Risco",
"AMBINGE"->"Amigos que se Embriagam",
"INSTRCHE"->"Instru\\c{c}\\~ao do Chefe",
"DROGAS30"->"LB:DROGAS30",
"DROGAS12"->"LB:DROGAS12",
"z6AUDITC"->"6:AUDITC",
"z6AUDITESC"->"6:AUDIT",
"z6DROGAS30"->"6:DROGAS30",
"z12AUDITC"->"12:AUDITC",
"z12AUDITESC"->"12:AUDIT",
"z12DROGAS30"->"12:DROGAS30",
"DROGAS30BIT"->"LB:DROGAS30",
"DROGAS12BIT"->"LB:DROGAS12",
"z6DROGAS30BIT"->"6:DROGAS30",
"z12DROGAS30BIT"->"12:DROGAS30",
"1"->"Constante",
"CLASSE"->"Classe Social"
|>;

assocvarnamesShortTex=<|
"SEXO"->"Sexo",
"SEXOS"->"Sexos",
"AUDITC"-> "AUDITC",
"AUDITESC"->"AUDIT",
"CATIDADEBEB"-> "Idade que iniciou a beber",
"DROGAS12"-> "Drogas no Ano",
"RELIGI"->"Prefer\\^encia Religiosa",
"RELIGIPR"->"Pr\\'atica da Religi\\~ao",
"RACA"->"Ra\\c{c}a",
"IDADEBEB"-> "Idade Primeiro Consumo",
"FAMILAL"->"Fam\\'{\\i}lia com Consumo de Risco",
"AMBINGE"->"Amigos que se Embriagam",
"INSTRCHE"->"Instru\\c{c}\\~ao do Chefe",
"DROGAS30"->"LB:DROGAS30",
"DROGAS12"->"DROGAS12",
"z6AUDITC"->"6:AUDITC",
"z6AUDITESC"->"6:AUDIT",
"z6DROGAS30"->"6:DROGAS30",
"z12AUDITC"->"12:AUDITC",
"z12AUDITESC"->"12:AUDIT",
"z12DROGAS30"->"12:DROGAS30",
"DROGAS30BIT"->"LB:DROGAS30",
"DROGAS12BIT"->"LB:DROGAS12",
"z6DROGAS30BIT"->"6:DROGAS30",
"z12DROGAS30BIT"->"12:DROGAS30",
"1"->"Constante",
"CLASSE"->"Classe Social"
|>;


assocvarnames=<|
"SEXO"->"Sexo",
"SEXOS"->"Sexos",
"AUDITC"-> "AUDITC",
"AUDITESC"->"AUDIT",
"CATIDADEBEB"-> "Idade que iniciou a beber",
"DROGAS12"-> "Drogas no Ano",
"RELIGI"->"Prefer\[EHat]ncia Religiosa",
"RELIGIPR"->"Pr\[AAcute]tica da Religi\[ATilde]o",
"RACA"->"Ra\[CCedilla]a",
"IDADEBEB"-> "Idade Primeiro Consumo",
"FAMILAL"->"Fam\[IAcute]lia com Consumo de Risco",
"AMBINGE"->"Amigos que se Embriagam",
"INSTRCHE"->"Instru\[CCedilla]\[ATilde]o do Chefe",
"DROGAS30"->"LB:DROGAS30",
"DROGAS12"->"LB:DROGAS12",
"z6AUDITC"->"6:AUDITC",
"z6AUDITESC"->"6:AUDITESC",
"z6DROGAS30"->"6:DROGAS30",
"z12AUDITC"->"12:AUDITC",
"z12AUDITESC"->"12:AUDITESC",
"z12DROGAS30"->"12:DROGAS30",
"DROGAS30BIT"->"LB:DROGAS30",
"DROGAS12BIT"->"LB:DROGAS12",
"z6DROGAS30BIT"->"6:DROGAS30",
"z12DROGAS30BIT"->"12:DROGAS30",
"1"->"Constante",
"CLASSE"->"Classe Social"
|>;

associnstr=<|0->0,1->1,2->4,3->8|>;


classeE=Interval[{0,7}];
classeD=Interval[{8,13}];
classeC2=Interval[{14,17}];
classeC1=Interval[{18,22}];
classeB2=Interval[{23,28}];
classeB1=Interval[{29,33}];
classeA2=Interval[{34,41}];
classeA1=Interval[{42,46}];

classe[x_]:=If[NumberQ[x],Which[IntervalMemberQ[classeE,x],"E",IntervalMemberQ[classeD,x],"D",IntervalMemberQ[classeC2,x],"C2",IntervalMemberQ[classeC1,x],"C1",IntervalMemberQ[classeB2,x],"B2",IntervalMemberQ[classeB1,x],"B1",IntervalMemberQ[classeA2,x],"A2",IntervalMemberQ[classeA1,x],"A1"],""]

strclasses="Televisoresemcores 0 1 2 3 4
Videocassete 0 2 2 2 2
R\[AAcute]dios 0 1 2 3 4
Banheiros 0 4 5 6 7
Autom\[OAcute]veis 0 4 7 9 9
Empregadasmensalistas 0 3 4 4 4
M\[AAcute]quinaslavar 0 2 2 2 2
Geladeira 0 4 4 4 4
Freezer* 0 2 2 2 2";



lispesos=(Partition[StringSplit[strclasses,{" ","\n"}],6])[[All,{2,3,4,5,6}]];
lisbens={"ABIPEMET","ABIPEMEV","ABIPEMER","ABIPEMEW","ABIPEMEC","ABIPEMEE","ABIPEMEM","ABIPEMEG","ABIPEMEF"};

listabbens=Transpose@Prepend[Transpose[lispesos],{"ABIPEMET","ABIPEMEV","ABIPEMER","ABIPEMEW","ABIPEMEC","ABIPEMEE","ABIPEMEM","ABIPEMEG","ABIPEMEF"}];
(* Do[hashbens[listabbens[[i,1]]]=Rest[listabbens[[i]]],{i,1,Length[listabbens]}]*) 
Do[hashbens[First@bens]=Rest[bens],{bens,listabbens}]











hashGrupos[1]="BR";
hashGrupos[2]="C";
hashGrupos[3]="I";



hashfreq[0]="Cerca de 1 $\\times$ m\[EHat]s";
hashfreq[1]="2 a 3 $\\times$ m\[EHat]s";
hashfreq[2]="1 a 2 $\\times$ semana";
hashfreq[3]="3 a 4 $\\times$ semana";
hashfreq[4]="Quase todos os dias";


fsex[x_]:=If[x==2,"F","M"]
errorBar[orig_,fim_]:={Arrowheads[{{.01,1,bar}}],Arrow[{{orig,fim}}],Arrow[{fim,orig}]}
bar=Graphics[Line[{{0,-1},{0,1}}]];


gerapart[part1_,var_]:=DeleteMissing@Normal[part1[All,var]]







(* ::Section:: *)
(*Fun\[CCedilla]\[OTilde]es Estat\[IAcute]sticas*)


geraglmAIC[lisvarnames_]:=Module[{datalm,lisvar,lisnominal,glm},
datalm=Join[Append[lisvarnames,1]/.Normal[bbapers],Append[lisvarnames,0]/.Normal[bbanpers]];
datalm=Cases[datalm,Table[_Integer,Length[lisvarnames]+1]];
lisvar=ToExpression/@(ToLowerCase/@lisvarnames);
lisnominal=Intersection[lisvarnames,{sexo,raca,religi,classe}];
glm=GeneralizedLinearModelFit[datalm,lisvar,lisvar, NominalVariables->lisnominal,ExponentialFamily->"Binomial"];
glm["AIC"]]

stepAIC[{lisvarnames_,num_}]:=Module[{},First[Sort[Append[{#,geraglmAIC[#]}&/@(Table[Delete[lisvarnames,i],{i,1,Length[lisvarnames]}]),{lisvarnames,num}],#2[[2]]>#1[[2]]&]]
]


GeneralizedLinearModelFitDataset[dataset_,lisvarname_,opts:OptionsPattern[]]:=Module[{lisvar,datapoints},
lisvar=ToExpression/@(ToLowerCase/@lisvarname);
datapoints=lisvarname/.Normal[dataset[All,lisvarname]];
datapoints=Cases[datapoints,Table[_Integer,Length[lisvarname]]];
GeneralizedLinearModelFit[datapoints,Delete[lisvar,-1],Delete[lisvar,-1]
,Sequence@@FilterRules[{opts},Options[GeneralizedLinearModelFit]]]
]
stepSelect[dataset_,lisvarname_,crit_,opts:OptionsPattern[]]:=Module[{},First[Sort[Append[{#,GeneralizedLinearModelFitDataset[dataset,#,Sequence@@FilterRules[{opts},GeneralizedLinearModelFit]][crit]}&/@(Table[Delete[lisvarname,i],{i,1,Length[lisvarname]-1}]),{lisvarname,GeneralizedLinearModelFitDataset[dataset,lisvarname,Sequence@@FilterRules[{opts},GeneralizedLinearModelFit]][crit]}],#2[[2]]>#1[[2]]&]]
]



bestGeneralizedLinearModelFitDataset[dataset_,lisvarname_,crit_,opts:OptionsPattern[]]:=Module[{best,minaic,subs},
subs=Subsets[Delete[lisvarname,-1]];
{best,minaic}=First[MinimalBy[{#,GeneralizedLinearModelFitDataset[dataset,Append[#,Last[lisvarname]],ExponentialFamily->"Binomial",NominalVariables->Intersection[#,{sexo,religi,raca}]]["AIC"]}&/@subs,Last]];
GeneralizedLinearModelFitDataset[dataset,Append[best,Last[lisvarname]],Sequence@@FilterRules[{opts},GeneralizedLinearModelFit]]]


calcEfeito[lisRTM_,L_,\[Rho]_]:=Module[{\[Mu],\[Sigma],z},
\[Mu]=Mean[lisRTM];
\[Sigma]=StandardDeviation[lisRTM];
\!\(TraditionalForm\`z = \((\[Mu] - L)\)/\[Sigma]\);
\[Sigma] PDF[NormalDistribution[0,1],(L-\[Mu])/\[Sigma]]/(CDF[NormalDistribution[0,1],(-L+\[Mu])/\[Sigma]])(1-\[Rho])
]

geraCorr[dataset_,var1_,var2_]:=Module[{liscorr},
liscorr=Cases[{var1,var2}/.Normal[dataset[All,{var1,var2}]],
{_Integer,_Integer}];{N[Correlation[liscorr[[All,1]],liscorr[[All,2]]]],CorrelationTest[liscorr]}];


geralisAncova[liscontrole_,lisintervencao_,liscontrole2_,lisintervencao2_]:=Module[{lislogcontrole,lislogintervencao},
lislogcontrole=Select[Transpose@{Log[liscontrole],-Log[liscontrole]+Log[liscontrole2]},#[[1]]!=-\[Infinity]&];
lislogintervencao=Select[Transpose@{Log[lisintervencao],-Log[lisintervencao]+Log[lisintervencao2]},#[[1]]!=-\[Infinity]&];
Join[Prepend[#, 0]&/@lislogcontrole,Prepend[#, 1]&/@lislogintervencao]]


(* ::Section:: *)
(*Fun\[CCedilla]\[OTilde]es de Tabelas*)


geraporcento[data_,crit_]:=N[100*Length[Select[data,crit]]/Length[data],4];
geraporcento2[data_,crit_]:={Length[Select[data,crit]],N[100*Length[Select[data,crit]]/Length[data],4]};
gerapar[data_,crit_]:={Length[Select[data,crit]],geraporcento[data,crit]};
geralinha[data_]:=Flatten[{gerapar[data,#SEXOS=="F"&],gerapar[data,#SEXOS=="M"&],gerapar[data,#SEXOS=="F"|| #SEXOS=="M"&]}]
geraComment[]:=StringTemplate["% Gerado por `filename` em `dia`/`mes`/`ano`\n"][<|"filename"->NotebookFileName[EvaluationNotebook[]],"dia"->ToString[DateValue["Day"]],"mes"->ToString[DateValue["Month"]],"ano"->ToString[DateValue["Year"]]|>]
geraFooter[]:=Module[{},"\\bottomrule\n \\end{tabular}\n"];


corrigeAcentos[string_]:=StringReplace[string,{"\[IAcute]"->"\\'{\\i}","\[OAcute]"->"\\'o","\[ATilde]"->"\\~a","\[AAcute]"->"\\'a","\[AHat]"->"\\^a","\[AGrave]"->"\\`a","\[EAcute]"->"\\'e","\[EHat]"->"\\^e","\[OHat]"->"\\^o","\[OTilde]"->"\\~o","\[CCedilla]"->"\\c{c}","\[UDoubleDot]"->"\\\"u","\[CapitalIAcute]"->"\\'I","\[CapitalOAcute]"->"\\'O","\[CapitalATilde]"->"\\~A","\[CapitalAHat]"->"\\^A","\[CapitalAGrave]"->"\\`A","\[CapitalEAcute]"->"\\'E","\[CapitalEHat]"->"\\^E","\[CapitalOHat]"->"\\^O","\[CapitalOTilde]"->"\\~O","\[CapitalUDoubleDot]"->"\\\"U""\[CapitalCCedilla]"->"\\c{C}","\.aa"->"$^a$"}]

geraTable[header_,stringtable_]:=Module[{comment,footer},comment=geraComment[];
footer=geraFooter[];
comment<>header<>stringtable<>footer]



geraTabCG[tabci_,prec_]:=Module[{stringtable,comment,header,footer},header="\\begin{tabular}{lrrr}
       \\toprule
        & \\multicolumn{3}{c}{Compara\\c{c}\\~oes entre Grupos} \\\\
       \\cmidrule(r){2-4}
       & Linha de Base& 6 meses& 12 meses \\\\\n  \\midrule \n";
stringtable=StringTemplate["Mulheres  `liswoman` \\\\\n Homens `lisman` \\\\\n"][<|"liswoman"->StringJoin[Table[" &"<>geraNumTex[tabci[[1,i]],prec],{i,1,Length[tabci[[2]]]}]],"lisman"->StringJoin[Table[" &"<>geraNumTex[tabci[[2,i]],prec],{i,Length[tabci[[2]]]}]]|>];
geraTable[header,stringtable]]

geratabmd[part1_,varwaves_]:=Module[{lis},
Table[{lis=gerapart[part1[[i]],#];N[Mean[lis]],N[StandardDeviation[lis]]},{i,1,4}]&/@varwaves]

geratabMDTex[tabci_,var_,gnames_,prec_]:=Module[{stringtableC,stringtableI,header},header="   \\begin{tabular}{llrrrrrr}
        \\toprule
          &  & \\multicolumn{6}{c}{"<>var<>"} \\\\
        \\cmidrule(r){3-8} 
        &   & \\multicolumn{2}{c}{Linha de Base} & \\multicolumn{2}{c}{6 meses}&
         \\multicolumn{2}{c}{12 meses}  \\\\\n
        \\midrule 
        &   & \\multicolumn{2}{c}{Estat\\'\\i stica} & \\multicolumn{2}{c}{Estat\\'\\i stica}&
              \\multicolumn{2}{c}{Estat\\'\\i stica}  \\\\\n
        \\cmidrule(r){3-4} \\cmidrule(r){5-6} \\cmidrule(r){7-8}
        Grupo & Sexo &  \\multicolumn{1}{c}{$\\mu$} & \\multicolumn{1}{c}{$\\sigma$} &  \\multicolumn{1}{c}{$\\mu$}& \\multicolumn{1}{c}{$\\sigma$} &  \\multicolumn{1}{c}{$\\mu$} & \\multicolumn{1}{c}{$\\sigma$} \\\\\n  \\midrule 
        ";
stringtableC=StringTemplate[applyLatexCommand["\\multirow{2}{*}",gnames[[1]]]<>" & Mulheres 
       `liswoman` \\\\ \n  & Homens `lisman` \\\\ \n
      "][<|"liswoman"->StringJoin[Table["& "<>geraNumTex[tabci[[i,1,1]],prec]<>" & "<>geraNumTex[tabci[[i,1,2]],prec],{i,1,3}]],"lisman"->StringJoin[Table["& "<>geraNumTex[tabci[[i,2,1]],prec]<>" & "<>geraNumTex[tabci[[i,2,2]],prec],{i,1,3}]]|>];
stringtableI=StringTemplate[applyLatexCommand["\\multirow{2}{*}",gnames[[2]]] <>" & Mulheres 
       `liswoman` \\\\ \n  & Homens `lisman` \\\\ 
      "][<|"liswoman"->StringJoin[Table["& "<>geraNumTex[tabci[[i,3,1]],prec]<>" & "<>geraNumTex[tabci[[i,3,2]],prec],{i,1,3}]],"lisman"->StringJoin[Table["& "<>geraNumTex[tabci[[i,4,1]],prec]<>" & "<>geraNumTex[tabci[[i,4,2]],prec],{i,1,3}]]|>];

geraTable[header,stringtableC<>stringtableI]]





funclsd[lis_]:=Module[{tallylis},
tallylis=Transpose[Sort[Tally[Normal[lis]]]];
Transpose[Append[tallylis,(tallylis/.{x_,y_}->N[y/Length[lis]*100])]]]

(* geratablsd[partlsd_,lisvar_]:=Module[{listemp,listemp2,count},Table[{
lisvar[[j]],
listemp=hashlsd[lisvar[[j]]];
Append[Table[{Last[listemp[[i]]],
Table[{count=Count[listemp2=Normal[partlsd[[k]][All,lisvar[[j]]]],
First[listemp[[i]]]],100*N[count/Length[listemp2]]},{k,1,Length[partlsd]}]},
{i,1,Length[listemp]}],
{"Total",Table[ 
{Length[Normal[partlsd[[k]][All,lisvar[[j]]]]],100},{k,1,Length[partlsd]}]} ]},{j,1,Length[lisvar]}]]
*)

geratablsd[partlsd_,lisvar_]:=Module[{listemp,listemp2,count},Table[{
var,
listemp=hashlsd[var];
Append[Table[{Last[temp],
Table[{count=Count[listemp2=Normal[partlsd[Key[key]][All,var]],
First[temp]],100*N[count/Length[listemp2]]},{key,Normal@Keys@partlsd}]},
{temp,listemp}],
{"Total",Table[ 
{Length[Normal[partlsd[Key[key]][All,var]]],100},{key,Normal@Keys@partlsd}]} ]},{var,lisvar}]]

geratablsdTex[tabci_,prec_]:=Module[{stringtable,len,header},
header="
\\small{
\\renewcommand{\\arraystretch}{0.5}
\\begin{tabular}{llrrrr}
\\toprule
 &  \\multicolumn{5}{c}{ Vari\'aveis S\'ocio Demogr\'aficas "<>geraSize[
tabci[[1,-1,-1,2,1,1]]]<>"} \\\\
& & \\multicolumn{2}{c}{C}&
 \\multicolumn{2}{c}{I}  \\\\\n
\\cmidrule(r){3-4} \\cmidrule(r){5-6}
%& &   \\multicolumn{2}{c}{Estat\\'\\i stica} & 
%      \\multicolumn{2}{c}{Estat\\'\\i stica}  \\\\\n
\\cmidrule(r){3-4} \\cmidrule(r){5-6} 
 & & $N$ & $\\%$ & $N$ & $\\%$ \\\\\n  \\midrule 
";
stringtable=StringJoin[Table[len=Length[tabci[[j,2]]];
StringJoin[Table[If[i==1,
"\\multirow{"<> ToString[len]<>"}{*}{"<>assocvarnames[tabci[[j,1]]]<>"} &","&"]<>tabci[[j,2,i,1]]<>
Table["& "<>ToString[CForm[tabci[[j,2,i,2,k,1]]]]<>
" & "<>geraNumTexFull[tabci[[j,2,i,2,k,2]],prec],{k,1,2}]
<>"\\\\\n",{i,1,len}]]<>" &  &  &  &  & \\\\",{j,1,Length[lisvar]}]];

stringtable=corrigeAcentos[stringtable];
geraTable[header,stringtable]<>"}"
]

geratabvarTex[tabci_,tit_,gnames_,prec_]:=Module[{stringtable,len},
header="
\\small{
\\renewcommand{\\arraystretch}{0.5}
\\begin{tabular}{llrrrr}
\\toprule
 &  \\multicolumn{5}{c}{"<>tit <> " "<>geraSize[
tabci[[1,-1,-1,2,1,1]]+tabci[[1,-1,-1,2,2,1]]]<>"} \\\\
\\cmidrule(r){2-6} 
& & \\multicolumn{2}{c}{"<>gnames[[1]]<>"}&
 \\multicolumn{2}{c}{"<>gnames[[2]]<>"}  \\\\\n
%\\cmidrule(r){3-4} \\cmidrule(r){5-6}
%& &   \\multicolumn{2}{c}{Estat\\'\\i stica} & 
%      \\multicolumn{2}{c}{Estat\\'\\i stica}  \\\\\n
\\cmidrule(r){3-4} \\cmidrule(r){5-6} 
 & & $N$ & $\\%$ & $N$ & $\\%$ \\\\\n  \\midrule 
";
stringtable=StringJoin[Table[len=Length[tabci[[j,2]]];
StringJoin[Table[If[i==1,
"\\multirow{"<> ToString[len]<>"}{*}{"<>assocvarnames[tabci[[j,1]]]<>"} &","&"]<>tabci[[j,2,i,1]]<>
Table["& "<>ToString[CForm[tabci[[j,2,i,2,k,1]]]]<>
" & "<>geraNumTexFull[tabci[[j,2,i,2,k,2]],prec],{k,1,2}]
<>"\\\\\n",{i,1,len}]]<>" &  &  &  &  & \\\\",{j,1,Length[lisvar]}]];

stringtable=corrigeAcentos[stringtable];
geraTable[header,stringtable]<>"}"
]






geratabfisio[partlsd_,lisvarfisio_]:=Module[{listemp},Table[{lisvarfisio[[i,2]],
Table[{listemp=N[Select[Normal[partlsd[[k]][All,lisvarfisio[[i,1]]]],NumberQ]];
Mean[listemp],StandardDeviation[listemp]},{k,1,Length[partlsd]}]},{i,1,Length[lisvarfisio]}]
]


geratabfisioTex[tabci_,veclen_,prec_]:=Module[{stringtable},
stringtable="% Gerado por " <>NotebookFileName[EvaluationNotebook[]] <>" em "<> ToString[DateValue["Day"]]<>"/"<>ToString[DateValue["Month"]]<>"/"<>ToString[DateValue["Year"]]<>"\n
\\begin{tabular}{lrrrrrr}
\\toprule
  &   \\multicolumn{6}{c}{ Vari\[AAcute]veis antropom\[EAcute]tricas " <>geraSize[Length[partlsd[[1]]]+Length[partlsd[[2]]]+Length[partlsd[[3]]]]<> "
} \\\\
\\cmidrule(r){2-7} 
&   \\multicolumn{2}{c}{BR} " <>geraSize[veclen[[1]]]<>" & \\multicolumn{2}{c}{C }"<>geraSize[veclen[[2]]]<>"&
 \\multicolumn{2}{c}{IB } " <>geraSize[veclen[[3]]]<> " \\\\ \n
\\midrule 
%&   \\multicolumn{2}{c}{Estat\\'\\i stica} & \\multicolumn{2}{c}{Estat\\'\\i stica}&
%      \\multicolumn{2}{c}{Estat\\'\\i stica}  \\\\\n
%\\cmidrule(r){2-3} \\cmidrule(r){4-5} \\cmidrule(r){6-7}
      &  \\multicolumn{1}{c}{$\\mu$} & \\multicolumn{1}{c}{$\\sigma$} &  \\multicolumn{1}{c}{$\\mu$}& \\multicolumn{1}{c}{$\\sigma$} &  \\multicolumn{1}{c}{$\\mu$} & \\multicolumn{1}{c}{$\\sigma$}\\\\\n  \\midrule 
";
stringtable=stringtable<>Table[tabci[[j,1]]<>Table[" &"<>geraNumTex[tabci[[j,2,k,1]],prec]<>"& "<>geraNumTex[tabci[[j,2,k,2]],2],{k,1,3}]<>"\\\\\n",{j,1,Length[tabci]}];
stringtable=stringtable<>" 
 \\bottomrule
\\end{tabular}
";
corrigeAcentos[stringtable]
]

geracorrtabTex[corrTab_,corrTabTeste_,lisvar_]:=Module[{headerString,colString,stringtable},
headerString=StringJoin[ " & "<> #&/@ assocvarnames/@lisvar]<>"\\\\\n \\midrule\n ";
colString="l|"<>StringJoin[Table["c",{i,1,Length[corrTab]}]];
header="\n\n\\begin{tabular}{"<>colString<>"}\n\\toprule\n";
stringtable=
headerString<>StringJoin[Table[StringJoin[" "<> assocvarnames[lisvar[[j]]],
 Table[" & \\nprounddigits{2}"<>If[ corrTabTeste[[i,j]]>0.05,
"\\numprint{"<>ToString[CForm[corrTab[[i,j]]]],"{\\bf \\numprint{ "<> 
ToString[CForm[corrTab[[i,j]]]]<>"}" ]<>"}",{i,1,Length[corrTab]}]]<> 
"\\\\\n",{j,1,Length[corrTab]}]];
geraTable[header,stringtable]
]

geracorrtabteste[dataset_,lisvar_]:=Module[{listemp},
Table[listemp=Cases[Transpose[{Normal[dataset[All,lisvar[[i]]]],
Normal[dataset[All,lisvar[[j]]]]}],{_Integer,_Integer}];
N[CorrelationTest[listemp],2],{i,1,Length[lisvar]},{j,1,Length[lisvar]}]]



geracorrtab[dataset_,lisvar_]:=Module[{listemp},
Table[listemp=Cases[Transpose[{Normal[dataset[All,lisvar[[i]]]],
                               Normal[dataset[All,lisvar[[j]]]]}],{_Integer,_Integer}];
N[Correlation[listemp[[All,1]],listemp[[All,2]]]]
,{i,1,Length[lisvar]},{j,1,Length[lisvar]}]]


geraFitTabTex[tabparam_,lisvar_]:=Module[{header,colString,stringtable,varstring},
colString="l"<>StringJoin[Table["c",{i,1,3}]];
header="\n\n\\begin{tabular}{"<>colString<>"}\n\\toprule\n";
varstring=StringJoin[ " & "<> #&/@ {"Estimativa","I. C.","p-value"}]
<>"\\\\\n \\midrule\n ";
stringtable=varstring<>StringJoin[Table[StringJoin[" "<> lisvar[[j]], 
Table[" & "<>geraNumTex[tabparam[[i,j]],3],{i,1,Length[tabparam]}]]<> 
"\\\\\n",{j,1,Length[tabparam[[1]]]}]];
geraTable[header,stringtable]
]


geraTabAtt[tabci_,prec_]:=Module[{stringtable,comment,header,footer},header="\\begin{tabular}{lrrr}
       \\toprule
        & \\multicolumn{3}{c}{Compara\\c{c}\\~oes entre Grupos} \\\\
       \\cmidrule(r){2-4}
       & AUDITESC & AUDITC & DROGAS12 \\\\\n  \\midrule \n";
stringtable=StringTemplate["Mulheres  `liswoman` \\\\\n Homens `lisman` \\\\\n"][<|"liswoman"->StringJoin[Table[" &"<>geraNumTex[tabci[[1,i]],prec],{i,1,Length[tabci[[2]]]}]],"lisman"->StringJoin[Table[" &"<>geraNumTex[tabci[[2,i]],prec],{i,Length[tabci[[2]]]}]]|>];
geraTable[header,stringtable]]

geralinha[data_,str_,total_]:=Prepend[Flatten[{gerapar[data,#SEXOS=="F"&],
gerapar[data,#SEXOS=="M"&],Length[Select[data,#SEXOS=="F"|| #SEXOS=="M"&]],
N[100*Length[Select[data,#SEXOS=="F"|| #SEXOS=="M"&]]/total]}],str]





geraTabPart[tabci_,titulo_,prec_]:=Module[{stringtable,comment,header,footer,lis1},header="\\begin{tabular}{lrrrrrr}
       \\toprule
&\\multicolumn{6}{c}{"<>titulo<>"}\\\\
\\midrule
        & \\multicolumn{2}{c}{Mulheres} & \\multicolumn{2}{c}{Homens} & \\multicolumn{2}{c}{Total} \\\\
       \\cmidrule(r){2-3}  \\cmidrule(r){4-5}  \\cmidrule(r){6-7} 
       & N & \% & N & \% & N & \%  \\\\\n  \\midrule \n";
stringtable=StringJoin[Table[
lis1=geraNumTex[#,prec]&/@tabci[[i,2;;-1]];
tabci[[i,1]]<>" & "<>concTex[lis1]<>"\n",{i,1,Length[tabci]}]
];
stringtable=corrigeAcentos[stringtable];
geraTable[header,stringtable]]



geraTabHelper[tabci_,titulo_,prec_]:=Module[{header,stringtable,lis1},
header="\\midrule
&\\multicolumn{6}{c}{"<>titulo<>" " <> geraSize[tabci[[-1,-2]]]<>"}\\\\
\\midrule
        & \\multicolumn{2}{c}{Mulheres} & \\multicolumn{2}{c}{Homens} & \\multicolumn{2}{c}{Total} \\\\
       \\cmidrule(r){2-3}  \\cmidrule(r){4-5}  \\cmidrule(r){6-7} 
       & N & \% & N & \% & N & \%  \\\\\n  \\midrule \n";
stringtable=StringJoin[Table[
lis1=geraNumTex[#,prec]&/@tabci[[i,2;;-1]];
tabci[[i,1]]<>" & "<>concTex[lis1]<>"\n",{i,1,Length[tabci]}]
];
header<>stringtable
]


geraTabPartVarias[listab_,tit_,prec_]:=Module[{stringtable,comment,header,footer,lis1},
header="\\begin{tabular}{lrrrrrr}
       \\toprule\n 
&\\multicolumn{6}{c}{"<> tit<>"}\\\\";
stringtable=StringJoin[Table[geraTabHelper[listab[[i,1]],listab[[i,2]],prec],{i,1,Length[listab]}]];
stringtable=corrigeAcentos[stringtable];
geraTable[header,stringtable]]

geratabtime[data_,varwaves_,varwavesnames_,varwavesnamess_]:=Module[{tt1,tt2,part1},
part1=GroupBy[data,{#GRUPO,#SEXOS}&];
tt1=Table[{varwavesnamess[[j]],{"C","I"},{"F","M","F","M"},
Table[N[Mean[gerapart[part1[[i]],varwaves[[j]]]]],{i,1,4}]},{j,1,3}];
tt2=Flatten[Table[{varwavesnamess[[j]]<>" $\\times$ "<>varwavesnamess[[k]],{"C","I"},
{"F","M","F","M"},Table[LocationTest[{gerapart[part1[[i]],varwavesnames[[j]]],
gerapart[part1[[i]],varwavesnames[[k]]]},0],{i,1,4}]},{k,2,3},{j,1,k-1}],1];
Join[tt1,tt2]]

geratabtimeTex[varname_,timetable_,prec_,statList_,gnames_,num_]:=Module[{temp,temp2,stringtable,lisfirst,strcols,header,len}, 
temp2=Transpose[timetable[[All,4]]];
lisfirst={applyLatexCommand["\\multirow{2}{*}",gnames[[1]]],"", applyLatexCommand["\\multirow{2}{*}",gnames[[2]]],""}; 
len=Length[timetable];
strcols=StringJoin[Table["r",len]];
header="\n\\begin{tabular}{cc"<>strcols<>"}";
stringtable="
\\toprule\n
    & & "<>applyLatexCommand["\\multicolumn{6}{c}",varname  <>" "<> geraSize[num]]<>" \\\\  \\cmidrule(r){3-"<>ToString[len+2]<>"}
   ";
stringtable= stringtable<>"Grupos & Sexo"<>Table["& "<>applyLatexCommand["\\multicolumn{1}{c}",timetable[[All,1]][[i]]],{i,1,Length[timetable[[All,1]]]}]<>"\\\\\n";
stringtable= stringtable<>"& & "<>concTex[applyLatexCommand["\\multicolumn{1}{c}", #]&/@statList];
stringtable=stringtable<>"\\midrule\n"<>StringJoin[Table[lisfirst[[j]]<>"&"<>If[EvenQ[j],"Homem","Mulher"]<>StringJoin[ Table[" & "<> geraNumTex[ temp2[[j,i]],prec],{i,1, Length[temp2[[j]]]}]]<>"\\\\ \n",{j,1,Length[temp2]}]];
geraTable[header,stringtable]]

geratabCG[data_,varwaves_,varwavesnames_,varwavesnamess_]:=Module[{bbaci,tt1,tt2},
bbaci=data[GroupBy["SEXOS"],GroupBy["GRUPO"]];
tt1=Table[{varwavesnamess[[j]],{"F","M"},{"I","C","I","C"},N[Mean[DeleteMissing@Normal[bbaci[#[[1]],#[[2]]][[All,varwaves[[j]]]]]]/.Times[x_,""]->x]&/@{{"F","I"},{"F","C"},{"M", "I"},{"M","C"
}}},{j,1,3}];
tt2=Table[{varwavesnamess[[k]],{"C","I"},{"F","M","F","M"},LocationTest[{DeleteMissing@Normal[bbaci[#,"C"][All,varwavesnames[[k]]]],DeleteMissing@Normal[bbaci[#,"I"][All,varwavesnames[[k]]]]}]&/@{"F","M"}},{k,1,3}];
Join[tt1,tt2]
]

geratabCGTex[varname_,timetable_,prec_,statList_,gnames_,num_]:=Module[{temp,temp2,stringtable,lisfirst,lislast,strcols,header,len}, 
temp2=Transpose[timetable[[1;;3,4]]]/.{a_,b_,c_,d_}->{b,a,d,c};
lisfirst={"\\multirow{2}{*}{Mulher}","", "\\multirow{2}{*}{Homem}",""}; 
lislast={"\\multirow{2}{*}{Mulher}","", "\\multirow{2}{*}{Homem}",""}; 
len=Length[timetable];
strcols=StringJoin[Table["r",len]];
header="\n\\begin{tabular}{cc"<>strcols<>"}";
stringtable="
\\toprule\n
    & & "<>applyLatexCommand["\\multicolumn{6}{c}",varname  <>" "<> geraSize[num]]<>" \\\\  \\cmidrule(r){3-"<>ToString[len+2]<>"}
   ";stringtable= stringtable<>"Sexo & Grupo "<>Table["& "<>applyLatexCommand["\\multicolumn{1}{c}",timetable[[All,1]][[i]]],{i,1,Length[timetable[[All,1]]]}]<>"\\\\\n";
stringtable= stringtable<>"& & "<>concTex[applyLatexCommand["\\multicolumn{1}{c}", #]&/@statList];
stringtable=stringtable<>"\\midrule\n"<>StringJoin[Table[lisfirst[[j]]<>"&"<>If[EvenQ[j],gnames[[2]],gnames[[1]]]<>StringJoin[ Table[" & "<> geraNumTex[ temp2[[j,i]],prec],{i,1, Length[temp2[[j]]]}]]<>"& "<>If[OddQ[j],concTex[applyLatexCommand["\\multirow{2}{*}",#]&/@{geraNumTex[timetable[[4,4,(j+1)/2]],3],geraNumTex[timetable[[5,4,(j+1)/2]],3],geraNumTex[timetable[[6,4,(j+1)/2]],3]}]," & & \\\\"]<>"\n",{j,1,Length[temp2]}]];

geraTable[header,stringtable]]

geraTabVar[listab_,titulo_,prec_]:=Module[{stringtable,comment,header,footer,lis1},header="\\begin{tabular}{lrrrrrr}
       \\toprule" <> "\\multicolumn{6}{c}{"<> titulo<>"}\\\\";
stringtable=StringJoin[Table[geraTabHelperVar[listab[[i,1]],listab[[i,2]],prec],{i,1,Length[listab]}]];
stringtable=corrigeAcentos[stringtable];
geraTable[header,stringtable]]
geraTabHelperVar[tabci_,titulo_,prec_]:=Module[{header,stringtable,lis1},
header="\\midrule
&\\multicolumn{6}{c}{"<>titulo<>"}\\\\
\\midrule
        & \\multicolumn{2}{c}{Controle} & \\multicolumn{2}{c}{Interven\\c{c}\\~ao} & \\multicolumn{2}{c}{Total} \\\\
       \\cmidrule(r){2-3}  \\cmidrule(r){4-5}  \\cmidrule(r){6-7} 
       & N & \% & N & \% & N & \%  \\\\\n  \\midrule \n";
stringtable=StringJoin[Table[
lis1=geraNumTexFull[#,prec]&/@tabci[[i,2;;-1]];
tabci[[i,1]]<>" & "<>concTex[lis1]<>"\n",{i,1,Length[tabci]}]
];
header<>stringtable
]



geratabAttrition[data_,var1_,var2_,var_,fline_]:=Module[{part2,part1,keys1,keys2},
part2=KeySort[data[GroupBy[var1],GroupBy[var2]]];
part1=KeySort[data[GroupBy[var1]]];
keys1=Normal[Keys[part1]];
keys2=Normal[Keys[part2[[1]]]];
Append[{#,Append[Table[Prepend[fline[part2[#,keys2[[i]]],var,
Length[part1[#]]],keys2[[i]]],{i,1,Length[keys2]}],
Prepend[fline[part1[#],var,Length[part1[#]]],"Total"]]},
LocationTest[{Select[Normal[part2[#,keys2[[1]]][All,var]],IntegerQ],
              Select[Normal[part2[#,keys2[[2]]][All,var]],IntegerQ]}]]&/@keys1]



geraassocAttrition[data_,var1_,var2_,var_,fline_]:=Module[{part2,part1,keys1,keys2,size,faux,faux2},
part2=KeySort[data[GroupBy[var1],GroupBy[var2]]];
part1=KeySort[data[GroupBy[var1]]];
keys1=Normal[Keys[part1]];
keys2=Normal[Keys[part2[[1]]]];
faux[dataset_,x_]:=Function[key,key-><|"Estat"->fline[Normal@dataset[x,key,All,var],Length[part1[x,All]]]|>];
faux2[dataset_,x_]:=<|"Estat"->fline[Normal@dataset[x,All,var],Length[part1[x,All]]]|>;
Dataset[Association[Function[x,x-><|"Grupos"-><|faux[part2,x]/@keys2,
"Total"-><|faux2[part1,x]|>|>,
"Test"->LocationTest[Function[key,Normal@part2[x,key,Select[IntegerQ[#[var]]&],var]]/@keys2]|>]/@keys1]]
]

geraassocAttTex[varname_,timeassoc_,prec_,statList_,gnames_]:=Module[{
temp,temp2,stringtable,lisfirst,lislast,strcols,header,len,ngrupos,nsexos}, 
lisfirst=<| 1->"\\multirow{3}{*}{Mulher}",2->"\\multirow{3}{*}{Homem}"|>; 

len=Length[statList]-1;
strcols=StringJoin[Table["r",len+1]];
header="\n\\begin{tabular}{cl"<>strcols<>"}";
nsexos=Length[timeassoc];
ngrupos=Length[gnames];
stringtable="
\\toprule\n
    & & "<>applyLatexCommand["\\multicolumn{"<>ToString[len]<>"}{c}",varname<>""]<>" \\\\ \n 
  \\cmidrule(r){3-"<>ToString[len+3]<>"} \\\\
   ";
stringtable=stringtable<>"Sexo & Grupo &"<>
concTex[applyLatexCommand["\\multicolumn{1}{c}", #]&/@statList];
stringtable=stringtable<>"\\midrule\n";
stringtable=stringtable<>StringJoin[Normal/@(Table[
 If[#=="True",lisfirst[ksexos], " "]<>" & " <> gnames[#] <>StringJoin[
Table["& "<>
geraNumTex[timeassoc[ksexos,"Grupos",#,"Estat",jesta],prec],{jesta,1,len}]] <>
 " & "<>
If[#=="True","\\multirow{"<>ToString[ngrupos]<>"}{*}{"<> 
geraNumTex[timeassoc[ksexos,"Test"],prec]<>"}  \\\\" ,  " \\\\"]<>"\n" &/@ Keys[timeassoc[ksexos,"Grupos"]],
{ksexos,1,nsexos}])];
geraTable[header,stringtable]]



geratabAttTex[varname_,timetable_,prec_,statList_,gnames_]:=Module[{
temp,temp2,stringtable,lisfirst,lislast,strcols,header,len,ngrupos,nsexos}, 
lisfirst={"\\multirow{3}{*}{Mulher}","\\multirow{3}{*}{Homem}"}; 
len=Length[statList];
strcols=StringJoin[Table["r",len]];
header="\n\\begin{tabular}{cl"<>strcols<>"}";
nsexos=Length[timetable];
stringtable="
\\toprule\n
    & & "<>applyLatexCommand["\\multicolumn{"<>ToString[len]<>"}{c}",varname  <>""]<>" \\\\ \n 
  \\cmidrule(r){3-"<>ToString[len+2]<>"} \\\\
   ";
ngrupos=Length[gnames];
stringtable=stringtable<>"Sexo & Grupo &"<>
concTex[applyLatexCommand["\\multicolumn{1}{c}", #]&/@statList];
stringtable=stringtable<>"\\midrule\n";
Do[
temp2=timetable[[All,2]][[ksexos]];
stringtable=stringtable<>Table[If[igrupos==1,lisfirst[[ksexos]] ,""]<>
" &"<>gnames[[igrupos]]<>StringJoin[ 
Table[" & "<> geraNumTex[ temp2[[igrupos,jesta]],prec],{jesta,2, len }]]<>
"& "<>If[igrupos==1&&igrupos<ngrupos,"\\multirow{3}{*}{"
<>geraNumTex[timetable[[ksexos,ngrupos]],prec]<>"}  \\\\"," \\\\"]<>"\n",{igrupos,1,ngrupos}],{ksexos,1,nsexos}];
geraTable[header,stringtable]]



(* ::Section:: *)
(*Fun\[CCedilla]\[OTilde]es para Gerar Gr\[AAcute]ficos*)


geraGraficoComparacao[part1_,varwaves_,varwavesnames_,varwavesnamess_,gnames_,maxplot_]:=Module[
{lissig,pos,pos2,gr1,errorBar2},
lissig=Flatten[Transpose[Flatten[Table[{Table[LocationTest[{gerapart[part1[[i]],varwaves[[j]]],
gerapart[part1[[i]],varwaves[[k]]]},0],{i,1,4}]},{k,2,3},{j,1,k-1}],2]]];
pos=Flatten[Table[{errorBar2[{i,maxplot-8},{i+1,maxplot-8}],errorBar2[{i,maxplot-6},
{i+2,maxplot-6}],errorBar2[{i+1,maxplot-7},{i+2,maxplot-7}]},{i,1,10,3}]];
pos2=(First/@Select[Transpose[{pos,lissig}],#[[2]]<0.05&])/.errorBar2[x_,y_]->errorBar[x,y];
BoxWhiskerChart[Flatten[Table[gerapart[part1[[i]],#]&/@varwaves,{i,1,4}],1],
ChartLabels->{"LB",6,12,"LB",6,12,"LB",6,12,"LB",6,12},
ChartStyle->{ColorData["Rainbow"][0.95],ColorData["Rainbow"][0.9],
ColorData["Rainbow"][0.85],ColorData["Rainbow"][0.2],ColorData["Rainbow"][0.25],
ColorData["Rainbow"][0.35],ColorData["Rainbow"][0.95],ColorData["Rainbow"][0.9],
ColorData["Rainbow"][0.85],ColorData["Rainbow"][0.2],ColorData["Rainbow"][0.25],
ColorData["Rainbow"][0.3]},PlotRange->{0,maxplot+5},FrameLabel->{"", Style[assocvarnames[var],18]},
Epilog-> {pos2,Inset[Framed[Style[gnames[[2]],14]],{11.42,maxplot+4},{Right,Top}],
Inset[Framed[Style[gnames[[1]],14]],{2.025,maxplot+4},{Left,Top}],{Dashed,Line[{{6.5,-0.75},{6.5,maxplot+6}}]}}]
]
geraGraficoAttrition[var_,part2_,maxplot_]:=
BoxWhiskerChart[{Normal[part2["F", "True"][All,var]],
Normal[part2["M","True"][All,var]],
Normal[part2["F","False"][All,var]],
Normal[part2["M","False"][All,var]]},
ChartLabels->{"Completo","Abandono","Completo","Abandono"},
ChartStyle->{ColorData["Rainbow"][0.95],ColorData["Rainbow"][0.85],
ColorData["Rainbow"][0.2],ColorData["Rainbow"][0.3]},PlotRange->{0,maxplot},
FrameLabel->{"", Style[assocvarnamesShortTex@var,18]}]





(* ::Input:: *)
(**)


(* ::Section:: *)
(*Fun\[CCedilla]\[OTilde]es Auxiliares LaTeX*)


geraNumTex[num_Real,prec_]:=If[Abs[num]<=10^-prec,
StringTemplate["$\\leq$\\nprounddigits{`prec`}\\numprint{`num`}"][<|"prec"->ToString[prec],
"num"->ToString[CForm[10^-prec]]|>],
StringTemplate["\\nprounddigits{`prec`}\\numprint{`num`}"][<|"prec"->ToString[prec],"num"->ToString[CForm[num]]|>]]
geraNumTex[x_String,prec_]:=x;
geraNumTexFull[num_,prec_]:=StringTemplate["\\nprounddigits{`prec`}\\numprint{`num`}"][<|"prec"->ToString[prec],"num"->ToString[num]|>]
geraNumTex[num_Integer,prec_]:=ToString[num];
geraNumTex[lis_List,prec_]:="("<>StringTake[StringJoin[(geraNumTexFull[#,prec]<>", ")&/@lis],{1,-3}]<>")"
geraSize[num_]:="(N= "<>ToString[num]<>")";
concTexHelper[list_]:=If[list=={},"",First[list]<>" & "<>concTexHelper[Rest[list]]]
concTex[list_]:=StringTake[concTexHelper[list],{1,-3}]<>"\\\\\n"
applyLatexCommand[command_,str_]:=command<>"{"<>str<>"}"
compilaTex[texstring_,prefix_]:=Module[{headertex,footertex,str},
headertex="\\documentclass{standalone} % say 
\\usepackage{tikz}
\\usetikzlibrary{calc,shapes.arrows,chains,positioning} 
\\usepackage{lastpage}			% Usado pela Ficha catalogr\[AAcute]fica
\\usepackage{indentfirst}		% Indenta o primeiro par\[AAcute]grafo de cada se\[CCedilla]\[ATilde]o.
\\usepackage{color, colortbl}				% Controle das cores
\\usepackage{graphicx}			% Inclus\[ATilde]o de gr\[AAcute]ficos
\\usepackage{microtype} 			% para melhorias de justifica\[CCedilla]\[ATilde]o\\(\\)
\\usepackage{wallpaper}
\\usepackage{courier}
\\usepackage{listings}
\\usepackage{rotating}
\\usepackage{tikz}
\\usetikzlibrary{shapes,chains,positioning} 
\\usepackage{tablefootnote}
\\usepackage{titlesec}
\\usepackage{hyperref}
\\usepackage{multirow}
\\usepackage{multicol}
\\usepackage{mathtools}
\\usepackage{array}
\\usepackage{pdfpages}
\\usepackage{booktabs}
\\usepackage{numprint}
\\usepackage{lscape}
\\usepackage{fancybox}
\\usepackage[framemethod=TikZ]{mdframed}
\\usepackage{comment}
\\begin{document}
%
";
footertex="\\end{document}";
Export[prefix<>"temp.tex",headertex<>texstring<>footertex,"String"];
SetDirectory[prefix];
RunProcess["gerapng"];
Import[prefix<>"temp.png"]
]



(* ::Section:: *)
(*Fun\[CCedilla]\[OTilde]es para Simula\[CCedilla]\[ATilde]o do Modelo pqq*)


Clear[sumpartial];
Log0[x_]:=If[x>0,Log[x],0];
sumpartial[lis_]:=Module[{sum,sumpart},
sum=0;
sumpart={};
Do[sum+=lis[[i]];
sumpart=Append[sumpart,sum];,{i,1,Length[lis]}];
sumpart/Last[sumpart]]
compara[lis1_,lis2_]:=Module[{lisT,xx,minx,maxx,listal1,listal2,lisplot1,lisplot2},
lisT=Join[lis1,lis2];
      {minx,maxx}={Min[lisT],Max[lisT]};
listal1=Tally[lis1]/.{x_,y_}->{x,Log[y]+1};
listal2=Tally[lis2]/.{x_,y_}->{x,Log[y]+1};
lisplot1=Table[N[Plus@@Select[listal1,#[[1]]<=xx&][[All,2]]],{xx,minx,maxx,1}];
lisplot2=Table[N[Plus@@Select[listal2,#[[1]]<=xx&][[All,2]]],{xx,minx,maxx,1}];
Plus@@Abs[lisplot1-lisplot2]
]
Step[p_,q_][x_]:=Module[{ran},
ran=Random[];
If[x==0 ,If[ ran<=p,0,1],
If[ran>q,x+1,x-1]]]
Steps[p_Real,q_Real,init_Integer,steps_Integer]:=
Nest[If[#==0,If[RandomReal[]<=p,0,1],If[RandomReal[]>q,#+1,#-1]]&,init,steps];
geraParcialSums[lis1_,minx_,maxx_]:=Module[{listal1},
listal1=Tally[lis1]/.{x_,y_}->{x,Log[y]+1};
Table[N[Plus@@Select[listal1,#[[1]]<=xx&][[All,2]]],{xx,minx,maxx,1}]]



