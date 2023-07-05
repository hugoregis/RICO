%% ['/Users/hugolinbergier/dev/RICO/RiCO-AI/resources/rdf2pl.pl'].
%% init_ILP(10).

%% load_rdf_files(100).
%% assert_rdf.
%% rdf_predicate(X).
%% two predicates such that P1(X,Y) and P2(Y,Z)
%% rdf_predicate(P1), rdf_predicate(P2), Term1 =.. [P1,X,Y], Term2 =.. [P2,Y,Z], Term1, Term2.
%% two non type predicates such that P1(X,Y) and P2(Y,Z), with X and Z having the same RDF type
%% rdf_predicate(P1), P1 \= rdf_type, rdf_predicate(P2), P2 \= rdf_type, Term1 =.. [P1,X,Y], Term2 =.. [P2,Y,Z], Term1, Term2, rdf_type(X,T), rdf_type(Z,T).
%% two non type predicates such that P1(X,Y) and P2(Y,Z), with X and Z having the same RICO type
%% rdf_predicate(P1), P1 \= rdf_type, rdf_predicate(P2), P2 \= rdf_type, Term1 =.. [P1,X,Y], Term2 =.. [P2,Y,Z], Term1, Term2, rdf_type(X,T), rdf_type(Z,T), get_prefix(T,rico). 
%% two non type predicates such that P1(X,Y) and P2(Y,Z), with X and Z being two different resources with the same RICO type
%% rdf_predicate(P1), P1 \= rdf_type, rdf_predicate(P2), P2 \= rdf_type, Term1 =.. [P1,X,Y], Term2 =.. [P2,Y,Z], Term1, Term2, rdf_type(X,T), rdf_type(Z,T), X \= Z, get_prefix(T,rico).

%% aleph:write_rules('rules.txt',user).

:- set_prolog_stack(global, limit(10000000000)).
:- set_prolog_stack(local, limit(10000000000)).
:- set_prolog_stack(trail, limit(10000000000)).
:- set_prolog_stack(global, spare(2048)).
:- set_prolog_stack(local, spare(2048)).
:- set_prolog_stack(trail, spare(2048)).

:- findall(X, prolog_stack_property(global, X), Xs), write(Xs).
:- findall(X, prolog_stack_property(local, X), Xs), write(Xs).
:- findall(X, prolog_stack_property(trail, X), Xs), write(Xs).

%% :- use_module(library(persistency)).
:- use_module(aleph_analysis).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(readutil)).
:- use_module(symmetricPred).
:- use_module(library(sparqlprog)).

%% :- rdf_load('FRAN_Agent_010128.rdf').

:- rdf_register_prefix(rdf, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#').
:- rdf_register_prefix(rdfs, 'http://www.w3.org/2000/01/rdf-schema#').
:- rdf_register_prefix(rico, 'https://www.ica.org/standards/RiC/ontology#').
:- rdf_register_prefix(owl, 'http://www.w3.org/2002/07/owl#').
:- rdf_register_prefix(base, 'http://data.archives-nationales.culture.gouv.fr/').
:- rdf_register_prefix(html, 'http://www.w3.org/1999/xhtml').
:- rdf_register_prefix(isni, 'http://isni.org/ontology#').
:- rdf_register_prefix(dc, 'http://purl.org/dc/elements/1.1/').
:- rdf_register_prefix(skos, 'http://www.w3.org/2004/02/skos/core#').


:- dynamic(rdf_predicate/1).


:- sparql_endpoint( fuseki, 'http://100.25.165.73:3030/ricoDataset').
:- sparql_endpoint( fusekiInfer, 'http://100.25.165.73:3030/ricoDatasetInferred').


fileSizeFactor(100).

degreeProb(0,1,N) :- fileSizeFactor(F), N is 30*F.
degreeProb(1,N1,N2) :- fileSizeFactor(F), N1 is (30*F)+1, N2 is 50*F.
degreeProb(2,N1,N2) :- fileSizeFactor(F), N1 is (50*F)+1, N2 is 75*F.
degreeProb(3,N1,N2) :- fileSizeFactor(F), N1 is (75*F)+1, N2 is 90*F.
degreeProb(4,N1,N2) :- fileSizeFactor(F), N1 is (90*F)+1, N2 is 100*F.
degreeProb(100,0,0).
maxDegree(4).
avgNumPos(200).
avgNumNeg(100).

write_rdf_aleph_init(Stream) :-
	write(Stream, ':- use_module(library(aleph)).'), nl(Stream),
	write(Stream, ':- if(current_predicate(use_rendering/1)).') , nl(Stream),
	write(Stream, ':- use_rendering(prolog).') , nl(Stream),
	write(Stream, ':- endif.'), nl(Stream),
	write(Stream, ':- aleph.'), nl(Stream),
	%% write(Stream, ':- style_check(-discontiguous).'), nl(Stream),
	%% write(Stream, ':- aleph_set(i,2).'), nl(Stream),
	%% write(Stream, ':- aleph_set(mode_overlap_threshold,0.95).'), nl(Stream),
	%% write(Stream, ':- aleph_set(search,df).'), nl(Stream),
	%% write(Stream, ':- aleph_set(search,heuristic).'), nl(Stream),
	%% write(Stream, ':- aleph_set(minacc,0.0).'), nl(Stream),
	%% write(Stream, ':- aleph_set(explore,true).'), nl(Stream),
	write(Stream, ':- aleph_set(goodfile,\'goodfile.txt\').'), nl(Stream),
	write(Stream, ':- aleph_set(good,true).'), nl(Stream),
	write(Stream, ':- aleph_set(minpos,2).'), nl(Stream).


rdf_auto :-
	working_directory(_,'/Users/hugolinbergier/RDF_DB'),
	(rdf(_,_,_) -> true ; \+ load_rdf_files),
	\+ retractSymm,
	working_directory(_,'/Users/hugolinbergier/dev/RICO/RiCO-AI/resources'),
	logw('Asserting all new RDF predicates...'),
	\+ assert_rdf_predicates,
	findall(Pred, rdf_predicate(Pred), ListPred),
	member(P, ListPred), 
	P \= rdf_type,
	length(ListPred, NbPred),
	nth0(I,ListPred,P),
	logw2('Writting Aleph file for predicate '), write(I), write('/'), write(NbPred), write(': '), write(P), nl,
	write_rdf_aleph('test_rdf.pl', P, 2),
	logw('Consutling new Aleph file...'),
	consult('test_rdf.pl'),
	logw('Inducing new Aleph file...'),
	induce_modes,
	%% induce_theory,
	%% induce_cover,
	%% induce_features,
	%% induce,
	aleph_analysis:write_rules,
	\+ closeAllFiles,
	fail.

load_rdf_files :- 
	rdf_load('/Users/hugolinbergier/RDF_DB/RiC-O_v0-2.rdf', [register_namespaces(true)]),
	directory_files('/Users/hugolinbergier/RDF_DB',Files),
	member(File, Files),
	file_name_extension(_, 'rdf', File),
	rdf_load(File, [register_namespaces(true)]),
	fail.

load_rdf_files(MAX) :- 
	rdf_load('/Users/hugolinbergier/RDF_DB/RiC-O_v0-2.rdf', [register_namespaces(true)]),
	directory_files('/Users/hugolinbergier/RDF_DB',Files),
	member(File, Files),
	file_name_extension(_, 'rdf', File),
	nth0(I,Files,File), 
	I < MAX,
	rdf_load(File, [register_namespaces(true)]),
	fail.

retractSymm:-
	symmPred(P),
	recompose_uri(P,Pred),
	rdf_retractall(_,Pred,_),
	fail.

move1degree(triple(X,Y,Z), newRDFTriple(X2,Y2,Z2)):-
	random_permutation([X,Y,Z],L1),
	random_permutation([X2,Y2,Z2],L2),
	member(FixedValue, L1),
	member(NewPosition, L2),
	NewPosition = FixedValue,
	rdf(X2,Y2,Z2),
	!.

moveNdegrees(2, triple(X,Y,Z), newRDFTriple(XN,YN,ZN)):-
	%% write('move 2 degree from : '), write(triple(X,Y,Z)), nl,
	move1degree(triple(X,Y,Z), newRDFTriple(X2,Y2,Z2)),
	move1degree(triple(X2,Y2,Z2), newRDFTriple(XN,YN,ZN)),
	!.
moveNdegrees(N, triple(X,Y,Z), newRDFTriple(XN,YN,ZN)):-
	N > 2,
	%% write('move N degree with N = '), write(N), write(' from : '), write(triple(X,Y,Z)), nl,
	N2 is N - 1,
	%% write('N2 is now: '), write(N2), nl,
	move1degree(triple(X,Y,Z), newRDFTriple(X2,Y2,Z2)),
	moveNdegrees(N2, triple(X2,Y2,Z2), newRDFTriple(XN,YN,ZN)).

contextual_rdf(Pred) :-
	%% write('predicate: '), write(Pred), nl,
	maxDegree(MaxDeg),
	degreeProb(MaxDeg,_,MaxProb),
	rdf(S,Pred,O),
	%% write('new rdf fact.'), nl,
	once(random(0,MaxProb,R)),
	%% write('Random number: '), write(R), nl,
	degreeProb(Deg,Min,Max), R >= Min, R =< Max,
	%% write('Degree: '), write(Deg), nl,
	assertContextRDFDegree(S,Pred,O,Deg).

assertContextRDFDegree(S,Pred,O,Deg) :-
	(Deg == 0 -> (assert(contextRDF(S,Pred,O)), !, fail) ; 
		(Deg == 1 -> (move1degree(triple(S,Pred,O), newRDFTriple(X,Y,Z)), assert(contextRDF(X,Y,Z)), !, fail) ; 
			(Deg == 2 -> (moveNdegrees(2, triple(S,Pred,O), newRDFTriple(X,Y,Z)), assert(contextRDF(X,Y,Z)), !, fail) ; 
				(Deg == 3 -> (moveNdegrees(3, triple(S,Pred,O), newRDFTriple(X,Y,Z)), assert(contextRDF(X,Y,Z)), !, fail) ; 
					true)))),
	!
	.



init_ILP(NbFiles) :- 
	get_time(T1),
	\+ load_rdf_files(NbFiles),
	get_time(T2),
	\+ rdf_auto,
	get_time(T3),
	T_load is T2 - T1,
	T_ILP is T3 - T2,
	write('--------------------------------'), nl,
	write('Results:'), nl,
	write('    # of predicates: '), count_rdf_predicates(PredN), write(PredN), nl,
	write('    # of triplets: '), count_rdf(TriopletN), write(TriopletN), nl,
	write('    # of predicates with rules found: '), rdf_rules_pred_count(LP_Count), write(LP_Count), nl,
	write('    # rules found: '), rdf_rules_count(Rules_Count), write(Rules_Count), nl,
	write('    RDF loading time: '), write(T_load), nl,
	write('    RDF ILP time: '), write(T_ILP), nl, nl,
	read_file_to_string('rules.txt',Rules,[]), 
	write(Rules), nl.


decompose_uri(URI, Prefix, LocalName) :- rdf_global_id(Prefix:LocalName, URI).
decompose_uri(URI, PrefixLocalName) :- 
	rdf_global_id(Prefix:LocalName, URI), 
	atomic_list_concat([Prefix, LocalName], '_', PrefixLocalName2),
	term_to_atom(PrefixLocalName2, PrefixLocalName),
	!.
decompose_uri(URI, URI).
recompose_uri(PrefixLocalName, URI) :-
	atomic_list_concat([Prefix, LocalName], '_', PrefixLocalName),
	rdf_global_id(Prefix:LocalName, URI).

get_prefix(PrefixLocalName, Prefix) :- 
	recompose_uri(PrefixLocalName, URI),
	decompose_uri(URI, Prefix, _).

count_rdf(N) :-
	findall([X,Y,Z], rdf(X,Y,Z), L), 
	length(L,N).

count_rdf_predicates(N) :-
	findall(P, rdf_current_predicate(P), L), 
	length(L,N).

rdf_rules(P,X) :- 
	rdf_current_predicate(P),
	decompose_uri(P, P2),
	Term =.. [P2, _, _],
	predicate_property(Term,number_of_rules(X)), 
	X > 0.

rdf_rules_pred_count(LP_count) :- 
	findall(P, rdf_rules(P,_), LP), 
	length(LP,LP_count).

rdf_rules_count(NbRules) :- 
	findall([P,X], rdf_rules(P,X), LP), 
	findall(N, member([_,N], LP), Ns),
	sum_list(Ns, NbRules).





two_rdf_resources(S,O):-
	rdf_resource(S),
	rdf_resource(O).
	%% findall(R,rdf_resource(R),Rs),
	%% random_member(S,Rs),
	%% random_member(O,Rs).

assert_rdf_predicates :- 
	findall(Pred, rdf(_,Pred,_), ListPred),
	sort(ListPred, ListPredDistinct),
	member(P,ListPredDistinct),
	decompose_uri(P, P2),
	\+ sub_atom(P2, _, _, _, '-'),
	\+ get_prefix(P2, dc),
	\+ get_prefix(P2, skos),
	assertz(rdf_predicate(P2)),
	%% write('asserted predicate '), write(P2), nl,
	fail.

assert_rdf :- 
	rdf(S, P, O), 
	decompose_uri(S, S2), decompose_uri(P, P2), decompose_uri(O, O2), 
	Term =.. [P2, S2, O2], 
	assertz(Term),
	(\+ rdf_predicate(P2) -> assertz(rdf_predicate(P2)) ; true),
	fail.

write_rdf_predicates(Stream) :-
	write(Stream, ':- module(rdf_db, ['), nl(Stream),
	rdf_current_predicate(P),
	decompose_uri(P, P2),
	write(Stream, P2), write(Stream, '/2,'), nl(Stream), 
	fail.

write_rdf_facts(Stream) :-
	rdf(S, P, O), 
	decompose_uri(S, S2), decompose_uri(P, P2), decompose_uri(O, O2), 
	Term =.. [P2, S2, O2], 
	writeq(Stream, Term), write(Stream, ','), nl(Stream),
	fail.

write_rdf_bg(Stream, Pred) :-
	retractall(contextRDF(_,_,_)),
	recompose_uri(Pred, PredFullURI),
	once(contextual_rdf(PredFullURI)),
	contextRDF(S, P, O), 
	decompose_uri(S, S2), decompose_uri(P, P2), decompose_uri(O, O2),
	Term =.. [P2, S2, O2], 
	%% write('background fact :'), write(Term), nl,
	writeq(Stream, Term), write(Stream, '.'), nl(Stream),
	fail.

write_rdf_bgRules(Stream) :-
	read_file_to_string('bgRules.pl',BGRules,[]), 
	write('Adding Background rules file...'),
	write(Stream, BGRules).


write_rdf_pos(Stream, Pred) :-
	recompose_uri(Pred, P),
	rdf(S, P, O), 
	avgNumPos(N), random(0,N,R), (R == 0 -> (!, fail) ; true),
	decompose_uri(S, S2), decompose_uri(O, O2), 
	Term =.. [Pred, S2, O2],	 
	writeq(Stream, Term), write(Stream, '.'), nl(Stream),
	fail.

write_rdf_neg(Stream, Pred) :-
	recompose_uri(Pred, P), 
	rdf_resource(S),
	rdf_resource(O),
	\+ rdf(S, P, O),
	avgNumNeg(N), random(0,N,R), (R == 0 -> (!, fail) ; true),
	decompose_uri(S, S2), decompose_uri(O, O2), 
	Term =.. [Pred, S2, O2], 
	writeq(Stream, Term), write(Stream, '.'), nl(Stream),
	fail.
	

write_rdf(FileName) :-
	open(FileName, append, Stream), 
	\+ write_rdf_predicates(Stream),
	write(Stream, ']).'), nl(Stream), nl(Stream),
	\+ write_rdf_facts(Stream),
	close(Stream).





	

write_rdf_aleph_mode(Stream) :-
	rdf_current_predicate(P),
		%% current_iteration(I),
		%% max_iteration(M),
		%% (I > M -> (!, fail) ; true),
		%% I2 is I + 1,
		%% retractall(current_iteration(_)),
		%% assert(current_iteration(I2)),
	%% once(rdf(S, P, O)), rdf(S, rdf:type, S_Type), rdf(O, rdf:type, O_Type),
	decompose_uri(P, P2), 
	%% decompose_uri(S_Type, S2), decompose_uri(O_Type, O2),
	write(Stream, ':- mode(*, '), write(Stream, P2), write(Stream, '(+rdf_thing, +rdf_thing)).'), nl(Stream),
		%% write(Stream, S2), write(Stream, ', +'), write(Stream, O2), write(Stream, ')).'), nl(Stream),
	fail.

write_rdf_aleph_det(Stream, Pred, Arity) :-
	rdf_current_predicate(P),
		%% current_iteration(I),
		%% max_iteration(M),
		%% (I > M -> (!, fail) ; true),
		%% I2 is I + 1,
		%% retractall(current_iteration(_)),
		%% assert(current_iteration(I2)),
	decompose_uri(P, P2),
	P2 \= Pred,
	write(Stream, ':- determination('), write(Stream, Pred), write(Stream, '/'), write(Stream, Arity), write(Stream, ', '), write(Stream, P2), write(Stream, '/2).'), nl(Stream),
	fail.

write_rdf_aleph_dyn(Stream) :-
	rdf_current_predicate(P),
		%% current_iteration(I),
		%% max_iteration(M),
		%% (I > M -> (!, fail) ; true),
		%% I2 is I + 1,
		%% retractall(current_iteration(_)),
		%% assert(current_iteration(I2)),
	decompose_uri(P, P2),
	write(Stream, ':- dynamic('), write(Stream, P2), write(Stream, '/2).'), nl(Stream),
	fail.

write_rdf_aleph(FileName, Pred, Arity) :-
	open(FileName, write, Stream),
	logw('    * Writing initialization queries...'), 
	write_rdf_aleph_init(Stream), nl(Stream), nl(Stream),
	logw('    * Writing mode queries...'),
	retractall(current_iteration(_)),
	assert(current_iteration(0)),
	\+ write_rdf_aleph_mode(Stream), nl(Stream), nl(Stream),
	logw('    * Writing determination queries...'),
	retractall(current_iteration(_)),
	assert(current_iteration(0)),
	\+ write_rdf_aleph_det(Stream, Pred, Arity), nl(Stream), nl(Stream),
	logw('    * Writing dynamic declarations...'),
	retractall(current_iteration(_)),
	assert(current_iteration(0)),
	\+ write_rdf_aleph_dyn(Stream), nl(Stream), nl(Stream),
	logw('    * Writing background facts...'),
	write(Stream, ':-begin_bg.'), nl(Stream),
	retractall(current_iteration(_)),
	assert(current_iteration(0)),
	\+ write_rdf_bg(Stream, Pred), nl(Stream), nl(Stream),
	write_rdf_bgRules(Stream), nl(Stream), nl(Stream),
	write(Stream, ':-end_bg.'), nl(Stream), nl(Stream), nl(Stream),
	logw('    * Writing positive examples...'),
	write(Stream, ':-begin_in_pos.'), nl(Stream),
	retractall(current_iteration(_)),
	assert(current_iteration(0)),
	\+ write_rdf_pos(Stream, Pred), nl(Stream), nl(Stream),
	write(Stream, ':-end_in_pos.'), nl(Stream), nl(Stream), nl(Stream),
	logw('    * Writing negative examples...'),
	write(Stream, ':-begin_in_neg.'), nl(Stream),
	retractall(current_iteration(_)),
	assert(current_iteration(0)),
	\+ write_rdf_neg(Stream, Pred), nl(Stream), nl(Stream),
	write(Stream, ':-end_in_neg.'), nl(Stream),
	logw('    * End of file writing.'),
	close(Stream),
	!.


closeAllFiles :- stream_property(S, file_name(_)), close(S), fail.

logw(T) :-
	write('RDF ILP LOG >>  '), write(T), nl.
logw2(T) :-
	write('RDF ILP LOG >>  '), write(T).



sum_list([], 0).
sum_list([H|T], Sum) :-
   sum_list(T, Rest),
   Sum is H + Rest.






%% rdf_current_predicate(P).