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

:- use_module(library(semweb/rdf_db)).
:- use_module(library(readutil)).
:- working_directory(_,'/Users/hugolinbergier/RDF_DB').

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

init_ILP5 :- init_ILP(5).

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
	atomic_list_concat([Prefix, LocalName], '_', PrefixLocalName),
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



rdf_auto :-
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
	induce('test_rdf.pl'),
	aleph:write_rules('newRules.txt',user),
	read_file_to_string('newRules.txt',AddRules,[]), 
	logw('Updating rules file...'),
	open('/Users/hugolinbergier/dev/RICO/RiCO-AI/resources/RICOrules.txt',append,Stream),
	write(Stream, AddRules), nl(Stream),
	close(Stream),
	fail.

two_rdf_resources(S,O):-
	rdf_resource(S),
	rdf_resource(O).
	%% findall(R,rdf_resource(R),Rs),
	%% random_member(S,Rs),
	%% random_member(O,Rs).

assert_rdf_predicates :- 
	rdf(_, P, _),
	decompose_uri(P, P2),
	(\+ rdf_predicate(P2) -> assertz(rdf_predicate(P2)) ; true),
	write('asserted predicate '), write(P2), nl,
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
	rdf(S, P, O), 
	decompose_uri(S, S2), decompose_uri(P, P2), decompose_uri(O, O2), 
	P2 \= Pred,
	Term =.. [P2, S2, O2], 
	writeq(Stream, Term), write(Stream, '.'), nl(Stream),
	fail.

write_rdf_pos(Stream, Pred) :-
	recompose_uri(Pred, P),
	rdf(S, P, O), 
	decompose_uri(S, S2), decompose_uri(O, O2), 
	Term =.. [Pred, S2, O2],	 
	writeq(Stream, Term), write(Stream, '.'), nl(Stream),
	fail.

write_rdf_neg(Stream, Pred) :-
	recompose_uri(Pred, P), 
	rdf_resource(S),
	rdf_resource(O),
	\+ rdf(S, P, O),
	decompose_uri(S, S2), decompose_uri(O, O2), 
	Term =.. [Pred, S2, O2], 
	writeq(Stream, Term), write(Stream, '.'), nl(Stream),
	random(1,10,R),
	(R > 8 -> true ; fail).

write_rdf(FileName) :-
	open(FileName, append, Stream), 
	\+ write_rdf_predicates(Stream),
	write(Stream, ']).'), nl(Stream), nl(Stream),
	\+ write_rdf_facts(Stream),
	close(Stream).




write_rdf_aleph_init(Stream) :-
	write(Stream, ':- use_module(library(aleph)).'), nl(Stream),
	write(Stream, ':- if(current_predicate(use_rendering/1)).') , nl(Stream),
	write(Stream, ':- use_rendering(prolog).') , nl(Stream),
	write(Stream, ':- endif.'), nl(Stream),
	write(Stream, ':- aleph.'), nl(Stream),
	write(Stream, ':- style_check(-discontiguous).'), nl(Stream),
	write(Stream, ':- aleph_set(i,2).'), nl(Stream),
	write(Stream, ':- aleph_set(verbosity,1).'), nl(Stream),
	write(Stream, ':- aleph_set(minpos,2).'), nl(Stream).
	

write_rdf_aleph_mode(Stream) :-
	rdf_current_predicate(P),
	once(rdf(S, P, O)), rdf(S, rdf:type, S_Type), rdf(O, rdf:type, O_Type),
	decompose_uri(P, P2), decompose_uri(S_Type, S2), decompose_uri(O_Type, O2),
	write(Stream, ':- mode(*, '), write(Stream, P2), write(Stream, '(+'), write(Stream, S2), write(Stream, ', +'), write(Stream, O2), write(Stream, ')).'), nl(Stream),
	fail.

write_rdf_aleph_det(Stream, Pred, Arity) :-
	rdf_current_predicate(P),
	decompose_uri(P, P2),
	P2 \= Pred,
	write(Stream, ':- determination('), write(Stream, Pred), write(Stream, '/'), write(Stream, Arity), write(Stream, ', '), write(Stream, P2), write(Stream, '/2).'), nl(Stream),
	fail.

write_rdf_aleph_dyn(Stream) :-
	rdf_current_predicate(P),
	decompose_uri(P, P2),
	write(Stream, ':- dynamic('), write(Stream, P2), write(Stream, '/2).'), nl(Stream),
	fail.

write_rdf_aleph(FileName, Pred, Arity) :-
	open(FileName, write, Stream),
	logw('    * Writing initialization queries...'), 
	write_rdf_aleph_init(Stream), nl(Stream), nl(Stream),
	logw('    * Writing mode queries...'),
	\+ write_rdf_aleph_mode(Stream), nl(Stream), nl(Stream),
	logw('    * Writing determination queries...'),
	\+ write_rdf_aleph_det(Stream, Pred, Arity), nl(Stream), nl(Stream),
	logw('    * Writing dynamic declarations...'),
	\+ write_rdf_aleph_dyn(Stream), nl(Stream), nl(Stream),
	logw('    * Writing background facts...'),
	write(Stream, ':-begin_bg.'), nl(Stream),
	\+ write_rdf_bg(Stream, Pred), nl(Stream), nl(Stream),
	write(Stream, ':-end_bg.'), nl(Stream), nl(Stream), nl(Stream),
	logw('    * Writing positive examples...'),
	write(Stream, ':-begin_in_pos.'), nl(Stream),
	\+ write_rdf_pos(Stream, Pred), nl(Stream), nl(Stream),
	write(Stream, ':-end_in_pos.'), nl(Stream), nl(Stream), nl(Stream),
	logw('    * Writing negative examples...'),
	write(Stream, ':-begin_in_neg.'), nl(Stream),
	write_rdf_neg(Stream, Pred), nl(Stream), nl(Stream),
	write(Stream, ':-end_in_neg.'), nl(Stream),
	logw('    * End of file writing.'),
	close(Stream),
	!.




logw(T) :-
	write('RDF ILP LOG >>  '), write(T), nl.
logw2(T) :-
	write('RDF ILP LOG >>  '), write(T).



sum_list([], 0).
sum_list([H|T], Sum) :-
   sum_list(T, Rest),
   Sum is H + Rest.






%% rdf_current_predicate(P).