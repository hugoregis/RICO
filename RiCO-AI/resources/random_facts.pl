:- module(random_facts, [get_random_pred/2, prepare_randomization_metadata/1, get_X_random_pred/3]).

% random_fact(Head, Instantiation, Index)
:- dynamic(random_fact/3).

% fact_count(Head, Count)
:- dynamic(fact_count/2).

% one big side-effect to make it possible to query for a random predicate
prepare_randomization_metadata(Goal) :-
  findall(Goal, Goal, Occurrances),
  prepare_randomization_metadata(Occurrances, 0),
  Goal =.. [Head|_],
  length(Occurrances, N),
  asserta(fact_count(Head, N)).

prepare_randomization_metadata([], _).
prepare_randomization_metadata([Goal|Goals], N) :-
  Goal =.. [Head|_],
  asserta(random_fact(Head, Goal, N)),
  N1 is N+1,
  prepare_randomization_metadata(Goals, N1), !.

get_random_pred(Head, Pred) :-
  fact_count(Head, N),
  % pick a random number between 0 and the # of facts we have for this pred
  random(0, N, I),
  random_fact(Head, Pred, I), !.

get_X_random_pred(Head, X, Preds) :-
  fact_count(Head, N),
  % pick a random number between 0 and the # of facts we have for this pred
  (X < N -> randseq(X,N,L) ; (T is N - 2, randseq(T,N,L))),
  findall(P, (member(I,L), random_fact(Head, P, I)), Preds), 
  !.

