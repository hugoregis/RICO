:- module(aleph_analysis, [write_rules/0]).

:- use_module(library(readutil)).
:- working_directory(_,'/Users/hugolinbergier/dev/RICO/RiCO-AI/resources').

timeFormatted(A) :- get_time(Now), format_time(atom(A), "%r", Now).


:- open('rules.txt',write,Stream), write(Stream, ":- module(rules, [])"), nl, nl, write(Stream, "%%Rules learned at "), timeFormatted(T), write(Stream, T), close(Stream).


write_rules :- 
	aleph:write_rules('newRules.txt',user),
	read_file_to_string('newRules.txt',AddRules,[]), 
	write('Updating rules file...'),
	open('rules.txt',append,Stream),
	write(Stream, "\n\n%% => Time: "),
	timeFormatted(T),
	write(Stream, T),
	write(Stream, "\n"),
	write(Stream, AddRules),
	close(Stream).

