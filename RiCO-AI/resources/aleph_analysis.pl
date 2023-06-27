:- module(aleph_analysis, [write_rules/0]).

:- use_module(library(readutil)).
:- open('rules.txt',write,Stream), write(Stream, ":- module(rules, [])"), nl, nl, write(Stream, "%%Rules learned:"), close(Stream).

write_rules :- 
	aleph:write_rules('newRules.txt',user),
	read_file_to_string('newRules.txt',AddRules,[]), 
	write('Updating rules file...'),
	open('rules.txt',append,Stream),
	write(Stream, "\n\n"),
	write(Stream, AddRules),
	close(Stream).