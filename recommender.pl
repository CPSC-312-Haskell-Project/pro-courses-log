:- use_module(library(http/http_open)).
:- use_module(library(http/json)).
:- use_module(library(http/json_convert)).
:- dynamic(found/2).

% load the database
kdb(Dict, R, S) :-
	setup_call_cleanup(
		http_open('LINK', In, []),
		json_read_dict(In, Dict),
		close(In)
	).