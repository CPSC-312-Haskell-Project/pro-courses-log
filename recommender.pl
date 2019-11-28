:- use_module(library(http/http_open)).
:- use_module(library(http/json)).
:- use_module(library(http/json_convert)).
:- dynamic(found/2).

% load the database
kdb(Dict, Videos, Courses) :-
	setup_call_cleanup(
		http_open('https://raw.githubusercontent.com/CPSC-312-Haskell-Project/pro-courses-log/master/database.json', In, []),
		json_read_dict(In, Dict),
		close(In)
	),
	get_videos(Dict, Videos),
	parse_videos(Videos),
	get_courses(Dict, Courses),
	parse_courses(Courses).
	
% Retrieve videos and its contents from the dictionary
get_videos(X, X.videos).
parse_videos([H|T]) :-
	% Get values from Dict
	term_to_atom(Level,H.level),
	term_to_atom(Subject,H.subject),
	term_to_atom(Topic,H.topic),
	% Build KB
	assert(video(H.name, H.creator, H.link, Level, Subject, Topic)),
	parse_videos(T).

% Retrieve courses and its information from the dictionary
get_courses(X, X.courses).
parse_courses([H|T]) :-
	% Get values from Dict
	term_to_atom(Courseid,H.courseid),
	term_to_atom(Topic,H.topic),
	% Build KB
	assert(course(H.coursename, Courseid, Topic)),
	parse_courses(T).

% Build the knowledge base first whenever the project is made before allowing
% users to make queries
:- initialization(kdb(_,_,_)).