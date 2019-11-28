:- use_module(library(http/http_open)).
:- use_module(library(http/json)).
:- use_module(library(http/json_convert)).
:- dynamic(found/2).

% load the database
load_db(Dict) :- 
	setup_call_cleanup(
		http_open('https://raw.githubusercontent.com/CPSC-312-Haskell-Project/pro-courses-log/master/database.json', In, []),
		json_read_dict(In, Dict),
		close(In)
	), 
	kdb(Dict,_,_).
	
% Our knowledge databases are of two types: videos and courses
kdb(Dict, Videos, Video) :-
	get_videos(Dict, Videos),
	parse_videos(Videos, Video).
kdb(Dict, Courses, Course) :-
	get_courses(Dict, Courses),
	parse_courses(Courses, Course).

% Retrieve videos and its contents from the dictionary
get_videos(X, X.videos).
parse_videos([H|T],H) :-
	% Get values from Dict
	term_to_atom(Id,H.id),
	atom_string(Name,H.name),
	atom_string(Creator,H.creator),
	atom_string(Link,H.link),
	term_to_atom(Level,H.level),
	term_to_atom(Subject,H.subject),
	term_to_atom(Topic,H.topic),
	% Build KB
	assert(video(Id)),
	assert(video_name(Id, Name)),
	assert(video_creator(Id, Creator)),
	assert(video_link(Id, Link)),
	assert(video_level(Id, Level)),
	assert(video_subject(Id, Subject)),
	assert(video_topic(Id, Topic)),
	parse_videos(T,_).

% Retrieve courses and its information from the dictionary
get_courses(X, X.courses).
parse_courses([H|T],H) :-
	% Get values from Dict
	atom_string(Coursename,H.coursename),
	term_to_atom(Courseid,H.courseid),
	term_to_atom(Minlevel,H.minlevel),
	term_to_atom(Topic,H.topic),
	% Build KB
	assert(course(Courseid)),
	assert(course_name(Courseid, Coursename)),
	assert(course_minlevel(Courseid, Minlevel)),
	assert(course_topic(Courseid, Topic)),
	parse_courses(T,_).

% Build the knowledge base first whenever the project is made before allowing
% users to make queries
:- initialization(load_db(_)).

% What to try:
% ?- course_topic(cpsc312,A).
% A = functionalprogramming.

% ?- video_name(1,X).
% X = 'Introduction to matrices'.

% ?- video_topic(I,linearalgebra), video_link(I,L), video_name(I,N).
% I = 1,
% L = 'https://youtu.be/xyAuNHPsq-g',
% N = 'Introduction to matrices'.