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
noun_phrase(L0,L4,Entity,C0,C4) :-
	det(L0,L1,Entity,C0,C1),
	adj(L1,L2,Entity,C1,C2),
	noun(L2,L3,Entity,C2,C3),
	mp(L3,L4,Entity,C3,C4).

% Determiners.
det([the | L],L,_,C,C).
det([by | L],L,_,C,C).
det([a | L],L,_,C,C).
det(L,L,_,C,C).

% Adjectives.
adj(L,L,_,C,C).

% Modifying phrases
mp(L,L,_,C,C).
mp(L0,L2,Subject,C0,C2) :-
	reln(L0,L1,Subject,Object,C0,C1),
	noun_phrase(L1,L2,Object,C1,C2).
mp([that|L0],L2,Subject,C0,C2) :-
	reln(L0,L1,Subject,Object,C0,C1),
	noun_phrase(L1,L2,Object,C1,C2).
mp([that,is|L0],L2,Subject,C0,C2) :-
	reln(L0,L1,Subject,Object,C0,C1),
	noun_phrase(L1,L2,Object,C1,C2).
mp([with|L0],L2,Subject,C0,C2) :-
	reln(L0,L1,Subject,Object,C0,C1),
	noun_phrase(L1,L2,Object,C1,C2).
mp([with, the|L0],L2,Subject,C0,C2) :-
	reln(L0,L1,Subject,Object,C0,C1),
	noun_phrase(L1,L2,Object,C1,C2).

% Noun
noun([videos| L],L,Entity,C,[isVideo(Entity)|C]).
noun([video| L],L,Entity,C,[isVideo(Entity)|C]).
noun([X| L],L,X,C,C) :- isVideo(X).
noun([courses| L],L,Entity,C,[isCourse(Entity)|C]).
noun([course| L],L,Entity,C,[isCourse(Entity)|C]).
noun([X| L],L,X,C,C) :- isCourse(X).


%noun([X | L],L,X,C,C) :- isVideoCreator(X).

% Relations.
reln([similar, topic, to| L],L,O1,O2,_,[sameTopic(O1,O2)]).
reln([similar, topic, as| L],L,O1,O2,_,[sameTopic(O1,O2)]).
reln([same, topic, as| L],L,O1,O2,_,[sameTopic(O1,O2)]).
reln([similar, subject, to| L],L,O1,O2,_,[sameSubject(O1,O2)]).
reln([similar, subject, as| L],L,O1,O2,_,[sameSubject(O1,O2)]).
reln([same, subject, as| L],L,O1,O2,_,[sameSubject(O1,O2)]).
reln([same, channel, as| L],L,O1,O2,_,[sameCreator(O1,O2)]).
reln([same, creator, as| L],L,O1,O2,_,[sameCreator(O1,O2)]).
reln([next, video, from| L],L,O1,O2,_,[nextVideo(O1,O2)]).
reln([previous, video, from| L],L,O1,O2,_,[previousVideo(O1,O2)]).
reln([made, by| L],L,O1,O2,_,[getCreator(O1,O2)]).
reln([created, by| L],L,O1,O2,_,[getCreator(O1,O2)]).
reln([creator, of| L],L,O1,O2,_,[getCreator(O1,O2)]).
reln([uploaded, by| L],L,O1,O2,_,[getCreator(O1,O2)]).
reln([link, for| L],L,O1,O2,_,[getLink(O1,O2)]).
reln([link, to| L],L,O1,O2,_,[getLink(O1,O2)]).
reln([level, of| L],L,O1,O2,_,[getLevel(O1,O2)]).
reln([level, for| L],L,O1,O2,_,[getLevel(O1,O2)]).
reln([subject, of| L],L,O1,O2,_,[getSubject(O1,O2)]).
reln([subject, for| L],L,O1,O2,_,[getSubject(O1,O2)]).
reln([topic, of| L],L,O1,O2,_,[getTopic(O1,O2)]).
reln([topic, for| L],L,O1,O2,_,[getTopic(O1,O2)]).
reln([course, topic, of| L],L,O1,O2,_,[getCourseTopic(O1,O2)]).
reln([course, topic, for| L],L,O1,O2,_,[getCourseTopic(O1,O2)]).
reln([level, requirement, of| L],L,O1,O2,_,[getCourseMinLevel(O1,O2)]).
reln([level, requirement, for| L],L,O1,O2,_,[getCourseMinLevel(O1,O2)]).
reln([minimum, level, of| L],L,O1,O2,_,[getCourseMinLevel(O1,O2)]).
reln([minimum, level, for| L],L,O1,O2,_,[getCourseMinLevel(O1,O2)]).
reln([video, related, to, course| L],L,O1,O2,_,[relatedVideoToCourse(O1,O2)]).
reln([videos, related, to, course| L],L,O1,O2,_,[relatedVideoToCourse(O1,O2)]).
reln([course, related, to, video| L],L,O1,O2,_,[relatedCourseToVideo(O1,O2)]).
reln([courses, related, to, video| L],L,O1,O2,_,[relatedCourseToVideo(O1,O2)]).


%reln([related, to| L],L,O1,O2,_,[relatedTo(O1,O2)]).

% Questions.
question([show | L0],L1,Entity,C0,C1) :-
		noun_phrase(L0,L1,Entity,C0,C1).
question([show | L0],L1,Entity,C0,C1) :-
		mp(L0,L1,Entity,C0,C1).
question([what,are | L0],L1,Entity,C0,C1) :-
		noun_phrase(L0,L1,Entity,C0,C1).
question([what,are | L0],L1,Entity,C0,C1) :-
		mp(L0,L1,Entity,C0,C1).
question([what,is | L0],L1,Entity,C0,C1) :-
		noun_phrase(L0,L1,Entity,C0,C1).
question([what,is | L0],L1,Entity,C0,C1) :-
		mp(L0,L1,Entity,C0,C1).


% Gives answer A to question Q
ask(Q,A) :-
		question(Q,[],A,[],C),
		prove_all(C).

% Gives video name A and link L to question Q
ask_vid(Q,A,L) :- ask(Q,A), getLink(L,A).

% Proves all elements of L against the database
prove_all([]).
prove_all([H|T]) :-
		call(H),
		prove_all(T).

isVideo(A) :- video_name(Id, A), video(Id).
isCourse(A) :- course_name(Courseid, A), course(Courseid).


%isVideoCreator(A) :- video_creator(Id, A), video(Id).
%isVideoLink(A) :- video_link(Id, A), video(Id).

% Find videos of same topic
sameTopic(X,Y) :-
	video_name(IdX, X),
	video_topic(IdX, Topic),
	video_topic(IdY, Topic),
	video_name(IdY, Y),
	IdX \= IdY.

% Find videos of same subject
sameSubject(X,Y) :-
	video_name(IdX, X),
	video_subject(IdX, Subject),
	video_subject(IdY, Subject),
	video_name(IdY, Y),
	IdX \= IdY.

% Find videos by same creator
sameCreator(X,Y) :-
	video_name(IdX, X),
	video_creator(IdX, Creator),
	video_creator(IdY, Creator),
	video_name(IdY, Y),
	IdX \= IdY.

% Find next level of topic
nextVideo(X,Y) :-
	video_name(IdX, X),
	video_topic(IdX, Topic),
	video_level(IdX, LevelX),
	video_level(IdY, LevelY),
	video_topic(IdY, Topic),
	video_name(IdY, Y),
	IdX \= IdY,
	LevelX is LevelY + 1.

% Find previous level of topic
previousVideo(X,Y) :-
	video_name(IdX, X),
	video_topic(IdX, Topic),
	video_level(IdX, LevelX),
	video_level(IdY, LevelY),
	video_topic(IdY, Topic),
	video_name(IdY, Y),
	IdX \= IdY,
	LevelX is LevelY - 1.


getCreator(X,Y) :-
	video_creator(IdX, X),
	video_name(IdX, Y).

getLevel(X,Y) :-
	video_level(IdX, X),
	video_name(IdX, Y).

getSubject(X,Y) :-
	video_subject(IdX, X),
	video_name(IdX, Y).

getTopic(X,Y) :-
	video_topic(IdX, X),
	video_name(IdX, Y).

getLink(X,Y) :-
    video_link(Id, X),
    video_name(Id, Y).


relatedVideoToCourse(X,Y) :-
	video_name(IdX, X),
	video_topic(IdX, Topic),
	video_level(IdX, Level),
    course_topic(IdY, Topic),
    course_name(IdY, Y),
	course_minlevel(IdY, MinLevel),
	MinLevel =< Level.

relatedCourseToVideo(X,Y) :-
	course_name(IdX, X),
	course_topic(IdX, Topic),
    video_topic(IdY, Topic),
    video_name(IdY, Y).

getCourseTopic(X,Y) :-
	course_topic(IdX, X),
	course_name(IdX, Y).

getCourseMinLevel(X,Y) :-
    course_minlevel(Id, X),
    course_name(Id, Y).


%linkTo(X,Y) :-
   %setof((X,Y), B^(video_link(Id, X), video_name(Id, Y), \+X=Y), Set), member((X,Y), Set).
   %setof((X,Y), linkHelp(X,Y), Set), member((X,Y), Set).

% try:
% ?- ask([show,videos,with,similar,topic,to,'Logic for Programmers: Propositional Logic'],A).
% A = '[Logic] Predicate Logic' ;
% A = 'Lecture 23 | Logic 3: Bottom-up and Top-down Proof Procedures' ;

% ?- ask([show,video,with,same,subject,as,'Logic for Programmers: Propositional Logic'],A).
% ?- ask([show,videos,with,same,creator,as,'UML Class Diagram Tutorial'],A).
% ?- ask([show,next,video,from,'[Logic] Predicate Logic'],A).
% ?- ask([show,previous,video,from,'[Logic] Predicate Logic'],A).
% ?- ask([show,level,of,'Logic for Programmers: Propositional Logic'],A).
% ?- ask([show,level,requirement,for,'Models of Computation'],A).
% ?- ask([show,course,related,to,video,'Logic for Programmers: Propositional Logic'],A).
% ?- ask([show,videos,related,to,course,'Models of Computation'],A).

% To get link as well as video title, try:
% ?- ask_vid([show,next,video,from,'[Logic] Predicate Logic'],A,L).
% ?- ask_vid([show,videos,related,to,course,'Introduction to Software Engineering'],A,L).
