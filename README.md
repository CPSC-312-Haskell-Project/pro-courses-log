# Course Video Recommender

## What is the problem?
Finding relevant videos related to courses we are taking in university or topics we are interested in should be easy and manageable. We will develop a program that implements an NLP based recommendation system for videos on course-related topics. A user should be able to query object data, as well as utilize and compare informative relations between videos and courses.  We will recommend YouTube and Khan Academy videos for topics in computer science and math specifically, but this can easily be extendable to other subjects areas as well.

## What is the something extra?
We plan to implement a JSON database that is parsed into Prolog using Prolog’s HTTP and JSON libraries. Furthermore, we will create an HTTP API for querying our Prolog program, if feasible. The HTTP API would be accessible to users through a web-browser as well, making the Course Video Recommender easier to use.

## What did we learn from doing this?
Prolog is powerful when it comes to implementing simple NLP queries to find information quickly. It was insightful to think about how to keep our queries intuitive and extract the most meaningful relationships so that we could best serve the user in creating a positive learning experience. We generally found it straightforward to model our queries, but we had to initially decide upon a database/proposition structure that was easy to use for the types of queries we implemented. Managing lists in Prolog (for our queries) also felt familiar given our experience with Haskell.

We faced some challenges when it came to parsing JSON and making an API. The primary challenge was to find relevant documentation and examples for using the HTTP and JSON packages. For retrieving and parsing JSON, a blog post was extremely helpful in getting us started, and we built the rest on top of it. 

The official documentation for writing web servers in Prolog is also sparse, which skips a lot of important information about Prolog’s inbuilt http functionalities. However, online examples served as great references for us to build our server on top of. After doing so, we found that writing REST servers in Prolog is actually efficient because of the minimal boilerplate code and setup required.

Overall, Prolog clearly has potential for rule-based logical query data systems coupled with web-based integrations. The benefits of using Prolog for this project was that Prolog was quite efficient and reasoning about and isolating the rule set for our queries was very readable and easy to expand. The major drawbacks, were the lack of documentation/resources and some struggles with debugging malformed query inputs and confusing warnings/errors.

In terms of future improvements, we would continue to build upon our queryable knowledgebase, expand the scope of our project to other subject areas and continue to support additional usability features such as wrapping our application in an expanded GUI.

## Instructions

Open recommender.pl in SWI-Prolog.

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
% ?- ask([what,are,courses,with,same,course,topic,as,'Introduction to Software Engineering'],A).
% ?- ask([get,full,details,for,'Logic for Programmers: Propositional Logic'],A).
% ?- ask([get,full,course,details,for,'Introduction to Software Engineering'],A).
% ?- ask([find,course,in,same,faculty,as,'Introduction to Software Engineering'],A).

% To get link as well as video title, try:
% ?- ask_vid([show,next,video,from,'[Logic] Predicate Logic'],A,L).
% ?- ask_vid([show,videos,related,to,course,'Introduction to Software Engineering'],A,L).

