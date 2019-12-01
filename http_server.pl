:- [recommender]. 
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_error)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_parameters)).

:- http_handler('/query', handle_query, []).

server(Port) :-
   http_server(http_dispatch, [port(Port)]).

   /*  
       browse http://127.0.0.1:8000/?baz=3&baz=4
       This demonstrates handling parameters.
       Not all type checking options are demonstrated, see
       http://www.swi-prolog.org/pldoc/doc_for?object=section%283,%273.9%27,swi%28%27/doc/packages/http.html%27%29%29
    */

   handle_query(Request) :-
      reply_html_page(
            title('Howdy'),
            [\page_content(Request)]).

      page_content(Request) --> 
{
            http_parameters(Request,
               [   
               % default for a missing param
               q(Q, [getLink(L,Q)])])   
},
{ 
   getLink(L,Q)
},
   html(
         [   
         h1('Pro-Courses-Log!'),
         p('A course video recommender built entirely on Prolog!'),
         p('The query is ~w' -Q),
         p('The link to a video related to this topic is ~w', -L)
         ]).

   page_content(_Request) --> 
   html(
         [   
         h1('Oops!'),
         p('Some parameter wasnt valid')
         ]).  



