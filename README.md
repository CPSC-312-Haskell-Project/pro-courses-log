# Course Video Recommender

## What is the problem?
Finding relevant videos related to courses we are taking in university or topics we are interested in should be easy and manageable. We will develop a program that implements an NLP based recommendation system for videos on course-related topics. We will recommend YouTube and Khan Academy videos for topics in computer science and math, but this can be extendable to other subjects/topic areas as well.

## What is the something extra?
We plan to implement a JSON database that is parsed into prolog using Prolog’s HTTP and JSON libraries. Furthermore, we will create an HTTP API for querying our prolog program, if feasible. The HTTP API would be accessible to users through a web-browser as well, making the Course Video Recommender easier to use.

## What did we learn from doing this?
Prolog is powerful when it comes to implementing simple NLP queries to find information quickly. We found it straightforward to model our queries, but we had to initially decide upon a database/proposition structure that was easy to use for the types of queries we implemented. We also found managing lists in Prolog (for our queries) intuitive.
We faced some challenges when it came to parsing JSON and making an API. The primary challenge was to find relevant documentation and examples for using the HTTP and JSON packages. For retrieving and parsing JSON, a blog post was extremely helpful in getting us started, and we built the rest on top of it. 
The official documentation for writing web servers in Prolog is also sparse, which skips a lot of important information about prolog’s inbuilt http functionalities. However, online examples served as great references for us to build our server on top of. After doing so, we found that writing REST servers in Prolog is actually efficient because of the minimal boilerplate code and setup required.