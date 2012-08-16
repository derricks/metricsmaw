Metricsmaw
==========

Note! Metricsmaw is still extraordinarily early in development! I mostly put it into Github because I wanted to put something into Github.

Metricsmaw came about because I needed a way to aggregate metrics data from disparate languages. While I really like <a href="http://metrics.codahale.com/">Coda Hale's Metrics library</a> for Java applications,
it's less useful in an environment that might include node.js applications, Erlang servers, C++ code, and more.

I wrote the server in Erlang largely because I'm still learning the language and wanted a solid project to work on. In truth, though, Erlang seems like a good fit for the task at hand: Long-lived, lightweight code that needs to smoothly handle concurrency and fault tolerance.

Note that metricsmaw isn't itself an analytics tool. It's designed to be a relay between your code and some other format.

Using Metricsmaw
----------------

Send a binary Erlang term to the server. metricsmaw:socket\_client(Host,Port,Command) provides a simple interface for testing. The term must be one of the following: {add,MetricsName,MetricsType,Data} or {get,MetricsName}. MetricsName can be any valid Erlang atom. MetricsType is one of counter, gauge, or meter_minute. Data is a float or an integer.

I'm working on client libraries as separate projects. Stay tuned.

Configuring Metricsmaw
----------------------

Metricsmaw looks for a file named metricsmaw.config. It is read in as Erlang terms, so it needs to conform to that syntax. An example is provided in the source. Note! This is the configuration I use. It's not yet well documented! See note about being very early.

Basic Architecture
------------------

I had used Coda Hale's library on a project, and its concepts inspired mine. Metrics are items that know how to interpret passed-in data, and Reporters are items that know how to pass that data to other
systems.

The current set of metrics is:
-   counter

    keeps a sum of all the numbers you pass to it. Decrement by passing -1
    
-   gauge

    a single value
    
-   meter_minute

    keep track of the average per-minute rate over the last minute, the last five minutes, and the last fifteen minutes
    
The current set of reporters is:

-   console

    logs information to the console
    
-   csv

    logs information to a csv file
    
But metrics and reporters are both defined as Erlang behaviours, providing (I hope) a straightforward plugin architecture.


