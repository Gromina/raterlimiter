Raterlimiter
====================

An Erlang application to limit rate of requests.
Initially developed to limit requests to Mochiweb server.

Installation
------------
Just udate your `rebar.config` file with

        {deps, [
          {raterlimiter, ".*",
           {git, "git://github.com/Gromina/raterlimiter.git", "master"}}
        ]}.
Raterlimiter needs 2 application parameters - 

`timeout` -  Timeout to delete unused bucket (milliseconds)  
`cleanup_rate` - How often to run clenup funciton (milliseconds)  

If you need to change them, update your `app.config` with

       {raterlimiter, [
         {timeout, 300000},      % bucket maximum lifetime (5 min)
         {cleanup_rate, 10000}   % cleanup every X milliseconds (every 10 secs)
       ]}

Usage
-----

Once you start the application with

    application:start(raterlimiter).

or with command line

    erl -s raterlimiter
    
you can start rate limiting any of your tasks.

    raterlimiter:check_rate(RequestID, BucketTimeFrame, RequestsLimit)

which returns either `{ok, continue}`  or `{fail, Limit}`

`RequestId` - Your task, request or client ID. In case of Mochiweb it is probably `mochiweb_request:get(peer)`  
`BucketTimeFrame` - Time frame to apply limit, in milliseconds. 1000 means change bucket every second  
`RequestsLimit` - Maximum number of requests allowed to do within one bucket  

Author
------
Alexander Sorokin gromina@gmail.com, [http://alexsorokin.ru](http://alexsorokin.ru)