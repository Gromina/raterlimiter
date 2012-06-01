#!/bin/sh
# NOTE: mustache templates need \ because they are not awesome.
exec erl -pa ebin -sname ratelimiter \
  -s ratelimiter