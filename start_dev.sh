#!/bin/sh
exec erl -pa ebin -sname ratelimiter \
  -s ratelimiter_app