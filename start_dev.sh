#!/bin/sh
exec erl -pa ebin -sname raterlimiter \
  -s raterlimiter_app