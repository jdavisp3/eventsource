#!/bin/sh

exec \
  erl \
    -pa ../demo/ebin deps/*/ebin \
    -s demo \
    -eval "io:format(\"Point your browser at http://localhost:8080/~nType:~ndemo:message(<<~ctest~c>>).~ndemo:number(5).~ndemo:hangup()~n~n\", [34, 34])."
