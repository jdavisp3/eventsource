EventSource Demo
================

To compile this example you need rebar in your PATH.

Type the following command:
```
$ rebar get-deps compile
```

You can then start the Erlang node with the following command:
```
./start.sh
```

To send events in the erlang shell:

demo:message(<<"This is a message.">>).
demo:number(12).

To hangup on all the clients:

demo:hangup().

The clients should automatically reconnect after 5 seconds.


Example
-------

Point your browser to http://localhost:8080 to see the demo with
supported browsers.
