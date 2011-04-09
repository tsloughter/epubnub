-module(epn_basic_examples).

-compile([export_all]).

publish() ->
    epubnub:publish("hello_world", <<"hello">>).

subscribe() ->
    epubnub:subscribe("hello_world", fun(X)-> io:format("~p~n", [X]) end).

history() ->
    epubnub:history("hello_world", 10).

time() ->
    epubnub:time().
