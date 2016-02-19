README
======
Author: Tristan Sloughter t@crashfast.com
website: http://blog.erlware.org

Version: 0.1.0

Quick Start
-----------

* Build

Install [rebar3](http://www.rebar3.org/docs/getting-started).

```bash
$ rebar3 compile
```

* Test

```bash
$ rebar3 shell
1> application:ensure_all_started(epubnub).
ok
2> epubnub:publish(<<"hello_world">>, <<"hello">>).
{[1,<<"Sent">>,<<"13612809348896246">>]}
```

Examples
--------

src/epn_example_client has a simple example of a gen_server subscribing to a channel. The init function takes an EPN record,
created with epubsub:new() in start_link and requests to subscribe to the "hello_world" channel and sends its PID with the
self() function so new messages are sent to this process.

```erlang

init([EPN]) ->
    {ok, PID} = epubnub_sup:subscribe(EPN, <<"hello_world">>, self()),
    {ok, #state{pid=PID}}.

```

Since currently the subscribe loop sends the message with the bang (!) the gen_server will handle the new message in handle_info.
The example simply prints out the contents of the message:

```erlang

handle_info({message, Message}, State) ->
    io:format("~p~n", [Message]),
    {noreply, State}.

```

To unscribe we call the module stop function that sends an async stop message to the process. The handle_cast function handles this
message and return stop telling the gen_server to go to terminate. In terminate we take the PID of the subscribed process to the
epubnub unsubscribe function which sends it a terminate message and it exits.

```erlang

stop() ->
    gen_server:cast(?SERVER, stop).

handle_cast(stop, State) ->
    {stop, normal, State}.

terminate(_Reason, #state{pid=PID}) ->
    epubnub:unsubscribe(PID),
    ok.

```
