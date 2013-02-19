README
======
Author: Tristan Sloughter tristan.sloughter@gmail.com
website: http://blog.erlware.org

Version: 0.0.4

Quick Start
-----------

* Dependencies

```bash
$ ./rebar get-deps
```

* Compile

```bash
$ ./rebar compile
```

* Test

```bash
$ erl +K true +A30 -pa ebin -env ERL_LIBS lib:deps -config config/sys.config
Erlang R15B03 (erts-5.9.3.1) [source] [smp:4:4] [async-threads:30] [hipe] [kernel-poll:true]

Eshell V5.9.3.1  (abort with ^G)
1> epubnub_app:start_deps().
ok
2> epubnub:publish(<<"hello_world">>, <<"hello">>).
{[1,<<"Sent">>,<<"13612809348896246">>],
 {client,hackney_tcp_transport,"pubsub.pubnub.com",80,netloc,
         [],#Port<0.2131>,infinity,false,5,false,nil,undefined,
         connected,done,nil,normal,#Fun<hackney_request.send.2>,done,
         4096,<<>>,
         {1,1},
         30,nil,<<"keep-ali"...>>,<<"text"...>>}}
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
