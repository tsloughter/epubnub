%%%-------------------------------------------------------------------
%%% @author Tristan Sloughter <>
%%% @copyright (C) 2011, Tristan Sloughter
%%% @doc
%%%
%%% @end
%%% Created :  9 Apr 2011 by Tristan Sloughter <>
%%%-------------------------------------------------------------------
-module(epubnub).

-export([publish/2,
         subscribe/2,
         history/2,
         time/0]).

-define(ORIGIN, "pubsub.pubnub.com").
-define(PUBKEY, "demo").
-define(SUBKEY, "demo").

-type json_string() :: atom | string() | binary().
-type json_number() :: integer() | float().
-type json_array() :: {array, [json_term()]}.
-type json_object() :: {struct, [{json_string(), json_term()}]}.
-type json_term() :: json_string() | json_number() | json_array() | json_object().

-spec publish(string(), string() | binary()) -> json_term().
publish(Channel, Msg) when is_list(Msg) ->
    publish(Channel, list_to_binary(Msg));
publish(Channel, Msg) ->
    Json = mochijson2:encode(Msg),
    _Body = request([?ORIGIN, "publish", ?PUBKEY, ?SUBKEY, "0", Channel, "0", Json]).

-spec subscribe(string(), pid() | fun()) -> ok.
subscribe(Channel, PID) when is_pid(PID) ->
    subscribe(Channel, fun(X) -> PID ! {message, X} end);
subscribe(Channel, Function) ->
    Messages = request([?ORIGIN, "subscribe", ?SUBKEY, Channel, "0", "0"]),
    lists:foreach(Function, Messages),
    subscribe(Channel, Function).

-spec history(string(), integer() | string()) -> json_term().
history(Channel, Limit) when is_integer(Limit) ->
    history(Channel, integer_to_list(Limit));
history(Channel, Limit) when is_list(Limit) ->
    request([?ORIGIN, "history", ?SUBKEY, Channel, "0", Limit]).

-spec time() -> integer().
time() ->
    hd(request([?ORIGIN, "time", "0"])).

-spec request(list(string())) -> json_term().
request(URLList) ->
    URL = string:join(["http:/" | URLList], "/"),
    {ok, "200", _ResponseHeaders, Body} = ibrowse:send_req(URL, [], get, [], []),
    mochijson2:decode(Body).
