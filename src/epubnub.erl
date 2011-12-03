%%%-------------------------------------------------------------------
%%% @author Tristan Sloughter <>
%%% @copyright (C) 2011, Tristan Sloughter
%%% @doc
%%%
%%% @end
%%% Created :  9 Apr 2011 by Tristan Sloughter <>
%%%-------------------------------------------------------------------
-module(epubnub).

-export([new/0,
         new/1,
         new/2,
         new/3,
         publish/2,
         publish/3,
         spawn_subscribe/2,
         spawn_subscribe/3,
         unsubscribe/1,
         subscribe/2,
         subscribe/3,
         history/2,
         history/3,
         time/0,
         uuid/0,
         uuid/1]).

-define(DEFAULT_ORIGIN, "pubsub.pubnub.com").
-define(DEFAULT_PUBKEY, "demo").
-define(DEFAULT_SUBKEY, "demo").
-define(DEFAULT_SSL, false).

-define(WITH_DEFAULT(X, Y), case X of
                                undefined ->
                                    {ok, Y};
                                _ ->
                                    X
                            end).


-record(epn, {origin, pubkey, subkey, secretkey, is_ssl}).

-type json_string() :: atom | string() | binary().
-type json_number() :: integer() | float().
-type json_array() :: {array, [json_term()]}.
-type json_object() :: {struct, [{json_string(), json_term()}]}.
-type json_term() :: json_string() | json_number() | json_array() | json_object().

%%%===================================================================
%%% New record functions
%%%===================================================================

-spec new() -> record(epn).
new() ->
    {ok, Origin} = ?WITH_DEFAULT(application:get_env(epubnub, origin), ?DEFAULT_ORIGIN),
    {ok, PubKey} = ?WITH_DEFAULT(application:get_env(epubnub, pubkey), ?DEFAULT_PUBKEY),
    {ok, SubKey} = ?WITH_DEFAULT(application:get_env(epubnub, subkey), ?DEFAULT_SUBKEY),
    {ok, SSL} = ?WITH_DEFAULT(application:get_env(epubnub, ssl), ?DEFAULT_SSL),
    #epn{origin=Origin, pubkey=PubKey, subkey=SubKey, is_ssl=SSL}.

-spec new(string()) -> record(epn).
new(Origin) ->
    #epn{origin=Origin}.

-spec new(string(), string()) -> record(epn).
new(PubKey, SubKey) ->
    #epn{pubkey=PubKey, subkey=SubKey}.

-spec new(string(), string(), string()) -> record(epn).
new(Origin, PubKey, SubKey) ->
    #epn{origin=Origin, pubkey=PubKey, subkey=SubKey}.

%%%===================================================================
%%% Publish functions
%%%===================================================================

-spec publish(string(), json_term()) -> json_term().
publish(Channel, Msg) ->
    publish(new(), Channel, Msg).

-spec publish(record(epn), string(), json_term()) -> json_term().
publish(EPN, Channel, Msg) ->
    Json = mochijson2:encode(Msg),
    _Body = request([EPN#epn.origin, "publish", EPN#epn.pubkey, EPN#epn.subkey, "0", Channel, "0", Json], EPN#epn.is_ssl).

%%%===================================================================
%%% Spawn subscribe functions
%%%===================================================================

-spec spawn_subscribe(string(), pid() | fun()) -> ok.
spawn_subscribe(Channel, Callback) ->
    spawn_subscribe(new(), Channel, Callback).

-spec spawn_subscribe(record(epn), string(), pid() | fun()) -> ok.
spawn_subscribe(EPN, Channel, Callback) ->
    {ok, spawn(epubnub, subscribe, [EPN, Channel, Callback])}.


%%%===================================================================
%%% Unsubscribe functions
%%%===================================================================

-spec unsubscribe(pid()) -> ok.
unsubscribe(PID) when is_pid(PID) ->
    PID ! terminate.

%%%===================================================================
%%% Subscribe functions
%%%===================================================================

-spec subscribe(string(), pid() | fun()) -> ok.
subscribe(Channel, Callback)  ->
    subscribe(new(), Channel, Callback).

-spec subscribe(record(epn), string(), pid() | fun()) -> ok.
subscribe(EPN, Channel, PID) when is_pid(PID) ->
    subscribe(EPN, Channel, fun(X) -> PID ! {message, X} end, "0");
subscribe(EPN, Channel, Callback) ->
    subscribe(EPN, Channel, Callback, "0").

-spec subscribe(record(epn), string(), fun(), string()) -> ok.
subscribe(EPN, Channel, Function, TimeToken) ->
    try
        NewTimeToken = case request([EPN#epn.origin, "subscribe", EPN#epn.subkey, Channel, "0", TimeToken], EPN#epn.is_ssl) of
                        [[], NewTimeToken1] ->
                            NewTimeToken1;
                        [Messages, NewTimeToken1] ->
                            lists:foreach(Function, Messages),
                            NewTimeToken1
                       end,

        %% Check if a terminate message has been sent to us, stop and return ok atom if so
        receive
            terminate ->
                ok
        after 0 ->
                subscribe(EPN, Channel, Function, NewTimeToken)
        end
    catch
        _:_ ->
            subscribe(EPN, Channel, Function, TimeToken)
    end.

%%%===================================================================
%%% History functions
%%%===================================================================

-spec history(string(), integer() | string()) -> json_term().
history(Channel, Limit) ->
    history(new(), Channel, Limit).

-spec history(record(epn), string(), integer() | string()) -> json_term().
history(EPN, Channel, Limit) when is_integer(Limit) ->
    history(EPN, Channel, integer_to_list(Limit));
history(EPN, Channel, Limit) when is_list(Limit) ->
    request([EPN#epn.origin, "history", EPN#epn.subkey, Channel, "0", Limit], EPN#epn.is_ssl).

%%%===================================================================
%%% Time functions
%%%===================================================================

-spec time() -> integer().
time() ->
    time(new()).

-spec time(record(epn)) -> integer().
time(EPN) ->
    hd(request([EPN#epn.origin, "time", "0"], EPN#epn.is_ssl)).


%%%===================================================================
%%% UUID functions
%%%===================================================================

-spec uuid() -> binary().
uuid() ->
    uuid(new()).

-spec uuid(record(epn)) -> binary().
uuid(EPN) ->
    hd(request(["pubnub-prod.appspot.com", "uuid"], EPN#epn.is_ssl)).

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec request(list(string()), boolean()) -> json_term().
request(URLList, IsSSL) ->
    Protocol = case IsSSL of
                   true ->
                       "https:/";
                   false ->
                       "http:/"
               end,
    URL = string:join([Protocol | URLList], "/"),
    {ok, "200", _ResponseHeaders, Body} = ibrowse:send_req(URL, [], get, [], [{is_ssl, IsSSL}]),
    mochijson2:decode(Body).
