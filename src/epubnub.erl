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
         new/4,
         publish/2,
         publish/3,
         spawn_subscribe/2,
         spawn_subscribe/3,
         unsubscribe/1,
         subscribe/2,
         subscribe/3,
         presence/2,
         presence/3,
         here_now/1,
         here_now/2,
         history/2,
         history/3,
         time/0,
         uuid/0,
         request/3,
         uuid/1]).

-define(DEFAULT_ORIGIN, <<"pubsub.pubnub.com">>).
-define(DEFAULT_PUBKEY, <<"demo">>).
-define(DEFAULT_SUBKEY, <<"demo">>).
-define(DEFAULT_SECRETKEY, <<"demo">>).
-define(DEFAULT_SSL, false).

-define(WITH_DEFAULT(X, Y), case X of
                                undefined ->
                                    {ok, Y};
                                _ ->
                                    X
                            end).


-record(epn, {origin, pubkey, subkey, secretkey, client, is_ssl=false}).

-type json_string() :: atom | string() | binary().
-type json_number() :: integer() | float().
-type json_array() :: {array, [json_term()]}.
-type json_object() :: {struct, [{json_string(), json_term()}]}.
-type json_term() :: json_string() | json_number() | json_array() | json_object().

%%%===================================================================
%%% New record functions
%%%===================================================================

-spec new() -> #epn{}.
new() ->
    {ok, Origin} = ?WITH_DEFAULT(application:get_env(epubnub, origin), ?DEFAULT_ORIGIN),
    {ok, PubKey} = ?WITH_DEFAULT(application:get_env(epubnub, pubkey), ?DEFAULT_PUBKEY),
    {ok, SubKey} = ?WITH_DEFAULT(application:get_env(epubnub, subkey), ?DEFAULT_SUBKEY),
    {ok, SecretKey} = ?WITH_DEFAULT(application:get_env(epubnub, secretkey), ?DEFAULT_SECRETKEY),
    {ok, SSL} = ?WITH_DEFAULT(application:get_env(epubnub, ssl), ?DEFAULT_SSL),
    #epn{origin=Origin, pubkey=PubKey, subkey=SubKey, secretkey=SecretKey, is_ssl=SSL}.

-spec new(binary()) -> #epn{}.
new(Origin) ->
    #epn{origin=Origin}.

-spec new(binary(), binary()) -> #epn{}.
new(PubKey, SubKey) ->
    #epn{pubkey=PubKey, subkey=SubKey}.

-spec new(binary(), binary(), binary()) -> #epn{}.
new(PubKey, SubKey, SecretKey) ->
    #epn{pubkey=PubKey, subkey=SubKey, secretkey=SecretKey}.

-spec new(binary(), binary(), binary(), binary()) -> #epn{}.
new(Origin, PubKey, SubKey, SecretKey) ->
    #epn{origin=Origin, pubkey=PubKey, subkey=SubKey, secretkey=SecretKey}.

%%%===================================================================
%%% Publish functions
%%%===================================================================

-spec publish(string(), json_term()) -> json_term().
publish(Channel, Msg) ->
    publish(new(), Channel, Msg).

-spec publish(#epn{}, string(), json_term()) -> json_term().
publish(EPN, Channel, Msg) ->
    Json = jsx:encode(Msg),
    request(EPN#epn.client, [EPN#epn.origin, <<"publish">>, EPN#epn.pubkey,
                             EPN#epn.subkey,  EPN#epn.secretkey, Channel, <<"0">>, Json],
            EPN#epn.is_ssl).

%%%===================================================================
%%% Spawn subscribe functions
%%%===================================================================

-spec spawn_subscribe(string(), pid() | fun()) -> ok.
spawn_subscribe(Channel, Callback) ->
    spawn_subscribe(new(), Channel, Callback).

-spec spawn_subscribe(#epn{}, string(), pid() | fun()) -> ok.
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

-spec subscribe(binary(), pid() | fun()) -> ok.
subscribe(Channel, Callback)  ->
    subscribe(new(), Channel, Callback).

-spec subscribe(#epn{}, binary(), pid() | fun()) -> ok.
subscribe(EPN, Channel, PID) when is_pid(PID) ->
    subscribe(EPN, Channel, fun(X) -> PID ! {message, X} end, <<"0">>);
subscribe(EPN, Channel, Callback) ->
    subscribe(EPN, Channel, Callback, <<"0">>).

-spec subscribe(#epn{}, binary(), fun(), binary()) -> ok.
subscribe(EPN, Channel, Function, TimeToken) ->
    try
        {[_, NewTimeToken], Client} = case request(EPN#epn.client, [EPN#epn.origin, <<"subscribe">>,
                                                                    EPN#epn.subkey, Channel, <<"0">>, TimeToken],
                                              EPN#epn.is_ssl) of
                                          {[[], _], _} = Result ->
                                              Result;
                                          {[Messages, _], _} = Result ->
                                              lists:foreach(Function, Messages),
                                              Result
                                      end,

        %% Check if a terminate message has been sent to us, stop and return ok atom if so
        receive
            terminate ->
                ok
        after 0 ->
                subscribe(EPN#epn{client=Client}, Channel, Function, NewTimeToken)
        end
    catch
        _:_ ->
            subscribe(EPN, Channel, Function, TimeToken)
    end.

%%%===================================================================
%%% Presence functions
%%%===================================================================

-spec presence(binary(), pid() | fun()) -> json_term().
presence(Channel, Callback) ->
    presence(new(), Channel, Callback).

-spec presence(#epn{}, binary(), pid() | fun()) -> json_term().
presence(EPN, Channel, Callback) ->
    subscribe(EPN, [Channel, <<"-pnpres">>], Callback).

%%%===================================================================
%%% Here Now functions
%%%===================================================================

-spec here_now(binary()) -> json_term().
here_now(Channel) ->
    here_now(new(), Channel).

-spec here_now(#epn{}, binary()) -> json_term().
here_now(EPN, Channel) ->
    request(EPN#epn.client, [EPN#epn.origin, <<"v2">>, <<"presence">>, <<"sub-key">>,
                             EPN#epn.subkey, <<"channel">>, Channel],
            EPN#epn.is_ssl).

%%%===================================================================
%%% History functions
%%%===================================================================

-spec history(binary(), integer() | binary()) -> json_term().
history(Channel, Limit) ->
    history(new(), Channel, Limit).

-spec history(#epn{}, binary(), integer() | binary()) -> json_term().
history(EPN, Channel, Limit) when is_integer(Limit) ->
    history(EPN, Channel, integer_to_list(Limit));
history(EPN, Channel, Limit) when is_list(Limit) ->
    request(EPN#epn.client, [EPN#epn.origin, <<"history">>, EPN#epn.subkey,
                             Channel, <<"0">>, Limit],
            EPN#epn.is_ssl).

%%%===================================================================
%%% Time functions
%%%===================================================================

-spec time() -> integer().
time() ->
    time(new()).

-spec time(#epn{}) -> integer().
time(EPN) ->
    hd(request(EPN#epn.client, [EPN#epn.origin, <<"time">>, <<"0">>], EPN#epn.is_ssl)).


%%%===================================================================
%%% UID functions
%%%===================================================================

-spec uuid() -> binary().
uuid() ->
    uuid(new()).

-spec uuid(#epn{}) -> binary().
uuid(EPN) ->
    hd(request(EPN#epn.client, [<<"pubnub-prod.appspot.com">>, <<"uuid">>], EPN#epn.is_ssl)).

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec request(tuple(), list(binary()), boolean()) -> json_term().
request(Client, URLList, IsSSL) ->
    case Client of
        undefined ->
            Protocol = case IsSSL of
                           true ->
                               <<"https:/">>;
                           false ->
                               <<"http:/">>
                       end,
            URL = bin_join([Protocol | URLList], <<"/">>),
            {ok, 200, _RespHeaders, Client1} = hackney:request(get, URL, [], <<>>, []);
        Client ->
            Path = bin_join([<<"/">> | tl(URLList)], <<"/">>),
            {ok, 200, _RespHeaders, Client1} = hackney:send_request(Client, {get, Path, [], <<>>})
    end,

    {ok, Body} = hackney:body(Client1),
    {jsx:decode(Body), Client1}.

bin_join([H | Rest], BinString) ->
    << H/binary, << <<BinString/binary, B/binary>>  || B <- Rest >>/binary >>.
