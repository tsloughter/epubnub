%%%----------------------------------------------------------------
%%% @author Tristan Sloughter <tristan.sloughter@gmail.com>
%%% @doc
%%%
%%% @end
%%% @copyright 2011 Tristan Sloughter
%%%----------------------------------------------------------------,
-module(epubnub_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1, start_deps/0]).

%%%===================================================================
%%% Application callbacks
%%%===================================================================

%% @private
-spec start(normal | {takeover, node()} | {failover, node()},
            any()) -> {ok, pid()} | {ok, pid(), State::any()} |
                      {error, Reason::any()}.
start(_StartType, _StartArgs) ->
    case epubnub_sup:start_link() of
        {ok, Pid} ->
            {ok, Pid};
        Error ->
            Error
    end.

%% @private
-spec stop(State::any()) -> ok.
stop(_State) ->
    ok.

start_deps() ->
    application:start(sasl),
    application:start(inets),
    application:start(crypto),
    application:start(mochiweb),
    application:start(public_key),
    application:start(ssl),
    application:start(ibrowse).

%%%===================================================================
%%% Internal functions
%%%===================================================================
