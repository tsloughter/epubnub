%%%-------------------------------------------------------------------
%%% @author Tristan Sloughter <>
%%% @copyright (C) 2011, Tristan Sloughter
%%% @doc
%%%
%%% @end
%%% Created :  9 Apr 2011 by Tristan Sloughter <>
%%%-------------------------------------------------------------------
-module(epn_example_client).

-behaviour(gen_server).

%% API
-export([start_link/0, start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).
-export_type([]).

-define(SERVER, ?MODULE).

-record(state, {}).

%%%===================================================================
%%% Public Types
%%%===================================================================

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    start_link(epubnub:new()).

start_link(EPN) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [EPN], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
init([EPN]) ->
    epubnub_sup:subscribe(EPN, "hello_world", self()),
    {ok, #state{}}.

%% @private
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%% @private
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @private
handle_info({message, Message}, State) ->
    io:format("~p~n", [Message]),
    {noreply, State}.

%% @private
terminate(_Reason, _State) ->
    ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
