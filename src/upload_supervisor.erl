%%%-------------------------------------------------------------------
%%% @author: Gokul Evuri
%%% @date: '15-12-2011'
%%% @copyright: 2011 by Gokul Evuri 
%%%-------------------------------------------------------------------
-module(upload_supervisor).
-behaviour(supervisor).

%%% API %%%
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).
%%%===================================================================
%%% API functions
%%%===================================================================
%%--------------------------------------------------------------------
%% @doc:
%% Starts the supervisor
%%--------------------------------------------------------------------
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================
%%--------------------------------------------------------------------
%% @doc:
%% whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%% @end
%%--------------------------------------------------------------------
init([]) ->
    RestartStrategy = one_for_one,
    MaxRestarts = infinity,
    MaxSecondsBetweenRestarts = 30,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    Restart = transient,
    Shutdown = infinity,
    Type = worker,

    AChild = {upload_server, {upload_server, start_link, []},
	      Restart, Shutdown, Type, [upload_server]},

    {ok, {SupFlags, [AChild]}}.

