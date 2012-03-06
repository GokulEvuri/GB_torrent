%%%-------------------------------------------------------------------
%%% @author  Gokul Evuri
%%% @date '15-12-2011'
%%% @copyright 2011 by Gokul Evuri 
%%%-------------------------------------------------------------------
-module(ser_for_mf_a).
-compile(export_all).
-behaviour(gen_server).

%%%  API %%%
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, sma). 
%%-define(PRC_M_F, parsing).
%%-define(PRC_M_F_E, parsing_e).

start_link(File) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [File], []).

init([File]) ->
    ser_hash(),
    Dic = parsing:get_torrent_dict(File),
    Info = parsing:get_torrent_info(Dic),
    {ok,{Dic,Info}}.

%% not used
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.
%% not used
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info({filename,Pid},{Dic,Info}) ->
    Pid!{ok,binary_to_list(parsing_e:get_file_name(Info))},
    {noreply,{Dic,Info}};
handle_info({filelength,Pid},{Dic,Info}) ->
    Pid!{ok,{parsing_e:get_file_length(Info)}},
    {noreply,{Dic,Info}};
handle_info({gethash,Pid},{Dic,Info}) ->
    hashinf!{gethash,Pid},
    {noreply,{Dic,Info}};
handle_info({announce,Pid},{Dic,Info}) ->
    Pid!binary_to_list(parsing:get_announce(Dic)),
    {noreply,{Dic,Info}};
handle_info({noofpieces,Pid},{Dic,Info}) ->
    {ok,FL} = parsing_e:get_file_length(Info),
    APL = parsing:get_piece_length(Info),
    Pid!{ok,parsing_e:get_noof_pieces(FL,APL)},
    {noreply,{Dic,Info}};
handle_info({announce_list,Pid},{Dic,Info}) ->
    Pid!(parsing:get_announce_list(Dic)),
    {noreply,{Dic,Info}};
handle_info({piece_length,Pid},{Dic,Info}) ->
    Pid!parsing:get_piece_length(Info),
    {noreply,{Dic,Info}};
handle_info({info,Pid},{Dic,Info}) ->
    Pid!{ok,Info},
    {noreply,{Dic,Info}};
handle_info({peices,Pid},{Dic,Info}) ->
    Pid!{ok,parsing:get_pieces(Info)},
    {noreply,{Dic,Info}};
handle_info({sha,Num,Pid},{Dic,Info}) ->
    Pid!{ok,parsing:get_piece_SHA(Info,Num)},
    {noreply,{Dic,Info}}.


terminate(_Reason, _State) ->
    ok.

%%not used
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
ser_hash()->
    case whereis(hashinf) of
	undefined ->
	    register(hashinf,spawn_link(?MODULE,init_h,[<<>>]));
	X ->
	    X
    end.
%% after receiving infhash with hash tag it loops the data.
%% when sma server sends request with gethash it responds with hash its looping.
init_h(InHas)->
    receive
	{hash,Infohash} ->
	    init_h(Infohash);
	{gethash,Pid} ->
	    Pid!{hash,InHas},
	    init_h(InHas)
    end.
