%%%-------------------------------------------------------------------
%%% @author: Gokul Evuri
%%% @date: '08-11-2011'
%%% @copyright: 2011 by Gokul Evuri
%%% @doc:
%%% This module requests the server, if provided with URL 
%%% and list of tuples with parameters and values 
%%% respectively. If no list is available can request only
%%% through URL.
%%% Reply will be a tuple of following format {ok,{P,L,S}}
%%% if you patteren-match
%%% P is a tuple, L is a List, S is atom
%%% @end
%%%-------------------------------------------------------------------
-module(ser_req).
-export([req/2,req/1]).
%%%===================================================================
%%% API
%%%===================================================================
%%--------------------------------------------------------------------
%% @doc 
%% BEFORE YOU USE THIS MODULE FOR IMPLEMENTATION MAKE SURE THAT 
%% inets:start() IS ALREADY RUNNING.
%% @spec
%% first argument in function should be URL of the server. 
%% example:"http://www.google.com"
%% second argument must be a list of parameters and values of 
%% pirticular values  
%% example:[{peer_id,98hdw9hh239}|T]. 
%% @end
%%--------------------------------------------------------------------
req(URL)->
    httpc:request(URL).

req(URL,L)->
	case L of 
	[] ->
	httpc:request(URL);
	_ ->
    	P = URL++"?",
    	reqL(L,P)
end.

%%%===================================================================
%%% Internal functions
%%%===================================================================
reqL([{P,V}|[]],R)->
    L = R++P++"="++V,
    httpc:request(L);

reqL([{P,V}|T],R)->
    L = R++P++"="++V++"&",
    reqL(T,L).


