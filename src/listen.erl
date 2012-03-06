%%%-------------------------------------------------------------------
%%% @author: Gokul Evuri
%%% @date: '12-11-2011'
%%% @copyright: 2011 by Gokul Evuri
%%%-------------------------------------------------------------------
-module(listen).
-export([port_connect/1,receiveport/3,getpeername/1]).

%%%===================================================================
%%% API
%%%===================================================================
%% This finction listens to a local port and returns the packets 
%% excluding the length prefix as a binary
port_connect(Port)->
    case catch gen_tcp:listen(Port,[binary,inet,{packet,4}]) of
	{error,_Reason}->
	    error;
	{ok,ListenSocket}->
	   case gen_tcp:accept(ListenSocket,6000) of
	       {ok,Socket}->
		   {Socket,connected};
	       {error,_} ->
		   error
	   end
    end.


getpeername(ListenSocket) ->
    case catch inet:peername(ListenSocket) of
	{error,_posix} ->
	    error;
	{ok,{Address,Port}} ->
	    {Address,Port}
    end.

%% @doc:
%% retrives the packet from teh socket with given length and in given time
receiveport(Socket,Length,Time)->
    case catch gen_tcp:recv(Socket,Length,Time) of 
	{error,_Reason} ->
	    error;
	{ok,Packet} ->
	    Packet;
	_ ->
	error	
    end.


