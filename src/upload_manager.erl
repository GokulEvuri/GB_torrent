%%%-------------------------------------------------------------------
%%% @authors: Gokul Evuri
%%% @date: '15-12-2011'
%%% @copyright: 2011 by Gokul Evuri 
%%% @doc:
%%% Module handles all uploading process, using Bittorrent protocols.
%%% It can only handle one peer at a time. 
%%% The default port number is 6884
%%% @end
%%%-------------------------------------------------------------------
-module(upload_manager).
-export([start/0,stop/0,init/0]).
-define(Port,6884).
-define(ID,"-GB1000-12345789123").

%% @doc:
%% spawns a process to maintain the loop for the uploading procedure.
start()->
   register(?MODULE, spawn(?MODULE,init,[])).
%% @doc:
%% stops the process registered by the reason "UserStopped"
stop()->
    exit(?MODULE,"UserStopped").
%% @doc:
%% the init function calls a function loop with an empty list as an argument
init()->
    loop([]).

%% @doc:
%% this handles all the uploading process,
%% initiates when loop/1 is called with an empty list.
%% It connects to the peer and returns Data = {Socket,State}|error
%% Socket = socket(), State = atom()
%% @end
loop([])->
    Data = listen:port_connect(?Port),
    case Data of
	error->
	    %io:format("~w~n",["Error In Connecting Peer"]),
	    loop([]);
	{Socket,State} ->
	    loop([{Socket,State}])
    end;
%% @doc:
%% This function initiates when loop/1 is called with an 
%% non empty list as an argument.
%% Data = {Socket,Packet}|error
%% Packet = binary()
%% After getting the packet this function calls the function analyze/2
%% with socket() and packet() as its arguments
%% @end
loop([{Socket,_State}|T]) ->
    Data = listen:receiveport(Socket,0,12000),
    case Data of
	error->
	    %io:format("~w~n",["Error in receiving from socket"]),
	    %% try closing socket here!!(reason maximum non responsive time)
	    loop(T);
	{Socket,Packet} ->
	    E = analyze(Socket,Packet),
	    case E of 
		0 ->
		    loop([{Socket,Packet}|T]);
		_ ->
		    loop([E|T])
	    end
    end.
	
%% @doc:
%% By giving Socket and Packet as arguments,this function analyzes 
%% the packet(patternmatches the packet), and calls the proper 
%% functions, either replying peer with proper data and returning 
%% the result, or just returning the result.
%% @end
analyze(Socket,Packet)->
    case Packet of 
	<<19,"BitTorrent protocol",_Reserved:64/bits,Info_hash:160/bits,_Peer_id:160/bits,_Rest/binary>> ->
	    BitField = track_pieces:get_native_bitfield(),
	    sma!{gethash,self()},
	    receive
		{ok,Hash} ->
		    case Hash == <<Info_hash>> of
			true ->
			    gen_tcp:send(Socket,<<19,"BitTorrent protocol",0,0,0,0,0,0,0,0,Hash,?ID>>),
			    gen_tcp:send(Socket,BitField),
			    {Socket,bitfieldsent};
			false ->
			    0
		    end;
		_ ->
		    {Socket,bitfieldnotsent}
	    end;
	<<>> ->
	    {Socket,keepalive};
	<<2>> ->
	    gen_tcp:send(Socket,<<1>>),
	    {Socket,unchoked};
	<<6,Rest/binary>> ->
	    requested(Socket,Rest),
	    {Socket,chunk_sent};
	<<8>> ->
	    0;
	_WE -> ok
    end.

%% @doc:
%% when request message is patternmatched in analyze/2, 
%% this function will be called with the socket and 
%% excluded 'Protocol ID' from the packet of request
%% It sends the requested chunk as piece message in PWP.
%% @end

requested(Socket,<<Index:32/binary,Begin:32/binary,Length:32/binary>>)->
    sma!{filename,self()},
    receive
	{ok,FN}->
	    sma!{piecelength,self()},
	    receive
		{ok,PL}->
		    Location = (Index * PL) + Begin,
		    Data = file_write:read(FN,Location,Length),
		    PWPData = <<7,Index/binary,Begin/binary,Data/binary>>,
		    gen_tcp:send(Socket,PWPData);
		_Else -> ok
		 %   io:format("~w~n",[{Else,"failed in sending message"}])
	    end
    end.
