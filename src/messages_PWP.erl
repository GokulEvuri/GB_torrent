%%%-------------------------------------------------------------------
%%% @author: Gokul Evuri
%%% @date:'24-11-2011'
%%% @doc This Module provides the basic functions of peer wire 
%%%      protocols, which can on calling with socket can send proper 
%%%      data to the other end.
%%% @end
%%% @copyright: 2011 by Gokul
%%%-------------------------------------------------------------------
-module(messages_PWP).

-export([keep_alive/1,choke/1,unchoke/1,interested/1,notInterested/1,have/2,bitfield/2,request/4,piece/4,cancel/4]).

-define(Alive,<<>>).
-define(Choke,<<0>>).
-define(Unchoke,<<1>>).
-define(Interested,<<2>>).
-define(NotInterested,<<3>>).
-define(Have,<<4>>).
-define(Bitfield,<<5>>).
-define(Request,<<6>>).
-define(Piece,<<7>>).
-define(Cancel,<<8>>).

%%%===================================================================
%%% API
%%%===================================================================

%% With the given argument, sends keep alive binary through the socket
keep_alive(Socket)->
    gen_tcp:send(Socket,?Alive).

%% Sends Choke binary
choke(Socket)->
    gen_tcp:send(Socket,?Choke).

%% Sends Unchoke binary
unchoke(Socket)->
    gen_tcp:send(Socket,?Unchoke).

%% Sends Interested binary
interested(Socket)->
    gen_tcp:send(Socket,?Interested).

%% Sends NotInterested binary
notInterested(Socket)->
    gen_tcp:send(Socket,?NotInterested).

%% Given socket and peice index as arguments after downloading a piece 
%% braodcasts have message through the socket
have(Socket,<<PieceIndex>>)->
    <<Have>> = ?Have,
    gen_tcp:send(Socket,<<Have/binary,PieceIndex/binary>>).

%% Given socket and bitfield sends bitfield through the socket to the other end
bitfield(Socket,BitField)->
    gen_tcp:send(Socket,<<5,BitField/binary>>).

%% Given Socket, Index of the piece, begin index of the chunk in the piece, 
%% length of the chunk, it sends request to the other end of the socket for
%% the chunk
request(Socket,Index,Begin,Length)->
    gen_tcp:send(Socket,<<6,Index:32,Begin:32,Length:32>>).

%% Given Socket, Index of the piece, begin index of the chunk in the piece, 
%% length of the chunk, it sends the chunk to the other end of the socket.
piece(Socket,Index,Begin,Block)->
    gen_tcp:send(Socket,<<7,Index:32,Begin:32,Block:32>>).

%% Sends a cancel messages to the Socket if a chunk is requested
%% with given Index, begin and length.
cancel(Socket,Index,Begin,Length)->
    gen_tcp:send(Socket,<<8,Index:32,Begin:32,Length:32>>).


