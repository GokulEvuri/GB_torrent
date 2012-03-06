%%%-------------------------------------------------------------------
%%% @author: Gokul Evuri
%%% @date: '15-11-2011'
%%% @copyright: 2011 by Gokul Evuri
%%% @doc:
%%% module parsing_e provides additionalfunctions on parsing.erl module;
%%% it only works with single file torrents.
%%% @end
%%%-------------------------------------------------------------------
-module(parsing_e).
-export([get_file_name/1,get_file_length/1,get_noof_pieces/2,get_last_piece_length/1]).

%% when given a dictionary of info this functions return the name of the file
get_file_name(Info)->
    {ok,An} = dict:find(<<"name">>,Info),
    An.
%% with given info dictionary, function returns {ok,Length},
%% where length is the length of the file.
get_file_length(Info)->
    An = dict:find(<<"length">>,Info),
    An.
%% when given total length of file and individual piece length(excluding last piece), 
%% returns total number of pieces
get_noof_pieces(TotlLen,PiecLen)->
    ((TotlLen div PiecLen)+1).
%% given info to the function calculates the size of the last piece, then returns it.
get_last_piece_length(Info)->
    All_Piece_Length = parsing:get_piece_length(Info),
    File_length = get_file_length(Info),
    NoOf_Pieces = (get_noof_pieces(File_length,All_Piece_Length)-1),
    (File_length)-(NoOf_Pieces*All_Piece_Length).
