%%%-------------------------------------------------------------------
%%% @author Gokul <l@l-MacBook>
%%% @copyright (C) 2011, Gokul
%%% @doc Keeps log of all the pieces downloaded from peers, up on 
%%%      request it can provide the bit field of the file we are 
%%%      downloading!!!
%%% @end
%%% Created : 25 Nov 2011 by Gokul <l@l-MacBook>
%%%-------------------------------------------------------------------

-module(track_pieces).

%% API
-export([is_avail/1,available/1,get_native_bitfield/0,refresh_all/0,upgrade/0]).
-define(File,piece_record.etrt).

%%%===================================================================
%%% API
%%%===================================================================

%%% @doc by calling this function will return a binary.
%%%      get_native_bitfield()-> Bitfield. 
%%%      Bitfield = binary().
%%% @end 
get_native_bitfield()->
	{ok,Data} = read(),
	convert_bitfield(Data,<<>>).

%%%@doc is_avail(PieceIndex) -> Boolean. 
%%%     Boolean = boolean().
%%%     will return either of true or false based on the availability 
%%%     of the piece.
%%%@end

is_avail(PieceIndex)->
    catch case read() of 
	      {error,_}->
		  false;
	      {ok,Binary}->
    LBinary = binary_to_list(Binary),
    Bool = lists:nth(PieceIndex,LBinary),
	case Bool of
	    "1" ->
		true;
	    "0"->
		false
	end
	  
	  end.

%%%@doc available(PieceIndex)-> ok | Error
%%%     does updates the file with the given given value
%%%@end

available(PieceIndex)->
    LBinary = binary_to_list(read()),
    {L11,L2} = lists:split(PieceIndex,LBinary),
    L1 = lists:split((PieceIndex-1),L11),
    List = lists:append([L1,[1],L2]),
    Data = list_to_binary(List),
    file:delete(?File),
    create_file(Data).

%%%@doc before working with a new torrent it must be called
%%%     refresh_all()-> ok | Error
%%%     creates a fresh file for the piece logging.
%%%@end
refresh_all()->
    create_file().

%%%@doc to upgrade the code 
%%%     upgrade()-> ok
%%%@end
upgrade()->
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%
%%  Internal Functions %%
%%%%%%%%%%%%%%%%%%%%%%%%%

%% Reads the file using file:read_file(FileName)
read()->
    case catch file:read_file(?File) of
	{ok,Binary}->
	    {ok,Binary};
	{error,Reason} ->
	    case Reason of 
		enoent->
		    create_file(),
		    read();
		_->
		    {error,{"M_error in file:read_file,track_pieces Module"}}
	    end
    end.

%% Creates a file for the logging and fills it with bytes with its value zero, as no. of bytes as the length of file we are downloading.
create_file()->
    sma!{filelength,self()},
    receive
	{ok,Length}->
	    case is_integer(Length) of
		true ->
		    Siz = (Length*8),
		    Data = <<0:Siz/integer>>,
		    create_file(Data);
		false ->
		    {error, "error in getting file length as integer"}
	    end;
	_ ->
	    {error,"Error in create_file,track_pieces module"}
    end.


%% This function when given the data read from the log file, will returns the bitfield.
convert_bitfield(<<>>,LData)->
    LData;
convert_bitfield(<<TH:64/integer,Rest/binary>>,LData)->
    <<A:8,B:8,C:8,D:8,E:8,F:8,G:8,H:8>> = <<TH:64>>,
    Data = <<A:1,B:1,C:1,D:1,E:1,F:1,G:1,H:1>>,
    convert_bitfield(Rest,<<LData/binary,Data/binary>>);

convert_bitfield(GData,LData) ->
    BitSiz = bit_size(GData) div 8,
    Num = 8 - BitSiz,
    Num1 = Num *8,
    BinE = <<0:Num1>>,
    Data = <<GData/binary,BinE/binary>>,
    ABinary = unsiz(Data,BitSiz+Num,<<>>),
    convert_bitfield(<<>>,<<LData/binary,ABinary/bits>>).

%% As used in convert_bitfield/2 to make non byte sized data to make it a byte by filling remaining bits with 0 value.
unsiz(_DLata,0,Data)->
    Data;
unsiz(<<TH:8/integer,Rest/binary>>,BitSiz,Data) ->
    D = <<TH:1>>,
    unsiz(Rest,BitSiz-1,<<Data/bits,D/bits>>).
    
%% Writes the given data to the macro file defined.
create_file(Data)->
    file:write_file(?File,Data).

