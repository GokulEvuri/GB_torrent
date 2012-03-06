%%%-------------------------------------------------------------------
%%% @author Gokul Evuri
%%% @date   '14-11-2011'
%%% @copyright 2011 by Gokul Evuri
%%% @changes: Marina Bykova function [read/3] added on 06.12.2011

%%%-------------------------------------------------------------------
-module(file_write).
-compile(export_all).
-define(SERVER, ?MODULE). 
-define(SMA_SER, ser_for_mf_a).
-define(SMA,sma).

%% starts and registers a server under modules name.
%% If the server already defined it returns the server PID
start()->
    case whereis(?MODULE) of
	undefined ->
	    PRC = spawn(?MODULE,init,[]),
	    register(?MODULE,PRC);
	PRC ->
	    PRC
    end,
    {ok,PRC}.

%% This is the init function of the design pattern
init() ->
    loop().

%% basic loop function which keeps the server running.
loop()->	
    receive
	{createoverwrite,FileName,FileSize}->
	    create_ow(FileName,FileSize),
	    loop();
	{create,FileName,FileSize}->
	    create(FileName,FileSize),
		loop();
	{write,FileName,Position,Data}->
	    write(FileName,Position,Data),
	    loop();
	{read,FileName,Position,Length}->
	    read(FileName,Position,Length),
	    loop()
    end.


%% creates a file with given name.extension and size; 
%% with the fake data to be replaced
create_ow(File,Size)->
    BSize = Size*8,
    Data = <<123:BSize>>,
    %io:format("~n~w",[Data]),
    case catch file:write_file(File,Data) of
	ok->
	    ok;
	{error,Reason}->
	    Reason
    end.

%% Creates teh file, if the file already exits returns ok(keeping all the previous data)
%% If not already existed will write a new file
create(File,Size)->
    case catch file:read_file_info(File) of
	{ok,_}->
	    ok;
	{error,Reason}->
	    case Reason of 
		eacess ->
		    "Permision Denied to File Acess";
		enoent ->
		   create_ow(File,Size);
		enotdir ->
		    "Ditectory doesnot exists"
	     end
    end.

%% when given file name,position and data; data will be written on to 
%% the file from the given position. 
%% Position(offset) should strictly be integer() 
write(File,Position,Data)->
    case catch file:open(File,[read,write,binary]) of
	{ok,IoDevice} ->
	    write(india,IoDevice,Position,Data),
	    file:close(IoDevice);
	{error,Reason} ->
	    Reason
    end.
%% Given IoDevice, position and data; Data will be written on to the file 
%% form the position.
write(_File,IoDevice,Position,Data)->
    case catch file:pwrite(IoDevice,{bof,Position},Data) of
	ok ->
	    ok;
	{error,Reason}->
	    Reason
    end.

%%%%%%%%%%%%%%%%%%%%Function added by Marina Bykova%%%%%%%%%%%%%%%%%%%%%%%%
%% function reads piece of the file, it starts reading from the 
%% Location, with the length- Length byte
%% if all is ok - give the result {ok, Data}, where
%% Data=binary()
%% otherwise {error, Reason} or eof (if we meet the end)
read(Filename,Location,Length)->
    case file:open(Filename,[read,binary]) of
	 {ok,File} -> Data = file:pread(File,{bof,Location},Length),
		      file:close(File),
			  Data ;
	{error,Reason} -> {error, Reason}
    end.
%%%%%%%%%%%%%%End  of the function by Marina Bykova%%%%%%%%%%%%%%%
%% creates data of given size
file_siz_data(Size)->
   Size*8.

%% returns either false or true 
%% when there is a processes registered in the Atom name true is returned 
%% else false
check_is_reg(Atom)->
     lists:member(Atom,registered()).
