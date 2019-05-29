-module(erlSoc).
-export([start_server/0, logon/1, remove/2, server/1, client/2]).
-define(TCP_OPTIONS, [binary, {packet, 0}, {active, false}, {reuseaddr, true}]).

%Listens for a socket connection
listen() ->
    {ok, LSocket} = gen_tcp:listen(5300, ?TCP_OPTIONS),
    accept(LSocket).

%Accepts a socket connection
accept(LSocket) ->
	{ok, CSocket} = gen_tcp:accept(LSocket),
	Ref = make_ref(),
	To = spawn(fun() -> init(Ref, CSocket) end),
	gen_tcp:controlling_process(CSocket, To),
	To ! {handoff, Ref, CSocket},
	accept(LSocket).

%part of the socket connections
init(Ref, Socket) ->
    receive
        {handoff, Ref, Socket} ->
            {ok, Peername} = inet:peername(Socket),
	    io:format("[S] peername ~p~n", [Peername]),	%some assurance of whats happening
            loop(Socket)
    end.

%Continually receives input from the user connected to this socket
loop(Socket) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, Data} ->
		List = binary_to_list(Data),	%turns the stream data into character
		List2 = string:tokens(List,","),	%Creates a list form the socket input
            io:format("[S] got ~p~n", [List2]),	%just some assurance of whats happening
            parse_data(Socket, List2),	%decide what the data says to do
            loop(Socket);	%loop back to receive more input
        {error, closed} ->
            io:format("[S] closed~n", []);
        E ->
            io:format("[S] error ~p~n", [E])
    end.

%Used when the server passes messages to itself. All connections between client and server are though sockets
server_node() ->
	erlSoc@localhost.

%Chooses a differest message for the server based on what the user passed into the socket
parse_data(Soc, [Task, Rname | Message]) -> 
	case Task of 
	"message" ->
		{erlSoc, server_node()} ! {message, Rname, Message};
	"create" ->
		{erlSoc, server_node()} ! {create, Rname, Soc};
	"list" ->
		{erlSoc, server_node()} ! {list, Soc};
	"join" ->
		{erlSoc, server_node()} ! {join, Rname, Soc};
	"leave" ->
		{erlSoc, server_node()} ! {leave, Rname, Soc}
	end.

start_server() ->
	register(erlSoc, spawn(erlSoc, server, [[[]]])),
	listen().

%removes an item X from a list L
remove(X, L) ->
    [Y || Y <- L, Y =/= X].

%Holds the chatrooms, with the users and messages in each room in tuples within Roomlist
server(RoomList) ->
	receive
		{message, Rname, Message} ->	%Sends a message to a specific chatroom
			{_,Users,Messages} = lists:keyfind(Rname, 1, RoomList),
			RoomUpdate = lists:keyreplace(Rname, 1, RoomList, {Rname,Users,[Messages|Message]}),
			server(RoomUpdate);
		{create, Rname, Soc} ->
			server([{Rname,Soc,[]}|RoomList]);
		{list, Soc} ->	%Lists all the chatrooms on the server
			Rooms = [A || {A,_,_} <- RoomList],
			gen_tcp:send(Soc,Rooms),
			server(RoomList);
		{join, Rname, User} ->	%When a user wishes to join a chatroom
			{Room,Users,Messages} = lists:keyfind(Rname, 1, RoomList),
			RoomUpdate = lists:keyreplace(Rname, 1, RoomList, {Rname,[Users|User],Messages}),
			gen_tcp:send(User, [Rname,Messages]),
			{Room,Users,Messages} = lists:keyfind(Rname, 1, RoomUpdate),
			server(RoomList);
		{leave, Rname, User} ->		%When a user wishes to leave a room
			{Room,Users,Messages} = lists:keyfind(Rname, 1, RoomList),
			NewU = remove(User, Users),
			RoomUpdate = lists:keyreplace(Rname, 1, RoomList, {Rname,NewU,Messages}),
			{Room,Users,Messages} = lists:keyfind(Rname, 1, RoomUpdate),
			server(RoomUpdate)
	end.

%Connects a user socket to the server and starts a thread for continous input from the user
logon(Uname) ->
	{ok, Sock} = gen_tcp:connect("localhost", 5300, [binary, {packet, 0}, {active, false}]),
	io:format("Create a Room: create~nList Rooms: list~nJoin Rooms: join~n Leave Rooms: leave~nSend a message: message ~n"),
	client(Sock, Uname).

%continually receives input from the user to send information to the server
client(Sock, Uname) ->
	{ok,[Task]} = io:fread("Task? : ", "~s"),	%user enters what they want their next move to be
	case Task of
		"message" ->	%asks what room to message, what is the message, then sends the message over a socket connection
			{ok, [Rname]} = io:fread("Send the message to which room? : ", "~s"),
			Message = io:get_line("Type your message: "),
			ok = gen_tcp:send(Sock,"message,"++Rname++","++Uname++": "++Message);
		"create" ->
			{ok, [Rname]} = io:fread("Enter a room name : ", "~s"),
			ok = gen_tcp:send(Sock, "create,"++Rname);
		"list" ->
			ok = gen_tcp:send(Sock, "list, , "),
			case gen_tcp:recv(Sock, 0) of	%Receives input from the server of all the chatrooms on the server
				{ok, Data} ->
					io:format("~p",[Data])
			end;
		"join" ->
			{ok, [Rname]} = io:fread("Join Which Room? : ", "~s"),
			ok = gen_tcp:send(Sock, "join,"++Rname),
			case gen_tcp:recv(Sock, 0) of	%receives input from the server of all the messages in the chatroom
				{ok, Data} ->
					io:format("~p",[Data])
			end;
		"leave" ->
			{ok, [Rname]} = io:fread("Leave Which Room? : ", "~s"),
			ok = gen_tcp:send(Sock, "leave,"++Rname);
		"help" ->	%shows the user what the commands are for the chatroom
			io:format("Create a Room: create~nList Rooms: list~nJoin Rooms: join~n Leave Rooms: leave~nSend a message: message ~n");
		"exit" ->
			gen_tcp:close(Sock)
	end,
	client(Sock,Uname).
