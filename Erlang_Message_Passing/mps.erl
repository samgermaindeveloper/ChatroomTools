-module(mps).
-export([start_server/0, server/2, send_message/4, client/2, logon/1, create/0, message/1, list/0, join/0, leave/0, shelp/0, logoff/0, chatroom/3, send_messages_to_user/3]).

%used for passing messages to the server
server_node() ->
	mps@localhost.

%Spawns a thread for the server, and registers the name mps to the server pid
start_server() ->
	register(mps, spawn(mps, server, [[[]],[[]]])).

%Keeps track of the users and the chatrooms and receives choices of actions from the user
server(UserList, RoomList) ->
	receive
		{logon, From} ->	%logs a user on
			server([From|UserList], RoomList);
		{logoff, From} ->
			New_User_List = UserList -- From,
			server(New_User_List, RoomList);
		{message, Message, From, Room} ->	%Sends a message to a chatroom
			{Rid, _} = lists:keyfind(Room, 2, RoomList),
			Rid ! {message, Message, From,Room},
			server(UserList, RoomList);
		{create, From, Rname} ->	%creates a chatroom
			io:format("Create Test ~n"),
			Room = start_chatroom(From),
			server(UserList, [{Room,Rname}|RoomList]);
		{list, From} ->	%Sends all the chatrooms available back to the user
			From ! {printRoom, RoomList},
			server(UserList, RoomList);
		{join, From, Room} ->	%puts a user in a chatroom
			{Rid, Room} = lists:keyfind(Room, 2, RoomList),	%finds a chatroom by it's name
			Rid ! {join, From, Room},
			server(UserList, RoomList);
		{test, Name} ->	%for testing
			io:format("Logon Test ~p~n", [Name]),
			server(UserList, RoomList);
		{leave, From, Rname} ->	%removes a user from a chatroom
			{Rid, Rname} = lists:keyfind(Rname, 2, RoomList),
			Rid ! {leave, From},
			server(UserList, RoomList);
		{removeRoom, rPid} ->	%removes a room from this servers room list
			{_, Room} = lists:keyfind(rPid, 1, RoomList),
			New_Room_List = RoomList - {rPid, Room},
			server(UserList,New_Room_List)
	end.


%Spawns a chatroom thread
start_chatroom(User) ->
	spawn(mps, chatroom, [[User], [], os:system_time(second)]).

%removes an item X from a list L
remove(X, L) ->
	[Y || Y <- L, Y =/= X].
	
%%How do I make sure I access the right chat room
%Keeps track of chatroom messages and the users in the chatroom, and sends users messages when a new one gets added to the chatroom
chatroom(User_List, Messages, _Time) ->
	receive
		{message, Message, From,Room} ->
			send_message(Message, From, Room, User_List),	%sends the message out to each user in this chatroom
			chatroom(User_List, [Messages|Message], os:system_time(seconds));	%restarts this function and adds the message to this chatrooms messages
		{leave, User} ->
			New_uList = remove(User,User_List),	%removes a user from the chatroom
			chatroom(New_uList, Messages, os:system_time(seconds));	
		{join, User, _Rname} ->
			User ! {message, Messages},
			%send_messages_to_user(User, Rname, Messages),
			chatroom([User|User_List], Messages, os:system_time(seconds))	%adds a user to the chatroom
	after 604800 ->		%The chatroom is destroyed if it hasn't be used in a week
		{mps, server_node()} ! {removeRoom, self()}
	end.

%Recursive function that sends a user every message from a room,
send_messages_to_user(_User, _Rname, []) ->
	void;
send_messages_to_user(User,Rname,[Message|Messages]) ->	
	User ! {message, Rname, Message, ""},
	send_messages_to_user(User,Rname,Messages).

%Recursive function that sends a message to every user in a room
send_message(_Message, _From, _ChatRoom, []) ->
	void;
send_message(Message, From, Chatroom, [To|User_List]) ->
	To ! {message, Chatroom, Message, From},
	send_message(Message, From, Chatroom, User_List).
		

%%%Logs a user onto server, does not do it if they are already logged on
logon(Name) ->
	case whereis(mess_client) of
		undefined ->
			register(mess_client, spawn(mps, client, [server_node(), Name]));
			_ -> already_logged_on
	end.

logoff() ->
	mess_client ! logoff.

%Performs message passing for the client based on the actions that the client selects
client(Server_Node) ->
	receive
		logoff ->
			{mps, Server_Node} ! {logoff, self()},
			exit(normal);
		{sendMessage, Message, Rname} ->	%sends a message to other users
			{mps, Server_Node} ! {message, Message, self(), Rname};
		{create, Rname} ->	%requests to create a room
			{mps, Server_Node} ! {create, self(), Rname};
		{list} ->
			{mps, Server_Node} ! {list, self()};	%requests the name of all the chatrooms
		{join, Rname} ->
			{mps, Server_Node} ! {join, self(), Rname};	%requests to join a room
		{leave, Rname} ->
			{mps, Server_Node} ! {leave, self(), Rname};	%requests to leave a room
		{printRoom, RoomList} ->	%receives the rooms available on the server
			io:format("The chatrooms available are\n"),
			printRooms(RoomList);	
		{message, Chatroom, Message, From} ->	%receives a message from the server
			io:format("Chatroom ~p~n: Message from ~p: ~p~n", [Chatroom, From, Message]);
		{message, Message} -> %receives all the messages from a chatroom, for when the chatroom is just joined
			io:format("Chatroom messages ~s\n", [Message]);
		{help} ->	%the instructions for the user to use the chatroom
			io:format("Create a Room: create()~nList Rooms: list()~nJoin Rooms: join()~n Leave Rooms: leave() ~nSend a message: message() ~n")
	end,
	client(Server_Node).

%client command to create a room
create() -> 
	case whereis(mess_client) of % Test if the client is running
		undefined ->
		not_logged_on;
	_ -> {ok, [Rname]} = io:fread("Enter a room name : ", "~s"),
		mess_client ! {create, Rname},
		ok
	end.
%%		ok.


%client command to post a message
message(Message) ->
	case whereis(mess_client) of % Test if the client is running
		undefined ->
		not_logged_on;
	_ -> {ok, [Rname]} = io:fread("Send the message to which room? : ", "~s"),
		mess_client ! {sendMessage, Message, Rname},
		ok
	end.

%client command to list all available rooms
list() ->
	case whereis(mess_client) of % Test if the client is running
		undefined ->
		not_logged_on;
	_ ->
		io:format("list test"),
		mess_client ! {list},
		ok
	end.

%client command to join a room
join() ->
	case whereis(mess_client) of % Test if the client is running
		undefined ->
		not_logged_on;
	_ -> {ok, [Rname]} = io:fread("Join Which Room? : ", "~s"),
		mess_client ! {join, Rname},
		ok
	end.

%Client command to leave a room
leave() ->
	case whereis(mess_client) of % Test if the client is running
		undefined ->
		not_logged_on;
	_ -> {ok, [Rname]} = io:fread("Leave Which Room? : ", "~s"),
		mess_client ! {leave, Rname},
		ok
	end.

%lists the commands for the client
shelp() ->
	case whereis(mess_client) of % Test if the client is running
		undefined ->
		not_logged_on;
	_ -> mess_client ! {help},
		ok
	end.

%Prints all the rooms returned to the user
printRooms([]) ->
	void;
printRooms([Room|RoomList]) ->
	io:format(" ~p ~n", [Room]),
	printRooms(RoomList).

%part of the logon for the user
client(Server_Node, Name) ->
	{mps, Server_Node} ! {self(), logon, Name},
	io:format("Server_Node ~p~n", [Server_Node]),
	{mps, Server_Node} ! {test, Name},
%%	await_result(),
	client(Server_Node).

