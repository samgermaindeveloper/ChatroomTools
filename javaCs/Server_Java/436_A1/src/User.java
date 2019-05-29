import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.Socket;
import java.util.Date;
import java.text.SimpleDateFormat;
import java.util.LinkedList;

//Each user represents a client that has connected to the server
public class User implements Runnable{
	private InputStream inputStream;
	private OutputStream outputStream;
	private Socket socket;
	private String name;
	protected LinkedList<Chatroom> chatRooms;
	private Server server;
	
	/*
	 * User Constructor, create a user for each client connecting to the server
	 * @socket The socket that the user will be communicated through
	 * The client is prompted to create a name for themself, they are they prompted to do an action.
	 */
	public User(Socket socket, Server server) {
		this.socket = socket;
		this.server = server;
		try {
			inputStream = socket.getInputStream();
			outputStream = socket.getOutputStream();
			chatRooms = new LinkedList<Chatroom>();
			this.name = this.getInput();
			print(this.name);
		} catch (IOException e) {	
		}
	}
	
	/*
	 * Returns the current amount of chatrooms this user is in
	 */
	protected int chatRoomLength() {
		return this.chatRooms.size();
	}

	/*
	 * Tells the user how many chatrooms they are in
	 */
	private void sendChatRoomLength() {
		this.send(Integer.toString(this.chatRooms.size()));
	}
	
	protected void writeUTF(String message) {
        byte[] bytes = message.getBytes();
        int len = bytes.length;
        byte[] lenBytes = new byte[4];
        lenBytes[0] = (byte)(len & 0xff);
        lenBytes[1] = (byte)((len >> 8) & 0xff);
        lenBytes[2] = (byte)((len >> 16) & 0xff);
        lenBytes[3] = (byte)((len >> 24) & 0xff);
        try {
        	outputStream.write(lenBytes);
        	outputStream.write(bytes);
        } catch (Exception e) {}
	}
	
	/*
	 * Gets the most recent input from the user
	 */
	protected String getInput() {
		try {
	        byte[] lenBytes = new byte[4];
	        inputStream.read(lenBytes, 0, 4);
	        int len = (((lenBytes[3] & 0xff) << 24) | ((lenBytes[2] & 0xff) << 16) |
	                  ((lenBytes[1] & 0xff) << 8) | (lenBytes[0] & 0xff));
	        byte[] receivedBytes = new byte[len];
	        inputStream.read(receivedBytes, 0, len);
	        String received = new String(receivedBytes, 0, len);
	        return received;
		}catch (Exception e) {
			return "Could not read input";
		}
	}
	
	
	/*
	 * Puts a user/client in a chatroom
	 * @param cRoom: The chatroom that the user will join 
	 */
	protected void joinRoom (Chatroom cRoom) {
		this.chatRooms.add(cRoom);
	}
	
	/*
	 * Removes a user/client from a chatroom
	 */
	protected void leaveRoom (Chatroom c) {
		chatRooms.removeFirstOccurrence(c);
	}
	
	/*
	 * Sends a message to the user
	 * @param message: The message to be sent to the user
	 */
	protected void send (String message) {
		this.writeUTF(message);
	}
	
	/*
	 * Closes the connection to the client
	 */
	protected void close () {
		try {
			inputStream.close();
			outputStream.close();
			socket.close();
		} catch (IOException e) {}
	}
		
	public void print(String x) {
		System.out.println(x);
	}
	
	private void create(String name) {
		Chatroom room = this.getChatRoom(name);
		if (room == null){
			Chatroom c = this.server.createChatRoom(name);
			this.joinRoom(c);
			c.addUser(this);
			this.send("You've created a room named " + name);
		}else{
			this.send("There's already a Chatroom named that");
		}
	}
	
	private void list() {
		String rooms = "All Chatrooms:\n";	
		for (int j = 0; j < server.chatRooms.size(); j++) {
			rooms = rooms + server.chatRooms.get(j).getName() + "\n";
		}
		this.send(rooms.trim());
	}
	
	private void listMyRooms() {
		String rooms = "Your Chatrooms:\n";	
		for (int j = 0; j < this.chatRooms.size(); j++) {
			rooms = rooms + this.chatRooms.get(j).getName() + "\n";
		}
		this.send(rooms.trim());
	}
	
	private void numChatRooms() {
		this.send(Integer.toString(server.chatRooms.size()));
	}
	
	private void join(String roomName) {
		Chatroom room = this.getChatRoom(roomName);
		if (room != null) {
			this.joinRoom(room);
			room.addUser(this);
			String message = "(Messages from " + roomName + ")\n";
			//Print the current messages in the chatroom to the user
			for (int j = 0; j < room.messages.size(); j++ ) {
				message = message + room.messages.get(j) + "\n";
			}
			this.send(message.trim());
		}else {
			this.send("There's no room by that name");
		}
	}
	
	private Chatroom getMyChatRoom(String roomName) {
		int end = this.chatRoomLength();
		for (int m = 0; m < end; m++) {	//find the chatroom by the same name
			Chatroom room = this.chatRooms.get(m);
			if (room.getName().equals(roomName)) {
				return room;
			}
		}
		return null;
	}
	
	private void leave(String roomName) {
		Chatroom room = this.getMyChatRoom(roomName);
		if (room != null) {
			this.chatRooms.remove(room);
			room.removeUser(this);
			this.send("Great! You've been removed from " + roomName);
		} else {
			this.send("You're not in a chatroom named " + roomName);
		}
	}
	
	private void roomTime(String roomName, boolean decreaseTime) {
		Chatroom room = getChatRoom(roomName);
		if (room != null) {
			this.send("Current time stamp on room: " + createDate(room.dateLastUsed));
			if (decreaseTime == true) {
				room.increaseTime();
				this.send("New time stamp on room: " + createDate(room.dateLastUsed));
			}
		}else {
			this.send("There's no room by that name");
		}
	}
	
	private String createDate(long time) {
		Date date = new java.util.Date(time * 1000L);
		SimpleDateFormat sdf = new java.text.SimpleDateFormat("yyyy-MM-dd HH:mm:ss z"); 
		// give a timezone reference for formatting (see comment at the bottom)
		sdf.setTimeZone(java.util.TimeZone.getTimeZone("GMT-4")); 
		String formattedDate = sdf.format(date);
		return formattedDate;
	}
	
	private void writeMessage(String room, String input) {
		Chatroom c = getMyChatRoom(room);
		if (c != null) {
			c.addMessage(input + "\n", this.name);
		}else {
			this.send("You're not in a room named " + room);
		}
	}
		
	private Chatroom getChatRoom(String roomName) {
		int end = server.chatRooms.size();
		for (int k = 0; k < end; k++) {
			Chatroom room = server.chatRooms.get(k);
			if (room.getName().equals(roomName)) {
				return room;
			}
		}
		return null;
	}

	
	/*
	 * Runs a thread for the client to constantly receive the clients input(non-Javadoc)
	 * @see java.lang.Thread#run()
	 */
        public void run() {
        	String input = this.getInput();	//Check if there is input from the user
			do {
				//if the user has disconnected from the server, remove them from the list
				String action[] = input.split(":");
				if (action[0].equals("create")){	//create a chat room
					this.create(action[1]);
				}else if (action[0].equals("list")) {	//List the current chatrooms
					this.list();
				}else if (action[0].equals("join")) {	//Join the user to a chat room
					this.join(action[1]);
				}else if (action[0].equals("leave")) {	//Remove the user from a chatroom
					this.leave(action[1]);
				}else if (action[0].equals("x")) {
					this.roomTime(action[1], true);
				}else if (action[0].equals("time")) {
					long time = System.currentTimeMillis() / 1000;
					this.send(String.valueOf(createDate(time)));
				}else if (action[0].equals("roomTime")) {
					this.roomTime(action[1], false);
				}else if (action[0].equals("myrooms")){
					this.listMyRooms();
				}else {	//All other input is interpreted as a message to be posted in the chatrooms that the user is in
					this.writeMessage(action[1], action[2]);
				}
				input = this.getInput();
			}while (input != null);
			this.server.userList.remove(this);
			for (int i=0; i<=this.chatRoomLength(); i++) {
				this.chatRooms.get(i).users.remove(this);
			}
			this.close();
	}
	
}

	

