package restServer;
import static spark.Spark.get;
import static spark.Spark.post;

import java.io.IOException;
import java.util.LinkedList;
//import java.util.List;


//import org.slf4j.Logger;
//import org.slf4j.LoggerFactory;

public class RestServer {
	
//	private static final Logger logger = LoggerFactory.getLogger(RestServer.class);

	public static final int SUCCESS = 200;
	public static final int BAD_REQUEST = 400;
	public static final int FORBIDDEN = 403;
	public static final int NOT_FOUND = 404;

	//All the chatrooms available on this server
	protected LinkedList<MyChatroom> chatRooms = new LinkedList<MyChatroom>();	//All the chatrooms on this Server. This chatrooms list and the userlist and messagelist in the chatrooms are the only stateful part of this server.

	/**
	 * Constructor for the server. Initializes the Chatroom list and JSON parser
	 */
	public RestServer() {
		chatRooms = new LinkedList<MyChatroom>();
		chatRooms.add(new MyChatroom("Room1"));
	}
	
	/**  Receives a message that is stored in the chatrooms messages list, and sends this message to all users in that chatroom.
	 *	Put request, Path: /rest/message
	 *  @Param user	The user who is sending the message
	 *	@Param room The room the message is to be sent to
	 *	@Param message The message to be sent
	 */
    public String message(String user, String room, String message) throws IOException {
    	String header = "Message";
    	String response = "Sent";
    	MyChatroom c = this.roomObject(room);
    	c.addMessage(message, user);
    	return header + response;
    }
    
	/**  Removes a user named in the user list of the chatroom
	 * 	Put request, Path: /rest/leave
     *	@Param user	The user to be removed
	 *  @Param room The room the user will be removed from
     */
    public String leaveRoom(String user, String room) throws IOException {
		String header = "Left Room";
		String response = room;
		MyChatroom c = this.roomObject(room);
		c.removeUser(user);
		return header + response + "\n";
	}

	/**  Adds a user to the user list of the chatroom
	 * Put request, Path: /rest/join
     *	@Param user	The user to be added
	 *  @Param room The room the user will be added to
     */
    public String joinRoom(String user, String room){
        String messages = "Messages from room " + room + ": ";
        MyChatroom c = this.roomObject(room);
        c.addUser(user);
        for (int j = 0; j < c.messages.size(); j++) {
			messages = messages + c.messages.get(j) + ", ";
		}
        return messages;
    }

    /** Post request Creates a room for this server and adds it to the servers room list
     * @Param room The name of the room
     */
    public String createRoom(String room){
    	String header = "Created_room";
    	String response = room;
    	MyChatroom c = new MyChatroom(room);
    	chatRooms.add(c);
    	return header + response;
    }
	
    /** Lists the names of all the rooms on this server
     * @return A string containing the names of all the rooms
     */
    public String listRooms() {//@RequestHeader("user") String room) {
    	String rooms = "Available_Rooms: ";	//% is used to determine if there needs to be user input or not
    	for (int j = 0; j < chatRooms.size(); j++) {
			rooms = rooms + chatRooms.get(j).getName() + ", ";
		}
		return rooms;
    }
    
    /** Takes a room name and returns the room object with that name
     * @param room: A string containing the name of the chatroom
     * @return A chatroom containing the same name as name
     */
	private MyChatroom roomObject(String room) {
    	for (int i = 0; i < chatRooms.size(); i++) {
    		MyChatroom c = chatRooms.get(i);
    		if (c.getName().equals(room)) {
    			return c;
    		}
    	}
    	return null;
    }

	/**
	 * Lazy form of printing a string
	 * @param x The string to be printed
	 */
	private void print(String x) {
		System.out.println(x);
	}
	
	public static void main(String[] args) {
		RestServer s = new RestServer();
        get("/rest", (req, res) -> s.listRooms());
        post("/rest", (req, res) -> s.createRoom(req.headers("room")));
        //Why isn't this working?
        //put("/rest", (req, res) -> s.createRoom(req.headers("room")));
        post("/rest/join", (req, res) -> s.joinRoom(req.headers("user"),req.headers("room")));
        post("/rest/leave", (req, res) -> s.leaveRoom(req.headers("user"),req.headers("room")));
        post("/rest/message", (req, res) -> s.message(req.headers("user"),req.headers("room"), req.headers("message")));
	}

}