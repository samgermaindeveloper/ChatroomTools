import java.io.IOException;
import java.net.ServerSocket;
import java.net.Socket;
import java.util.ArrayList;
import java.util.LinkedList;

/*
 * Creates a server that can host up to 10 clients who can join chat rooms, post messages in chatrooms, and view posts made by other clients within the same chat room
 */
public class Server{
	protected ArrayList<User> userList;	//A list of users, where each user is a client connected to the server
	protected LinkedList<Chatroom> chatRooms;	//A list of chatrooms where each client can post messages in, where messages can be seen by all clients in the chatroom
	private ServerSocket serverSocket;	//The socket for the server
	private roomTimeThread roomThread;
	
	/*
	 * Constructor for the server class. Initializes the server attributes,
	 */
	public Server() {
		this.userList = new ArrayList<User>(10);
		this.chatRooms = new LinkedList<Chatroom>();
		try {
			this.serverSocket = new ServerSocket(5000);
			this.roomThread = new roomTimeThread();
			this.roomThread.start();
			this.createUser();
		}catch (IOException e) {
			e.printStackTrace();
		}
	}
	
	/*
	 * Creates a new user when a client connects to the server, and starts a user thread
	 */
	public void createUser() {
		try {
			Socket userSocket = serverSocket.accept();
			Thread user = new Thread(new User(userSocket, this));
			user.start();
			this.createUser();
		} catch (IOException e) {
			e.printStackTrace();
		}
	}
	
	/*
	 * Creates a chatroom for clients to interact in
	 * @param roomName: The name of the chat room to be created
	 */
	protected Chatroom createChatRoom(String roomName) {
		Chatroom room = new Chatroom(roomName);
		this.chatRooms.add(room);
		return room;
	}

	public class roomTimeThread extends Thread{
		private long currentTime;
		
		//public roomTimeThread() {
		//}
		
		public void run() {
			while(true) {
				currentTime = System.currentTimeMillis() / 1000;
					//Loop through each chatroom and check if the chatroom has been used(joined or had a message sent to it) and remove that chatroom if it hasn't been used in a week
				for (int i = 0; i < chatRooms.size(); i++) {
					Chatroom room = chatRooms.get(i);
					if ((currentTime - 604800) >= room.dateLastUsed) {
						chatRooms.remove(room);
						//Also remove the chatroom from clients lists of chatrooms
					}
				}
					//Loop through all the users currently connected to this server
			}
		}
	}
	
	public static void main(String args[]) {
		Server server = new Server ();
		//Error with which chatroom list is being read when joining a chatroom or leaving a chatroom
		//Messages written to the chatroom don't get sent out
	}
}
	
	

