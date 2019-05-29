package everythingElse;
import CorbaChat.*;
import java.io.IOException;
import java.net.ServerSocket;
import java.net.Socket;
import java.util.ArrayList;
import java.util.LinkedList;

import org.omg.CORBA.ORB;

/**
 * Creates a server that can host up to 10 clients who can join chat rooms, post messages in chatrooms, and view posts made by other clients within the same chat room
 */
public class ServerHelper extends CorbaServerPOA{
	protected ArrayList<String> userList = new ArrayList<String>(10);;	//A list of users, where each user is a client connected to the server
	protected LinkedList<Chatroom> chatRooms = new LinkedList<Chatroom>();;	//A list of chatrooms where each client can post messages in, where messages can be seen by all clients in the chatroom
	private roomTimeThread roomThread = new roomTimeThread();;
	private ORB orb;	//The object request broker
	
	public void setORB(ORB orb_val) {
		orb = orb_val;
	}
	  
	/**
	 * Starts the timer to see if any chatrooms have been unused for a week.
	 */
	public void startRoomThread() {
		this.roomThread.start();
	}		

	/**
	 * For deleting a chatroom when the chatroom has not been used for a week. Constantly check all chatrooms to see how old they are.
	 */
	public class roomTimeThread extends Thread{
		private long currentTime;
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

	/**
	 * Creates a chatroom
	 * @room The name of the chatroom
	 * @user The user creating the chatroom who will be added into the chatroom
	 */
	@Override
	public void create(String room, String user) {
		Chatroom cRoom = new Chatroom(room);
		this.chatRooms.add(cRoom);
		cRoom.addUser(user);
	}

	/**
	 * Lists all the chatrooms on the server
	 */
	@Override
	public String list() {
		String rooms = "";
		for (int j = 0; j < chatRooms.size(); j++) {
			rooms = rooms + chatRooms.get(j).getName() + "\n";
		}
		return rooms;
	}
	
	/**
	 * Puts a client into a chatroom on the server end
	 * @room The name of the chatroom
	 * @user The name of the client joining
	 * @return All the messages in the chatroom
	 */
	@Override
	public String join(String room, String user) {
		int end = chatRooms.size();
		String message = "";
		for (int k = 0; k < end; k++) {
			Chatroom r = chatRooms.get(k);
			if (r.getName().equals(room)) {
				r.addUser(user);
				for(int i=0; i < r.messages.size(); i++) {
					message = message + r.messages.get(i);	//Add all the messages in the chatroom together
				}
				return message;
			}
		}
		return "No Room";	//Theres no chatroom by thatname
	}

	/**
	 * Removes a user/client from a chatroom
	 * @param room The room the user will leave
	 * @param user The user who will leave
	 * @return A notification saying the user left or the user isn't in a room by that name
	 */
	@Override
	public String leave(String room, String user) {
		int end = chatRooms.size();
		for (int k = 0; k<end; k++) {
			Chatroom r = chatRooms.get(k);
			if (r.getName().equals(room)) {
				r.removeUser(user);
				return "Great, you've been removed from " + room;
			}
		}
		return "You are not in a room named " + room;
	}

	/**
	 * Writes a message into a specific chatroom
	 * @param msg The message to be written to the chatroom
	 * @param room The room to be written to 
	 * @param user The user/client writing the message
	 */
	@Override
	public String writeMessage(String msg, String room, String user) {
		int end = chatRooms.size();
		for(int k = 0; k < end; k++) {
			Chatroom r = chatRooms.get(k);
			if (r.getName().equals(room)) {
				r.addMessage(user + ":" + msg);
				break;
			}
		}
		return "";
	}
}
