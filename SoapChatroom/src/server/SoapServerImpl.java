package server;

import java.util.LinkedList;

//import javax.jws.WebMethod;
import javax.jws.WebService;

@WebService(endpointInterface = "server.SoapServer")
public class SoapServerImpl implements SoapServer {

	private LinkedList<Chatroom> chatRooms;
	
	public SoapServerImpl() {
		this.chatRooms = new LinkedList<Chatroom>();
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
				r.addMessage(user + ":" + msg + ", ");
				break;
			}
		}
		return "Message sent";
	}
	
}
