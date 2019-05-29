package everythingElse;

import java.time.format.DateTimeFormatter;
import java.util.LinkedList;
import java.sql.Date;
import java.text.SimpleDateFormat;
import java.time.LocalDateTime;    

//Each chatroom represents a space on the server that clients/users can interact with each other
public class Chatroom {
	private String name;		//Name of the chatroom
	protected LinkedList<String> messages;	//List of text messages that have been sent by users to the chatroom and are displayed in the chatroom
	protected long dateLastUsed;		//The last time the chatroom was joined or had a message sent to it
	protected LinkedList<String> users;	//The clients/users that are currently in the chatroom
	
	/*
	 * Chatroom constructor
	 * @param name The name of the chatroom, as determined by the user creating it
	 */
	public Chatroom(String name) {
		dateLastUsed = System.currentTimeMillis() / 1000;		//Sent the time that the chatroom was used last to the current UNIX Epoch time
		messages = new LinkedList<String>();
		users = new LinkedList<String>();
		this.name = name;
	}
	
	/* 
	 * Adds a message into the chatroom
	 * @param message The message to be added to the chatroom
	 */
	protected void addMessage(String message) {
		messages.add(message);
		for (int n = 0; n < users.size(); n++) {
			String u = users.get(n);
			//Figure out how to do this line in corba
////////////	////////////////////////////////////////////////////////////////////////////////////////////////////////////
			//u.send("%Chatroom" + getName() + ":" + message);
/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
		}
		dateLastUsed = System.currentTimeMillis() / 1000;
	}
	
	protected void addUser(String user) {
		users.add(user);
	}
	
	protected void removeUser(String user) {
		users.remove(user);
	}
	
	protected void increaseTime() {
		dateLastUsed = dateLastUsed - 604790;
	}
	
	/*
	 * Returns the name of the chatroom
	 * @return String equal to the name of the chatroom
	 */
	protected String getName() {
		return this.name;
	}
	
}
