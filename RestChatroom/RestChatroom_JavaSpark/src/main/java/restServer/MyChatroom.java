package restServer;

import java.util.LinkedList;    

//Each chatroom represents a space on the server that clients/users can interact with each other
public class MyChatroom {
	private String name;		//Name of the chatroom
	protected LinkedList<String> messages;	//List of text messages that have been sent by users to the chatroom and are displayed in the chatroom
	protected long dateLastUsed;		//The last time the chatroom was joined or had a message sent to it
	protected LinkedList<String> users;	//The clients/users that are currently in the chatroom, this list exists so that it is known who to message when a message is received
	
	/*
	 * Chatroom constructor
	 * @param name The name of the chatroom, as determined by the user creating it
	 */
	public MyChatroom(String name) {
		dateLastUsed = System.currentTimeMillis() / 1000;		//Sent the time that the chatroom was used last to the current UNIX Epoch time
		messages = new LinkedList<String>();
		users = new LinkedList<String>();
		this.name = name;
	}
	
	/* 
	 * Adds a message into the chatroom
	 * @param message The message to be added to the chatroom
	 */
	protected void addMessage(String message, String user) {
		messages.add(message);
		String outMessage = "%Chatroom: " + getName() + " User: " + user + "\n" + message;
		for (int n = 0; n < users.size(); n++) {
			//this.users.get(n).write(outMessage.getBytes());
		}
		dateLastUsed = System.currentTimeMillis() / 1000;
	}
	
	/*
	 * Names a user in the user list of the chatroom
	 */
	protected void addUser(String user) {
		users.add(user);
	}
	
	/*
	 * Removes a user named in the user list of the chatroom
	 */
	protected void removeUser(String user) {
		users.remove(user);
	}
	
	/*
	 * Set's the time of the room so that the room looks like it's almost a week old
	 */
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
	
	/*
	 * Returns the name of the chatroom in Unix Epoch Time
	 * @return Long equal to the time of the chatroom
	 */
	protected long getTime() {
		return this.dateLastUsed;
	}
	
}
