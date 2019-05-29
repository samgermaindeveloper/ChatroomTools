
import CorbaChat.*;

import java.util.LinkedList;
import java.util.Scanner;
import org.omg.CORBA.ORB;
import org.omg.CosNaming.NamingContextExt;
import org.omg.CosNaming.NamingContextExtHelper;

/*
 * Client to communicate with a Corba Chat Server. Sends requests to create chatrooms, write to rooms, join rooms, leave rooms, and list the rooms available
 */
public class Client extends Thread{
	private String name;
	static CorbaServer cServ;
	private LinkedList<String> rooms;
	Scanner reader = new Scanner(System.in);
	
	/*
	 * Constructor for the client. Saves a name for the client and starts the cycle of commands
	 */
	public Client() {
		try {
			reader = new Scanner(System.in);
			print("Enter a name");
			name = reader.nextLine();
			rooms = new LinkedList<String>();
			this.run();
		}catch (Exception e) {
			e.printStackTrace();
		}
	}
	
	/*
	 * Returns a string of the commands that the client is able to give
	 */
	private String help() {
		String message = "Create a chatroom: create \nList Chat Rooms: list \nJoin Chat Room: join \nLeave Chat Room: leave\nHelp: help\nWrite Message: Write anything else";
		return message;
	}
	
	/*
	 * Creates a new chatroom on the server
	 * @cServ The Server object that can be interacted with using Corba
	 * @return A notification that you've joined the room
	 */
	private String create(CorbaServer cServ) {
		print("Name the Chatroom");
		String input = reader.nextLine();
		cServ.create(input, this.name);
		rooms.add(input);
		return "Joined room " + input;
	}
	
	/*
	 * Adds a Client into a chatroom
	 * @cServ The Server object that can be interacted with using Corba
	 * @return All the messages that are in this chatroom OR a notification that there is no room by that name or that you are already in the room
	 */
	private String join(CorbaServer cServ) {
			Scanner reader = new Scanner(System.in);
			print("Which room");	//Asks which room the client wants to join
			String room = reader.nextLine();
			for (int i = 0; i < rooms.size(); i++) {
				if (rooms.get(i).equals(room)) {
					return "You're already in that room";	//Checks if the client is already in the room
				}
			}
			String output = cServ.join(room, this.name);	//Add the client to the chatroom on the server
			if (! output.equals("no room")) {	//if the room actually exists
				rooms.add(room);	//Then add this room to the clients rooms list
			}
			return output;	//Returns the list of all the messages in this chatroom
	}
	
	/*
	 * Removes a client from a Chatroom
	 * @cServ The Server object that can be interacted with using Corba
	 * @return A notification that you have left the room, or that you are not in the room to begin with
	 */
	private String leave(CorbaServer cServ) {
		Scanner reader = new Scanner(System.in);
		print("Which room");
		String room = reader.nextLine();
		for (int i = 0; i < rooms.size(); i++) {
			String r = rooms.get(i);
			if (r.equals(room)) {
				String output = cServ.leave(room, this.name);	//Remove the client from the room on the server
				rooms.remove(r);	//Remove the chatroom from the clients list of rooms
				return output;
			}
		}
		//Checks all the clients chatrooms, and if the client isn't in a room by that name, then the client is notified
		return "You're not in that room";
	}
	
	/*
	 * Writes a message into a chatroom
	 * @msg The message to be written into the chatroom
	 * @cServ The Server object that can be interacted with using Corba 
	 */
	private String writeMessage(String msg, CorbaServer cServ) {
		Scanner reader = new Scanner(System.in);
		print("Write the message to which room");
		String room = reader.nextLine();	//Get which room the message will go into
		return cServ.writeMessage(msg, room, this.name);
	}
	
	/*
	 * Runs a thread for the client to constantly receive the clients input(non-Javadoc)
	 * @see java.lang.Thread#run()
	 */
	public void run(CorbaServer cServ) {
//		try {
			Scanner reader = new Scanner(System.in);
			String output;
			String input;
			print(this.help());
			do {
				input = reader.nextLine();
				//if the user has disconnected from the server, remove them from the list
				if (input.equals("create")){	//create a chat room
					output = this.create(cServ);
					print(output);
				}else if (input.equals("list")) {	//List the current chatrooms
					output = cServ.list();
					print(output);
				}else if (input.equals("join")) {	//Join the user to a chat room
					output = this.join(cServ);
					print(output);
				}else if (input.equals("leave")) {	//Remove the user from a chatroom
					output = this.leave(cServ);
					print(output);
				}else if (input.equals("help")) {
					print(this.help());
				}else {	//All other input is interpreted as a message to be posted in the chatrooms that the user is in
					output = this.writeMessage(input, cServ);
					print(output);
				}
					
			} while (input != null);
			reader.close();
	}
	
	//For easier implementation of printing strings
	static void print(String x) {
		System.out.println(x);
	}
	
    public static void main(String[] args) {
        try {
        	//Standard corba code
        	ORB orb = ORB.init(args, null);
  	    	org.omg.CORBA.Object objRef =   orb.resolve_initial_references("NameService");
  	    	NamingContextExt ncRef = NamingContextExtHelper.narrow(objRef);
  	    	CorbaServer cServ = (CorbaServer) CorbaServerHelper.narrow(ncRef.resolve_str("ABC"));
   
  	    	//Create a Client object and starting running their request cycle
  	    	Client a = new Client();
  	    	a.run(cServ);
         }catch (Exception e) {
        	 System.out.println("Client exception: " + e);
        	 e.printStackTrace();
         }
    }
}

