package server;

import java.net.MalformedURLException;
import java.net.URL;
import java.util.LinkedList;
import java.util.Scanner;

import javax.xml.namespace.QName;
import javax.xml.ws.Service;

public class SoapClient {
	private SoapServer serv;
	private Service clientService;
	Scanner reader = new Scanner(System.in);
	String name;
	private LinkedList<String> rooms;
	
	public SoapClient() throws MalformedURLException{
		reader = new Scanner(System.in);
		print("Enter a name");
		name = reader.nextLine();
		rooms = new LinkedList<String>();
		//wsdl: web service description language
		URL url = new URL("http://127.0.0.1:8080/ss?wsdl");
		QName qname = new QName("http://server/", "SoapServerImplService");
		clientService = Service.create(url, qname);	//Client view for web service
		this.serv = clientService.getPort(SoapServer.class);
	}
	
	/**
	 * Returns a string of the commands that the client is able to give
	 */
	private String help() {
		String message = "Create a chatroom: create \nList Chat Rooms: list \nJoin Chat Room: join \nLeave Chat Room: leave\nHelp: help\nWrite Message: Write anything else";
		return message;
	}
	
	/**
	 * Creates a new chatroom on the server
	 * @return A notification that you've joined the room
	 */
	private String create() {
		print("Name the Chatroom");
		String input = reader.nextLine();
		this.serv.create(input, this.name);
		rooms.add(input);
		return "Joined room " + input;
	}
	
	/**
	 * Adds a Client into a chatroom
	 * @return All the messages that are in this chatroom OR a notification that there is no room by that name or that you are already in the room
	 */
	private String join() {
			reader = new Scanner(System.in);
			print("Which room");	//Asks which room the client wants to join
			String room = reader.nextLine();
			for (int i = 0; i < rooms.size(); i++) {
				if (rooms.get(i).equals(room)) {
					return "You're already in that room";	//Checks if the client is already in the room
				}
			}
			String output = serv.join(room, this.name);	//Add the client to the chatroom on the server
			if (! output.equals("no room")) {	//if the room actually exists
				rooms.add(room);	//Then add this room to the clients rooms list
			}
			return output;	//Returns the list of all the messages in this chatroom
	}
	
	/**
	 * Removes a client from a Chatroom
	 * @return A notification that you have left the room, or that you are not in the room to begin with
	 */
	private String leave() {
		reader = new Scanner(System.in);
		print("Which room");
		String room = reader.nextLine();
		for (int i = 0; i < rooms.size(); i++) {
			String r = rooms.get(i);
			if (r.equals(room)) {
				String output = serv.leave(room, this.name);	//Remove the client from the room on the server
				rooms.remove(r);	//Remove the chatroom from the clients list of rooms
				return output;
			}
		}
		//Checks all the clients chatrooms, and if the client isn't in a room by that name, then the client is notified
		return "You're not in that room";
	}
	
	/**
	 * Writes a message into a chatroom
	 * @msg The message to be written into the chatroom
	 */
	private String writeMessage(String msg) {
		reader = new Scanner(System.in);
		print("Write the message to which room");
		String room = reader.nextLine();	//Get which room the message will go into
		for (int i = 0; i < rooms.size(); i++) {
			String r = rooms.get(i);
			if (r.equals(room)) {
				String output = serv.writeMessage(msg, room, this.name);	//Remove the client from the room on the server
				return output;
			}
		}
		//Checks all the clients chatrooms, and if the client isn't in a room by that name, then the client is notified
//		return "You're not in that room";
		return "You're not in that room";
	}
	
	/**
	 * Runs a thread for the client to constantly receive the clients input(non-Javadoc)
	 * @see java.lang.Thread#run()
	 */
	public void run() {
//		try {
			Scanner reader = new Scanner(System.in);
			String output;
			String input;
			print(this.help());
			do {
				print("next?");
				input = reader.nextLine();
				//if the user has disconnected from the server, remove them from the list
				if (input.equals("create")){	//create a chat room
					output = this.create();
					print(output);
				}else if (input.equals("list")) {	//List the current chatrooms
					output = serv.list();
					print(output);
				}else if (input.equals("join")) {	//Join the user to a chat room
					output = this.join();
					print(output);
				}else if (input.equals("leave")) {	//Remove the user from a chatroom
					output = this.leave();
					print(output);
				}else if (input.equals("help")) {
					print(this.help());
				}else {	//All other input is interpreted as a message to be posted in the chatrooms that the user is in
					output = this.writeMessage(input);
					print(output);
				}
					
			} while (input != null);
			reader.close();
	}
	
	//For easier implementation of printing strings
	static void print(String x) {
		System.out.println(x);
	}
	
    public static void main(String args[]) {
        try {   
  	    	//Create a Client object and starting running their request cycle
  	    	SoapClient sc = new SoapClient();
  	    	sc.run();
         }catch (Exception e) {
        	 System.out.println("Client exception: " + e);
        	 e.printStackTrace();
         }
    }
}
