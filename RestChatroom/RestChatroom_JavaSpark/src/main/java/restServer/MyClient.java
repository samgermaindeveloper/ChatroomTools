package restServer;
import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.LinkedList;
import java.util.Scanner;

import org.apache.http.HttpResponse;
import org.apache.http.client.ClientProtocolException;
import org.apache.http.client.HttpClient;
import org.apache.http.client.methods.HttpGet;
import org.apache.http.client.methods.HttpPost;
import org.apache.http.client.methods.HttpPut;
import org.apache.http.impl.client.HttpClientBuilder;
 
/*
 * Restful Web Client that connects to an HTTP server
 */
public class MyClient {
 
		private String name;
		private String sUrl = "http://localhost:4567/rest";
		private LinkedList<String> rooms;
		private Scanner reader;
		
		public MyClient(String name) {
			this.name = name;
			this.reader = new Scanner(System.in);
			this.rooms = new LinkedList<String>();
			this.run();
		}
		
		/*
		 * Formats an HttpResponse so that a person can read it
		 */
		private String sResponse(HttpResponse response){
			try {
				if (response.getStatusLine().getStatusCode() != 200) {
					throw new RuntimeException("Failed : HTTP error code : " + response.getStatusLine().getStatusCode());
				}
 
				BufferedReader br = new BufferedReader(new InputStreamReader((response.getEntity().getContent())));
				String output;
				String rvalue = "";
				
				while ((output = br.readLine()) != null) {
					rvalue = rvalue + output;
				}
				return rvalue;
			}catch (Exception e) {
				return "";
			}
		}
		
		/*
		 * prints the commands for the user to use
		 */
		private String help() {
			String message = "Create a chatroom: create \nList Chat Rooms: list \nJoin Chat Room: join \nLeave Chat Room: leave";
			return message;
		}
		
		/*
		 * A post request to create a chatroom on the server
		 */
		private String create() {
			print("Name the Chatroom");
			String input = reader.nextLine();
			this.post(sUrl, input);
			return "Created room " + input + "\n";
		}

		/**
		 * Adds a Client into a chatroom
		 * @return All the messages that are in this chatroom OR a notification that there is no room by that name or that you are already in the room
		 */
		private String joinr() {
//				Scanner reader = new Scanner(System.in);
				print("Which room");	//Asks which room the client wants to join
				String room = reader.nextLine();
				for (int i = 0; i < rooms.size(); i++) {
					if (rooms.get(i).equals(room)) {
						return "You're already in that room";	//Checks if the client is already in the room
					}
				}
				String output =  this.put(sUrl + "/join", room, this.name, "");	//Add the client to the chatroom on the server
				if (! output.equals("no room")) {	//if the room actually exists
					rooms.add(room);	//Then add this room to the clients rooms list
				}
				return output + "\n";	//Returns the list of all the messages in this chatroom
		}
		
/*		private String joinr() {
				print("Which room would you like to join?");
				String room = reader.nextLine();
				return this.put(sUrl + "/join", room, this.name, "");
		}
*/

		/**
		 * Removes a client from a Chatroom
		 * @return A notification that you have left the room, or that you are not in the room to begin with
		 */
		private String leave() {
			Scanner reader = new Scanner(System.in);
			print("Which room");
			String room = reader.nextLine();
			for (int i = 0; i < rooms.size(); i++) {
				String r = rooms.get(i);
				if (r.equals(room)) {
					rooms.remove(r);	//Remove the chatroom from the clients list of rooms
					return this.put(sUrl + "/leave", room, this.name, "");
				}
			}
			//Checks all the clients chatrooms, and if the client isn't in a room by that name, then the client is notified
			return "You're not in that room";
		}
		
/*
		private String leave() {
			print("Which room would you like to leave");
			String room = reader.nextLine();
			return this.put(sUrl + "/leave", room, this.name, "");
		}
*/
		
		/*
		 * Used for testing if a chatroom is deleted after a week
		 */
		private String increaseTime() {
			print("Increase the time for which room?");
			String room = reader.nextLine();
			return this.put(sUrl + "increase", room, this.name, "");
		}
		
		/*
		 * Get request that receives the current timestamp of a room
		 */
		private String roomTime() {
			print("Which room do you want the time for?");
			//Scanner reader = new Scanner(System.in);
			String room = reader.nextLine();
			//reader.close();
			return this.get(sUrl + "{" + room + "}");
		}

		/*
		 * Sends a get request that is responded to with a string than contains all the rooms
		 */
		private String list() {
			return this.get( sUrl ) + "\n";
		}
		
		/*
		 * Writes a message to a chatroom
		 */
		private void write(String msg) {
			print("\nWhich room would you like to Write to?");
			String room = reader.nextLine();
			for (int i = 0; i < rooms.size(); i++) {
				String r = rooms.get(i);
				if (r.equals(room)) {
					this.put(sUrl + "/message", room, this.name, msg);
					return;
				}
			}
			//Checks all the clients chatrooms, and if the client isn't in a room by that name, then the client is notified
			print( "You're not in that room");
		}
		
		private String post(String url, String rName) {
			try {	
				HttpClient httpClient = HttpClientBuilder.create().build();
				HttpPost postRequest = new HttpPost(url);
				postRequest.addHeader("accept", "application/json");
				postRequest.addHeader("room", rName);
				// get requests accepts application/json data 
				HttpResponse response = httpClient.execute(postRequest);
				// Check for HTTP response code: 200 = success
				return sResponse(response);
			} catch (ClientProtocolException e) { e.printStackTrace();
			} catch (IOException e) { e.printStackTrace();
			}
			return null;
		}
		
		private String put(String url, String room, String user, String message){
			try {	
				HttpClient httpClient = HttpClientBuilder.create().build();
				HttpPost putRequest = new HttpPost(url);
				putRequest.addHeader("accept", "application/json");				// get requests accepts application/json data 
				putRequest.addHeader("room", room);
				putRequest.addHeader("user", user);
				putRequest.addHeader("message", message);
				HttpResponse response = httpClient.execute(putRequest);
				// Check for HTTP response code: 200 = success
				return sResponse(response);
			} catch (ClientProtocolException e) { e.printStackTrace();
			} catch (IOException e) { e.printStackTrace();}
			return null;
		}
		
		private String get(String url) {
			try {	
				HttpClient httpClient = HttpClientBuilder.create().build();
				HttpGet getRequest = new HttpGet(url);
				getRequest.addHeader("accept", "application/json");				// get requests accepts application/json data 
				getRequest.addHeader("user", this.name);
				//getRequest.addHeader("GET / HTTP/1.1", "url");
				getRequest.addHeader("Host", "developer.mozilla.org");
				getRequest.addHeader("Accept-Language", "en");
				HttpResponse response = httpClient.execute(getRequest);
				// Check for HTTP response code: 200 = success
				return sResponse(response);
			} catch (ClientProtocolException e) { e.printStackTrace();
			} catch (IOException e) { e.printStackTrace();
			}
			return null;
		}
			
		public void run() {
				String input;//Check if there is input from the user
				String output;
				do {
					System.out.println(this.help());
					input = reader.nextLine();
					//if the user has disconnected from the server, remove them from the list
					if (input.equals("create")){	//create a chat room
						output = this.create();
						print(output);
					}else if (input.equals("list")) {	//List the current chatrooms
						output = this.list();
						print(output);
					}else if (input.equals("join")) {	//Join the user to a chat room
						output = this.joinr();
						print(output);
					}else if (input.equals("leave")) {	//Remove the user from a chatroom
						output = this.leave();
						print(output);
					}else if (input.equals("roomTime")) {
						output = this.roomTime();
						print(output);
					}else if (input.equals("time")) {
						long time = System.currentTimeMillis() / 1000;
						output = String.valueOf(this.createDate(time));
						print(output);
					}else if (input.equals("increaseTime")) {
						this.increaseTime();
					}else if (input.equals("help")) {
						print(this.help());
					}else {	//All other input is interpreted as a message to be posted in the chatrooms that the user is in
						this.write(input);
					}
				} while (input != null);
		}

		/*
		 * Turns the current time into a DATE format, for testing if rooms are deleted after a week
		 */
		public String createDate(long time) {
			Date date = new java.util.Date(time * 1000L);
			SimpleDateFormat sdf = new java.text.SimpleDateFormat("yyyy-MM-dd HH:mm:ss z"); 
			sdf.setTimeZone(java.util.TimeZone.getTimeZone("GMT-4")); 
			String formattedDate = sdf.format(date);
			return formattedDate;
		}
		
		static void print(String x) {
			System.out.println(x);
		}
		
		public static void main(String args[]) {	
			print("Enter a name for yourself");
			Scanner reader = new Scanner(System.in);
			String name = reader.nextLine();
			MyClient client = new MyClient(name);
		}
}