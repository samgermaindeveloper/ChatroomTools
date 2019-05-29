using System;
using System.Text;
using System.Net.Sockets;
using System.Threading;
using System.IO;
using System.Net;

namespace Chatroom{
class Client{
    private Socket socket;
    //private Stream stream;
        
    public Client(string ip, int port){
        try{
            //Connect to the server through a socket
            Console.Write("\nEnter a name: ");
            String name = Console.ReadLine();
            socket = new Socket(AddressFamily.InterNetwork, SocketType.Stream, ProtocolType.Tcp);
            socket.Connect(new IPEndPoint(IPAddress.Parse(ip), port));
            this.Send(name);
            Thread getMsgs = new Thread(ReadMsgs);
            getMsgs.Start();
            this.run();
        }catch (IOException) { }
        catch (ObjectDisposedException) { }
        if (this != null)
            this.Close();
    }

    private void Close(){
        this.socket.Close();
        Environment.Exit(0);
    }

    private void Send(string message){
        int len = System.Text.Encoding.ASCII.GetByteCount(message);
        byte[] bytes = System.Text.Encoding.ASCII.GetBytes(message);
        byte[] lenBytes = System.BitConverter.GetBytes(len);
        socket.Send(lenBytes);
        socket.Send(bytes);
    }

    private String Receive() {
        byte[] lenBytes = new byte[4];
        socket.Receive(lenBytes);
        int len = System.BitConverter.ToInt32(lenBytes, 0);
        byte[] bytes = new byte[len];
        socket.Receive(bytes);
        String message = System.Text.Encoding.ASCII.GetString(bytes);
        return message;
    }

    private void ReadMsgs(){
        byte[] buffer = new byte[2048];
        try{
            while (true){
                String input = this.Receive();
                if (input.Length < 1){
                    break;
                } else {
                    Console.WriteLine(input);
                    Console.Write("\n: ");
                }
            }
        }catch (IOException) { }
        catch (ObjectDisposedException) { }
        this.Close();
    }

	/*
	 * Runs a thread for the client to constantly receive the clients input(non-Javadoc)
	 * @see java.lang.Thread#run()
	 */
    public void run() {
		this.DefaultMessage();
        Console.Write("\n: ");
        String action = Console.ReadLine();	//Check if there is input from the user
        do {
			//if the user has disconnected from the server, remove them from the list
			if (string.Equals(action, "create")){	//create a chat room
				this.Create();
        	}else if (string.Equals(action,"list")) {	//List the current chatrooms
				this.Send("list");
			}else if (string.Equals(action,"join")) {	//Join the user to a chat room
				this.Join();
			}else if (string.Equals(action,"leave")) {	//Remove the user from a chatroom
				this.Leave();
			}else if (string.Equals(action,"x")) {
				this.X();
			}else if (string.Equals(action,"time")) {
                this.Send("time");
			}else if (string.Equals(action,"roomTime")) {
				this.RoomTime();
            }else if (string.Equals(action,"options")){
                this.DefaultMessage();
            }else if (string.Equals(action,"myrooms")){
                this.Send("myrooms");
			}else if (string.Equals(action,"msg")){	//All other input is interpreted as a message to be posted in the chatrooms that the user is in
				this.WriteMsg();
            }else if (string.Equals(action,"op")){
                this.DefaultMessage();
                Console.Write("\n: ");
			}else{
                Console.WriteLine("Invalid input");
                Console.Write("\n: ");
            }
            action = Console.ReadLine();
		}while (action != null);
		this.Close();
	}

	protected void DefaultMessage() {
		String message = "Create a chatroom: create \nList Chat Rooms: list \nJoin Chat Room: join \nList Join Rooms: myrooms\nLeave Chat Room: leave\nWrite Message: msg\nOptions: op";
		Console.WriteLine("\n" + message);
	}

	private void Create() {
        Console.Write("Name the Chatroom: ");
		String name = Console.ReadLine();
		this.Send("create:" + name);
    }

    private void Join() {
        Console.Write("Which room would you like to join: ");
		String name = Console.ReadLine();
		this.Send("join:" + name);
    }

    private void Leave() {
        Console.Write("Which room would you like to leave: ");
        String name = Console.ReadLine();
		this.Send("leave:" + name);
    }

	private void RoomTime() {
        Console.Write("Which room do you want the time for: ");
        String name = Console.ReadLine();
		this.Send("roomTime:" + name);
    }

    private void X() {
        Console.Write("Which room: ");
        String name = Console.ReadLine();
		this.Send("x:" + name);
    }

    private void WriteMsg(){
        Console.Write("Send message to which room: ");
        String room = Console.ReadLine();
        Console.Write("Enter message: ");
        String msg = Console.ReadLine();
        this.Send("msg:" + room + ":" + msg);
    }

    static void Main(string[] args){
        Client client = new Client("127.0.0.1", 5000);
    }

}}

            