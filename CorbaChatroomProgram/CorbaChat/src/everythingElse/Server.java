package everythingElse;

import CorbaChat.*;
import java.io.IOException;
import java.net.ServerSocket;
import java.net.Socket;
import java.util.ArrayList;
import java.util.LinkedList;

import org.omg.CORBA.ORB;
import org.omg.CosNaming.NameComponent;
import org.omg.CosNaming.NamingContextExt;
import org.omg.CosNaming.NamingContextExtHelper;
import org.omg.PortableServer.POA;
import org.omg.PortableServer.POAHelper;


/*
 * Creates a server that can host up to 10 clients who can join chat rooms, post messages in chatrooms, and view posts made by other clients within the same chat room
 */
public class Server{
	//protected ArrayList<String> userList = new ArrayList<String>(10);;	//A list of users, where each user is a client connected to the server
//	protected LinkedList<Chatroom> chatRooms = new LinkedList<Chatroom>();;	//A list of chatrooms where each client can post messages in, where messages can be seen by all clients in the chatroom
	//private roomTimeThread roomThread = new roomTimeThread();;
			
		 
	public static void main(String args[]) {
		try{
		      // create and initialize the ORB //// get reference to rootpoa &amp; activate the POAManager
		      ORB orb = ORB.init(args, null);      
		      POA rootpoa = POAHelper.narrow(orb.resolve_initial_references("RootPOA"));
		      rootpoa.the_POAManager().activate();
		 
		      // create servant and register it with the ORB
		      ServerHelper addobj = new ServerHelper();
		      addobj.setORB(orb); 
		 
		      // get object reference from the servant
		      org.omg.CORBA.Object ref = rootpoa.servant_to_reference(addobj);
		      CorbaServer href = CorbaServerHelper.narrow(ref);
		 
		      org.omg.CORBA.Object objRef =  orb.resolve_initial_references("NameService");
		      NamingContextExt ncRef = NamingContextExtHelper.narrow(objRef);
		 
		      NameComponent path[] = ncRef.to_name( "ABC" );
		      ncRef.rebind(path, href);
		 
		      System.out.println("Corba Chatroom Server ready and waiting ...");
		 
		      // wait for invocations from clients
		      for (;;){
			  orb.run();
		      }
		    } 
		 
		      catch (Exception e) {
		        System.err.println("ERROR: " + e);
		        e.printStackTrace(System.out);
		      }
		 
		      System.out.println("HelloServer Exiting ...");
		 
		  }
		}

