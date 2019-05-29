import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.net.Socket;
import java.util.Scanner;

public class Client2 {
    final public static String host = "localhost";
    final public static int port = 5000;
    Socket socket;
    DataInputStream in;
    DataOutputStream ou;
    Scanner chat;
    boolean run;
    String name;

    public Client2(String n) throws IOException {
        name = n ;
        socket = new Socket(host , port);
        System.out.println("Connection Successful");
        run = true;
        runOutput();
    }

    public void runOutput() {
        Thread input = new Thread(new Runnable() {
            public void run() {
                while (run) {
                    try {
                        in = new DataInputStream(socket.getInputStream());
                        String s = in.readLine();
                        System.out.println(s);
                        if(chat.nextLine().compareTo("QUIT") == 0)
                            run = false;
                    } catch (IOException e) {
                        e.printStackTrace();
                    }
                }
            }
        });
        input.start();

        Thread t = new Thread(new Runnable() {
            public void run() {
                while (run) {
                    try {
                        ou = new DataOutputStream(socket.getOutputStream());
                        chat = new Scanner(System.in);
                        ou.writeBytes(name + " says :" + chat.nextLine() + "\n");

                    } catch (IOException e) {
                        e.printStackTrace();
                    }
                }
            }
        });

        t.start(); 
    } 

    public static void main(String[] args) throws IOException {
        Client2 client = new Client2("Ahmed");
    }   
}