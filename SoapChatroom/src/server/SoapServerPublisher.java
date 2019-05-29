package server;

import javax.xml.ws.Endpoint;

public class SoapServerPublisher {
	
	public static void main(String[] args){
		Endpoint.publish("http://127.0.0.1:8080/ss", new SoapServerImpl());
	}

}
