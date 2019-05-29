/**
 * 
 */
package server;

import javax.jws.WebMethod;
import javax.jws.WebService;
import javax.jws.soap.SOAPBinding;

/**
 * @author sjg970
 *
 */
@WebService
@SOAPBinding(style = SOAPBinding.Style.RPC)
public interface SoapServer {

	@WebMethod
	public void create(String room, String user);
	
	@WebMethod
	public String list();
	
	@WebMethod
	public String join(String room, String user);
	
	@WebMethod
	public String leave(String room, String user);

	@WebMethod
	public String writeMessage(String msg, String room, String user);	
	
}
