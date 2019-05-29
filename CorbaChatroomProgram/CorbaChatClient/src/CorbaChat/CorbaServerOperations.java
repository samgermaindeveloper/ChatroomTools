package CorbaChat;


/**
* CorbaChat/CorbaServerOperations.java .
* Generated by the IDL-to-Java compiler (portable), version "3.2"
* from CorbaServer.idl
* Wednesday, December 19, 2018 6:46:32 AM CST
*/

public interface CorbaServerOperations 
{
  void create (String room, String user);
  String list ();
  String join (String room, String user);
  String leave (String room, String user);
  String writeMessage (String msg, String room, String user);
} // interface CorbaServerOperations