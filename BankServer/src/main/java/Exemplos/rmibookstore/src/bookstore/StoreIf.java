package bookstore;

import java.rmi.Remote;
import java.rmi.RemoteException;

public interface StoreIf extends Remote {
	public BookIf find(String author) throws RemoteException;
	
	// Factory
	public CartIf mkCart() throws RemoteException;
}