package bookstore;

import java.rmi.Remote;
import java.rmi.RemoteException;

public interface CartIf extends Remote {

	void add(BookIf b) throws RemoteException;

	boolean buy() throws RemoteException;

}