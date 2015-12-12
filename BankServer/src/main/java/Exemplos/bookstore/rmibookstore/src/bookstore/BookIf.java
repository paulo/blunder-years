package bookstore;

import java.rmi.Remote;
import java.rmi.RemoteException;

public interface BookIf extends Remote {

	int getStock() throws RemoteException;

	void setStock(int stock) throws RemoteException;

	int getIsbn() throws RemoteException;

	String getTitle() throws RemoteException;

	String getAuthor() throws RemoteException;

}