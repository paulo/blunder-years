package bookstore;

import java.rmi.RemoteException;
import java.rmi.server.UnicastRemoteObject;

public class Store extends UnicastRemoteObject implements StoreIf {
	private BookIf[] books;

	public Store(BookIf...books) throws RemoteException {
		this.books = books;
	}
	
	public BookIf find(String author) throws RemoteException {
		for(BookIf b: books)
			if (b.getAuthor().equals(author))
				return b;
		
		return null;
	}

	@Override
	public CartIf mkCart() throws RemoteException {
		return new Cart();
	}
}
