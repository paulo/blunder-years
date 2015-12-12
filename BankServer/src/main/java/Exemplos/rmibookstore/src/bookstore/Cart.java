package bookstore;

import java.rmi.RemoteException;
import java.rmi.server.UnicastRemoteObject;
import java.util.ArrayList;
import java.util.List;

public class Cart extends UnicastRemoteObject implements CartIf {
	protected Cart() throws RemoteException {
		super();
	}

	private List<BookIf> books = new ArrayList<>();
	
	@Override
	public void add(BookIf b) throws RemoteException {
		books.add(b);
	}
	
	@Override
	public boolean buy() throws RemoteException {
		for(BookIf b: books)
			if (b.getStock() <= 0)
				return false;
		for(BookIf b: books)
			b.setStock(b.getStock()-1);
		return true;
	}
}
