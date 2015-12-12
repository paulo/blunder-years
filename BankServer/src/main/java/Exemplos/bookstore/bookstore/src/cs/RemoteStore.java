package cs;

import java.io.IOException;
import java.net.Socket;
import java.net.UnknownHostException;

import bookstore.Book;
import bookstore.BookIf;
import bookstore.StoreIf;
import bookstoreproto.Bookstore;
import bookstoreproto.Bookstore.Reply;
import bookstoreproto.Bookstore.Request;

public class RemoteStore implements StoreIf {
	RemoteStore(String host, int port) {
		
	}

	@Override
	public BookIf find(String author) throws UnknownHostException, IOException {
			Request r = Bookstore.Request.newBuilder().setFind("Autor Mil").build();
			
			Socket s = new Socket(host, port);			
			r.writeDelimitedTo(s.getOutputStream());
			Reply y = Bookstore.Reply.parseDelimitedFrom(s.getInputStream());
			
			BookIf b = new RemoteBook(y.getFind());
			
			return new Book(b.getIsbn(), b.getTitle(), b.getAuthor(), 0);
	}

}
