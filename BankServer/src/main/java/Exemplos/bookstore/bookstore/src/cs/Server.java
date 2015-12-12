package cs;

import java.net.ServerSocket;
import java.net.Socket;

import bookstore.Book;
import bookstore.BookIf;
import bookstore.Store;
import bookstoreproto.Bookstore.Reply;
import bookstoreproto.Bookstore.Request;

public class Server {
	public static void main(String[] args) throws Exception {
		BookIf b1 = new Book(1000, "Livro Mil", "Autor Mil", 10);
		BookIf b2 = new Book(2000, "Livro Dois Mil", "Autor Dois Mil", 5);
		
		Store store = new Store(b1, b2);
		
		ServerSocket ss = new ServerSocket(12345);
		
		while(true) {
			Socket s = ss.accept();
			
			Request r = Request.parseDelimitedFrom(s.getInputStream());
			
			BookIf b = store.find(r.getFind());
			
			Reply y = Reply.newBuilder().setFind(
					Reply.Book.newBuilder().setAuthor(b.getAuthor())
					.setIsbn(b.getIsbn())
					.setTitle(b.getTitle()).build()).build();
			
			y.writeDelimitedTo(s.getOutputStream());
			
			s.close();
		}
	}
}



