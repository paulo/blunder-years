package rmi;

import java.rmi.Naming;

import bookstore.Book;
import bookstore.Store;

public class Server {
	public static void main(String[] args) throws Exception {
		Book b1 = new Book(1000, "Livro Mil", "Autor Mil", 10);
		Book b2 = new Book(2000, "Livro Dois Mil", "Autor Dois Mil", 5);
		
		Store s = new Store(b1, b2);
		
		Naming.rebind("bookstore", s);
	}
}
