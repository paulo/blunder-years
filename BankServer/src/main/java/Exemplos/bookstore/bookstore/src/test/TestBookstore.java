package test;

import bookstore.Book;
import bookstore.Cart;
import bookstore.Store;

public class TestBookstore {

	public static void main(String[] args) {
		Book b1 = new Book(1000, "Livro Mil", "Autor Mil", 10);
		Book b2 = new Book(2000, "Livro Dois Mil", "Autor Dois Mil", 5);
		
		Store s = new Store(b1, b2);
		
		Book b3 = s.find("Autor Mil");
		
		Cart c = new Cart();
		c.add(b3);
		c.buy();
		
		System.out.println("stock: "+b1.getStock());
	}
}
