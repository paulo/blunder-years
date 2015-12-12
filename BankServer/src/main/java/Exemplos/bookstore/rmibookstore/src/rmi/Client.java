package rmi;

import java.rmi.Naming;

import bookstore.Book;
import bookstore.BookIf;
import bookstore.Cart;
import bookstore.CartIf;
import bookstore.Store;
import bookstore.StoreIf;

public class Client {

	public static void main(String[] args) throws Exception {
		StoreIf s = (StoreIf) Naming.lookup("//localhost/bookstore");
		
		BookIf b3 = s.find("Autor Mil");
		
		CartIf c = s.mkCart();
		c.add(b3);
		c.buy();
		
		System.out.println("stock: "+b3.getStock());

	}

}
