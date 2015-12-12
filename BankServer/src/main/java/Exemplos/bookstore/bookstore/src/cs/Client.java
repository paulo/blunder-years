package cs;

import java.io.OutputStream;
import java.net.Socket;

import bookstore.BookIf;
import bookstore.StoreIf;

public class Client {

	public static void main(String[] args) throws Exception {
		StoreIf s = new RemoteStore();
		
		BookIf b = s.find("Autor Mil"); 
		
		System.out.println("isbn="+b.getIsbn());
	}
}
