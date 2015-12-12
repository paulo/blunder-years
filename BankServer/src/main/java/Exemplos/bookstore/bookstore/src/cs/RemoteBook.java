package cs;

import java.net.InetAddress;

import bookstore.BookIf;

public class RemoteBook extends GenerciStub implements BookIf {
	RemoteBook(InetAddress host, int port, int isbn) {
		super(host, port, isbn);
	}

	@Override
	public int getStock() {
		// TODO Auto-generated method stub
		return 0;
	}

	@Override
	public void setStock(int stock) {

		/*
		 
		 message SetStock {
		 	int32 isbn;
		 
		 	int32 stock;
		 }
		 
		 */
	}

	@Override
	public int getIsbn() {
		// TODO Auto-generated method stub
		return 0;
	}

	@Override
	public String getTitle() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public String getAuthor() {
		// TODO Auto-generated method stub
		return null;
	}

}
