package bookstore;

import java.rmi.RemoteException;
import java.rmi.server.UnicastRemoteObject;

public class Book extends UnicastRemoteObject implements BookIf {
	private int isbn;
	private String title, author;
	private int stock;
	
	public Book(int isbn, String title, String author, int stock) throws RemoteException {
		super();
		this.isbn = isbn;
		this.title = title;
		this.author = author;
		this.stock = stock;
	}

	/* (non-Javadoc)
	 * @see bookstore.BookIf#getStock()
	 */
	@Override
	public int getStock() {
		return stock;
	}

	/* (non-Javadoc)
	 * @see bookstore.BookIf#setStock(int)
	 */
	@Override
	public void setStock(int stock) {
		this.stock = stock;
	}

	/* (non-Javadoc)
	 * @see bookstore.BookIf#getIsbn()
	 */
	@Override
	public int getIsbn() {
		return isbn;
	}

	/* (non-Javadoc)
	 * @see bookstore.BookIf#getTitle()
	 */
	@Override
	public String getTitle() {
		return title;
	}

	/* (non-Javadoc)
	 * @see bookstore.BookIf#getAuthor()
	 */
	@Override
	public String getAuthor() {
		return author;
	}
}
