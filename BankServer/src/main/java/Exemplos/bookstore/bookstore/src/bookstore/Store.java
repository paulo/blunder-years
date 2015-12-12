package bookstore;

public class Store implements StoreIf {
	private BookIf[] books;

	public Store(BookIf...books) {
		this.books = books;
	}
	
	public BookIf find(String author) {
		for(BookIf b: books)
			if (b.getAuthor().equals(author))
				return b;
		
		return null;
	}
}
