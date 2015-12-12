package bookstore;

import java.util.ArrayList;
import java.util.List;

public class Cart implements CartIf {
	private List<Book> books = new ArrayList<>();
	
	/* (non-Javadoc)
	 * @see bookstore.CartIf#add(bookstore.Book)
	 */
	@Override
	public void add(Book b) {
		books.add(b);
	}
	
	/* (non-Javadoc)
	 * @see bookstore.CartIf#buy()
	 */
	@Override
	public boolean buy() {
		for(BookIf b: books)
			if (b.getStock() <= 0)
				return false;
		for(BookIf b: books)
			b.setStock(b.getStock()-1);
		return true;
	}
}
