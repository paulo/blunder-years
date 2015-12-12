package bookstore;

import java.util.ArrayList;
import java.util.List;

import javax.ejb.Stateful;
import javax.persistence.EntityManager;
import javax.persistence.PersistenceContext;

/**
 * Session Bean implementation class Cart
 */
@Stateful
public class Cart implements CartRemote {
	
	@PersistenceContext
	private EntityManager em;

	private List<Integer> books;
	
    public Cart() {
    	books = new ArrayList<>(); 
    }

	@Override
	public void add(int isbn) {
		books.add(isbn);
	}

	@Override
	public boolean buy() {
		for(int i: books) {
			Book b = em.find(Book.class, i);
			b.setStock(b.getStock()-1);
		}
		return true;
	}
}
