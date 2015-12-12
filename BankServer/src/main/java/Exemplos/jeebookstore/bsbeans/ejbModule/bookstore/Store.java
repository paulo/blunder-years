package bookstore;

import javax.ejb.Stateless;
import javax.persistence.EntityManager;
import javax.persistence.PersistenceContext;

/**
 * Session Bean implementation class Store
 */
@Stateless
public class Store implements StoreRemote {
	@PersistenceContext
	private EntityManager em;

	@Override
	public int find(String title) {
		Book b = em.find(Book.class, 1);
		return b.getIsbn();
	}

}
