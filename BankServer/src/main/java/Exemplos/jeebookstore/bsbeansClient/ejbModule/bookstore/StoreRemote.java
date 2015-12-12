package bookstore;

import javax.ejb.Remote;

@Remote
public interface StoreRemote {
	public int find(String title);
}
