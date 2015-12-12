package bookstore;

import javax.ejb.Remote;

@Remote
public interface CartRemote {
	public void add(int isbn);
	public boolean buy();
}
