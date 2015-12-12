import javax.ejb.EJB;

import bookstore.CartRemote;
import bookstore.StoreRemote;

public class Main {
	@EJB
	private static StoreRemote store;

	@EJB
	private static CartRemote cart;
	
	public static void main(String[] args) {
		System.out.println("r="+store.find("um nome"));
		
		cart.add(1);
		System.out.println("r="+cart.buy());
		
	}
}