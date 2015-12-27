package BankServer;

import java.rmi.RemoteException;

public class Account {
    
    private final String client_id;
    private int balance;
    
    public Account(String number, int balance) throws RemoteException{
        super();
        this.client_id = number;
        this.balance = balance;
    }

    public String getId() throws RemoteException {
        return this.client_id;
    }
    
    public int getBalance() throws RemoteException {
        return this.balance;
    }  

    public void setBalance(int amount) throws RemoteException {
        this.balance = amount;
    }
}
