package Model;

import java.rmi.RemoteException;
import java.rmi.server.UnicastRemoteObject;

public class Account extends UnicastRemoteObject implements AccountIf {
    
    private String id;
    private int balance;
    
    public Account(String number, int balance) throws RemoteException{
        super();
        this.id = number;
        this.balance = balance;
    }

    @Override
    public String getId() throws RemoteException {
        return this.id;
    }
    
    @Override
    public int getBalance() throws RemoteException {
        return this.balance;
    }  

    @Override
    public void setBalance(int amount) throws RemoteException {
        this.balance = amount;
    }
}
