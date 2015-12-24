package BankServer;

import java.rmi.RemoteException;
import java.rmi.server.UnicastRemoteObject;

public class Client extends UnicastRemoteObject implements AccountIf {

    private String name;
    private int account_balance, account_number;

    public Client(String name, int account_number, int account_balance) throws RemoteException {
        super();
        this.name = name;
        this.account_number = account_number;
        this.account_balance = account_balance;
    }

    public String getName() {
        return name;
    }

    public int getAccount_balance() {
        return account_balance;
    }

    public void setAccount_balance(int account_balance) {
        this.account_balance = account_balance;
    }

    public int getAccount_number() {
        return account_number;
    }

}
