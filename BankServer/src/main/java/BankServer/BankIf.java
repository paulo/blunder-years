package BankServer;

import java.rmi.Remote;
import java.rmi.RemoteException;

public interface BankIf extends Remote{
    public AccountIf findAccount(int account_number) throws RemoteException;
    
    public AccountIf Transfer(int accountnumber_send, int accountnumber_receive) throws RemoteException;
    
    public AccountIf Deposit(int accountnumber, int quantity) throws RemoteException;
    
    public AccountIf Lifting(int accountnumber, int quantity) throws RemoteException;
    
}
