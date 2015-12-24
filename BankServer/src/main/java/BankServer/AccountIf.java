package BankServer;

import java.rmi.Remote;
import java.rmi.RemoteException;

public interface AccountIf extends Remote {
    
    String getName() throws RemoteException;
    
    int getAccount_number() throws RemoteException;
    
    int getAccount_balance() throws RemoteException;
    
    void setAccount_balance(int account_balance) throws RemoteException;
      
}
