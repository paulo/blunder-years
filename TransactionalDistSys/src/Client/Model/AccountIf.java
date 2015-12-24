package Model;

import java.rmi.Remote;
import java.rmi.RemoteException;

public interface AccountIf extends Remote{
    String getId() throws RemoteException;
    int getBalance() throws RemoteException;
    void setBalance(int amount) throws RemoteException;
}
