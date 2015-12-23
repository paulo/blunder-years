package Model;

import java.rmi.Remote;
import java.rmi.RemoteException;

public interface BankIf extends Remote{
    public AccountIf getAccount(int number) throws RemoteException;
    public TransactionIf makeTransaction() throws RemoteException;
    public AccountIf openAccount(int amount) throws RemoteException;
}
