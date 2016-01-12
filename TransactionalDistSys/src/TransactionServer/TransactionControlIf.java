package TransactionServer;

import java.rmi.Remote;
import java.rmi.RemoteException;

public interface TransactionControlIf extends Remote{
    public String beginTransaction() throws RemoteException;
    public void commitTransaction(String Txid) throws RemoteException;
    public void abortTransaction() throws RemoteException;
}