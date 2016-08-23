package TransactionServer;

import BankServer.TXid;
import java.rmi.Remote;
import java.rmi.RemoteException;

public interface TransactionControlIf extends Remote{
    public TXid beginTransaction() throws RemoteException;
    public void commitTransaction(TXid Txid) throws RemoteException;
    public void abortTransaction(TXid Txid) throws RemoteException;
}