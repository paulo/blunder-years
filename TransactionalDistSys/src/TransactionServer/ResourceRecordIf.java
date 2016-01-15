package TransactionServer;

import BankServer.TXid;
import java.rmi.Remote;
import java.rmi.RemoteException;

public interface ResourceRecordIf extends Remote{
    public void registerResource(TXid Txid, int type, String resource_id) throws RemoteException;
}
