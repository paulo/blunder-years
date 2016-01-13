package TransactionServer;

import java.rmi.Remote;
import java.rmi.RemoteException;

public interface ResourceRecordIf extends Remote{
    public void registerResource(String Txid, int type, String resource_id) throws RemoteException;
}
