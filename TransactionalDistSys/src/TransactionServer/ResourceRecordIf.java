package TransactionServer;

import java.rmi.Remote;
import java.rmi.RemoteException;

public interface ResourceRecordIf extends Remote{
    //type 1 -> source . type 2 -> target
    //meter para dar false se nao der para inserir no banco
    public void registerResource(String Txid, int type, String resource_id) throws RemoteException;
}
