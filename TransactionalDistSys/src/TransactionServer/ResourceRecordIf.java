package TransactionServer;

import java.rmi.Remote;
import java.rmi.RemoteException;

public interface ResourceRecordIf extends Remote{
    /**
     * Register resource at Transactional Server.
     * @param Txid Transaction context id
     * @param type Value 1 if resource/bank is the one where the withdraw occurs, 2 otherwise
     * @param resource_id Resource id
     * @throws RemoteException 
     */
    public void registerResource(String Txid, int type, String resource_id) throws RemoteException;
}
