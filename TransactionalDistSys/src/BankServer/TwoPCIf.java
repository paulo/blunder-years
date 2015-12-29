package BankServer;

import java.rmi.RemoteException;

public interface TwoPCIf {
    public boolean prepare(String Txid) throws RemoteException;
    public boolean commit(String Txid) throws RemoteException;
    public boolean rollbackPhase1(String Txid) throws RemoteException;
    public boolean rollbackPhase2(String Txid) throws RemoteException;
}
