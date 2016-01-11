package BankServer;

import java.rmi.Remote;
import java.rmi.RemoteException;

public interface TwoPCIf extends Remote {
    public boolean prepare(String Txid) throws RemoteException;
    public void commit(String Txid) throws RemoteException;
    public void rollback(String Txid) throws RemoteException;
}
//mudar para usar tudo RMI
//uma connection tem de ser criada à entrada do método, nunca pode ser partilhada por duas threads
//nao é preciso guardar a info do cliente, nem responder com sucesso ou nao