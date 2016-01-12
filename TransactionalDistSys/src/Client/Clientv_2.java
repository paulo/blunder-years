package Client;

import BankServer.BankIf;
import TransactionServer.TransactionControlIf;
import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.IOException;
import java.net.Socket;
import java.rmi.NotBoundException;
import java.rmi.RemoteException;
import java.rmi.registry.LocateRegistry;
import java.rmi.registry.Registry;

public class Clientv_2 {

    private final int t_server_port;
    Socket s;
    BufferedWriter writer;
    BufferedReader reader;

    Clientv_2(int server_port) throws IOException {
        t_server_port = server_port;
        s = new Socket("localhost", 55555);
    }

    private void transactionTest() throws IOException, RemoteException, NotBoundException {

        System.out.println("Transaction test has begun");
        String txid = sendBeginMessage();
        if (txid == null) {
            System.out.println("Error registering transaction at Transaction Server");
        } else {
            System.out.println("TXID Recebido: " + txid);
            if (withrawMoney(txid, "bank10", "0000005", 10)) {
                System.out.println("Primeira chamada RMI com sucesso");
                if (depositMoney(txid, "bank20", "0000005", 10)) {
                    System.out.println("Segunda chamada RMI com sucesso");
                } else {
                    rollbackTransaction(txid, "bank20");
                    System.out.println("RollBack efetuado");
                }
                sendCommitMessage(txid);
            } else {
                System.out.println("Chamadas RMI com insucesso");
                sendCancelMessage(txid);
            }
        }

    }

    private boolean depositMoney(String txid, String target_bank, String target_account, int amount) throws RemoteException, NotBoundException {
        BankIf bi = getBankObject(target_bank);

        return bi.deposit(txid, amount, target_account);
    }

    private boolean withrawMoney(String txid, String source_bank, String source_account, int amount) throws RemoteException, NotBoundException {
        BankIf bi = getBankObject(source_bank);

        return bi.withdraw(txid, amount, source_account);
    }

    private String sendBeginMessage() throws RemoteException, NotBoundException {
        TransactionControlIf tc = getTransactionObject();

        return tc.beginTransaction();
    }

    private void sendCommitMessage(String txid) throws RemoteException, NotBoundException {
        TransactionControlIf tc = getTransactionObject();

        tc.commitTransaction(txid);
    }

    private TransactionControlIf getTransactionObject() throws RemoteException, NotBoundException {
        Registry registry = LocateRegistry.getRegistry(3333);
        return (TransactionControlIf) registry.lookup("transactionManager");
    }

    private BankIf getBankObject(String bank) throws RemoteException, NotBoundException {
        Registry registry = LocateRegistry.getRegistry(3333);
        return (BankIf) registry.lookup(bank);
    }

    public static void main(String[] args) throws Exception {
        Clientv_2 c = new Clientv_2(55555);

        c.transactionTest();
    }

    private void sendCancelMessage(String txid) {
        throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
    }

    private void rollbackTransaction(String txid, String target) throws RemoteException, NotBoundException {
        BankIf bi = getBankObject(target);

        bi.rollback(txid);
    }
    
    private void abortTransaction(String txid) {
        TransactionControlIf tc = getTransactionObject();
        
        
    }

}
