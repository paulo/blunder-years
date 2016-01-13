package Client;

import BankServer.BankIf;
import TransactionServer.TransactionControlIf;
import java.io.IOException;
import java.rmi.NotBoundException;
import java.rmi.RemoteException;
import java.rmi.registry.LocateRegistry;
import java.rmi.registry.Registry;

class ClientTransaction {

    private final String target_account_nmr, source_account_nmr, target_bank_id, source_bank_id;
    private final int ammount;

    ClientTransaction(String sourceAccount, String targetAccount, int ammount) {
        this.target_account_nmr = targetAccount.substring(3);
        this.source_account_nmr = sourceAccount.substring(3);
        this.target_bank_id = targetAccount.substring(0, 2);
        this.source_bank_id = sourceAccount.substring(0, 2);
        this.ammount = ammount;
    }

    //apagar depois
    ClientTransaction() {
        throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
    }

    public void transactionTest() throws IOException, RemoteException, NotBoundException {

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
                abortTransaction(txid);
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

    private void rollbackTransaction(String txid, String target) throws RemoteException, NotBoundException {
        BankIf bi = getBankObject(target);

        bi.rollback(txid);
    }

    private void abortTransaction(String txid) throws RemoteException, NotBoundException {
        TransactionControlIf tc = getTransactionObject();

        tc.abortTransaction(txid);
    }

    
    void makeOperations(String txid) throws RemoteException, NotBoundException {

        if (withrawMoney(txid, source_bank_id, source_account_nmr, ammount)) {
            System.out.println("Primeira chamada RMI com sucesso");
            if (depositMoney(txid, target_bank_id, target_account_nmr, ammount)) {
                System.out.println("Segunda chamada RMI com sucesso");
                sendCommitMessage(txid);
            } else {
                rollbackTransaction(txid, source_bank_id);
                abortTransaction(txid);
                System.out.println("RollBack efetuado");
            }
        } else {
            System.out.println("Chamadas RMI com insucesso");
            abortTransaction(txid);
        }
    }

    void beginTransaction() throws RemoteException, NotBoundException {
        String txid = sendBeginMessage();
        if (txid == null) {
            System.out.println("Error registering transaction at Transaction Server");
        } else {
            System.out.println("TXID Recebido: " + txid);
            makeOperations(txid);
        }
    }
}
