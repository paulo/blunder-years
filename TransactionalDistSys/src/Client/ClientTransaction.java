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

    /**
     * Create client transaction instance. The bank server id is represented by the first three
     * digits from the account number. The account within that bank is represented by the remaining digits.
     * @param sourceAccount
     * @param targetAccount
     * @param ammount 
     */
    ClientTransaction(String sourceAccount, String targetAccount, int ammount) {
        this.target_account_nmr = targetAccount.substring(3);
        this.source_account_nmr = sourceAccount.substring(3);
        this.target_bank_id = targetAccount.substring(0, 2);
        this.source_bank_id = sourceAccount.substring(0, 2);
        this.ammount = ammount;
    }

    ClientTransaction() {
        this.target_account_nmr = null;
        this.source_account_nmr = null;
        this.target_bank_id = null;
        this.source_bank_id = null;
        this.ammount = 0;
    }

    
    public void transactionTest() throws IOException, RemoteException, NotBoundException {

        System.out.println("Transaction test has begun");
        String txid = sendBeginMessage();
        if (txid == null) {
            System.out.println("Error registering transaction at Transaction Server");
        } else {
            System.out.println("TXID Recebido: " + txid);
            if (withdrawMoney(txid, "bank10", "0000005", 10)) {
                System.out.println("Primeira chamada RMI com sucesso");
                if (depositMoney(txid, "bank20", "0000005", 10)) {
                    System.out.println("Segunda chamada RMI com sucesso");
                    sendCommitMessage(txid);
                } else {
                    abortTransaction(txid, 1);
                    //rollbackTransaction(txid, "bank20");
                    //abortTransaction(txid);
                    System.out.println("RollBack efetuado");
                }
            } else {
                System.out.println("Chamadas RMI com insucesso");
                abortTransaction(txid, 0);
            }
        }

    }

    /**
     * Issues call to operation deposit on a bank server
     * @param txid Transaction context
     * @param target_bank Bank server id
     * @param target_account Account id to where deposit the money
     * @param amount Amount of money to deposit
     * @return True if operation was successful (client exists), false otherwise
     * @throws RemoteException
     * @throws NotBoundException 
     */
    private boolean depositMoney(String txid, String target_bank, String target_account, int amount) throws RemoteException, NotBoundException {
        BankIf bi = getBankObject(target_bank);

        return bi.deposit(txid, amount, target_account);
    }

    /**
     * Issues call to operation withdrawn on a bank server
     * @param txid Transaction context
     * @param source_bank Bank server id
     * @param source_account Account id from where to withdraw the money
     * @param amount Amount of money to be withdrawn
     * @return True if operation was successful (client exists and has enough money), false otherwise
     * @throws RemoteException
     * @throws NotBoundException 
     */
    private boolean withdrawMoney(String txid, String source_bank, String source_account, int amount) throws RemoteException, NotBoundException {
        BankIf bi = getBankObject(source_bank);

        return bi.withdraw(txid, amount, source_account);
    }

    /**
     * Send begin transaction message to the transactional using RMI
     * @return The transaction context id
     * @throws RemoteException
     * @throws NotBoundException 
     */
    private String sendBeginMessage() throws RemoteException, NotBoundException {
        TransactionControlIf tc = getTransactionObject();

        return tc.beginTransaction();
    }

    /**
     * Send commit message to the transactional server using RMI
     * @param txid Transaction context
     * @throws RemoteException
     * @throws NotBoundException 
     */
    private void sendCommitMessage(String txid) throws RemoteException, NotBoundException {
        TransactionControlIf tc = getTransactionObject();

        tc.commitTransaction(txid);
    }

    /**
     * Retrieve RMI interface from the transactional server
     * @return Transactional server interface for transaction control (commit, abort, rollback)
     * @throws RemoteException
     * @throws NotBoundException 
     */
    private TransactionControlIf getTransactionObject() throws RemoteException, NotBoundException {
        Registry registry = LocateRegistry.getRegistry(3333);
        return (TransactionControlIf) registry.lookup("transactionManager");
    }

    /**
     * Retrieve RMI interface from the bank server
     * @param bank
     * @return Bank server interface for operation control (withdraw, deposit)
     * @throws RemoteException
     * @throws NotBoundException 
     */
    private BankIf getBankObject(String bank) throws RemoteException, NotBoundException {
        Registry registry = LocateRegistry.getRegistry(3333);
        return (BankIf) registry.lookup(bank);
    }


    /**
     * Calls for transaction abort on the transactional server.
     * @param txid
     * @param resource_type Value 1 if the it is to rollback on the withdrawn bank, 2 for rollback on 
     * both servers (only needed at TransactionManager) and 0 to only remove transaction context from transactional server
     * @throws RemoteException
     * @throws NotBoundException 
     */
    private void abortTransaction(String txid, int resource_type) throws RemoteException, NotBoundException {
        TransactionControlIf tc = getTransactionObject();

        tc.abortTransaction(txid, resource_type);
    }

    /**
     * Controls transaction logic by issuing calls to the bank operations and transaction control operations
     * @param txid Transaction context id
     * @throws RemoteException
     * @throws NotBoundException 
     */
    void makeOperations(String txid) throws RemoteException, NotBoundException {

        if (withdrawMoney(txid, source_bank_id, source_account_nmr, ammount)) {
            System.out.println("Withdraw call successful.");
            if (depositMoney(txid, target_bank_id, target_account_nmr, ammount)) {
                System.out.println("Deposit call successful.");
                sendCommitMessage(txid);
            } else {
                abortTransaction(txid, 1);
                System.out.println("Transaction rollback.");
            }
        } else {
            System.out.println("Transaction unsuccessful.");
            abortTransaction(txid, 0);
        }
    }

    /**
     * Begins transaction logic by issuing a begin message to the transaction server and invoking
     * the make operations method
     * @throws RemoteException
     * @throws NotBoundException 
     */
    void beginTransaction() throws RemoteException, NotBoundException {
        String txid = sendBeginMessage();
        if (txid == null) {
            System.out.println("Error registering transaction at Transaction Server");
        } else {
            System.out.println("TXID: " + txid);
            makeOperations(txid);
        }
    }
}
