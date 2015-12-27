package BankServer;

import java.rmi.RemoteException;
import java.rmi.server.UnicastRemoteObject;

public class Transaction extends UnicastRemoteObject implements OperationIf {

    BankDataOperator bdo;
    
    Transaction(BankDataOperator bank_operator) throws RemoteException{
        this.bdo = bank_operator;
    }

    //vai fazer start da operação
    //
    //
    //
    //
    @Override
    public boolean deposit(String Txid, int amount, String account_nmr) throws RemoteException {
        //int currentBalance = account.getBalance();
        //account.setBalance(currentBalance + amount);
        System.out.println("New deposit");
        return false;
    }

    //vai fazer start da operação
    @Override
    public boolean withdraw(String Txid, int amount, String account_nmr) throws RemoteException {
        /*int currentBalance = account.getBalance();
        if(currentBalance - amount > 0){
            account.setBalance(currentBalance - amount);
            return true;
        }
        else{
            return false;
        }*/
        System.out.println("New withdraw");
        return false;
    }
    
}
