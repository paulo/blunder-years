package BankServer;

import java.rmi.RemoteException;

//isto nao se pode chamar transaction... 
public class Transaction implements TransactionIf{

    Transaction(){}
    
    @Override
    public void deposit(int amount, AccountIf account) throws RemoteException {
        int currentBalance = account.getBalance();
        account.setBalance(currentBalance + amount);
    }

    @Override
    public boolean withdraw(int amount, AccountIf account) throws RemoteException {
        int currentBalance = account.getBalance();
        if(currentBalance - amount > 0){
            account.setBalance(currentBalance - amount);
            return true;
        }
        else{
            return false;
        }
    }
    
}
