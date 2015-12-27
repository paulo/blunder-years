package BankServer;

import Utils.Normalize;
import java.rmi.RemoteException;


public class Trash {
    /*   @Override
    public AccountIf getAccount(int number) throws RemoteException {
        return accounts.get(number);
    }

    @Override
    public TransactionIf makeTransaction() throws RemoteException {
        return new Transaction();
    }

    @Override
    public AccountIf openAccount(int balance) throws RemoteException {
        String number = getIdentification();
        AccountIf newAccount = new Account(number, balance);
        this.accounts.put(number, (Account) newAccount);
        return newAccount;
    }
    
    private String getIdentification(){
        int internalAccountId = accounts.size();
        String normalizedId = Normalize.number(5, internalAccountId);
        return this.id+normalizedId;
    }*/
}
