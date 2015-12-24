/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package BankServer;

import Utils.Normalize;
import java.rmi.RemoteException;
import java.util.HashMap;

/**
 *
 * @author SimaoDias
 */
public class Bank implements BankIf{

    String id;
    HashMap<String, Account> accounts;
    
    public Bank(String id) throws RemoteException{
        this.id = id;
        this.accounts = new HashMap<String, Account>(){};
    }
    
    @Override
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
    }
    
}
