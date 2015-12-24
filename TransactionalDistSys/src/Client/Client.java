package Client;

import BankServer.AccountIf;
import BankServer.BankIf;
import BankServer.TransactionIf;
import java.rmi.Naming;

public class Client {
    public static void main(String[] args) throws Exception {
        BankIf bank = (BankIf) Naming.lookup("//localhost/firstbank");

        AccountIf acc = bank.getAccount(00100002);
        System.out.println("Current Balance: " + acc.getBalance());
        
        TransactionIf trans = bank.makeTransaction();
        trans.deposit(100, acc);
        System.out.println("We made a deposit of 100");
        System.out.println("Current Balance: " + acc.getBalance());
    }
    
}
