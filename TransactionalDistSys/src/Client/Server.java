package Client;

import Utils.Normalize;
import BankServer.BankServer;
import java.rmi.Naming;

public class Server {

    public static void main(String[] args) throws Exception{
        BankServer bank = new BankServer(Normalize.number(3, 1));
        
       /* AccountIf acc0 = bank.openAccount(0);
        AccountIf acc1 = bank.openAccount(1000);
        AccountIf acc2 = bank.openAccount(2000);
        
        System.out.println("Accounts opened:");
        System.out.println(acc0.getId());
        System.out.println(acc1.getId());
        System.out.println(acc2.getId());
*/
        //Naming.rebind("firstbank", bank);  
    }
    
}
