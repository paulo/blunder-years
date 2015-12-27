package BankServer;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.rmi.Naming;
import java.rmi.RemoteException;
import java.rmi.registry.LocateRegistry;
import java.rmi.registry.Registry;
import java.sql.SQLException;

public class BankServer {

    static String bank_id;
    static BankDataOperator bdo;
    
    public BankServer(String id) throws RemoteException{
        this.bank_id = id;
        this.bdo = new BankDataOperator(id);
    }
    
    
    public static String setBankInfo() throws IOException{
        BufferedReader userIn = new BufferedReader(new InputStreamReader(System.in));   
        System.out.print("Set bank id: ");
 
        return "bank"+userIn.readLine();
    }
    
    
    public static void main(String[] args) throws RemoteException, IOException, SQLException{
                
        BankServer bs = new BankServer(setBankInfo());
        
        bdo.initDBConnection();
        
        Transaction t = new Transaction(bdo);
        
        try {
            //Registry registry = LocateRegistry.getRegistry();
            Registry registry = LocateRegistry.createRegistry(3333);
            registry.rebind(bank_id, t);
            System.out.println("Transaction bound");
        } catch (Exception e) {
            System.err.println("ComputeEngine exception:");
            e.printStackTrace();
        }
        
        System.out.println(bank_id);
        
        while(true);
    }
}
