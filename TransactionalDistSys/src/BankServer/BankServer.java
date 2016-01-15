package BankServer;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.rmi.RemoteException;
import java.rmi.registry.LocateRegistry;
import java.rmi.registry.Registry;

public class BankServer {

    static String bank_id;
    static BankDataOperator bdo;

    public BankServer(String id) throws RemoteException {
        BankServer.bank_id = id;
        BankServer.bdo = new BankDataOperator(id);
    }

    /**
     * User sets the bank id
     * @return Bank Id
     * @throws IOException 
     */
    public static String setBankInfo() throws IOException {
        BufferedReader userIn = new BufferedReader(new InputStreamReader(System.in));
        System.out.print("Set bank id: ");

        return "bank" + userIn.readLine();
    }

    /**
     * Export bank object through RMI
     * @param t Bank object to export
     * @throws RemoteException 
     */
    public void exportBankObject(Bank t) throws RemoteException {
        try {
            Registry registry = LocateRegistry.getRegistry(3333);

            registry.rebind(bank_id, t);
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    public static void main(String[] args) throws Exception {

        BankServer bs = new BankServer(setBankInfo());
        bdo.initDB();
        
        Bank bank_object = new Bank(bdo, bank_id);
        //bank_object.recover();
        bs.exportBankObject(bank_object);

        System.out.println("Created New Bank: "+bank_id);

        while (true);
    }
}
