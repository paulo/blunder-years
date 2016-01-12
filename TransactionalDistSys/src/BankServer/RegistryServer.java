package BankServer;

import java.rmi.RemoteException;
import java.rmi.registry.LocateRegistry;
import java.rmi.registry.Registry;
import java.util.logging.Level;
import java.util.logging.Logger;

public class RegistryServer {
    
    public static void main(String[] args){
        try {
            Registry registry = LocateRegistry.createRegistry(3333);
            while(true);
        } catch (RemoteException ex) {
            Logger.getLogger(RegistryServer.class.getName()).log(Level.SEVERE, null, ex);
        }
    }
}
