package BankServer;

import java.rmi.RemoteException;
import java.rmi.registry.LocateRegistry;
import java.rmi.registry.Registry;
import java.util.logging.Level;
import java.util.logging.Logger;

public class RegistryServer {
    
    public static void main(String[] args){
        try {
                       // System.setProperty("java.rmi.server.hostname","192.168.1.91");
            Registry registry = LocateRegistry.createRegistry(3333);
            while(true);
        } catch (RemoteException ex) {
            Logger.getLogger(RegistryServer.class.getName()).log(Level.SEVERE, null, ex);
        }
    }
}
