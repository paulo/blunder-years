package Server;

import Factory.*;
import Users.UserBase;
import java.io.IOException;
import java.net.ServerSocket;
import java.net.Socket;
import java.net.SocketException;

/**
 * @author Grupo 30
 */


public class ServerEngine {
    
    
    private static ServerSocket sckt;
    private static WareHouse warehouse;
    private static UserBase userbase;
    
    public static void main(String[] args) throws IOException {
       
        try {
            sckt = new ServerSocket(4321);
            userbase = new UserBase();
            warehouse = new WareHouse();
            

            //inicializa servidor local
            Thread localClient = new Thread(new ClientHandler(warehouse, userbase, sckt));
            localClient.start();
            
            //fica a espera de conexoes remotas
            while(true){ 
                Socket sock = sckt.accept();   
                Thread remoteClient = new Thread(new ClientHandler(sock, warehouse, userbase));          
                remoteClient.start();
            }
        } catch (SocketException s) {
              System.out.println("Servidor desligado com sucesso.");
        } 
        catch (IOException e) {
            System.out.println(e.getMessage());
        } 
        finally {
            if (sckt != null) {
                try {
                    sckt.close();
                } catch (IOException e) {
                    System.out.println(e.getMessage());
                }
            }
        }
    }
}
    