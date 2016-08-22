package Clients;

import java.io.BufferedReader;
import java.io.IOException;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * @author Grupo 30
 */


public class ServerHandler implements Runnable{
    
    private BufferedReader in;
    
    public ServerHandler(BufferedReader input){
        in = input;
    }
    
    
    @Override
    public void run() {
        String res;
        boolean connection = true;
        try {
            while(connection && (res = in.readLine()) != null){
                if(res.equals("Sessão terminada")) connection = false;
                System.out.println(res);
            }
        } catch (IOException ex) {
            Logger.getLogger(ServerHandler.class.getName()).log(Level.SEVERE, null, ex);
        }

    }

}
