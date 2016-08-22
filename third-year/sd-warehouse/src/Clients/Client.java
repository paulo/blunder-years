package Clients;

import java.io.*;
import java.net.Socket;

/**
 * @author Grupo 30
 */


public class Client {
    
public static void main(String[] args) throws IOException, InterruptedException {
        
        Socket client;
        BufferedReader in;
        BufferedWriter out;
        boolean connection = true;
        
        try {         
            client = new Socket("localhost",4321);// talvez pedir ao user o numero da porta a qual se conetar
            in = new BufferedReader(new InputStreamReader(client.getInputStream()));//stream de entrada
            out = new BufferedWriter(new OutputStreamWriter(client.getOutputStream()));//stream de saida
            BufferedReader userInput = new BufferedReader(new InputStreamReader(System.in));//stream input utilizador
            String res;
             
            /*
             *   envia os comandos do system.in do cliente remoto para o servidor
             *   imprime no system.out do cliente remoto a resposta do servidor
             */
            
            /*Thread que recebe respostas do servidor*/
            ServerHandler sh = new ServerHandler(in); 
            Thread tsHandler = new Thread(sh);
            
            tsHandler.start();
            
            System.out.println("ComandoRemoto> ");
            while(connection && (res=userInput.readLine())!=null){
                if(res.toLowerCase().equals("sair")) connection = false;
                    out.write(res+"\n");
                    out.flush();
                    if(connection) System.out.println("ComandoRemoto> ");
            }
            
            tsHandler.join();
            client.shutdownInput();
            client.shutdownOutput();
            
            client.close();
            
        } catch (IOException e){
            System.out.println(e.getMessage());
        }
    }  
}
