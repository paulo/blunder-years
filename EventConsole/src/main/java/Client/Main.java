/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package Client;
import java.io.IOException;
import org.zeromq.ZMQ;
/**
 *
 * @author SimaoDias
 */
public class Main{
    


    private static void startListening(ZMQ.Socket socket) {
        while(true){
                byte[] b = socket.recv();
                System.out.print(new String(b));
        }
    }
    
    public static void main(String[] args) throws IOException {
        int default_port = 12346, port;
        ZMQ.Context ctx = ZMQ.context(1);
        ZMQ.Socket socket = ctx.socket(ZMQ.SUB); 
        
        if (args.length > 0 && args[0] != null) {
            for (String arg : args) {
                System.out.println("Parsing port number: " + arg);
                try {
                    port = Integer.parseInt(arg);
                    System.out.println("Connecting to server on port number: " + port);
                    InputReader commands = new InputReader(socket);
                    commands.start();

                    socket.connect("tcp://localhost:"+port); 
                } catch (NumberFormatException e) {
                    System.out.println("Port " + arg + " is not valid.");
                }
            }
        } else {
            InputReader commands = new InputReader(socket);
            commands.start();

            socket.connect("tcp://localhost:"+default_port);  
        }
        startListening(socket);
        socket.close();
        ctx.term();
    } 

}



        
        