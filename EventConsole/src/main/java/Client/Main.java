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
        
        ZMQ.Context ctx = ZMQ.context(1);
        ZMQ.Socket socket = ctx.socket(ZMQ.SUB); 
        
        InputReader commands = new InputReader(socket);
        commands.start();

        socket.connect("tcp://localhost:"+12346);  
        startListening(socket);
        socket.close();
        ctx.term();
    } 

}
