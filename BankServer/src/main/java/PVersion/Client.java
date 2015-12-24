package PVersion;

import PVersion.Message.Transaction;
import org.zeromq.ZMQ;

public class Client {

    private static ZMQ.Context context = ZMQ.context(1);
    private static ZMQ.Socket socket = context.socket(ZMQ.REQ);

    private static void initConnectionToBankServer() {
        socket.connect("tcp://localhost:55555");
        
        /*ZMQ.Context context = ZMQ.context(1);
         ZMQ.Socket socket = context.socket(ZMQ.REQ);
         socket.connect("tcp://localhost:55555");
        
         while (true) {
         String s = System.console().readLine();
         if (s == null) {
         break;
         }
         socket.send(s);
         byte[] b = socket.recv();
         System.out.println(new String(b));
         }
        
         socket.close();
         context.term();*/
    }

    public static void main(String[] args) throws Exception {
        int to_send = 10;

        initConnectionToBankServer();
       
        Transaction t = Transaction.newBuilder().setId(to_send).build();
        
        socketSend(t);
        
        byte[] b = socket.recv();
        
        System.out.println(new String(b));
        
    }

    private static void socketSend(Transaction t) {
        
        
        socket.send(t.toByteArray(), 0);
    }
}
