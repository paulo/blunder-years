package PVersion;

import org.zeromq.ZMQ;

//meter porta/s a receber como argumento da main
public class BankServer {

    static ZMQ.Context context = ZMQ.context(1);
    static ZMQ.Socket socket = context.socket(ZMQ.REP);
    static ZMQ.Socket socket_to_server = context.socket(ZMQ.REQ);

    private static void initBankServerConnection() {
        socket.bind("tcp://*:55555");
        socket_to_server.connect("tcp://localhost:123456");
    }

    /*private static int receiveFromClient() throws InvalidProtocolBufferException {
        byte b[] = socket.recv();
        //Transaction t = Message.Transaction.parseFrom(b);
        socket.send("Number received on BankServer");
        
        return t.getId();
    }

    private static void sendToTServer(int to_send_to_tserver) {
        //Transaction t = Transaction.newBuilder().setId(to_send_to_tserver).build();
        socket_to_server.send(t.toByteArray(), 0);
    }*/

    public void makestuff() {
        /*ZMQ.Context context = ZMQ.context(1);
         ZMQ.Socket socket = context.socket(ZMQ.REP);
         socket.bind("tcp://*:123456");

         while (true) {
         byte[] b = socket.recv();
         String s = new String(b);
         System.out.println("Received " + s);
         String res = s.toUpperCase();
         socket.send(res);
         }
         //socket.close();
         //context.term();*/
    }

    public static void main(String[] args) throws Exception {

        //implementar isto como rmi
        initBankServerConnection();
       //int to_send_to_tserver = receiveFromClient();

        //System.out.println("Received number:"+to_send_to_tserver);
       // sendToTServer(to_send_to_tserver);
    }

}
