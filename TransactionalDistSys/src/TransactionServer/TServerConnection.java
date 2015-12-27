package TransactionServer;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.Socket;

//as mensagens vao ter de ser alteradas
public class TServerConnection {

    private Socket s;
    private int port;
    private InputStream in;
    private OutputStream out; 
    
    TServerConnection(int port) {
        this.port = port;
    }

    public void initConnection() throws IOException {
        s = new Socket("localhost", port);
        in = s.getInputStream();
        out = s.getOutputStream();
        s.
    }

    String sendBeginMessage() {
        out.
        //socket.send("begin");
    }
}
