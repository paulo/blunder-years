package TransactionServer;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.net.Socket;
import java.util.logging.Level;
import java.util.logging.Logger;


//depois implementar troca de mensagens com protobuf
//http://stackoverflow.com/questions/30564404/how-to-determine-message-type-in-protobuf-so-that-i-can-use-that-type-parsefrom
public class ClientHandler extends Thread {

    TServerLog log;
    Socket c_socket;
    BufferedReader reader;
    BufferedWriter writer;
    TransactionManager t_manager;

    ClientHandler(Socket cli, TransactionManager t_manager) throws IOException {
        c_socket = cli;
        reader = new BufferedReader(new InputStreamReader(c_socket.getInputStream()));
        writer = new BufferedWriter(new OutputStreamWriter(c_socket.getOutputStream()));
        this.t_manager = t_manager;
    }

    @Override
    public void run() {
        String msg = null;

        try {
            while ((msg = reader.readLine()) != null) {
                processLine(msg); 
            }
        } catch (IOException ex) {
            Logger.getLogger(ClientHandler.class.getName()).log(Level.SEVERE, null, ex);
        }

    }
 
    private void processLine(String msg) throws IOException {
        String[] args = msg.split(" ");
        
        switch(args[0]){
            case "begin":
                beginMessage();
                break;
            case "commit":
                commitMessage(args[1]);
                break;
            case "restart": //quando o user se ligar pode enviar esta mensagem para atualizar o seu estado na transação
                break;
        }
        
    }

    private void beginMessage() throws IOException {
        String new_txid = t_manager.createNewTContext(c_socket);
        
        writeToClient(new_txid);
    }
    
    private void commitMessage(String TxId) throws IOException {
        String response = t_manager.commitTransaction(TxId);
        
        writeToClient(response);
    }
    
    private void writeToClient(String msg) throws IOException {
        writer.write(msg);
        writer.flush();
    }

}
