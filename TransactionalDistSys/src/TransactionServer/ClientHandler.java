package TransactionServer;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.net.Socket;
import java.util.logging.Level;
import java.util.logging.Logger;

public class ClientHandler extends Thread {

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

        switch (args[0]) {
            case "begin":
                System.out.println("2. Begin message received");
                //beginMessage();
                break;
            case "commit":
                System.out.println("5. Commit message received");
                //commitMessage(args[1]);
                break;
            case "restart": //quando o user se ligar pode enviar esta mensagem para atualizar o seu estado na transação
                System.out.println("Restart message received");
                break;
        }
    }

    /*
    private void beginMessage() throws IOException {
        String new_txid = t_manager.createNewTContext(c_socket);
        System.out.println("3. New TXID created:"+new_txid);
        
        writeToClient(new_txid);
    }

    private void commitMessage(String TxId) throws IOException {
        String response = t_manager.commitTransaction(TxId);

        writeToClient(response);
    }

    private void writeToClient(String msg) throws IOException {
        writer.write(msg+"\n");
        writer.flush();
    }*/

}
