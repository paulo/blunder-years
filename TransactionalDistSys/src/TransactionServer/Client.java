package TransactionServer;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;

public class Client {

    private TServerConnection t_con;
    private final int t_server_port;
    private BufferedReader br;

    Client(int server_port) {
        t_server_port = server_port;
        br = new BufferedReader(new InputStreamReader(System.in));
    }

    //alterar esta lógica se afinal não for para fazer depositos e levantamentos diretos no bank server
    public void initTServerConnection() {
        t_con = new TServerConnection();
        t_con.initConnection(t_server_port);
    }
    
    private String registerNewTransaction(){
        return t_con.sendBeginMessage();
    }
    
    private void makeTransaction() throws IOException {
        Transaction t = readTransactionInfo();
        
        String txnId = registerNewTransaction();
        
    }

    //isto pode ser abstraido para outra classe de leitura de comandos
    private Transaction readTransactionInfo() throws IOException {

        System.out.println("What is the number of the source account?");
        String sourceAccount = br.readLine().trim();

        System.out.println("What is the number of the target account?");
        String targetAccount = br.readLine().trim();

        System.out.println("What is the ammount($) to transfer?");
        int ammount = Integer.parseInt(br.readLine().trim());

        return new Transaction(sourceAccount, targetAccount, ammount);
    }

    private void initOperations() throws IOException {
        String op;
        System.out.println("Client ready");

        //abstrair isto para outra classe
        while ((op = br.readLine()) != null) {
            switch (op.trim()) {
                case "transfer":
                    makeTransaction();
                    break;
                default:
                    break;
            }
        }
    }

    public static void main(String[] args) throws IOException {
        Client c = new Client(55555);

        c.initTServerConnection();

        c.initOperations();        
    }
    
}
