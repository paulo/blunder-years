package Client;

import BankServer.BankIf;
import Utils.Transaction;
import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.net.MalformedURLException;
import java.rmi.NotBoundException;
import java.rmi.RemoteException;
import java.rmi.registry.LocateRegistry;
import java.rmi.registry.Registry;

public class Client {

    private final int t_server_port;
    private BufferedReader br;

    Client(int server_port) {
        t_server_port = server_port;
        br = new BufferedReader(new InputStreamReader(System.in));
    }

    //alterar esta lógica se afinal não for para fazer depositos e levantamentos diretos no bank server
    public void initTServerConnection() {
        //t_con = new TServerConnection();
        //t_con.initConnection(t_server_port);
    }

    /* private String registerNewTransaction(){
     return t_con.sendBeginMessage();
     }*/
    private void makeTransaction() throws IOException {
        Transaction t = readTransactionInfo();

        //String txnId = registerNewTransaction();
    }

    //isto pode ser abstraido para outra classe de leitura de comandos
    private Transaction readTransactionInfo() throws IOException {

        System.out.println("What is the number of the source account?");
        String sourceAccount = br.readLine().trim();

        System.out.println("What is the number of the target account?");
        String targetAccount = br.readLine().trim();

        System.out.println("What is the ammount($) to transfer?");
        int ammount = Integer.parseInt(br.readLine().trim());
        return null;
        //return new Transaction(sourceAccount, targetAccount, ammount);
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

    private void testRMI() throws NotBoundException, MalformedURLException, RemoteException {
        Registry registry = LocateRegistry.getRegistry(3333);
        BankIf of = (BankIf) registry.lookup("bank10");

        of.deposit("10", 10, "0000009");
    }

    public static void main(String[] args) throws Exception {
        Client c = new Client(55555);

        c.initTServerConnection();
        //c.initOperations();        
        //c.testRMI();
    }

}
