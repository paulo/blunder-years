package PVersion;

import TransactionServer.TServerConnection;
import TransactionServer.Transaction;
import Client.*;
import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;

//vai haver uma class client onde vai estar a lógica de fazer operações
//vai haver uma class TServerConnection onde vai estar a lógica da coneção com o TServer e de fazer transações
//vai haver uma class BServerConnection onde vai estar a lógica da coneção com os BankServers e de fazer levantamentos e depósitos
public class Client {

    private static TServerConnection t_con;
    private static int t_server_port;
    private BufferedReader br;

    Client(int server_port) {
        t_server_port = server_port;
        br = new BufferedReader(new InputStreamReader(System.in));
    }

    //alterar esta lógica se afinal não for para fazer depositos e levantamentos diretos no bank server
    public static void initTServerConnection() {
        t_con = new TServerConnection();
        t_con.initConnection(t_server_port);
    }

    private void processTransfer(Transaction t) {
        
    }
    
    /*private int beginTransaction(){
        return t_con.sendBeginMessage();
    }*/

    private void makeTransaction() throws IOException {
        Transaction t = readTransactionInfo();
        
        processTransfer(t);
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
        Client c = new Client(123456);

        initTServerConnection();

        c.initOperations();

        /* 
         BankIf bank = (BankIf) Naming.lookup("//localhost/firstbank");

         AccountIf acc = bank.getAccount(00100002);
         System.out.println("Current Balance: " + acc.getBalance());
        
         TransactionIf trans = bank.makeTransaction();
         trans.deposit(100, acc);
         System.out.println("We made a deposit of 100");
         System.out.println("Current Balance: " + acc.getBalance());*/
    }
}
