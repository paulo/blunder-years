package Client;

import BankServer.BankIf;
import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.net.Socket;
import java.rmi.NotBoundException;
import java.rmi.RemoteException;
import java.rmi.registry.LocateRegistry;
import java.rmi.registry.Registry;

public class Clientv_2 {

    private final int t_server_port;
    Socket s;
    BufferedWriter writer;
    BufferedReader reader;
    
    Clientv_2(int server_port) throws IOException {
        t_server_port = server_port;
        s = new Socket("localhost", 55555);
        writer = new BufferedWriter(new OutputStreamWriter(s.getOutputStream()));
        reader = new BufferedReader(new InputStreamReader(s.getInputStream()));
    }
     
    private void transactionTest() throws IOException, RemoteException, NotBoundException {
        
        System.out.println("Transaction test has begun");
        sendMessage("begin\n");
        String txid = receiveMessage();
        System.out.println("TXID Recebido: "+txid);
        if(withrawMoney(txid, "bank10", "0000005", 10)) /* && 
                depositMoney(txid, "bank20", "0000005", 10))*/
            System.out.println("Chamadas RMI com sucesso");
        /* feito até aqui -> é preciso mudar a inicializaçao dos bank servers (para nao usar a mesma porta, ou entao fazer o servidor rmi disjunto)
        sendMessage("commit");
        System.out.println(receiveMessage());*/
    }

    private boolean depositMoney(String txid, String target_bank, String target_account, int amount) throws RemoteException, NotBoundException{
        Registry registry = LocateRegistry.getRegistry(3333);
        BankIf bi = (BankIf) registry.lookup("bank10");

        return bi.deposit(txid, amount, target_account);
    }
    
    private boolean withrawMoney(String txid, String source_bank, String source_account, int amount) throws RemoteException, NotBoundException{
        Registry registry = LocateRegistry.getRegistry(3333);
        BankIf bi = (BankIf) registry.lookup(source_bank);
        
        return bi.withdraw(txid, amount, source_account);
    }
    
    private void sendMessage(String msg) throws IOException {
        writer.write(msg);
        writer.flush();
    }
    
    private String receiveMessage() throws IOException {
        return reader.readLine();
    }
 
    public static void main(String[] args) throws Exception {
        Clientv_2 c = new Clientv_2(55555);

        c.transactionTest();
    }

}
