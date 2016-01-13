package Client;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.rmi.NotBoundException;
import java.rmi.RemoteException;

public class Clientv_2 {

    private final String t_server_name;
    BufferedReader userIn;

    Clientv_2(String t_man) throws IOException {
        t_server_name = t_man;
        userIn = new BufferedReader(new InputStreamReader(System.in));
    }

    private void makeTransaction() throws IOException, RemoteException, NotBoundException{ 
        System.out.println("What is the number of the source account?");
        String sourceAccount = userIn.readLine().trim();

        System.out.println("What is the number of the target account?");
        String targetAccount = userIn.readLine().trim();

        System.out.println("What is the ammount($) to transfer?");
        int ammount = Integer.parseInt(userIn.readLine().trim());
        
        ClientTransaction t = new ClientTransaction(sourceAccount, targetAccount, ammount);
        t.beginTransaction();
    }

    public static void main(String[] args) throws Exception {
        //Clientv_2 c = new Clientv_2(port);

        Clientv_2 c = new Clientv_2("transactionManager");

        new ClientTransaction().transactionTest();
        /*while (true) {
            c.makeTransaction();
        }*/
    }

}
