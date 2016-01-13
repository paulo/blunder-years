package Client;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.rmi.NotBoundException;
import java.rmi.RemoteException;

public class Client {

    private final String t_server_name;
    private final BufferedReader userIn;

    Client(String t_man) throws IOException {
        t_server_name = t_man;
        userIn = new BufferedReader(new InputStreamReader(System.in));
    }

    /**
     * Read transaction information from stdin
     * @throws IOException
     * @throws RemoteException
     * @throws NotBoundException 
     */
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

        Client c = new Client("transactionManager");

        //new ClientTransaction().transactionTest();
        
        while (true) {
         //   c.makeTransaction();
            new ClientTransaction().transactionTest();
        }
    }

}
