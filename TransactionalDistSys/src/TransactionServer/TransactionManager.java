package TransactionServer;

import java.net.Socket;

public class TransactionManager {

    TServerLog server_log;
    int transaction_number;

    TransactionManager(TServerLog server_log) {
        this.server_log = server_log;
        transaction_number = server_log.getCurrentTransactionNumber();
    }
    
    String commitTransaction(String TxId) {
        throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
    }

    public synchronized String createNewTContext(Socket c_socket) {
        int new_t_number = this.transaction_number;
        String new_txn_id = "TXN"+transaction_number++;
        
        server_log.insertNewLog(new_txn_id, c_socket);
        
        return new_txn_id;
    }

}
