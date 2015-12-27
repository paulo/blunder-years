package TransactionServer;

import java.sql.Connection;
import java.sql.SQLException;
import org.apache.derby.jdbc.EmbeddedDataSource;
import org.zeromq.ZMQ;

public class ManagerConnection {
    static ZMQ.Context context;
    static ZMQ.Socket socket;
    static Connection connection;

    public ManagerConnection() {
        context = ZMQ.context(1);
        socket = context.socket(ZMQ.REP);
    }

    
    

}
