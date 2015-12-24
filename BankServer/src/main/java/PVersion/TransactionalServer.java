package PVersion;

import com.google.protobuf.InvalidProtocolBufferException;
import java.sql.Connection;
import java.sql.SQLException;
import java.sql.Statement;
import org.apache.derby.jdbc.EmbeddedDataSource;
import org.zeromq.ZMQ;

public class TransactionalServer {

    static ZMQ.Context context = ZMQ.context(1);
    static ZMQ.Socket socket = context.socket(ZMQ.REP);
    static Connection connection;

    public static int receiveFromBank() throws InvalidProtocolBufferException {
        byte b[] = socket.recv();
        return Message.Transaction.parseFrom(b).getId();
    }

    private static void initTServerConnection() {
        socket.bind("tcp://*:123456");
    }

    private static void initDBConnection() throws SQLException {

        EmbeddedDataSource rawDataSource = new EmbeddedDataSource();

        rawDataSource.setDatabaseName("../testDB");
        //rawDataSource.setCreateDatabase("create");

        connection = rawDataSource.getConnection();
    }

    public static void main(String[] args) throws Exception {
        initDBConnection();
        initTServerConnection();

        int received = receiveFromBank();
        insertIntoTable(received);

        connection.close();

        /*ZMQ.Context context = ZMQ.context(1);
         ZMQ.Socket socket = context.socket(ZMQ.REP);
         socket.bind("tcp://*:123456");

         while (true) {
         byte[] b = socket.recv();
         String s = new String(b);
         System.out.println("Received " + s);
         String res = s.toUpperCase();
         socket.send(res);
         }
         //socket.close();
         //context.term();*/
    }

    private static void insertIntoTable(int i) {
        Statement stmt = null;

        try {
            stmt = connection.createStatement();
            stmt.execute("insert into APP.TESTTABLE values (+i+)");
            stmt.close();
        } catch (SQLException sqlExcept) {
            sqlExcept.printStackTrace();
        }
    }
}
