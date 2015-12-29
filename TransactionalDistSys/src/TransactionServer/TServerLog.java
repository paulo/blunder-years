package TransactionServer;

import java.net.Socket;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.logging.Level;
import java.util.logging.Logger;
import org.apache.derby.jdbc.EmbeddedDataSource;

//arranjar os metodos de forma a nao estar sempre a fazer createStatement
public class TServerLog {

    Connection connection;
    EmbeddedDataSource rawDataSource;

    public void initDBConnection() throws SQLException {

        rawDataSource = new EmbeddedDataSource();

        rawDataSource.setDatabaseName("../ServerLog");
        rawDataSource.setCreateDatabase("create");

        connection = rawDataSource.getConnection();
        createTables();
    }

    public void createTables() throws SQLException {
        Statement s = null;

        try {
            s = connection.createStatement();
            s.executeUpdate("DROP TABLE LOGTABLE");
        } catch (SQLException e) {
            if (!e.getSQLState().equals("42Y55")) {
                s.close();
            }
        }

        s = connection.createStatement();
        s.executeUpdate("create table LOGTABLE (TXID VARCHAR(10) PRIMARY KEY, "
                + "CLIENT BLOB NOT NULL, "
                + "RESOURCEN1 VARCHAR(3) NOT NULL, "
                + "RESOURCEN2 VARCHAR(3) NOT NULL)");
        s.close();
    }

    public void logResource(String Txid, int resourceNmr, String value) {
        Statement stmt = null;

        try {
            stmt = connection.createStatement();
            stmt.execute("update APP.LOGTABLE set RESOURCEN" + resourceNmr + " = " + value + " where TXID = " + Txid);
            stmt.close();
        } catch (SQLException sqlExcept) {
            sqlExcept.printStackTrace();
        }
    }

    public void insertNewLog(String Txid, Socket client) {
        PreparedStatement stmt = null;

        try {
            stmt = connection.prepareStatement(
                    "insert into APP.LOGTABLE values (?, ?)");
            stmt.setString(1, Txid);
            stmt.setObject(2, client);
            stmt.execute();
            stmt.close();
        } catch (SQLException sqlExcept) {
            sqlExcept.printStackTrace();
        }
    }

    public void printAccounts() throws SQLException {
        try ( // getting the data back
                Statement s = connection.createStatement();
                ResultSet res = s.executeQuery(
                        "SELECT * FROM APP.LOGTABLE")) {
            System.out.println("List of log entrys: ");
            while (res.next()) {
                System.out.println("TxId: " + res.getString("TXID")
                        + "\nResourceN1: " + res.getString("RESOURCEN1")
                        + "\nResourceN2: " + res.getString("RESOURCEN2"));
            }
        }
    }

    public void removeLog(String Txid) {
        Statement stmt = null;

        try {
            stmt = connection.createStatement();
            stmt.execute("delete from APP.LOGTABLE where TXID = " + Txid);
            stmt.close();
        } catch (SQLException sqlExcept) {
            sqlExcept.printStackTrace();
        }
    }

    int getCurrentTransactionNumber() {
        int nmr = 1;
        
        try ( // getting the data back
                Statement s = connection.createStatement();
                ResultSet res = s.executeQuery(
                        "SELECT TXID FROM APP.LOGTABLE")) {

            if(res.last()) nmr = Integer.parseInt(res.getString("TXID").substring(3)); 

        } catch (SQLException ex) {
            Logger.getLogger(TServerLog.class.getName()).log(Level.SEVERE, null, ex);
        }
        
        return nmr;
    }

    /*
    public void updateClient(String TxId, Socket new_client) {
        Statement stmt = null;

        try {
            stmt = connection.createStatement();
            stmt.execute("update APP.LOGTABLE set RESOURCEN" + resourceNmr + " = " + value + " where TXID = " + Txid);
            stmt.close();
        } catch (SQLException sqlExcept) {
            sqlExcept.printStackTrace();
        }
    }*/
}
