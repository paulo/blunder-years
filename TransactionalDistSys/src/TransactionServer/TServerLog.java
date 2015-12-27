package TransactionServer;

import java.net.Socket;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.SQLException;
import java.sql.Statement;
import org.apache.derby.jdbc.EmbeddedDataSource;


//arranjar os metodos de forma a nao estar sempre a fazer createStatement
public class TServerLog {
    
    Connection connection;
    EmbeddedDataSource rawDataSource;
   
    private void initDBConnection() throws SQLException {

        rawDataSource = new EmbeddedDataSource();

        rawDataSource.setDatabaseName("../testDB");
        //rawDataSource.setCreateDatabase("create");

        connection = rawDataSource.getConnection();
    }
    
    private void logResource(String Txid, int resourceNmr, String value){
        Statement stmt = null;

        try {
            stmt = connection.createStatement();
            stmt.execute("update APP.LOGTABLE set RESOURCEN"+resourceNmr+" = "+value+" where TXID = "+Txid);
            stmt.close();
        } catch (SQLException sqlExcept) {
            sqlExcept.printStackTrace();
        }
    }
    
    private void insertNewLog(String Txid, Socket client){
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
    
    private void removeLog(String Txid){
        Statement stmt = null;

        try {
            stmt = connection.createStatement();
            stmt.execute("delete from APP.LOGTABLE where TXID = "+Txid);
            stmt.close();
        } catch (SQLException sqlExcept) {
            sqlExcept.printStackTrace();
        }
    }
    
    private void getTransaction(String Txid){
        
    }
    
    
    private static void main(String[] args) throws SQLException{
        TServerLog tsl = new TServerLog();
        
        tsl.initDBConnection();
    }
    
}
