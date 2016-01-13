package TransactionServer;

import java.io.IOException;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import static java.sql.Types.VARCHAR;
import org.apache.derby.jdbc.EmbeddedDataSource;

public class TServerLog {

    EmbeddedDataSource rawDataSource;

    /**
     * Initialize database (always creates in test mode)
     *
     * @throws SQLException
     */
    public void initDBConnection() throws SQLException {

        rawDataSource = new EmbeddedDataSource();

        rawDataSource.setDatabaseName("../ServerLog");
        rawDataSource.setCreateDatabase("create");

        createTables();
    }

    /**
     * Create database tables for storing transaction information (Transaction
     * id, Source Bank Id, Target Bank Id)
     *
     * @throws SQLException
     */
    public void createTables() throws SQLException {
        Statement s = null;
        Connection c = rawDataSource.getConnection();

        try {
            s = c.createStatement();
            s.executeUpdate("DROP TABLE LOGTABLE");
        } catch (SQLException e) {
            if (!e.getSQLState().equals("42Y55")) {
                s.close();
            }
        }

        s = c.createStatement();
        s.executeUpdate("create table LOGTABLE (TXID VARCHAR(10) PRIMARY KEY, "
                //+ "CLIENT BLOB NOT NULL, "
                + "RESOURCEN1 VARCHAR(10), "
                + "RESOURCEN2 VARCHAR(10))"
                + "STATUS VARCHAR(10");
        s.close();
    }

    /**
     * Register resource (bank) for transaction
     *
     * @param Txid Transaction Context Id
     * @param resourceNmr Value 1 if the withdraw is to be made in this
     * resource, 2 otherwise
     * @param value Id of the resource
     * @throws java.sql.SQLException
     */
    public void logResource(String Txid, int resourceNmr, String value) throws SQLException {
        try (PreparedStatement stmt = rawDataSource.getConnection().prepareStatement("update APP.LOGTABLE set RESOURCEN" + resourceNmr + " = ? where TXID = ?")) {
            stmt.setString(1, value);
            stmt.setString(2, Txid);
            stmt.execute();
        }
    }

    /**
     * Create new entry on log table
     *
     * @param Txid New Transaction Context Id
     * @throws IOException
     */
    public void insertNewLog(String Txid) throws IOException, SQLException {
        PreparedStatement stmt = null;

        stmt = rawDataSource.getConnection().prepareStatement(
                "insert into APP.LOGTABLE values (?,?,?,?)");
        stmt.setString(1, Txid);
        stmt.setNull(2, VARCHAR);
        stmt.setNull(3, VARCHAR);
        stmt.setNull(4, VARCHAR);
        stmt.execute();
        stmt.close();

    }

    /**
     * Print log table contents (for test purposes only)
     *
     * @throws SQLException
     */
    public void printTransactionLogs() throws SQLException {
        try (
                Statement s = rawDataSource.getConnection().createStatement();
                ResultSet res = s.executeQuery(
                        "SELECT * FROM APP.LOGTABLE")) {
            System.out.println("List of log entrys: ");
            while (res.next()) {
                System.out.println("TxId: " + res.getString("TXID")
                        + "\nResourceN1: " + res.getString("RESOURCEN1")
                        + "\nResourceN2: " + res.getString("RESOURCEN2")
                        + "\nStatus: " + res.getString("STATUS"));
            }
        }
    }

    /**
     * Retrieve resource (bank) Id from the log table (if present)
     *
     * @param Txid Transaction Context Id
     * @param type Value 1 if the withdraw was made in this resource, 2
     * otherwise
     * @return Resource Id if present, null otherwise
     * @throws SQLException
     */
    public String getResource(String Txid, int type) throws SQLException {
        String resource_name = null;

        try (PreparedStatement s = rawDataSource.getConnection().prepareStatement("SELECT RESOURCEN" + type + " FROM APP.LOGTABLE WHERE TXID = ?")) {
            s.setString(1, Txid);
            ResultSet res = s.executeQuery();
            if (res.next()) {
                resource_name = res.getString("RESOURCEN" + type);
            }
        }
        return resource_name;
    }

    /**
     * Remove log after transaction finishes
     *
     * @param Txid Transaction Context Id
     * @throws java.sql.SQLException
     */
    public void removeLog(String Txid) throws SQLException {

        Statement stmt = rawDataSource.getConnection().createStatement();
        stmt.execute("delete from APP.LOGTABLE where TXID = " + Txid);
        stmt.close();

    }

    /**
     * Retrieve last transaction number
     *
     * @return Transaction Context Id of the last transaction made
     */
    int getCurrentTransactionNumber() {
        int nmr = 1;

        try (
                Statement s = rawDataSource.getConnection().createStatement(ResultSet.TYPE_SCROLL_INSENSITIVE, ResultSet.CONCUR_READ_ONLY);
                ResultSet res = s.executeQuery(
                        "SELECT TXID FROM APP.LOGTABLE")) {

            if (res.last()) {
                nmr = Integer.parseInt(res.getString("TXID").substring(3));
            }

        } catch (SQLException ex) {
            return nmr;
        }
        return nmr;
    }

    public void updateStatus(String Txid, String status) throws SQLException {
        try (PreparedStatement stmt = rawDataSource.getConnection().prepareStatement("update APP.LOGTABLE set STATUS = ? where TXID = ?")) {
            stmt.setString(1, status);
            stmt.setString(2, Txid);
            stmt.execute();
        }
    }

    public ResultSet getActiveTransactions() throws SQLException {
        Statement s = rawDataSource.getConnection().createStatement();
        return s.executeQuery("SELECT * FROM APP.LOGTABLE");    
    }
}
