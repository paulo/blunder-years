package BankServer;

import java.io.File;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.HashMap;
import java.util.Map;
import javax.sql.XAConnection;
import javax.transaction.xa.XAException;
import javax.transaction.xa.XAResource;
import org.apache.derby.jdbc.EmbeddedXADataSource;

public class BankDataOperator {

    EmbeddedXADataSource rawDataSource;
    String bankID, bankDBName;
    Map<String, TXid> t_ids;

    public BankDataOperator(String bank_id) {
        bankID = bank_id;
        t_ids = new HashMap<>();
    }

    /**
     * Initiate database with the current bank name
     *
     * @throws SQLException
     */
    public void initDB() throws SQLException {
        rawDataSource = new EmbeddedXADataSource();
        bankDBName = "Bank" + bankID;
        rawDataSource.setDatabaseName("../" + bankDBName);

        File f = new File("../" + bankDBName);
        if (!f.exists()) {
            rawDataSource.setCreateDatabase("create");
        } else {
            if (!f.isDirectory()) {
                rawDataSource.setCreateDatabase("create");
            }
        }

        try (Connection con = rawDataSource.getXAConnection().getConnection()) {
            createTables(con);
            populateDB(con);
            con.close();
        }
    }

    /**
     * Create accounts table
     *
     * @param con Connection to the database
     * @throws SQLException
     */
    public void createTables(Connection con) throws SQLException {
        Statement s = null;

        try {
            s = con.createStatement();
            s.executeUpdate("DROP TABLE ACCOUNTS");
            s.close();
        } catch (SQLException e) {
            if (!e.getSQLState().equals("42Y55")) {
                s.close();
            }
        }

        s = con.createStatement();
        s.executeUpdate("create table ACCOUNTS (CLIENTID VARCHAR(7) PRIMARY KEY, BALANCE INT NOT NULL)");
        s.close();
    }

    /**
     * Populate database with test accounts
     *
     * @param con Connection to the database
     */
    public void populateDB(Connection con) {
        PreparedStatement stmt = null;

        try {
            for (int i = 0; i < 10; i++) {
                stmt = con.prepareStatement(
                        "insert into APP.ACCOUNTS values (?, ?)");
                stmt.setString(1, "000000" + i);
                stmt.setObject(2, 1000);
                stmt.execute();
                stmt.close();
            }
        } catch (SQLException sqlExcept) {
            sqlExcept.printStackTrace();
        }
    }

    /**
     * Print all account information in the database
     *
     * @throws SQLException
     */
    public void printAccounts() throws SQLException {
        Statement s = rawDataSource.getXAConnection().getConnection().createStatement();

        try ( // getting the data back
                ResultSet res = s.executeQuery(
                        "SELECT * FROM APP.ACCOUNTS")) {
                    System.out.println("List of Accounts: ");
                    while (res.next()) {
                        System.out.println("Client: " + res.getInt("CLIENTID") + "\nBalance: " + res.getString("BALANCE"));
                    }
                    res.close();
                }
                s.close();
    }

    /**
     * Checks whether a certain account with account_nmr exists in the database
     *
     * @param account_nmr Account number to look for
     * @param con Connection to the database
     * @return Returns true if the account exists
     * @throws SQLException
     */
    private boolean clientExists(String account_nmr, Connection con) throws SQLException {
        boolean exists = true;

        try (PreparedStatement stmt = con.prepareStatement(
                "SELECT BALANCE FROM APP.ACCOUNTS WHERE CLIENTID = ?")) {
            stmt.setString(1, account_nmr);
            stmt.execute();
            try (ResultSet res = stmt.getResultSet()) {
                if (!res.next()) {
                    exists = false;
                }
                res.close();
            }
            stmt.close();
        }

        return exists;
    }

    /**
     * Returns the current balance from an account with account number equal to
     * account_nmr
     *
     * @param account_nmr Account number to search for
     * @param con Connection to the database
     * @return Balance from the account
     * @throws SQLException
     */
    private int getFunds(String account_nmr, Connection con) throws SQLException {
        int funds = 0;

        PreparedStatement stmt = con.prepareStatement(
                "SELECT BALANCE FROM APP.ACCOUNTS WHERE CLIENTID = ?");
        stmt.setString(1, account_nmr);
        stmt.execute();
        ResultSet res = stmt.getResultSet();
        while (res.next()) {
            funds = res.getInt("BALANCE");
        }
        res.close();
        stmt.close();

        return funds;
    }

    /**
     * Update a given account balance
     *
     * @param new_balance The new balance to update the account
     * @param account_nmr The account number to be updated
     * @param con The database connection
     * @throws SQLException
     */
    void changeBalance(int new_balance, String account_nmr, Connection con) throws SQLException {
        try (PreparedStatement stmt = con.prepareStatement(
                "update APP.ACCOUNTS set BALANCE = ? where CLIENTID = ?")) {
            stmt.setInt(1, new_balance);
            stmt.setString(2, account_nmr);
            stmt.execute();
            stmt.close();
        }
    }

    /**
     * Begin work on transaction context.
     * @param TXid Transaction context id.
     * @param amount Value to be withdrawn from the account.
     * @param account_nmr Account from where to withdrawn.
     * @return True if operation is possible, false otherwise.
     * @throws SQLException
     * @throws XAException 
     */
    boolean beginWithdraw(TXid TXid, int amount, String account_nmr) throws SQLException, XAException {
        XAConnection xa_con = rawDataSource.getXAConnection();
        Connection con = xa_con.getConnection();
        XAResource xa_res = xa_con.getXAResource();
        if (clientExists(account_nmr, con)) {
            int current_balance = getFunds(account_nmr, con);
            if (current_balance > amount) {
                try {
                    xa_res.setTransactionTimeout(3);
                    xa_res.start(TXid, 0);
                    changeBalance(current_balance - amount, account_nmr, con);
                } finally {
                    xa_res.end(TXid, XAResource.TMSUCCESS);
                }
            } else {
                System.out.println("Client doesn't have enough funds");
                return false;
            }
        } else {
            System.out.println("Client doesn't exist");
            return false;
        }
        con.close();
        xa_con.close();

        return true;
    }

    /**
     * Begin work on transaction context.
     * @param TXid Transaction context id.
     * @param amount Value to be deposited to the account.
     * @param account_nmr Account where to deposit.
     * @return True if operation is possible, false otherwise.
     * @throws SQLException
     * @throws XAException 
     */
    public boolean beginDeposit(TXid TXid, int amount, String account_nmr) throws SQLException, XAException {
        XAConnection xa_con = rawDataSource.getXAConnection();
        Connection con = xa_con.getConnection();
        XAResource xa_res = xa_con.getXAResource();

        if (clientExists(account_nmr, con)) {
            int current_balance = getFunds(account_nmr, con);

            try {
                xa_res.setTransactionTimeout(3);
                xa_res.start(TXid, 0);
                changeBalance(current_balance + amount, account_nmr, con);
            } finally {
                xa_res.end(TXid, XAResource.TMSUCCESS);
            }

        } else {
            System.out.println("Client doesn't exist");
            return false;
        }

        con.close();
        xa_con.close();

        return true;
    }

    /**
     * Prepare transaction for commit under TPC
     *
     * @param txid Transaction context id
     * @return True if prepared successfully, false otherwise
     * @throws Exception
     */
    boolean phase1prepare(TXid txid) throws Exception {
        boolean r = true;
        XAConnection xa_con = rawDataSource.getXAConnection();
        XAResource xa_res = xa_con.getXAResource();

        r = r && (xa_res.prepare(txid) == XAResource.XA_OK);
        xa_con.close();

        return r;
    }

    /**
     * Commit transaction under TPC
     *
     * @param txid Transaction context id
     * @throws Exception
     */
    void phase2Commit(TXid txid) throws Exception {
        XAConnection xa_con = rawDataSource.getXAConnection();
        XAResource xa_res = xa_con.getXAResource();

        xa_res.commit(txid, false);

        xa_con.close();
    }

    /**
     * Rollback transaction
     *
     * @param txid Transaction context id
     * @throws Exception
     */
    void rollbackTransaction(TXid txid) throws Exception {
        XAConnection xa_con = rawDataSource.getXAConnection();
        XAResource xa_res = xa_con.getXAResource();

        xa_res.rollback(txid);
        xa_con.close();
    }

    /**
     * Recover prepared but uncommitted transactions and start phase2 of TPC for
     * each one
     *
     * @throws Exception
     */
    void recover() throws Exception {
        XAConnection xc = rawDataSource.getXAConnection();
        xc.getXAResource().recover(XAResource.TMNOFLAGS);
        xc.close();
    }
}
