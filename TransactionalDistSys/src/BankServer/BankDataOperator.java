package BankServer;

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

    XAConnection xa_con;
    Connection con;
    EmbeddedXADataSource rawDataSource;
    String bankID;
    String bankDBName;
    XAResource xa_res;
    Map<String, TXid> t_ids;

    public BankDataOperator(String bank_id) {
        bankID = bank_id;
        t_ids = new HashMap<>();
    }

    //iniciar base de dados
    public void initDBConnection() throws SQLException {
        rawDataSource = new EmbeddedXADataSource();

        rawDataSource.setDatabaseName("../Bank" + bankID);
        bankDBName = "Bank" + bankID;
        rawDataSource.setCreateDatabase("create");

        xa_con = rawDataSource.getXAConnection();
        con = xa_con.getConnection();
        xa_res = xa_con.getXAResource();

        createTables();
        populateDB();
        //printAccounts();
        //endConnection();
    }

    //criar tabela de clientes
    public void createTables() throws SQLException {
        Statement s = null;

        try {
            s = con.createStatement();
            s.executeUpdate("DROP TABLE ACCOUNTS");
        } catch (SQLException e) {
            if (!e.getSQLState().equals("42Y55")) {
                s.close();
            }
        }

        s = con.createStatement();
        s.executeUpdate("create table ACCOUNTS (CLIENTID VARCHAR(7) PRIMARY KEY, BALANCE INT NOT NULL)");
        s.close();
    }

    //carregar 10 clientes na base de dados
    public void populateDB() {
        PreparedStatement stmt = null;

        try {
            for (int i = 0; i < 10; i++) {
                stmt = con.prepareStatement(
                        "insert into APP.ACCOUNTS values (?, ?)");
                stmt.setString(1, "000000" + i);
                stmt.setObject(2, 100);
                stmt.execute();
                stmt.close();
            }
        } catch (SQLException sqlExcept) {
            sqlExcept.printStackTrace();
        }
    }

    public void printAccounts() throws SQLException {
        Statement s = con.createStatement();

        try ( // getting the data back
                ResultSet res = s.executeQuery(
                        "SELECT * FROM APP.ACCOUNTS")) {
                    System.out.println("List of Accounts: ");
                    while (res.next()) {
                        System.out.println("Client: " + res.getInt("CLIENTID") + "\nBalance: " + res.getString("BALANCE"));
                    }
                }

                s.close();
    }

    public void endConnection() throws SQLException {
        con.close();
        xa_con.close();
    }

    boolean clientExists(String account_nmr) throws SQLException {
        PreparedStatement stmt = null;
        boolean exists = true;

        stmt = con.prepareStatement(
                "SELECT BALANCE FROM APP.ACCOUNTS WHERE CLIENTID = ?");
        stmt.setString(1, account_nmr);
        stmt.execute();
        ResultSet res = stmt.getResultSet();

        if (!res.next()) {
            exists = false;
        }

        stmt.close();

        return exists;
    }

    int getFunds(String account_nmr) throws SQLException {
        PreparedStatement stmt = null;
        int funds = 0;

        stmt = con.prepareStatement(
                "SELECT BALANCE FROM APP.ACCOUNTS WHERE CLIENTID = ?");
        stmt.setString(1, account_nmr);
        stmt.execute();
        ResultSet res = stmt.getResultSet();
        while (res.next()) {
            funds = res.getInt("BALANCE");
        }

        stmt.close();
        
        return funds;
    }

    //isto tem de estar sincronizado, ele nao pode fazer o deposito enquanto estao a acontecer outras coisas
    void changeBalance(int new_balance, String account_nmr) throws SQLException {
        PreparedStatement stmt = null;
        int funds = 0;

        stmt = con.prepareStatement(
                "update APP.ACCOUNTS set BALANCE = ? where CLIENTID = ?");
        stmt.setInt(1, new_balance);
        stmt.setString(2, account_nmr);
        stmt.execute();
        
        stmt.close();
    }

    //se nao der, o cliente tem de mandar uma mensagem ao tServer a dizer para cancelar a transação
    TXid beginWithraw(String TXid, int amount, String account_nmr) throws SQLException, XAException {
        TXid new_txid = null;

        if (clientExists(account_nmr)) {
            int current_balance = getFunds(account_nmr);
            if (current_balance > amount) {
                new_txid = new TXid(TXid);

                xa_res.start(new_txid, 0);

                changeBalance(current_balance - amount, account_nmr);
                //printAccounts();
                xa_res.end(new_txid, XAResource.TMSUCCESS);

            } else {
                System.out.println("Client doesn't have enough funds");
            }
        } else {
            System.out.println("Client doesn't exist");
        }
        return new_txid;
    }

    TXid beginDeposit(String TXid, int amount, String account_nmr) throws SQLException, XAException {
        TXid new_txid = null;

        if (clientExists(account_nmr)) {
            int current_balance = getFunds(account_nmr);
            new_txid = new TXid(TXid);
            xa_res.start(new_txid, 0);

            changeBalance(current_balance + amount, account_nmr);

            xa_res.end(new_txid, XAResource.TMSUCCESS);

        } else {
            System.out.println("Client doesn't exist");
        }

        return new_txid;
    }
    /*
    
     public void commit() {
     //boolean r = phase1();

     // escrever r para o log do coordenador
     //System.out.println("resultado = " + r);
     //parar();
     //phase2(r);
     }
     */

    //nao percebo pq é que o prof mete o true...
    boolean phase1prepare(TXid txid) throws Exception {
        return true && (xa_res.prepare(txid) == XAResource.XA_OK);
    }

    void phase2Commit(boolean r) throws Exception {
        // for(...)
        //if (r) {
        //    xar.commit(xid, false);
        //} else {
        //    xar.rollback(xid);
        // }
    }
    /*
     void recover() throws Exception {
     // Isto devia ser lido do log...
     // for(...)
     //xid = new MiniXid();
     //phase2(true);
     }

     public void parar() throws Exception {
     //System.out.println("parado...");
     //System.in.read();
     }*/

}
