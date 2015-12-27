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
        printAccounts();
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

    //fazer controlo de erros
    //o prof divide isto
    public void beginProcedure(String txid) throws XAException {
        TXid new_t = new TXid(txid);

        t_ids.put(txid, new_t);

        xa_res.start(new_t, 0);

        //--------falta aqui o statement
        /*		Statement s = c.createStatement();
         s.executeUpdate("insert into t values (1,2)");
         s.close();*/
        //nao tenho a certeza disto
        xa_res.end(new_t, XAResource.TMSUCCESS);
    }

    public void commit() {
        //boolean r = phase1();

        // escrever r para o log do coordenador
        //System.out.println("resultado = " + r);
        //parar();
        //phase2(r);
    }

    boolean phase1() throws Exception {
        //boolean r = true;
        // for(...)
        //r = r && (xar.prepare(xid) == XAResource.XA_OK);
        //return r;
        return false;
    }

    void phase2(boolean r) throws Exception {
        // for(...)
        //if (r) {
        //    xar.commit(xid, false);
        //} else {
        //    xar.rollback(xid);
       // }
    }

    void recover() throws Exception {
     // Isto devia ser lido do log...
        // for(...)
        //xid = new MiniXid();
        //phase2(true);
    }

    public void parar() throws Exception {
        //System.out.println("parado...");
        //System.in.read();
    }
}
