package BankServer;

import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.sql.ResultSetMetaData;

public class Bank {

    private static String dbURL = "jdbc:derby:BANKDB;create=true";
    private static String tableName = "BANK";
    // jdbc Connection
    private static Connection conn = null;
    private static Statement stmt = null;

    private static void createConnection() {
        try {

            Class.forName("org.apache.derby.jdbc.ClientDriver").newInstance();
            //Get a connection
            conn = DriverManager.getConnection(dbURL);
        } catch (Exception except) {
            except.printStackTrace();
        }
    }

    private static void insertUser(int account_number, String name, int account_balance) {
        try {
            stmt = conn.createStatement();
            stmt.execute("insert into " + tableName + " values ("
                    + name + ",'" + account_number + "','" + account_balance + "')");
            stmt.close();
        } catch (SQLException sqlExcept) {
            sqlExcept.printStackTrace();
        }
    }

    private static void selectUsers() {
        try {
            stmt = conn.createStatement();
            ResultSet results = stmt.executeQuery("select * from " + tableName);
            ResultSetMetaData rsmd = results.getMetaData();
            int numberCols = rsmd.getColumnCount();
            for (int i = 1; i <= numberCols; i++) {
                //print Column Names
                System.out.print(rsmd.getColumnLabel(i) + "\t\t");
            }

            System.out.println("\n-------------------------------------------------");

            while (results.next()) {
                int id = results.getInt(1);
                String name = results.getString(2);
                String account = results.getString(3);
                String balance = results.getString(4);
                System.out.println(id + "\t\t" + name + "\t\t" + account + "\t\t" + balance);
            }
            results.close();
            stmt.close();
        } catch (SQLException sqlExcept) {
            sqlExcept.printStackTrace();
        }
    }

    private void Transfer(int account_number1, int account_number2, int quantity) {
        Lifting(account_number1, quantity);
        Deposit(account_number2, quantity);

    }

    private void Lifting(int account_number1, int quantity) {
        try {
            stmt = conn.createStatement();
            String comand = "SELECT BALANCE FROM BANKDB WHERE ACCOUNT = '" + account_number1 + "')";
            ResultSet rs = stmt.executeQuery(comand);
            while (rs.next()) {
                //Retrieve by column name
                int balance_account1 = rs.getInt("BALANCE");
                int account1_newBalance = balance_account1 - quantity;
                String comandUpd = "UPDATE BANKDB" + " SET BALANCE = '" + account1_newBalance + "' WHERE ACCOUNT = '" + account_number1 + "')";
                stmt.executeQuery(comandUpd);
            }
            rs.close();
        } catch (SQLException sqlExcept) {
            sqlExcept.printStackTrace();
            System.out.println("Operação de Levantamento Interrompida");
        }
    }

    private void Deposit(int account_number, int quantity) {
        int account1_newBalance;
        try {
            stmt = conn.createStatement();
            String comand = "SELECT BALANCE FROM BANKDB WHERE ACCOUNT ='" + account_number + "')";
            ResultSet rs = stmt.executeQuery(comand);
            while (rs.next()) {
                //Retrieve by column name
                int balance_account1 = rs.getInt("BALANCE");
                account1_newBalance = balance_account1 + quantity;
                String comandUpd = "UPDATE BANKDB" + "SET BALANCE = '" + account1_newBalance + "' WHERE ACCOUNT = '" + account_number + "')";
                stmt.executeQuery(comandUpd);
            }
            rs.close();
        } catch (SQLException sqlExcept) {
            sqlExcept.printStackTrace();
            System.out.println("Operação de Depósito Interrompida");
        }
    }

    private static void shutdown() {
        try {
            if (stmt != null) {
                stmt.close();
            }
            if (conn != null) {
                DriverManager.getConnection(dbURL + ";shutdown=true");
                conn.close();
            }
        } catch (SQLException sqlExcept) {

        }

    }

    public static void main(String[] args) {
        createConnection();
        //insertUser(1,"Joao",100);
        shutdown();
    }

}
