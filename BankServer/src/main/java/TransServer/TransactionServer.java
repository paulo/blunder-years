package TransServer;

import java.sql.Connection;
import java.sql.Statement;

import javax.sql.XAConnection;
import javax.transaction.xa.XAResource;
import javax.transaction.xa.Xid;

import org.apache.derby.jdbc.ClientXADataSource;

public class TransactionServer {
	
	// Logica de negocio
	public void run() throws Exception {
		// feito automaticamente!
		xar.start(xid, 0);
		
		Statement s = c.createStatement();
		s.executeUpdate("insert into t values (1,2)");
		s.close();
		
		xar.end(xid, XAResource.TMSUCCESS);		
	}
	
	// Coordenador de 2PC
	private XAConnection xac;
	private Connection c;	
	private MiniXid xid; 	// ThreadLocal!
	private XAResource xar;

	void init() throws Exception {
		ClientXADataSource ds = new ClientXADataSource();
		ds.setServerName("localhost");
		ds.setDatabaseName("teste");
		
		xac = ds.getXAConnection();
		c = xac.getConnection();
		xar = xac.getXAResource();
	}
	
	void begin() {
		xid = new MiniXid();
	}
	
	void commit() throws Exception {
		boolean r = phase1();
			
		// escrever r para o log do coordenador
		System.out.println("resultado = "+r);
		parar();
	
		phase2(r);
	}

	boolean phase1() throws Exception {
		boolean r = true;
		// for(...)
			r = r && (xar.prepare(xid) == XAResource.XA_OK);
		return r;
	}

	void phase2(boolean r) throws Exception {
		// for(...)
			if (r)
				xar.commit(xid, false);
			else
				xar.rollback(xid);
	}
	
	void recover() throws Exception {
		// Isto devia ser lido do log...
		// for(...)
			xid = new MiniXid();
			phase2(true);
	}
	
	public void parar() throws Exception {
		System.out.println("parado...");
		System.in.read();
	}

	public static void main(String[] args) throws Exception {
		TransactionServer as = new TransactionServer();
		as.init();
		as.recover();
		
		/*
		as.begin();
		as.run();
		as.commit();
		*/
	}
}
