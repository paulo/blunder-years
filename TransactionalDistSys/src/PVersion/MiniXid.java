package PVersion;

import javax.transaction.xa.Xid;

public class MiniXid implements Xid {
	
	private static int next = 0;
	private String id;
	
	public MiniXid() {
		this.id = "txn"+(next++);
	}

	public byte[] getBranchQualifier() {
		return new byte[0];
	}

        //devolve sempre 0
	@Override
	public int getFormatId() {
		return 0;
	}

        //isto mais o branch qualifier têm de ser globalmente únicos
        // The gtrid and bqual can each contain up to 64 bytes of binary code to identify the global transaction 
        //and the branch transaction, respectively. The only requirement is that the gtrid and bqual taken together
        //must be globally unique.
	@Override
	public byte[] getGlobalTransactionId() {
		return id.getBytes();
	}
        
        
// A transaction branch is associated with a request to each resource manager involved in the 
// distributed transaction. Requests to three different RDBMSs, therefore, require three transaction 
// branches. Each transaction branch must be committed or rolled back by the local resource manager. 
// The transaction manager controls the boundaries of the transaction and is responsible for the final 
// decision as to whether or not the total transaction should commit or rollback. This decision is made 
// in two phases, called the Two-Phase Commit Protocol.
}
