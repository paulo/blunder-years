package twopc;

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

	@Override
	public int getFormatId() {
		return 0;
	}

	@Override
	public byte[] getGlobalTransactionId() {
		return id.getBytes();
	}
}
