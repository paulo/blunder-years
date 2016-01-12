package BankServer;

import javax.transaction.xa.Xid;

public class TXid implements Xid {

    private final String id;

    public TXid(String id) {
        this.id = id;
    }

    @Override
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
