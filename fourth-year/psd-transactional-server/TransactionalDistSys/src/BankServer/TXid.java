package BankServer;

import java.io.Serializable;
import javax.transaction.xa.Xid;

public class TXid implements Xid, Serializable {

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
    
    public String getId(){
        return this.id;
    }
}
