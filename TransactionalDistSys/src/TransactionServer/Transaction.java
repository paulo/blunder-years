package TransactionServer;

public class Transaction {

    private final String source, target;
    private final int amount;

    public Transaction(String src, String tgt, int amt) {
        this.source = src;
        this.target = tgt;
        this.amount = amt;
    }

    public String getSourceBankServer() {
        return source.substring(0, 2);
    }

    public int getSourceBankServerNumber() {
        return Integer.parseInt(getSourceBankServer());
    }

    public String getTargetBankServer() {
        return target.substring(0, 2);
    }

    public int getTargetBankServerNumber() {
        return Integer.parseInt(getTargetBankServer());
    }

    public int getSourceAccountNumber() {
        return Integer.parseInt(source.substring(3));
    }

    public int getTargetAccountNumber() {
        return Integer.parseInt(target.substring(3));
    }

    public int getAmount() {
        return amount;
    }
}
