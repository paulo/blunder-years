package Factory;

import java.util.concurrent.TimeUnit;
import java.util.concurrent.locks.Condition;
import java.util.concurrent.locks.ReentrantLock;

/**
 * Representa uma ferramenta
 * @author Grupo 30
 */

public class Tool {
    
    /**
     * @param name Nome da ferramenta
     * @param quantity Quantidade da ferramenta
     */
    
    private String name;
    private int quantity;
    private ReentrantLock useToolLock;
    private Condition notEmpty;
    
    
    public Tool(){
        this.name="";
        this.quantity=0;
        this.useToolLock=new ReentrantLock();
        notEmpty = useToolLock.newCondition();
    }
    
    public Tool(String name){
        this.name=name;
        this.quantity=0;
        this.useToolLock=new ReentrantLock();
        notEmpty = useToolLock.newCondition();
    }
    
    public Tool(String nome, int quantity){
        this.name=nome;
        this.quantity=quantity;
        this.useToolLock=new ReentrantLock();
        notEmpty = useToolLock.newCondition();
    }
    
    public Tool(Tool f){
        this.name = f.getName();
        this.quantity = f.getQuantity();
        this.useToolLock= new ReentrantLock();
        notEmpty = useToolLock.newCondition();
    }
    
    public String getName(){return this.name;}
    public int getQuantity(){
        useToolLock.lock();
        try {
            return this.quantity;
        } finally {
            useToolLock.unlock();
        }
    }
    public ReentrantLock getLock(){return this.useToolLock;}
    
    public void setName(String name){this.name=name;}
    public void setQuantity(int quantity){this.quantity=quantity;}
     
    /**
     * Incrementa a quantidade associada a uma ferramenta
     * 
     * @param quantity Quantidade que se pretende adicionar
     * @throws InterruptedException 
     */
    public void incrementQuantity(int quantity) throws InterruptedException{
        
        this.useToolLock.lock();
        try {
            this.quantity+=quantity;
            notEmpty.signalAll();            
        } finally {
            this.useToolLock.unlock();
        }
    }
    
    /**
     * 
     * Decrementa a quantidade associada a uma ferramenta
     * 
     * @param qtNeeded Quantidade
     * @return Booleano que indica se a operação foi efetuada com sucesso
     * @throws InterruptedException 
     */
    public boolean decrementQuantity(int qtNeeded) throws InterruptedException{
        int tried = 0, initial_qtd_needed = qtNeeded;
        this.useToolLock.lock();
        try {
            while(qtNeeded > this.quantity && tried < 3) {
                qtNeeded -= this.quantity;
                this.quantity=0;
                if(!notEmpty.await(10, TimeUnit.SECONDS)) tried++;
            }
            if(tried == 3 && this.quantity == 0){
                this.quantity += initial_qtd_needed - qtNeeded;
                return false;
            }  
            else { 
                this.quantity -= qtNeeded;            
                return true;
            }
        } finally {
            this.useToolLock.unlock();
        }
    }
    
    
    
    @Override
    public String toString(){
        StringBuilder sb = new StringBuilder();

        sb.append("Nome: ").append(this.name).append("\n");
        sb.append("Quantidade: ").append(this.quantity).append("\n");
        
        return sb.toString();
    }
    
    @Override
    public Tool clone(){
        return new Tool(this);
    }
    
    public boolean equals(Object o){
        if (this == o) return true;
        if ((o == null) || (this.getClass() != o.getClass()))
            return false;
        Tool t = (Tool) o;
        return (this.name.equals(t.getName()) 
            && this.quantity==t.getQuantity());
    }    
}
