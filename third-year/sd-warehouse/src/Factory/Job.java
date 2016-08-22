package Factory;

import java.util.*;
import java.util.concurrent.locks.Condition;
import java.util.concurrent.locks.ReentrantLock;

/**
 * Representa uma tarefa
 * @author Grupo 30
 */


public class Job {
    
    /**
     * @param name Nome da tarefa
     * @param toolList Lista de ferramentas necessárias para a tarefa
     * @param active Flag que indica se a tarefa está ativa
     */
    
    private String name;
    private Map<String, Integer> toolList;
    private boolean active;
    private ReentrantLock lockState;
    private Condition isOff;
    
    public Job(){
        this.name="";
        toolList = new HashMap<>();
        this.active=false;
        this.lockState = new ReentrantLock();
        this.isOff = lockState.newCondition();
    }
    
    public Job(String name, Map<String, Integer> toolList){
        this.name=name;
        this.toolList = toolList;
        this.active=false;
        this.lockState = new ReentrantLock();
        this.isOff = lockState.newCondition();
        
    }
    
    public Job(Job t){
        this.name = t.getName();
        this.toolList = t.getToolList();
        this.active=false;
        this.lockState = new ReentrantLock();
        this.isOff = lockState.newCondition();
    }
    
    public String getName(){return this.name;}
    public Map<String, Integer> getToolList(){
        Map<String, Integer> aux = new HashMap<>();
        for(Map.Entry<String, Integer> e:this.toolList.entrySet()){
            aux.put(e.getKey(), e.getValue());
        }
        return aux;
    }
    public boolean getActive(){return this.active;}
    
    public void setName(String name){this.name=name;}
    public void setToolList(Map<String, Integer> toolList){this.toolList=toolList;}
    public void setActive(boolean state){this.active=state;}
    
    /**
     * @throws InterruptedException 
     */
    public void checkOff() throws InterruptedException {
    
        this.lockState.lock();
        try {
            while(active) {
                isOff.await();
            }
        } finally {
            this.lockState.unlock();
        }
    }
    
    /**
     * Sinaliza que uma tarefa acabou
     * 
     * @throws InterruptedException 
     */
    public void setOff() throws InterruptedException{
        
        this.lockState.lock();
        try {
            this.active=false;
            isOff.signal();            
        } finally {
            this.lockState.unlock();
        }
    }
    
    @Override
    public String toString(){
        StringBuilder sb = new StringBuilder();

        sb.append("Nome: "+this.name+"\n");
        sb.append("Ferramentas: \n");
        for(Map.Entry<String,Integer> e : this.toolList.entrySet()){
            sb.append("Ferramenta: "+e.getKey()+" Quantidade: "+e.getValue()+"\n");
        
        }    
        return sb.toString();
    }
    
    @Override
    public boolean equals(Object o){
        if (this == o) return true;
        if ((o == null) || (this.getClass() != o.getClass()))
            return false;
        Job j = (Job) o;
        return (this.name.equals(j.getName()) && this.toolList.equals(j.getToolList()));
    }
    
    @Override
    public Job clone(){
        return new Job(this);
    }
}
