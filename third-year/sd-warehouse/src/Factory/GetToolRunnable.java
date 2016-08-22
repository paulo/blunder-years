package Factory;

import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * @author Grupo 30
 */


public class GetToolRunnable implements Runnable {
    
    private Tool tool;
    private int quantity;
    private boolean finished;
            
    public GetToolRunnable(Tool t, int quantity){
        this.tool=t;
        this.quantity = quantity;
        this.finished = false;
    }
    
    /**
     * @return Booleano que indica se já acabou
     */
    public boolean isFinished() {
        return this.finished;
    }
    
    public String getToolName(){
        return tool.getName();
    }
    
    public int getToolQuantity(){
        return this.quantity;
    }

    @Override
    public void run() {
        try {
            this.finished = tool.decrementQuantity(quantity);
        } catch (InterruptedException ex) {
            Logger.getLogger(GetToolRunnable.class.getName()).log(Level.SEVERE, null, ex);
        }  
    }
}
