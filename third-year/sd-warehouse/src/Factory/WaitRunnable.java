package Factory;

import java.io.BufferedWriter;
import java.io.IOException;
import java.util.List;

/**
 * @author Grupo 30
 */


public class WaitRunnable implements Runnable{
    
    private WareHouse wh;
    private BufferedWriter out;
    private List<Integer> idsList;

    
    public WaitRunnable(WareHouse warehouse, BufferedWriter out, List<Integer> list) {
        this.wh=warehouse;
        this.out=out;
        this.idsList=list;
    }
    
    @Override
    public void run() {
        try {
            try {
                wh.waitNotify(idsList);
            } catch (InterruptedException e) {
                out.write("Erro inesperado\n");
                out.flush();
            }
            out.write("Todas as tarefas foram terminadas!\n");
            out.flush();
        } catch (IOException e) {
            System.out.println("Connection closed!\n");                        
        }        
    }
    
    
}
