package Factory;

import Exceptions.JobNotDefinedException;
import Exceptions.JobNotDoneException;
import Users.UserBase;
import java.io.BufferedWriter;
import java.io.IOException;

/**
 * @author Grupo 30
 */


public class RequestJobRunnable implements Runnable{
    
    private WareHouse wh;
    private BufferedWriter out;
    private String taskName;
    private UserBase userBase;
    private String clientname;
    
    public RequestJobRunnable(WareHouse warehouse, BufferedWriter out, String task,
                              UserBase ub, String name) {
        this.wh=warehouse;
        this.out=out;
        this.taskName=task;
        this.userBase=ub;
        this.clientname=name;
    }
    
    @Override
    public void run() {
        try {
            try {
                int id=wh.executeJob(taskName.toLowerCase());
                userBase.insertJob(id, clientname);
                out.write("_________________________________\n");
                out.write("Requisição concluída com sucesso.\n");
                out.write("Tarefa: "+taskName+" ID: "+id+"\n");
                out.write("_________________________________\n\n");
                out.flush();
            } catch(JobNotDefinedException ex){
                out.write("A tarefa "+ex.getMessage()+" nao esta definida.\n");
                out.flush();
            } catch (InterruptedException ex) {
                out.write("Erro inesperado\n");
                out.flush();
            } catch (JobNotDoneException ex) {
                out.write("A tarefa "+ex.getMessage()+" demorou muito a requisitar os objetos e foi cancelada.\n");
                out.flush();
            }
        }catch(IOException e) {
            System.out.println("Connection closed!\n");
        }       
    }
    
    
}
