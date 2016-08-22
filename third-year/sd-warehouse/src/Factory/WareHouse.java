package Factory;

import Exceptions.*;
import Users.UserBase;
import java.util.*;
//import java.util.concurrent.locks.Condition;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;

/**
 * Representa um armazém
 * @author Grupo 30
 */


public class WareHouse {
    
    /**
     * @param tools Lista de ferramentas
     * @param definedJobs Lista de todas as tarefas definidas no armazém
     * @param activeJobs Lista de tarefas ativas e repetivos identificadores
     * @param lastId Identificador mais recente de tarefas ativas
     */
    
    private Map<String, Tool> tools;
    private Map<String, Job> definedJobs; //guarda todas as tarefas ja definidas na wh
    private Map<Integer, Job> activeJobs; //guarda todos os ids e jobs activos
    private int lastId; //ultimo id atribuido a um job activo
    private Lock changeTool, changeJob, changeActive;
    //private Condition hasTools;

    public WareHouse(){
        this.tools = new HashMap<>();
        this.activeJobs = new HashMap<>();
        this.definedJobs = new HashMap<>();
        lastId=0;
        changeJob = new ReentrantLock();
        changeTool = new ReentrantLock();
        changeActive = new ReentrantLock();
        //hasTools = changeTool.newCondition();
    }
    
    //abastece a tool com uma dada quantidade no stock
    
    /**
     * Abastece o armazém com certa quantidade de dada ferramenta
     * 
     * @param name Nome da ferramenta
     * @param quantity Quantidade da ferramenta
     * @throws InterruptedException 
     */
    public void supplyTool(String name, int quantity) throws InterruptedException{      
        Tool aux;
        if(this.tools.containsKey(name)){//ter em atençao para meter o lock na tool
            aux = this.tools.get(name);
            aux.incrementQuantity(quantity);
        } else {
            changeTool.lock();
            try {
                aux = new Tool(name, quantity);
                tools.put(name, aux);
            } finally {
                changeTool.unlock();
            }
        }
    }
        
    //para um dado nome de um job, cria um job, vê a lista de tools necessarias
    //e tem de esperar que estejam todas disponiveis
    //se nao estiver definido lanca excepcao
    //este vai precisar de signals e assim das tools
    //para saber quando pode decrementar as tools necessarias
    
    /**
     * Inicia uma tarefa anteriormente definida
     * 
     * @param name Nome da tarefa
     * @return Identificador atribuído à tarefa que foi iniciada
     * @throws JobNotDefinedException
     * @throws InterruptedException
     * @throws JobNotDoneException 
     */
    public int executeJob(String name) throws JobNotDefinedException, InterruptedException, JobNotDoneException {            
        ArrayList<Thread> threadList = new ArrayList<>();
        ArrayList<GetToolRunnable> getToolList = new ArrayList<>();
        boolean done = true;
                
        int auxId;
        if (definedJobs.containsKey(name)) {
            Job requestedJob = definedJobs.get(name).clone();
            Map<String, Integer> usedTools = definedJobs.get(name).getToolList();
            for (Map.Entry<String, Integer> entrySet : usedTools.entrySet()) {
                String key = entrySet.getKey();
                Integer value = entrySet.getValue();
                if (!tools.containsKey(key)) {
                    changeTool.lock();
                    try {
                        Tool newTool = new Tool(key);                               //se nao existir a ferramenta necessaria, é criada uma nova ferramenta com qtd 0
                        tools.put(key, newTool);
                    } finally {
                        changeTool.unlock();
                    }
                }
                Tool aux = tools.get(key);
                GetToolRunnable gt = new GetToolRunnable(aux, value);
                getToolList.add(gt);
                Thread taux = new Thread(gt);
                threadList.add(taux);
                taux.start();
            }
            
            for(Thread t:threadList){
                t.join();
            }
            
            for(Iterator<GetToolRunnable> git = getToolList.iterator();git.hasNext() && done;){
                if(git.next().isFinished() == false) done = false; 
            }        
            
            if(!done){
                for(GetToolRunnable g: getToolList){
                    if(g.isFinished()){
                        supplyTool(g.getToolName(),g.getToolQuantity());  
                    }
                }
                throw new JobNotDoneException(name);
            }
            else {
                changeActive.lock();
                try {
                    lastId++;
                    auxId = lastId;
                    requestedJob.setActive(true);
                    activeJobs.put(auxId, requestedJob);
                } finally {
                    changeActive.unlock();
                }
                return auxId;
            }
        }
        else
            throw new JobNotDefinedException(name);
    }
    
    
    
    //define um novo job com um nome dado e os materiais e qtds necessarios
    //lanca excepcao se ja existir
    
    /**
     * Define uma nova tarefa
     * 
     * @param name Nome da tarefa
     * @param tools Lista de ferramentas necessárias para a tarefa
     * @throws JobDefinedException 
     */
    public void defineJob(String name, Map<String, Integer> tools) throws JobDefinedException{
        
        changeJob.lock();
        try {
            if (definedJobs.containsKey(name))
                throw new JobDefinedException(name);
            else{
                Job newJob = new Job(name, tools);
                definedJobs.put(name,newJob);
            }
        } finally {
            changeJob.unlock();
        }
    }
    
    
    
    //termina um job com o id recebido
    //para todas as tools que o job usa, tem de voltar a colocar no stock
    //lanca excepcao se nao existir nenhum job activo com aquele id
    
    /**
     * Termina uma dada tarefa ativa
     * 
     * @param id Identificador de tarefa
     * @throws ActiveJobNotFoundException
     * @throws InterruptedException 
     */
    public void endJob(int id) throws ActiveJobNotFoundException, InterruptedException {
        changeActive.lock();
        try {
            if (activeJobs.containsKey(id)) {
                Map<String, Integer> usedTools = activeJobs.get(id).getToolList();
                for (Map.Entry<String, Integer> entrySet : usedTools.entrySet()) {
                    String key = entrySet.getKey();
                    Integer value = entrySet.getValue();
                    tools.get(key).incrementQuantity(value);
                }
                activeJobs.get(id).setOff();
                activeJobs.remove(id);
            }
            else {
                throw new ActiveJobNotFoundException();
            }
        } finally {
            changeActive.unlock();
        }
    }
    
    /**
     * @param jobs Lista de identificadores de tarefa
     * @throws InterruptedException 
     */
    public void waitNotify(List<Integer> jobs) throws InterruptedException {
        for (int j : jobs) {
            if (activeJobs.containsKey(j))
                activeJobs.get(j).checkOff();
        } 
    }
    
    /**
     * Gera a descrição de uma dada tarefa ativa
     * 
     * @param id Identificador de tarefa
     * @return Descrição da tarefa
     */
    public String listActiveJob(int id){
        changeActive.lock();
        try {
            StringBuilder str = new StringBuilder();
            str.append("Id Tarefa: ");
            str.append(id);
            str.append("\t\tNome Tarefa: ");
            str.append(activeJobs.get(id).getName());
            str.append("\n");
            return str.toString();
        } finally {
            changeActive.unlock();
        }
    }
    
    /**
     * Gera a descrição de todas as tarefas ativas
     * 
     * @param ub Base de dados de utilizadores
     * @return Descrição de todas as tarefas ativas
     * @throws NoActiveJobsException 
     */
    public String listAllActiveJobs(UserBase ub) throws NoActiveJobsException{
        changeActive.lock();
        try {
            if(activeJobs.isEmpty()) throw new NoActiveJobsException();
            StringBuilder str = new StringBuilder();
        
            for (Map.Entry<Integer, Job> entrySet : activeJobs.entrySet()) {
                int id = entrySet.getKey();
                String name = entrySet.getValue().getName();

                str.append("Id Tarefa: ");
                str.append(id);
                str.append("\tNome Tarefa: ");
                str.append(name);
                str.append("\tNome Utilizador: ");
                str.append(ub.findRequester(id));
                str.append("\n");                
            }       
            return str.toString();
        } finally {
            changeActive.unlock();
        }
    }
}
