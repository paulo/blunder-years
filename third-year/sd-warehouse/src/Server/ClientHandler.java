package Server;

import static Errors.ErrorList.erro;
import Exceptions.*;
import Factory.RequestJobRunnable;
import Factory.WaitRunnable;
import Factory.WareHouse;
import Users.UserBase;
import java.io.*;
import java.net.ServerSocket;
import java.net.Socket;
import java.util.*;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * @author Grupo 30
 */


public class ClientHandler implements Runnable {

    private final Socket client;
    private final WareHouse warehouse;
    private final UserBase userbase;
    private final BufferedReader in;
    private final BufferedWriter out;
    private boolean state;
    private String clientname;
    private ServerSocket server;

    //Construtor para clientes remotos
    public ClientHandler(Socket client, WareHouse wh, UserBase ub) throws IOException{
        this.client = client;
        in = new BufferedReader(new InputStreamReader(client.getInputStream()));
        out = new BufferedWriter(new OutputStreamWriter(client.getOutputStream())); 
        warehouse = wh;
        userbase = ub;
        state = false;
        clientname= null;
        server = null;
    }
    
    //Construtor para cliente local
    public ClientHandler(WareHouse wh, UserBase ub, ServerSocket s){
        client = null;
        in = new BufferedReader(new InputStreamReader(System.in));
        out = new BufferedWriter(new OutputStreamWriter(System.out));
        warehouse = wh;
        userbase = ub;
        state = false;
        clientname= null;
        server = s;
    }

    
    @Override
    public void run() {
        String comando;
        boolean connection = true;
        
        try {     
            
            if (client == null){
                /**
                 * se o cliente for local imprime no System.out
                 */
                out.write("Comando> \n");
                out.flush();
                while(connection && (comando = in.readLine())!=null){
                    if(comando.toLowerCase().equals("sair")) {
                       if(state==true) logOutUser();
                        connection=false;
                        out.write("Sessão local terminada\n");
                        out.flush();
                    }else {
                        execute(comando);
                        out.write("Comando> \n");
                        out.flush();
                    }
                } 
            }else {
                /**
                 * Se o cliente não for local envia resposta para o do cliente
                 */
                while(connection && (comando = in.readLine())!=null){
                    if(comando.toLowerCase().equals("sair")) {
                        if(state==true) logOutUser();
                        connection=false;
                        out.write("Sessão terminada\n");
                        out.flush();
                    }
                    else execute(comando);   
                }
            }

            if(client != null){
                client.shutdownInput();
                client.shutdownOutput();
                client.close();
            } else {
                server.close();
            }

        } 
        catch (IOException ex) {
            Logger.getLogger(ClientHandler.class.getName()).log(Level.SEVERE, null, ex);
        }

    }
    
    /**
     * Regista um novo utilizador
     * 
     * @param username Nome de utilizador
     * @param password Password do utilizador
     */
    private void registerNewUser(String username, String password){//e necessario informar o user que correram bem as cenas
        try {
            try {
                userbase.register(username, password);//poder-se-ia ter feito tipo uma conversa entre o servidor e o cliente
                out.write("Utilizador registado com sucesso.\n");
                out.flush();
            } catch(UserAlreadyExistsException e){
                out.write("O utilizador "+e.getMessage()+" ja esta registado.\n");
                out.flush();
            } 
        } catch (IOException e) {
            System.out.println("Connection closed!\n");
        }
    }
    
    /**
     * Iniciar a sessão de um dado utilizador
     * @param username Nome de utilizador
     * @param password Password do utilizador
     */
    private void logInUser(String username, String password){
        if(state==false){
            try {
                try {
                    userbase.logIn(username, password);
                    state = true;
                    clientname = username;
                    out.write("LogIn efetuado com sucesso.\n");
                    out.flush();
                } catch(UserIsAlreadyLoggedInException e){
                    out.write("O utilizador "+e.getMessage()+" ja esta logged in.\n");
                    out.flush();
                } catch (UserDoesNotExistException ex) {
                    out.write("O utilizador "+ex.getMessage()+" nao esta registado.\n");
                    out.flush();
                } catch (WrongPasswordException ex) {
                    out.write("A password esta errada.\n");
                    out.flush();
                }
            } catch (IOException e) {
                System.out.println("Connection closed!\n");
            }
        } else {
            try {
                out.write("Ja existe um utilizador conectado.\n");
                out.flush();
            } catch (IOException ex) {
                System.out.println("Connection closed!\n");
            }
        }
    }
    
    /**
     * Termina a sessão de um utilizador
     */
    private void logOutUser(){
        try {
            try {
                userbase.logOut(clientname);
                state = false;
                clientname = null;
                out.write("LogOut efetuado com sucesso.\n");
                out.flush();
            } catch(UserIsNotLoggedInException e){
                out.write("O utilizador "+e.getMessage()+" nao esta logged in.\n");
                out.flush();
            } catch (UserDoesNotExistException ex) {
                out.write("O utilizador "+ex.getMessage()+" nao esta registado.\n");
                out.flush();
            }
        } catch (IOException e) {
            System.out.println("Connection closed!\n");
        }
    }
    
    
    /**
     * Abastece um armazém com dadas ferramentas
     * @param tools Lista de ferramentas
     */
    public void supply(Map<String,Integer> tools) {
        
        try {
            for (Map.Entry<String, Integer> entrySet : tools.entrySet()) {
                String name = entrySet.getKey();
                Integer quant = entrySet.getValue();
                try {
                    warehouse.supplyTool(name, quant);
                } catch (InterruptedException ex) {
                    out.write("Erro inesperado\n");
                    out.flush();
                }
            }        
            out.write("Abastecimento concluído!\n");
            out.flush();
        } catch (IOException e) {
            System.out.println("Connection closed!\n");
        }
    }
    
    /**
     * Define uma nova tarefa
     * 
     * @param name Nome da tarefa
     * @param mats Lista de ferramentas necessárias para a tarefa
     */
    public void defineNewJob(String name, Map<String, Integer> mats) {
        try {
            try {
                warehouse.defineJob(name.toLowerCase(), mats);
                out.write("Nova tarefa definida!\n");
                out.flush();
            } catch(JobDefinedException ex){
                out.write("Tarefa "+name+" já está definida!\n");
                out.flush();
            }
        }catch(IOException e) {
            System.out.println("Connection closed!\n");
        }
    }

    /**
     * Requisita uma dada tarefa
     * @param taskName Nome da tarefa
     */
    public void requestJob(String taskName){
        RequestJobRunnable rjr = new RequestJobRunnable(warehouse, out, taskName,
                                                        userbase, clientname);
        Thread requestThread = new Thread(rjr);
        
        requestThread.start();
    }
    
    /**
     * Termina uma dada tarefa
     * 
     * @param id Identificador de tarefa
     */
    public void terminateJob(int id) {
        try {
            if (userbase.findRequester(id).equals(clientname) || client==null) {
                try {
                    warehouse.endJob(id);
                    userbase.removeJob(id);
                    out.write("Tarefa "+id+" terminada com sucesso!\n");
                    out.flush();
                } catch (InterruptedException ex) {
                    out.write("Erro inesperado\n");
                    out.flush();
                } catch(ActiveJobNotFoundException ex){
                    out.write("Não existe nenhuma tarefa activa com identificador "+id+"!\n");
                    out.flush();
                }
            }
            else {
                out.write("A tarefa com id: "+id+" não existe ou não foi definida por si!\n");
                out.flush();
            }           
        }catch(IOException e) {
            System.out.println("Connection closed!\n");
        }     
    }
    
    /**
     * @param ids Lista de identificadores de tarefa
     */
    public void waitToNotify(List<Integer> ids) {
        
        try {
            List<Integer> listIds = userbase.findRequests(clientname);
            if (!listIds.containsAll(ids)) {
                ids.retainAll(listIds);
            }
            if (!ids.isEmpty()) {

                WaitRunnable wr = new WaitRunnable(warehouse, out, ids);
                Thread waitThread = new Thread(wr);

                waitThread.start();

                out.write("Lista de espera definida com sucesso!\n");
                out.write("IDs: ");
                for (int i : ids) {
                    out.write(i+" ");
                }
                out.write("\n");
                out.flush();
            }
            else {
                out.write("Nenhuma das tarefas inseridas foi definida por si!\n");
                out.flush();                
            }
        } catch (IOException ex) {
            System.out.println("Connection closed!\n");
        }
    }
        
    /**
     * Informa que um certo comando não existe
     * @throws IOException 
     */
    private void commandNotFound() throws IOException{
        out.write("Comando não existe\n");
        out.flush();
    }

    /**
     * Gera uma descrição de tarefas
     */
    public void listJobs(){
        try {    
            if (client!=null) {
                List<Integer> listIds = userbase.findRequests(clientname);
                if(listIds.isEmpty()){
                    out.write("Nao existem tarefas activas\n");
                    out.flush();
                }
                else {
                    for(int i : listIds){
                        out.write(warehouse.listActiveJob(i));
                    }
                    out.flush();
                }
            }
            else {
                
                out.write(warehouse.listAllActiveJobs(userbase));
                out.flush();
            }
        } catch (IOException ex) {
            System.out.println("Connection closed!\n");
        } catch (NoActiveJobsException ex) {
            try {
                out.write("Nao existem tarefas activas\n");
                out.flush();
            } catch (IOException ex1) {
                System.out.println("Connection closed!\n");
            }
        }
        
    }
    
    

    
    
    /**
     * @param args raw input do utilizador
     * @param nargs numero de argumentos mandados em input pelo utilizador
     * @param start em que indice comecar a verificar se sao inteiros
     * @param jump de quanto em quanto indice queremos avancar para testar o proximo inteiro
     * @return 
     */
    public boolean checkValidIntegers(String[] args, int nargs, int start, int jump){
        boolean valid = true;
            for(int i=start;i<nargs && valid;i+=jump){
                valid=testNumeric(args[i]); //testa se os argumentos que devem
            } 
            if (!valid) {
                try {
                    out.write(erro(3));
                    out.flush();
                } catch (IOException ex) {
                    System.out.println("Connection error!\n");
                }
            }
        return valid;
    }
    
     /**
     * @param nArgs número de argumentos mandados em input pelo utilizador
     * @param validNArgs número de argumentos necessarios para a funcao correr
     * @return retorna falso caso numero de argumentos do input nao coincidir com o número
     * de argumentos necessarios para a funcao; retorna verdadeiro caso contrario
     */
    public boolean checkTotalArgs(int nArgs, int validNArgs){
        if (nArgs!=validNArgs) {
            try {
                out.write(erro(2));
                out.flush();                        
                return false;
            } catch (IOException ex) {
                System.out.println("Connection error!\n");
            }
        }
        return true; 
    }
    
    /**
     * @param nArgs número de argumentos mandados em input pelo utilizador
     * @param minimumValidNArgs número de argumentos necessarios para a funcao correr
     * @param mustBeOdd true se é necessario que o numero total de argumentos seja impar (Ex: abastecer baldes 10)
     * @param mustBeEven true se é necesario que o numero total de argumentos seja par (EX: Definir martelar martelo 1 pregos 2)
     * @return retorna falso caso numero de argumentos do input nao coincidir com o número
     * de argumentos necessarios para a funcao; retorna verdadeiro caso contrario
     */
    public boolean checkMinimumArgs(int nArgs, int minimumValidNArgs, boolean mustBeOdd, boolean mustBeEven){
        try {    
            if ((mustBeOdd && nArgs%2 == 0) ||
                (mustBeEven && nArgs%2 != 0) ||
                (nArgs<minimumValidNArgs)) {
                
                out.write(erro(2));
                out.flush();                        
                return false;
            }
            return true;
        } catch (IOException ex) {
            System.out.println("Connection closed!\n");
            return false;
        }
    }
    
    //testa se uma string é convertivel para inteiro
    
    /**
     * Verifica se uma dada palavra corresponde a um número
     * 
     * @param number Palavra que se quer testar
     * @return Booleano que indica se a palavra corresponde a um número
     */
    public boolean testNumeric(String number) {
        try {
            int aux=Integer.parseInt(number);
        } catch (NumberFormatException e) {
            return false;
        }
        return true;
    }
    

    
    
    //metodo que faz parse do comando e chama o método correspondente
    
    /**
     * Interpreta e ordena a execução de um dado comando
     * 
     * @param command Comando que se pretende executar
     * @throws IOException 
     */
    public void execute(String command) throws IOException{
        String[] args = command.split("\\s+");
        int nargs = args.length;
        String operation= args[0].toLowerCase();
        int i, quantity;
        
        
        if(state == false && (operation.equals("login") || operation.equals("registar")) == false){
            out.write("Ainda nao esta logged in.\n");
            out.flush();
            return;
        }     
         
        switch(operation) {
            case "login": 
                if (checkTotalArgs(nargs, 3)){
                logInUser(args[1],args[2]);
                }
                break;              
            case "registar":
                if (checkTotalArgs(nargs, 3)){
                registerNewUser(args[1],args[2]);
                }
                break;
            case "abastecer":
                if (checkMinimumArgs(nargs, 3, true, false))
                    if (checkValidIntegers(args, nargs, 2, 2)){
                        Map<String,Integer> tools = new HashMap<>();
                        for(i=1;i<nargs;i+=2){
                            quantity=Integer.parseInt(args[i+1]);
                            tools.put(args[i], quantity);
                        }
                        supply(tools);
                    }
                break;
            case "definir":
                if (checkMinimumArgs(nargs, 4, false, true))
                    if ( checkValidIntegers(args, nargs, 3, 2)){
                        Map<String, Integer> materials = new HashMap<>();
                        for(i=2;i<nargs;i+=2){
                            quantity=Integer.parseInt(args[i+1]);
                            materials.put(args[i], quantity);
                        }
                        defineNewJob(args[1],materials);
                    }
                break;
            case "requisitar":
                for(i=1;i<nargs;i++) {
                    requestJob(args[i]);
                }
                break;
            case "devolver":
                if (checkValidIntegers(args, nargs, 1, 1)){
                    for(i=1;i<nargs;i++){
                        terminateJob(Integer.parseInt(args[i]));
                    }                    
                }
                break;
            case "esperar":
                if (checkValidIntegers(args, nargs, 1, 1)){
                    List<Integer> tasks= new ArrayList<>();
                    for(i=1;i<nargs;i++){
                        tasks.add(Integer.parseInt(args[i]));
                    }
                    waitToNotify(tasks);
                }
                break;
            case "listar": 
                if (checkTotalArgs(nargs, 1)){
                    listJobs();
                }
                break;
            case "logout":
                if (checkTotalArgs(nargs, 1)){
                    logOutUser();
                }
                break;
            default: 
                out.write(erro(4));
                out.flush();
                break;
        }
    }
    


}
