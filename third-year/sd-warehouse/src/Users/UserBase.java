package Users;

import Exceptions.*;
import java.util.*;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;

/**
 * @author Grupo 30
 */


public class UserBase {
    
    /**
     * @param users Lista de utilizadores
     * @param changeUser Lock associado à variável users
     * @param changeJobs Lock associado à variável jobsUsers
     * @param jobsUsers Lista de identificadores de tarefa e utilizadores associados
     */
    
    private final Map<String, User> users;
    private final Lock changeUser, changeJobs;
    private final Map<Integer,String> jobsUsers;
    
    public UserBase(){
        users = new HashMap<>();
        changeUser = new ReentrantLock();
        jobsUsers = new HashMap<>();
        changeJobs = new ReentrantLock();
        
    }
    
    
    /**
     * Regista um novo utilizador na UserBase
     * 
     * @param username username do respetivo utilizador
     * @param password password do respetivo utilizador
     * @throws Exceptions.UserAlreadyExistsException
     */
    public void register(String username, String password) throws UserAlreadyExistsException {
        changeUser.lock();
        try {
            if(users.containsKey(username)) throw new UserAlreadyExistsException(username);
            else users.put(username, new User(username, password));
        } finally {
            changeUser.unlock();
        }           
    }
    
    /**
     * Verifica se o username recebido está registado e a password recebida
     * corresponde com a password registada. Caso correspondam, autentica o utilizador
     * indicado.
     * 
     * @param username username a autenticar
     * @param password password do username a autenticar
     * @throws Exceptions.UserIsAlreadyLoggedInException
     * @throws Exceptions.UserDoesNotExistException
     * @throws Exceptions.WrongPasswordException
     */
    public void logIn(String username, String password) throws UserIsAlreadyLoggedInException, UserDoesNotExistException, WrongPasswordException{
        User aux;
        if(users.containsKey(username) == false) throw new UserDoesNotExistException(username);
        else {
            aux = users.get(username);
            aux.getLock().lock();
            try {
                if(aux.getState() == true) throw new UserIsAlreadyLoggedInException(username);
                else {
                    if(aux.getPass().equals(password) == false) throw new WrongPasswordException();
                    aux.logIn();
                }
            } finally {
                aux.getLock().unlock();
            }
        }
    }
    
    
    /**
     * Faz logout do utilizador cujo username é recebido como parâmetro
     * 
     * @param username username do utilizador a fazer logout
     * @throws Exceptions.UserDoesNotExistException
     * @throws Exceptions.UserIsNotLoggedInException
     */
    public void logOut(String username) throws UserDoesNotExistException, UserIsNotLoggedInException{
        User aux;
        if(users.containsKey(username) == false) throw new UserDoesNotExistException(username);
        aux = users.get(username);
        aux.getLock().lock();
        try {
            if(aux.getState() == false) throw new UserIsNotLoggedInException(username);
            aux.logOut();
        } finally {
            aux.getLock().unlock();
        } 
    }
    
    
    /**
     * Insere uma nova tarefa activa na UserBase
     * 
     * @param id identificador da tarefa a associar com o respetivo utilizador
     * @param username username do utilizador a associar com o id da tarefa activa
     */
    public void insertJob(int id, String username) {
        changeJobs.lock();
        try {
            if (!jobsUsers.containsKey(id)) {
                jobsUsers.put(id, username);
            }
        } finally {
            changeJobs.unlock();
        }
    }
    
    
    /**
     * Remove uma tarefa activa da UserBase
     * 
     * @param id identificador da tarefa a remover das tarefas activas
     */
    public void removeJob(int id) {
        changeJobs.lock();
        try {
            if (jobsUsers.containsKey(id)) {
                jobsUsers.remove(id);
            }
        } finally {
            changeJobs.unlock();
        }
    }
    
    /**
     * Determina o utilizador que requisitou a tarefa recebida como parâmetro
     * 
     * @param id identificador da tarefa activa
     * @return devolve o nome do utilizador responsável por requisitar a tarefa recebida
     */
    public String findRequester(int id) {
        String username="";
        changeJobs.lock();
        try {
            if (jobsUsers.containsKey(id)) {
                username=jobsUsers.get(id);
            }
            return username;
        } finally {
            changeJobs.unlock();
        }        
    }
    
    /**
     * Determina todas as tarefas activas de um utilizador
     * 
     * @param username username do utilizador cujas tarefas activas pretendemos identificar
     * @return devolve uma lista com todos os ids das tarefas activas do utilizador
     */
    public List<Integer> findRequests(String username) {
        changeJobs.lock();
        List<Integer> listIDs = new ArrayList<>();
        try {
            for (Map.Entry<Integer, String> entrySet : jobsUsers.entrySet()) {
                Integer id = entrySet.getKey();
                String clientname = entrySet.getValue();
                if (clientname.equals(username))
                    listIDs.add(id);               
            }
            return listIDs;
        } finally {
            changeJobs.unlock();
        }
    }
}