package examples;

import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;
import se.sics.jasper.Query;
import se.sics.jasper.SICStus;
import se.sics.jasper.SPException;
import se.sics.jasper.SPPredicate;

/**
 *
 * @author paulo
 */
public class Example {
    
    //Java Object to Interact with SICStus virtual Machine
    private SICStus sp;
    
    public Example(){
        try {
            sp = new SICStus();
        } catch (SPException ex) {
            Logger.getLogger(Example.class.getName()).log(Level.SEVERE, null, ex);
        }
    }
    
    //Initalize SICStus virtual machine
    public void loadSICStus() throws SPException {
        sp = new SICStus();
    }
    
    public void insertNewPredicate(String predicateName, int numOfTerms, String variable){
        try {      
            SPPredicate predicate = new SPPredicate (sp, predicateName, numOfTerms, variable);
        } catch (SPException ex) {
            Logger.getLogger(Example.class.getName()).log(Level.SEVERE, null, ex);
        }
    }
        
    //esta mal, arranjar
    public void askQuestion(String predicateName, List<String> predicateArgs){
        String query = predicateName+"(";
        
        for(Iterator itr = predicateArgs.iterator(); itr.hasNext();){
            query = query.concat(", "+itr);
        }
    }
    
    public void askQuestion(String queryString){
        //String queryS = “predicate(‘term’,X).”;

        //predicate(‘term’,X).
        HashMap map = new HashMap();
        Query query;
        try {
            query = sp.openPrologQuery(queryString,map);
        

            while (query.nextSolution()) {
                System.out.println(map.toString());
        }
        

        query.close();
        
        } catch (Exception ex) {
            Logger.getLogger(Example.class.getName()).log(Level.SEVERE, null, ex);
        } 
    }
    
    //Load SICStus script
    public void loadSICStusScript(String pathToFile) throws SPException{
        sp.load(pathToFile);
    }
    
    public static void main(String[] args){
          
        Example ex1 = new Example();
        
        try {
            ex1.loadSICStusScript("/home/paulo/SRCRTP2/src/aulas/ficha01.pl");
        } catch (SPException ex) {
            Logger.getLogger(Example.class.getName()).log(Level.SEVERE, null, ex);
        }

        ex1.askQuestion("filho(carlos,X).");
        
        //ex1.askQuestion("listing(filho).");
        
        ex1.insertNewPredicate("listing", 1, "filho");
    }
}
