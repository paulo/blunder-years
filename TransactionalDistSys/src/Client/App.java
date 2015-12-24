package Client;

import Model.Account;
import Utils.Colour;

import java.util.HashMap;
import java.util.Scanner;

public class App {

    static HashMap<String, String> existing_commands = new HashMap<String, String>();
    
    public static void main(String[] args) {
        Scanner in = new Scanner(System.in);
        String res[];
        String input;
        
        initialize_existing_commands();
        printInfo("The available commands are: \n", existing_commands);
        
        input = in.nextLine().trim(); 
        res = input.split(" ");
        
        while(true){
                switch(res[0].toLowerCase()){
                    case("commands"):
                        printInfo("The available commands are: \n", existing_commands);
                        break;
                    case("deposit"):
                        System.out.println("Not supported yet.");
                        break;
                    case("withdraw"):
                        System.out.println("Not supported yet.");
                        break;
                    case("transfer"):
                        System.out.println("Not supported yet.");
                        break;
                    case("balance"):
                        System.out.println("Not supported yet.");
                        break;
                    default:
                        System.out.println("Error: Unknown Command");
                }
                res = in.nextLine().trim().split(" ");
            }  
    }
    
    public static void initialize_existing_commands(){
        existing_commands.put("commands", ": Displays the available commands\n");
        existing_commands.put("deposit", ": Deposits currency into a bank account(s)\n");
        existing_commands.put("withdraw", ": Withdraws currency from a bank account(s)");
        existing_commands.put("transfer", ": Transfers currency from one bank account to another.\n");
        existing_commands.put("balance", ": Displays the current balance of a bank account(s)\n");
    }
    
    /**
     * TODO
     * @param title
     * @param hash 
     */
    public static void printInfo(String title, HashMap<String, String> hash) {
        StringBuilder res = new StringBuilder(title);
        for(Object elem: hash.keySet()){
            res.append(Colour.ANSI_RED + ((String) elem).toUpperCase()+ Colour.ANSI_RESET +  hash.get(elem)); 
        }
        System.out.println(res.toString());
    }
    
    
    /**
     * TODO
     * @param transfer
     * @param source
     * @param destination 
     */
    public void transfer(int transfer, Account source, Account destination) {
        throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
    }
    
}

