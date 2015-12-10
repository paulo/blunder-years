/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package Client;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Scanner;
import java.util.regex.Pattern;
import org.zeromq.ZMQ;

/**
 *
 * @author SimaoDias
 */
public class InputReader extends Thread{
    
    static ZMQ.Socket socket;
    static ArrayList topics_subscribed = new ArrayList<>();
    static HashMap<String, String> existing_topics = new HashMap<String, String>();
    static HashMap<String, String> existing_commands = new HashMap<String, String>();
    static String userPattern = "@<[^>]+>";
    
    public static final String ANSI_RESET = "\u001B[0m";
    public static final String ANSI_BLACK = "\u001B[30m";
    public static final String ANSI_RED = "\u001B[31m";
    public static final String ANSI_GREEN = "\u001B[32m";
    public static final String ANSI_YELLOW = "\u001B[33m";
    public static final String ANSI_BLUE = "\u001B[34m";
    public static final String ANSI_PURPLE = "\u001B[35m";
    public static final String ANSI_CYAN = "\u001B[36m";
    public static final String ANSI_WHITE = "\u001B[37m";
    
    InputReader(ZMQ.Socket socket){
        this.socket = socket;
    }
    
    private static void printInfo(String title, HashMap hash){
        StringBuilder res = new StringBuilder(title);
        for(Object elem: hash.keySet()){
            res.append(ANSI_RED + ((String) elem).toUpperCase()+ ANSI_RESET +  hash.get(elem)); 
        }
        System.out.println(res.toString());
    }
    
    private static void subscribe(String[] res) {
        for(String x: res){
            if(existing_topics.containsKey(x) || Pattern.matches(userPattern, x)){
                if(!topics_subscribed.contains(x)){
                    socket.subscribe(x.toUpperCase().getBytes());
                    topics_subscribed.add(x);
                    System.out.println("You are subscribed to " + x);
                }
                else{
                    System.out.println("You are already subscribed to " + x);
                }
            }            
        }
    }
    
    private static void unsubscribe(String[] res) {
        for(String x: res){
            if(topics_subscribed.contains(x)){
                socket.unsubscribe(x.toUpperCase().getBytes());
                topics_subscribed.remove(x);
                System.out.println("You are now NOT subscribed to " + x);
            }
            else{
                System.out.println("You are already NOT subscribed to " + x);
            }
        }
    }
    
    private static void printSubscriptions(ArrayList<String> topics) {
        if(topics.size()==0){
            System.out.println("There are no current subscriptions");
        }
        else{
            System.out.println("You are subscribed to: ");
            for(String x: topics){
               System.out.println("> "+ ANSI_RED + x + ANSI_RESET);
            }
        }
    }
    
    @Override
    public void run(){
        
        Scanner in = new Scanner(System.in);
        String res[];
        String input;
        
        initialize_existing_topics();
        initialize_existing_commands();
        printInfo("The available commands are: \n", existing_commands);
        
        input = in.nextLine();
        res = input.split(" ");
        
        while(true){
                switch(res[0].toLowerCase()){
                    case("commands"):
                        printInfo("The available commands are: \n", existing_commands);
                        break;
                    case("subscribe"):
                        doSubscriptions(Arrays.copyOfRange(res, 1, res.length), true);
                        break;
                    case("unsubscribe"):
                        doSubscriptions(Arrays.copyOfRange(res, 1, res.length), false);
                        break;
                    case("topics"):
                        printInfo("The provided topics are: \n", existing_topics);
                        break;
                    case("status"):
                        printSubscriptions(topics_subscribed);
                        break;
                    default:
                        System.out.println("Error: Unknown Command");
                }
                res = in.nextLine().split(" ");
            }  
        }

    private void initialize_existing_topics() {
        existing_topics.put("@login", ": Displays login and logout related events.\n");
        existing_topics.put("@user", ": Displays user creation and removal events.\n"); 
        existing_topics.put("@<username>", ": Displays the event related to the specified username\n");
        existing_topics.put("@private_room_management", ":  Displays private rooms creation and removal events.\n");
        existing_topics.put("@public_room_management", ": Displays public rooms creation and removal events.\n");
        existing_topics.put("@room_management", ": Displays public and private rooms creation and removal events.\n");
        existing_topics.put("@public_room_users", ": Displays users joinning and leaving public rooms.\n");        
        existing_topics.put("@private_room_users", ": Displays users joinning and leaving private rooms.\n");
        existing_topics.put("@room_users", ": Displays users joinning and leaving public and private rooms.\n");
        existing_topics.put("@all", ": Displays all existing events.\n");
    }

    private void initialize_existing_commands() {
        existing_commands.put("commands", ": Displays the available commands\n");
        existing_commands.put("subscribe", ": Subscribes to the specified topic(s)\n");
        existing_commands.put("unsubscribe", ": Unsubscribes to the specified topic(s)\n");
        existing_commands.put("topics", ": Displays the topics provided by the server\n");
        existing_commands.put("status", ": Displays the topics you are subscribed to\n");
    }

    private void doSubscriptions(String[] topics, boolean addSub) {
        String res = new String();
        String[] splitRes;
       
        for(String topic: topics){
            switch(topic.toLowerCase()){
                case "@login": 
                case "@user":
                case "@private_room_management":
                case "@public_room_management":
                case "@public_room_users":
                case "@private_room_users":
                    res += topic + " ";
                    break;
                case "@room_management":
                    res += "@private_room_management @public_room_management";
                    break;
                case "@room_users":
                    res += "@public_room_users @private_room_users";
                    break;
                case "@all":
                    for(String elem: existing_topics.keySet()){
                        if(!elem.toLowerCase().equals("@all"))
                        res += elem + " ";
                    }
                    break;
                default:
                    if(Pattern.matches(userPattern, topic)){
                        res += topic + " ";
                    }
                    else{
                        System.out.println("There is no such topic: " + topic + ".");
                    }   
            }
        }
        splitRes = res.split(" ");
        if(addSub){
            subscribe(splitRes);  
        }
        else{
            unsubscribe(splitRes);
        }
    }
}
