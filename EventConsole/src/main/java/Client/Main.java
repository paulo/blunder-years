/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package Client;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Scanner;
import org.zeromq.ZMQ;
/**
 *
 * @author SimaoDias
 */
public class Main{
    
    private static void printCommands() {
        StringBuilder res = new StringBuilder("The available commands are: \n");
        res.append("start: Starts listening to the subscriptions\n");
        res.append("commands: Displays the available commands\n");
        res.append("subscribe: Subscribes to the specified topic(s)\n");
        res.append("unsubscribe: Unsubscribes to the specified topic(s)\n");
        res.append("topics: Displays the topics provided by the server\n");
        res.append("status: Displays the topics you are subscribed to\n");
        System.out.println(res.toString());
    }

    private static void startListening(ZMQ.Socket socket) {
        while(true){
                byte[] b = socket.recv();
                System.out.print(new String(b));
        }
    }

    private static void subscribe(String[] res, ZMQ.Socket socket, ArrayList topics) {
        for(String x: res){
            if(!topics.contains(x)){
                socket.subscribe(x.toUpperCase().getBytes());
                topics.add(x);
                System.out.println("You are subscribed to " + x);
            }
            else{
                System.out.println("You are already subscribed to " + x);
            }
            
        }
    }

    private static void printTopics() {
        StringBuilder res = new StringBuilder("The provided topics are: \n");
        res.append("@ROOMMANAGER: Displays room based events\n");
        res.append("@USERMANAGER: Displays room based events\n");
        System.out.println(res.toString());
    }

    private static void unsubscribe(String[] res, ZMQ.Socket socket, ArrayList topics) {
        for(String x: res){
            if(topics.contains(x)){
                socket.unsubscribe(x.toUpperCase().getBytes());
                topics.remove(x);
                System.out.println("You are NOT subscribed to " + x);
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
        System.out.println("You will be subscribed to: ");
        for(String x: topics){
           System.out.println("> "+ x);
       }
    }
    
    public static void main(String[] args) throws IOException {
        
        ZMQ.Context ctx = ZMQ.context(1);
        ZMQ.Socket socket = ctx.socket(ZMQ.SUB); 
        Scanner in = new Scanner(System.in);
        String res[];
        String input;
        ArrayList topics = new ArrayList<>();
        
        socket.connect("tcp://localhost:"+12346);
        
        printCommands();
        input = in.nextLine();
        res = input.split(" ");
        
        
        
        while(true){
            switch(res[0].toLowerCase()){
                case("commands"):
                    printCommands();
                    break;
                case("subscribe"):
                    subscribe(Arrays.copyOfRange(res, 1, res.length), socket, topics);
                    break;
                case("unsubscribe"):
                    unsubscribe(Arrays.copyOfRange(res, 1, res.length), socket, topics);
                    break;
                case("topics"):
                    printTopics();
                    break;
                case("status"):
                    printSubscriptions(topics);
                    break;
                case("start"):
                    startListening(socket);
                    break;
                default:
                    System.out.println("Error: Unknown Command");
            }
            res = in.nextLine().split(" ");
        }  
        
        //socket.close();
        //ctx.term();
    } 

}
