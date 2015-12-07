package ServerActors;

public class Main {

    public static void main(String[] args) throws Exception {
        int default_port = 12345, port;
        
        if (args.length > 0 && args[0] != null) {
            for (String arg : args) {
                System.out.println("Parsing port number: " + arg);
                try {
                    port = Integer.parseInt(arg);
                    System.out.println("Creating server on port number: " + port);
                    new ChatServer(port).start();
                } catch (NumberFormatException e) {
                    System.out.println("Port " + arg + " is not valid.");
                }
            }
        } else {
            new ChatServer(default_port).start();
        }
    }
}
