package Client;

import java.io.IOException;
import org.zeromq.ZMQ;

public class Main {

    private static void startListening(ZMQ.Socket socket) {
        while (true) {
            byte[] b = socket.recv();
            System.out.print(new String(b));
        }
    }

    public static void main(String[] args) throws IOException {
        int default_port = 12346, port;
        ZMQ.Context ctx = ZMQ.context(1);
        ZMQ.Socket socket = ctx.socket(ZMQ.SUB);

        if (args.length > 0 && args[0] != null) {
            System.out.println("Parsing port number: " + args[0]);
            try {
                port = Integer.parseInt(args[0]);
                System.out.println("Connecting to server on port number: " + port);
                InputReader commands = new InputReader(socket);
                commands.start();

                socket.connect("tcp://localhost:" + port);
            } catch (NumberFormatException e) {
                System.out.println("Port " + args[0] + " is not valid.");
            }
        } else {
            InputReader commands = new InputReader(socket);
            commands.start();

            socket.connect("tcp://localhost:" + default_port);
        }
        startListening(socket);
        socket.close();
        ctx.term();
    }

}
