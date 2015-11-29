package ServerActors;

public class Main {

    //adicionar alguma lógica de correção de erros, para casos tipo quando 
    //os argumentos nao sao portas viaveis ou sao letras
    public static void main(String[] args) throws Exception {
        int default_port = 12345, port;
        
        if(args.length>0 && args[0] != null) 
            for (String arg : args) {
                port = Integer.parseInt(arg);
                new ChatServer(port).init();
        } else new ChatServer(default_port).init();
    }
}
