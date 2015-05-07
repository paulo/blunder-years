import java.io.Console;
import java.io.FileInputStream;
import java.io.InputStream;
import java.text.ParseException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Scanner;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;
import java.util.stream.IntStream;
import se.sics.jasper.Query;
import se.sics.jasper.SICStus;
import se.sics.jasper.SPException;

/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
/**
 *
 * @author ruioliveiras
 */
public class MainStand {

    private int numberIncerto = 0;
    private int numberIntertito = 0;
    private final SICStus sp;

    public MainStand(String pathlib) throws SPException {
        sp = new SICStus();
        sp.load(pathlib);
    }

    public void interpExecute(String cmd, String predicade, String... args) throws ParseException, Exception {
        int i;
        switch (cmd) {
            case "demo":
                // in this case the predicade is a normal word to the expression
                this.executePrint("demo(" + predicade + " " + String.join(" ", args) + ",R).");
                break;
            case "evolucaoPos":
                this.executePrint("evolucao(" + predicade + "(" + String.join(", ", args) + ")).");
                break;
            case "evolucaoNeg":
                this.executePrint("evolucao(-" + predicade + "(" + String.join(", ", args) + ")).");
                break;
            case "evolucaoIncerto":
                int iIncerto = 0;
                for (i = 0; i < args.length; i++) {
                    if (args[i].equals("incerto")) {
                        iIncerto = i;
                        args[iIncerto] = "incerto" + numberIncerto;
                        break;
                    }
                }
                if (i == args.length) {
                    throw new ParseException(cmd + " estava a espera de 'incerto' e nao encontrou", i);
                }
                
                List<String> argsList = IntStream.range(0, args.length)
                            .mapToObj(j -> "A" + j)
                            .collect(Collectors.toList());
                
                String argsP1 = String.join(", ", argsList);
                //String argsP1 = String.join(",",args);
                argsList.set(iIncerto, "incerto" + numberIncerto);
              //  argsList.set(iIncerto, "incerto" + numberIncerto );
                String argsP2 = String.join(",",argsList);
                this.executePrint("evolucao("+ predicade+"(" + String.join(",",args) + " )).");
                this.executePrint("assert((excecao(" + predicade + "("+ argsP1 +")):-"+predicade+"(" + argsP2 + " ))).");
                numberIncerto++;
                break;
            case "evolucaoInterdito":
                for (i = 0; i < args.length; i++) {
                    if (args[i].equals("interdito")) {
                        args[i] = "interdito" + numberIntertito;
                        break;
                    }
                }
                if (i == args.length) {
                    throw new ParseException(cmd + " estava a espera de 'interdito' e nao encontrou", i);
                }
                this.executePrint("evolucao(" + predicade + "(" + String.join(", ", args) + ")).");
                this.executePrint("evolucao(interdito(interdito" + numberIntertito + ")).");

                numberIntertito++;
                break;
            case "evolucaoImpreciso":
                this.executePrint("evolucao(excecao(" + predicade + "(" + String.join(", ", args) + "))).");
                break;
            default:
                throw new ParseException("Command not found '" + cmd + "'", 0);
        }

    }

    public void executePrint(String queryString) throws Exception {
        //System.out.println(s);
        //predicate(‘term’,X).
        HashMap map = new HashMap();
        Query query = sp.openPrologQuery(queryString, map);

        if (query.nextSolution()){
            System.out.println("yes " + map.toString());
//while (query.nextSolution()) {
//                System.out.println(map.toString());
//            }
        } else {
            System.out.println("no (" + queryString +")" );
        }
        query.close();

    }

    public boolean readFromInput(Scanner reader) throws ParseException, Exception {
        if (!reader.hasNextLine()) {
            return false;
        }
        String input = reader.nextLine();

        if (input.length() < 2) {
            return true;
        }
        if (input.startsWith("## ")){
            executePrint(input.substring(2));
            return true;
        }
        if (input.startsWith("$$ ")){
            System.out.println(input.substring(2));
            return true;
        }
        

        List<String> matchList = new ArrayList<String>();
        Pattern regex = Pattern.compile("[^\\s\"']+|\"[^\"]*\"|'[^']*'");
        Matcher regexMatcher = regex.matcher(input);
        while (regexMatcher.find()) {
            matchList.add(regexMatcher.group());
        }
        String[] cmd = matchList.toArray(new String[matchList.size()]);
        if (cmd.length < 3) {
            throw new ParseException("wrong command sintaxe", 0);
        }
        String[] args = Arrays.copyOfRange(cmd, 2, cmd.length);
        interpExecute(cmd[0], cmd[1], args);

        return true;
    }

    static MainStand ms;

    public static void main(String[] args) {
        try {
            //inicializa atravez do ficheiro .pl inicial
            // aqui faz load ao evolução e essas cenas...
            ms = new MainStand("inicial.pl");
            // faz load da base de conhecimento incial
            InputStream is = new FileInputStream("input.txt");
            Scanner scanner = new Scanner(is);
            while (ms.readFromInput(scanner)) {
            }

            scanner = new Scanner(System.in);
            boolean b = true;
            do {
                try{
                    b = ms.readFromInput(scanner);
                }catch(ParseException | SPException ex){
                    System.err.println(ex.toString());            
                }
            } while (b);

            //Corre os testes
            //test01();
        } catch (Exception ex) {
            System.err.println(ex.toString());
        }
    }

    public static void test01() throws ParseException, Exception {
        ms.interpExecute("evolucaoPos", "proprietario", "a", "b");
        ms.interpExecute("evolucaoNeg", "proprietario", "a", "b");
        try {
            ms.interpExecute("evolucaoIncerto", "proprietario", "a", "b");
        } catch (ParseException pe) {
            System.out.println("Test_erro:" + pe.getMessage());
        }
        ms.interpExecute("'", "proprietario", "a", "incerto");
        ms.interpExecute("evolucaoInterdito", "proprietario", "a", "interdito");
        ms.interpExecute("evolucaoInterdito", "proprietario", "d", "interdito");
        ms.interpExecute("evolucaoInterdito", "proprietario", "b", "interdito");
        ms.interpExecute("evolucaoInterdito", "proprietario", "c", "interdito");

        ms.interpExecute("evolucaoImpreciso", "proprietario", "a", "b");

        try {
            ms.interpExecute("asd", "proprietario", "a", "b");
        } catch (ParseException pe) {
            System.out.println("Test_erro:" + pe.getMessage());
        }

    }

}
