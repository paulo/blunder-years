package Errors;

/**
 * @author Grupo 30
 */


public class ErrorList {
    
    public static String erro(int numErro){
       switch (numErro){
           case 1 : return "Argumentos incorretos\n";
           case 2 : return "Número de argumentos inválido\n";
           case 3 : return "Formato de argumentos inválido\n";
           case 4 : return "Comando inválido\n";
           case 5 : return "Uma tarefa com esse nome está definida!\n";
           case 6 : return "Não existe nenhuma tarefa com esse nome!\n";
           default : return "Erro inesperado\n";
       } 
    }
}
