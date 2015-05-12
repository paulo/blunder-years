package cc.client;

import cc.pdu.PDU;
import cc.pdu.PDUType;
import java.time.LocalDate;
import java.time.LocalTime;
import java.util.List;
import java.util.Map;

/**
 *
 * @author paulo
 */
public class PDUToUser {

    boolean oneline;
    String name;
    
    public PDUToUser() {
        oneline = false;
        name = "";
    }
    
    public PDUToUser(boolean ol, String nome) {
        oneline = ol;
        name = nome;
    }
    
    public void processOk() {
        System.out.println(name+": Servidor: OK");        
    }

    void processLogin(PDU receive) {
        String name = null; int score = 0;
        StringBuilder sb = new StringBuilder();
        String answer;
        
        while(receive.hasParameter(PDUType.REPLY_NAME)){
            name = (String)receive.popParameter(PDUType.REPLY_NAME);
            score = (Short) receive.popParameter(PDUType.REPLY_SCORE);
            
            sb.append(this.name).append(": Servidor: \nNome: ")
                        .append(name).append("\nScore: ").append(score);
            
            answer = sb.toString();
            if (oneline) {
                answer = answer.replace('\n', ' ');
                System.out.println(answer);
            }
            else {
                System.out.println(answer);
            }            
        }
        
    }

    void processEnd(PDU receive) {
        StringBuilder sb = new StringBuilder();
        String answer;
        sb.append(this.name);
        
        while(receive.hasParameter(PDUType.REPLY_SCORE)){
           int score = (Short) receive.popParameter(PDUType.REPLY_SCORE);
           String name = (String) receive.popParameter(PDUType.REPLY_NAME);
             
           sb.append(": Servidor end:\n").append(name).append(" - ").append(score).append("\n");
        }
        answer = sb.toString();
        if(oneline) {
            answer = answer.replace('\n', ' ');
            System.out.println(answer);
        }
        else {
            System.out.println(answer);
        }
    }

    void processChallenges(PDU receive) {
        String answer, desafio = null; LocalDate ld= null; LocalTime lt = null;
        StringBuilder sb = new StringBuilder();
        
        while(receive.hasParameter(PDUType.REPLY_CHALLE)){
            desafio = (String)receive.popParameter(PDUType.REPLY_CHALLE);
            ld = (LocalDate)receive.popParameter(PDUType.REPLY_DATE);
            lt = (LocalTime)receive.popParameter(PDUType.REPLY_HOUR);
            
            sb.append(name).append(": Servidor: \nDesafio: ").append(desafio)
                    .append("\nData: ").append(ld.toString()).append("\nHora: ")
                    .append(lt.toString());
            answer = sb.toString();
        
            if(oneline) {
                answer = answer.replace('\n', ' ');
                System.out.println(answer);
            }
            else {
                System.out.println(answer);
            }
        }
    }

/*    void processAnswer(PDU receive) {

        throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
    }
*/
    void processRankings(PDU receive) {
        String answer, name = null; String nick = null; int score = 0;
        StringBuilder sb = new StringBuilder();
        
        while(receive.hasParameter(PDUType.REPLY_NAME)){
            name = (String)receive.popParameter(PDUType.REPLY_NAME);
            nick = (String)receive.popParameter(PDUType.REPLY_NICK);
            score = (int)receive.popParameter(PDUType.REPLY_SCORE);
            
            sb.append(this.name).append(": Servidor: \nNome: ").append(name)
                    .append("\nAlcunha: ").append(nick).append("\nPontuação: ")
                    .append(score);
            
            answer = sb.toString();
            
            if(oneline) {
                answer = answer.replace('\n', ' ');
                System.out.println(answer);
            }
            else {
                System.out.println(answer);
            }
        }
    }
}
