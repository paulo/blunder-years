package cc.client;

import cc.pdu.PDU;
import cc.pdu.PDUType;
import java.time.LocalDate;
import java.time.LocalTime;

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
        System.out.println(name + ": Servidor: OK");
    }

    void processLogin(PDU receive) {
        String name = null;
        int score = 0;
        StringBuilder sb = new StringBuilder();
        String answer;

        sb.append(this.name);
        if (receive.hasParameter(PDUType.REPLY_NAME)) {
            name = (String) receive.popParameter(PDUType.REPLY_NAME);
            score = (Short) receive.popParameter(PDUType.REPLY_SCORE);

            sb.append(": Servidor: \nNome: ")
                    .append(name).append("\nScore: ").append(score);
        }else if (receive.hasParameter(PDUType.REPLY_ERRO)) {
            sb.append("Erro: ").append((String) receive.popParameter(PDUType.REPLY_ERRO));
        }
        answer = sb.toString();
        if (oneline) {
            answer = answer.replace('\n', ' ');
            System.out.println(answer);
        } else {
            System.out.println(answer);
        }

    }

    void processEnd(PDU receive) {
        StringBuilder sb = new StringBuilder();
        String answer;
        sb.append(this.name);

        while (receive.hasParameter(PDUType.REPLY_SCORE)) {
            int score = (Short) receive.popParameter(PDUType.REPLY_SCORE);
            String name = (String) receive.popParameter(PDUType.REPLY_NAME);

            sb.append(": Servidor end:\n").append(name).append(" - ").append(score).append("\n");
        }
        answer = sb.toString();
        if (oneline) {
            answer = answer.replace('\n', ' ');
            System.out.println(answer);
        } else {
            System.out.println(answer);
        }
    }

    void processChallenges(PDU receive) {
        String answer, desafio = null;
        LocalDate ld = null;
        LocalTime lt = null;
        StringBuilder sb = new StringBuilder();

        while (receive.hasParameter(PDUType.REPLY_CHALLE)) {
            desafio = (String) receive.popParameter(PDUType.REPLY_CHALLE);
            ld = (LocalDate) receive.popParameter(PDUType.REPLY_DATE);
            lt = (LocalTime) receive.popParameter(PDUType.REPLY_HOUR);

            sb.append(name).append(": Servidor: \nDesafio: ").append(desafio)
                    .append("\nData: ").append(ld.toString()).append("\nHora: ")
                    .append(lt.toString());
            answer = sb.toString();

            if (oneline) {
                answer = answer.replace('\n', ' ');
                System.out.println(answer);
            } else {
                System.out.println(answer);
            }
        }
    }

    void processRankings(PDU receive) {
        String answer = "", name = null;
        String nick = null;
        int score = 0;
        StringBuilder sb = new StringBuilder();

        sb.append(this.name).append(": [ListingRating]Servidor disse:");
        while (receive.hasParameter(PDUType.REPLY_NAME)) {
            name = (String) receive.popParameter(PDUType.REPLY_NAME);
            nick = (String) receive.popParameter(PDUType.REPLY_NICK);
            score = (short) receive.popParameter(PDUType.REPLY_SCORE);

            sb.append("\n(Nome: ").append(name)
                    .append(" Alcunha: ").append(nick)
                    .append(" Pontuação: ").append(score)
                    .append(")");

        }
        answer = sb.toString();
        if (oneline) {
            answer = answer.replace('\n', ' ');
            System.out.println(answer);
        } else {
            System.out.println(answer);
        }
    }
}
