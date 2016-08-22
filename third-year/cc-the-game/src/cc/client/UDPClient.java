package cc.client;

import cc.model.Question;
import cc.pdu.PDU;
import cc.pdu.PDUType;
import cc.server.udpServer.UDPComunication;
import cc.swinggame.MainInterface;
import java.net.*;
import java.time.LocalDate;
import java.time.LocalTime;
import java.util.List;
import java.util.TreeMap;
import java.util.stream.Collectors;
import java.util.stream.IntStream;
import javafx.util.Pair;
import util.T2;

/**
 *
 * @author paulo
 */
public class UDPClient {

    private UDPComunication udp_com;
    private PDUToUser ptu;
    private String name;

    /**
     * This attribute is used for testing
     */
    private boolean withoutInterface = false;

    public UDPClient(String sourceIp, int sourcePort, String dest, int port) {
        try {
            udp_com = new UDPComunication(sourcePort, InetAddress.getByName(sourceIp), port, InetAddress.getByName(dest));
            ptu = new PDUToUser(true, "C" + sourceIp + "]");
        } catch (UnknownHostException ex) {
            System.out.println("Não foi possível criar Cliente.");
        }
    }

    public UDPClient(String dest, int port) {
        try {
            udp_com = new UDPComunication(0, null, port, InetAddress.getByName(dest));
            ptu = new PDUToUser();
        } catch (UnknownHostException ex) {
            System.out.println("Não foi possível criar Cliente.");
        }
    }

    public UDPComunication getUDPClientCom() {
        return udp_com;
    }

    public PDUToUser getPDUToUser() {
        return ptu;
    }

    public void makeDatagramHello() {
        PDU send = new PDU(PDUType.HELLO);
        udp_com.sendPDU(send);

        PDU receive = udp_com.nextPDU();
        ptu.processOk();
    }

    public void makeDatagramRegister(String name, String alcunha, byte[] sec_info) {
        PDU send = new PDU(PDUType.REGISTER);

        send.addParameter(PDUType.REGISTER_NAME, name);
        send.addParameter(PDUType.REGISTER_NICK, alcunha);
        send.addParameter(PDUType.REGISTER_PASS, sec_info);

        udp_com.sendPDU(send);

        PDU receive = udp_com.nextPDU();
        ptu.processOk();
    }

    public void makeDatagramLogin(String alcunha, byte[] sec_info) {
        PDU send = new PDU(PDUType.LOGIN);

        send.addParameter(PDUType.LOGIN_NICK, alcunha);
        send.addParameter(PDUType.LOGIN_PASS, sec_info);

        udp_com.sendPDU(send);

        PDU receive = udp_com.nextPDU();
        this.name = alcunha;
        ptu.processLogin(receive);

    }

    public void makeDatagramLogout() {
        PDU send = new PDU(PDUType.LOGOUT);

        udp_com.sendPDU(send);

        PDU receive = udp_com.nextPDU();
        ptu.processOk();

    }

    public void makeDatagramQuit() {
        PDU send = new PDU(PDUType.QUIT);

        udp_com.sendPDU(send);

        PDU receive = udp_com.nextPDU();
        ptu.processOk();
    }

    public void makeDatagramEnd() {
        PDU send = new PDU(PDUType.END);

        udp_com.sendPDU(send);

        PDU receive = udp_com.nextPDU();
        ptu.processEnd(receive);
    }

    public void makeDatagramListChallenges() {
        PDU send = new PDU(PDUType.LIST_CHALLENGES);

        udp_com.sendPDU(send);

        PDU receive = udp_com.nextPDU();
        ptu.processChallenges(receive);
    }

    public void makeDatagramMakeChallenge(String desafio, LocalDate data, LocalTime hora) {
        PDU send = new PDU(PDUType.MAKE_CHALLENGE);

        send.addParameter(PDUType.MAKE_CHALLENGE_CHALLENGE, desafio);
        send.addParameter(PDUType.MAKE_CHALLENGE_DATE, data);
        send.addParameter(PDUType.MAKE_CHALLENGE_HOUR, hora);

        udp_com.sendPDU(send);

        PDU receive = udp_com.nextPDU();
        ptu.processChallenges(receive);

        if (!withoutInterface) {
            startChallenge(desafio);
        }

    }

    //estas cenas devem retornar tuplos
    public void makeDatagramAcceptChallenge(String desafio) {
        PDU send = new PDU(PDUType.ACCEPT_CHALLENGE);

        send.addParameter(PDUType.ACCEPT_CHALLENGE_CHALLENGE, desafio);

        udp_com.sendPDU(send);

        PDU receive = udp_com.nextPDU();
        //determinar se receive é uma questão.

        ptu.processOk();

//        doChallenge(desafio);
        if (!withoutInterface) {
            new Thread(()->{
                startChallenge(desafio);
            }).start();
        }

        //@todo: ficar a espera de resposta do servidor com proxima questao ou erro
        //esta funcionalidade talvez melhor implementar no desafio...
    }

    public void makeDatagramDeleteChallenge(String desafio) {
        PDU send = new PDU(PDUType.DELETE_CHALLENGE);

        send.addParameter(PDUType.DELETE_CHALLENGE_CHALLENGE, desafio);

        udp_com.sendPDU(send);

        PDU receive = udp_com.nextPDU();
        ptu.processChallenges(receive);
    }

    public T2<Integer,Boolean> makeDatagramAnswer(Byte escolha, String desafio, Byte questao) {
        PDU send = new PDU(PDUType.ANSWER);

        send.addParameter(PDUType.ANSWER_CHOOSE, escolha);
        send.addParameter(PDUType.ANSWER_CHALLENGE, desafio);
        send.addParameter(PDUType.ANSWER_NQUESTION, questao);

        udp_com.sendPDU(send);

        PDU receive = udp_com.nextPDU();
        return new T2<>((int)(byte) receive.popParameter(PDUType.REPLY_POINTS), ((byte) receive.popParameter(PDUType.REPLY_CORRECT)) != 0);
        //ptu.processAnswer(receive);
    }

    //
    public byte[] makeDatagramRetransmit(String desafio, byte questao, byte bloco) {
        PDU send = new PDU(PDUType.RETRANSMIT);

        send.addParameter(PDUType.RETRANSMIT_CHALLENGE, desafio);
        send.addParameter(PDUType.RETRANSMIT_NQUESTION, questao);
        send.addParameter(PDUType.RETRANSMIT_NBLOCK, bloco);

        udp_com.sendPDU(send);
        PDU response = udp_com.nextPDU();

        if (response.hasParameter(PDUType.REPLY_BLOCK)) {
            return (byte[]) response.popParameter(PDUType.REPLY_BLOCK);
        } else {
            return null;
        }
    }

    public void makeDatagramList_Ranking() {
        PDU send = new PDU(PDUType.LIST_RANKING);

        udp_com.sendPDU(send);

        PDU receive = udp_com.nextPDU();

        ptu.processRankings(receive);
    }

    public void closeCSocket() {
        udp_com.close();
    }

    public void startChallenge(String desafio) {
        MainInterface mInt = new MainInterface(this);
        mInt.start(name, desafio);
    }

    public PDU getNextPDU() {
        return udp_com.nextPDU();
    }

    public Question getNextQuestion() {
        Question question;
        int numberBlocks = 0;

        udp_com.setLabelMode(false);
        PDU receive = udp_com.nextPDU();
        udp_com.setLabelMode(true);

        String questionText = (String) receive.popParameter(PDUType.REPLY_QUESTION);
        String challengeName = (String) receive.popParameter(PDUType.REPLY_CHALLE);
        int questionId = (byte) receive.popParameter(PDUType.REPLY_NUM_QUESTION);
        int correct = (byte) receive.popParameter(PDUType.REPLY_CORRECT);
        byte[] img = (byte[]) receive.popParameter(PDUType.REPLY_IMG);
        String[] answers = (String[]) receive.getAllParameter(PDUType.REPLY_ANSWER)
                .toArray(new String[receive.getAllParameter(PDUType.REPLY_ANSWER).size()]);

        // MUSIC WOWOW
        TreeMap<Integer, byte[]> musicBytes = new TreeMap<>();

        while (receive.hasParameter(PDUType.REPLY_BLOCK)) {
            if (!receive.hasParameter(PDUType.REPLY_NUM_BLOCK)) {
                //error
            }
            int blockNumber = (byte) receive.popParameter(PDUType.REPLY_NUM_BLOCK);
            byte[] blockMusic = (byte[]) receive.popParameter(PDUType.REPLY_BLOCK);
            if (numberBlocks < blockNumber) {
                numberBlocks = blockNumber;
            }

            musicBytes.put(blockNumber, blockMusic);
        }

        IntStream.rangeClosed(0, numberBlocks)
                // not in the list
                .filter((a) -> !musicBytes.containsKey(a))
                // prepare the id and get de bytes
                .mapToObj((a) -> new Pair<>(a, makeDatagramRetransmit(challengeName, (byte) questionId, (byte) a)))
                .filter((pair) -> pair.getValue() != null)
                // add
                .forEach((t) -> musicBytes.put(t.getKey(), t.getValue()));

//        ByteBuffer buffer = ByteBuffer.allocate((numberBlocks + 1) * musicBytes.firstEntry().getValue().length);
//        musicBytes.entrySet().stream().sequential()
//                .forEach((a) -> buffer.put(a.getValue()));
        List<byte[]> buffer = musicBytes.entrySet().stream().sequential()
                .map(pair -> pair.getValue())
                .collect(Collectors.toList());
// percorrer todos e se for maior que o anterior entao faz pede retrasmit do bloco
//@todo receber os blocos da musica
        question = new Question(questionText, answers, correct, img, buffer);

        return question;
    }

    public void setWithoutInterface(boolean withoutInterface) {
        this.withoutInterface = withoutInterface;
    }

}
