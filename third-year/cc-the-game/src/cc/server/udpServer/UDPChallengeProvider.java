package cc.server.udpServer;

import cc.model.Challenge;
import cc.model.Question;
import cc.pdu.PDU;
import cc.pdu.PDUType;
import cc.server.tcpServer.ServerState;
import cc.server.tcpServer.facade.TcpClient;
import java.time.LocalDateTime;
import java.time.temporal.ChronoUnit;
import java.util.List;
import java.util.Random;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;
import java.util.stream.IntStream;
import util.T3;
import util.T2;

/**
 *
 * @author paulo
 */
public class UDPChallengeProvider {

    public static final int CHALLENGE_NUMQUESTION = 3;

    public UDPComunication socket;
    public ServerState state;

    public UDPChallengeProvider(UDPComunication socket, ServerState state) {
        this.socket = socket;
        this.state = state;
    }

    public void startChallenge(Challenge challenge) {
        long timeMili;

        if (challenge.getName().equals("Circo")) {
            timeMili = 2000;
        } else if (challenge.getName().equals("GrandaFesta")) {
            timeMili = 2000;
        } else {
            timeMili = LocalDateTime.now().until(challenge.getDateTime(), ChronoUnit.MILLIS);
        }

        ScheduledExecutorService executor = Executors.newScheduledThreadPool(1024);
        executor.schedule(() -> {
            try {
                if (challenge.getSubscribers().size() > 1) {
                    this.startChallengeNow(challenge);
                } else {
                    // error
                }
            } catch (Exception ex) {
                ex.printStackTrace();
                System.err.println(ex.toString());
            }
        }, timeMili, TimeUnit.MILLISECONDS);

    }

    public void startChallengeNow(final Challenge challenge) throws InterruptedException {
        int i, nQuestion;
        int sizeQ = state.getQuestions().size();
        Random r = new Random();

        /**
         * generate all the questions for this challenge
         */
        if (challenge.getName().equals("Circo")) {
            for (i = 0; i < CHALLENGE_NUMQUESTION; i++) {
                challenge.addQuestion(state.getQuestion(i + 1));
            }
        } else {
            for (i = 0; i < CHALLENGE_NUMQUESTION; i++) {
                Question q;
                do {
                    q = state.getQuestion(r.nextInt(sizeQ - 1) + 1);
                    // avoid repetead questions
                } while (challenge.hasQuestion(q));
                challenge.addQuestion(q);
            }
        }

        for (i = 0; i < CHALLENGE_NUMQUESTION; i++) {
            final Question q = challenge.getQuestion(i);
            final int iAux = i;
            q.loadImage();
            q.loadMusic();
            challenge.getServers().stream()
                    .forEach((TcpClient s) -> {
                        s.question(challenge.getName(), iAux, q.getQuestion(),
                                q.getCorrect(), q.getAnwser(), q.getImageArray(),
                                q.getMusicArray());
                    });
            this.sendQuestion(challenge.getName(), i, q.getQuestion(), q.getCorrect(),
                    q.getAnwser(), q.getImageArray(), q.getMusicArray());

            int waitTime;
            if (challenge.getName().equals("circo")) {
                waitTime = 1000;
            } else {
                waitTime = 1000 * 30;                
            }
            challenge.waitToNext(waitTime);
           
        }
    }

    public void sendQuestion(String challengeName, int nQuestion, String question,
            int correct, String[] answers, byte[] img, List<byte[]> music) {
        Challenge challenge = state.getChallenge(challengeName);
        PDU ans = makeQuestionPDU(challengeName, nQuestion, question, correct, answers, img);

        challenge.getSubscribers().stream()
                // this condition are here to avoid send PDU to people from other server( that has not ip ), 
                .filter(user -> user.getIP() != null)
                .forEach((user) -> {
                    socket.sendPDU(ans, user.getIP(), user.getPort());
                });

        IntStream.range(0, music.size())
                .filter(i -> i < music.size())
                .mapToObj(i -> new T3<>(i, music.get(i), new PDU(PDUType.REPLY)))
                .peek(t -> t.c.addParameter(PDUType.REPLY_NUM_BLOCK, (byte) (int) t.a))
                .peek(t -> t.c.addParameter(PDUType.REPLY_BLOCK, t.b))
                .peek(t -> {
                    if (t.a < music.size() - 1) {
                        t.c.addParameter(PDUType.CONTINUE, (byte) 0);
                    }
                })
                .map(t -> t.c)
                .flatMap(pdu -> challenge.getSubscribers().stream().map(u -> new T2<>(u, pdu)))
                .filter(t -> t.a.getIP() != null)
                .forEach(t -> {
                    socket.sendPDU(t.b, t.a.getIP(), t.a.getPort());
                });

    }

    public PDU makeQuestionPDU(String challengeName, int nQuestion, String question,
            int correct, String[] answers, byte[] img) {
        PDU questionPDU = new PDU(PDUType.REPLY);

        questionPDU.addParameter(PDUType.REPLY_CHALLE, challengeName);
        questionPDU.addParameter(PDUType.REPLY_NUM_QUESTION, (byte) nQuestion);
        questionPDU.addParameter(PDUType.REPLY_QUESTION, question);
        questionPDU.addParameter(PDUType.REPLY_CORRECT, (byte) correct);
        for (int i = 0; i < answers.length; i++) {
            questionPDU.addParameter(PDUType.REPLY_NUM_ANSWER, (byte) (i + 1));
            questionPDU.addParameter(PDUType.REPLY_ANSWER, answers[i]);
        }
        //@todo fazer o loadImage na classe Question
        questionPDU.addParameter(PDUType.REPLY_IMG, img);
        questionPDU.addParameter(PDUType.CONTINUE, (byte) 0);

        return questionPDU;
    }
}
