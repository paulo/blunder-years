    /*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package cc.server.udpServer;

import cc.model.Challenge;
import cc.model.Question;
import cc.model.User;
import cc.pdu.PDU;
import cc.pdu.PDUType;
import cc.server.tcpServer.ServerState;
import cc.server.tcpServer.facade.TcpClient;
import cc.server.tcpServer.facade.TcpHub;
import cc.server.tcpServer.facade.TcpLocal;
import java.net.InetAddress;
import java.time.LocalDate;
import java.time.LocalTime;
import java.util.Arrays;

/**
 *
 * @author paulo
 */
public class UDPClientHandler {

    private final ServerState state;
    private final UDPComunication socket;
    private final UDPChallengeProvider challengeProvider;
    private final TcpLocal tcpLocal;
    private final TcpHub tcpHub;

    public UDPClientHandler(ServerState serverState, TcpLocal tcpLocal, TcpHub tcpHub, UDPComunication socket, UDPChallengeProvider challengeProvider) {
        this.state = serverState;
        this.socket = socket;
        this.challengeProvider = challengeProvider;
        this.tcpHub = tcpHub;
        this.tcpLocal = tcpLocal;
    }

    public PDU decodePacket(PDU pdu, InetAddress innetAddress, int port) {

        PDU answer = new PDU(PDUType.REPLY);
        String ip = innetAddress.toString();
        String challengeName;
        int questionId;

        switch (pdu.getType()) {
            case HELLO:
                answer.addParameter(PDUType.REPLY_OK, 0);
                break;
            case REGISTER:
                String name = (String) pdu.popParameter(PDUType.REGISTER_NAME);
                String nick = (String) pdu.popParameter(PDUType.REGISTER_NICK);
                byte[] secInfo = (byte[]) pdu.popParameter(PDUType.REGISTER_PASS);
                answer = register(name, nick, secInfo);
                break;
            case LOGIN:
                nick = (String) pdu.popParameter(PDUType.LOGIN_NICK);
                secInfo = (byte[]) pdu.popParameter(PDUType.LOGIN_PASS);
                answer = login(innetAddress, port, nick, secInfo);
                break;
            case LOGOUT:
                answer = logout(ip);
                break;
            case QUIT:
                answer.addParameter(PDUType.REPLY_OK, 0);
                break;
            case END:
                answer = end(ip);
                break;
            case LIST_CHALLENGES:
                answer = listChallenges();
                break;
            case MAKE_CHALLENGE:
                challengeName = (String) pdu.popParameter(PDUType.MAKE_CHALLENGE_CHALLENGE);
                LocalDate date = (LocalDate) pdu.popParameter(PDUType.MAKE_CHALLENGE_DATE);
                LocalTime time = (LocalTime) pdu.popParameter(PDUType.MAKE_CHALLENGE_HOUR);
                answer = makeChallenge(ip, challengeName, date, time);
                //also start the challenge
                challengeProvider.startChallenge(state.getChallenge(challengeName));
                break;
            case ACCEPT_CHALLENGE:
                challengeName = (String) pdu.popParameter(PDUType.ACCEPT_CHALLENGE_CHALLENGE);
                answer = acceptChallenge(ip, challengeName);
                break;
            case DELETE_CHALLENGE:
                challengeName = (String) pdu.popParameter(PDUType.DELETE_CHALLENGE_CHALLENGE);
                answer = deleteChallenge(challengeName);
                break;
            case ANSWER:
                int choice = (byte) pdu.popParameter(PDUType.ANSWER_CHOOSE);
                challengeName = (String) pdu.popParameter(PDUType.ANSWER_CHALLENGE);
                questionId = (byte) pdu.popParameter(PDUType.ANSWER_NQUESTION);
                answer = answer(ip, challengeName, choice, questionId);
                break;
            case RETRANSMIT:
                challengeName = (String) pdu.popParameter(PDUType.RETRANSMIT_CHALLENGE);
                questionId = (byte) pdu.popParameter(PDUType.RETRANSMIT_NQUESTION);
                int nblock = (byte) pdu.popParameter(PDUType.RETRANSMIT_NBLOCK);
                answer = retransmit(challengeName, questionId, nblock);
                break;
            case LIST_RANKING:
                answer = listRanking();
                break;
        }

        return answer;
    }

    private PDU register(String name, String nick, byte[] secInfo) {
        PDU answer = new PDU(PDUType.REPLY);

        User user = new User(name, secInfo, nick);
        if (!state.hasLocalUser(nick)) {
            state.addLocalUser(nick, user);
            answer.addParameter(PDUType.REPLY_OK, 0);
        } else {
            answer.addParameter(PDUType.REPLY_ERRO, "Utilizador já existe");
        }
        return answer;
    }

    private PDU login(InetAddress ip, int port, String nick, byte[] secInfo) {
        User user;
        String name;
        PDU answer = new PDU(PDUType.REPLY);

        if (state.hasLocalUser(nick)) {
            boolean check = Arrays.equals(state.getLocalUser(nick).getPass(), secInfo);
            if (check) {
                user = state.getLocalUser(nick);
                user.setIP(ip);
                user.setPort(port);
                state.addSession(ip.toString(), user);
                name = state.getLocalUser(nick).getName();
                //ver se é isto que é para enviar no reply
                int score = state.getLocalUser(nick).getRating();
                answer.addParameter(PDUType.REPLY_NAME, name);
                answer.addParameter(PDUType.REPLY_SCORE, (short) score);
            } else {
                answer.addParameter(PDUType.REPLY_ERRO, "Palavra-passe errada");
            }
        } else {
            answer.addParameter(PDUType.REPLY_ERRO, "Utilizador não registado");
        }

        return answer;
    }

    private PDU logout(String ip) {
        PDU answer = new PDU(PDUType.REPLY);

        state.getSession(ip).setIP(null);
        state.removeSession(ip);
        answer.addParameter(PDUType.REPLY_OK, 0);

        return answer;
    }

    private PDU end(String ip) {
        PDU answer = new PDU(PDUType.REPLY);
        Challenge challenge = state.getChallenge(state.getSession(ip).getActualChallenge());

        challenge.getSubscribers().stream()
                .map((user) -> user.getNick())
                .peek((userName) -> answer.addParameter(PDUType.REPLY_NAME, userName))
                .map((userName) -> challenge.getScore(userName))
                .forEach((score) -> answer.addParameter(PDUType.REPLY_SCORE, (short) (int) score));

        return answer;
    }

    private PDU listChallenges() {
        PDU answer = new PDU(PDUType.REPLY);
        LocalTime time;
        LocalDate date;
        String challengeName;

        for (Challenge challenge : state.getChallenges()) {
            challengeName = challenge.getName();
            time = challenge.getTime();
            date = challenge.getDate();
            answer.addParameter(PDUType.REPLY_CHALLE, challengeName);
            answer.addParameter(PDUType.REPLY_DATE, date);
            answer.addParameter(PDUType.REPLY_HOUR, time);
        }

        return answer;
    }

    private PDU makeChallenge(String ip, String challengeName, LocalDate date, LocalTime time) {
        User user = state.getSession(ip);
        Challenge challenge = new Challenge(challengeName, date, time);
        challenge.addSubscribers(user);
        user.setActualChallenge(challengeName);

        state.addChallenge(challengeName, challenge);
        state.addOwner(challengeName, "localhost");
        tcpHub.registerChallenge(challengeName, date, time, user.getName(), user.getNick());

        PDU answer = new PDU(PDUType.REPLY);

        answer.addParameter(PDUType.REPLY_CHALLE, challengeName);
        answer.addParameter(PDUType.REPLY_DATE, date);
        answer.addParameter(PDUType.REPLY_HOUR, time);

        return answer;
    }

    private PDU acceptChallenge(String ip, String challengeName) {
        PDU answer = new PDU(PDUType.REPLY);

        User user = state.getSession(ip);

        String ownerIp = state.getOwnerIp(challengeName);
        state.getChallenge(challengeName)
                .addSubscribers(user);

        if (!ownerIp.equals("localhost")) {
            state.getNeighbor(ownerIp).registerAcceptChallenge(challengeName, user.getName(), user.getNick());
        }
        user.setActualChallenge(challengeName);

        answer.addParameter(PDUType.REPLY_OK, 0);
        return answer;
    }

    private PDU deleteChallenge(String challengeName) {
        PDU answer = new PDU(PDUType.REPLY);
        Challenge challenge = state.getChallenge(challengeName);

        answer.addParameter(PDUType.REPLY_CHALLE, challengeName);
        answer.addParameter(PDUType.REPLY_DATE, challenge.getDate());
        answer.addParameter(PDUType.REPLY_HOUR, challenge.getTime());

        state.getChallenges().remove(challenge);

        return answer;
    }

    private PDU answer(String ip, String challengeName, int choice, int questionId) {
        PDU answer = new PDU(PDUType.REPLY);
        Challenge challange = state.getChallenge(challengeName);
        Question question = challange.getQuestion(questionId);
        String nickname = state.getSession(ip).getNick();

        answer.addParameter(PDUType.REPLY_CHALLE, challengeName);
        answer.addParameter(PDUType.REPLY_NUM_QUESTION, (byte) questionId);

        int points;
        if (question.isClosed()) {
            boolean correct = question.getCorrect() == choice;
            points = challange.answer(nickname, correct);

            if (correct) {
                answer.addParameter(PDUType.REPLY_CORRECT, (byte) 1);
            } else {
                answer.addParameter(PDUType.REPLY_CORRECT, (byte) 0);
            }
        } else {
            points = 0;
            answer.addParameter(PDUType.REPLY_CORRECT, (byte) 0);
            // may i add type to timeout?
        }
        answer.addParameter(PDUType.REPLY_POINTS, (byte) points);

        tcpLocal.registerScore(challengeName, nickname, points);
        state.getNeighbors().stream()
                .filter((server) -> (server instanceof TcpClient))
                .forEach(s -> s.registerScore(challengeName, nickname, points));
//        String owner = state.getOwnerIp(challengeName);
//        if (owner.equals("localhost")) {
//
//        } else {
//            state.getNeighbor(owner).registerScore(challengeName, nickname, points);
//        }

        return answer;
    }

    private PDU listRanking() {
        PDU answer = new PDU(PDUType.REPLY);

        for (User user : state.getGlobalUsers()) {
            answer.addParameter(PDUType.REPLY_NAME, user.getName());
            answer.addParameter(PDUType.REPLY_NICK, user.getNick());
            answer.addParameter(PDUType.REPLY_SCORE, (Short) (short) user.getRating());
        }

        return answer;
    }

    public PDU retransmit(String challengeName, int questionId, int nblock) {
        Question q = state.getChallenge(challengeName).getQuestion(questionId);
        q.loadMusic();
        if (q == null) {
            //erro;
        }
        PDU p = new PDU(PDUType.REPLY);
        createMusicBlockPDU(p, q, nblock);
        return p;
    }

    /**
     * Auxiliar function to calculate the PDU
     *
     * @param pduAux
     * @param q
     * @param index
     * @return if there are next, the last fragment return false, otherwise true
     */
    static boolean createMusicBlockPDU(PDU pduAux, Question q, int index) {
//        int sizeofBlock = 48 * 1024;
//        int size = sizeofBlock;
//        if ((sizeofBlock * index + sizeofBlock) > q.getMusicArray().length) {
//            size = q.getMusicArray().length - (sizeofBlock * index + sizeofBlock);
//        }
//        if (size < 0) {
//            return false;
//        }
//        byte[] res = ByteBuffer.allocate(size).put(q.getMusicArray(), index * sizeofBlock, size).array();
        pduAux.addParameter(PDUType.REPLY_NUM_BLOCK, (byte) index);
        pduAux.addParameter(PDUType.REPLY_BLOCK, q.getMusicArray().get(index));
        //pduAux.addParameter(PDUType.CONTINUE, (byte) 0);

        return true;
    }

//<<<<<<< HEAD
//            for (i = 1; i <= 5; i++) {
//                Question q = state.getChallenge(challenge.getName()).getQuestion(i);
//                PDU ans = makeQuestionPDU(challenge.getName(), i);
//=======
    /*private void startChallengeNow(Challenge challenge) {

     int i, nQuestion;
     int sizeQ = state.getQuestions().size();
     Random r = new Random();
     */
    /**
     * generate all the questions for this challenge
     */
    /*for (i = 1; i <= 10; i++) {
     Question q;
     do {
     q = state.getQuestion(r.nextInt(sizeQ - 1) + 1);
     // avoid repetead questions
     } while (challenge.hasQuestion(q));
     challenge.addQuestion(q);
     }

     for (i = 1; i <= 10; i++) {
     PDU ans = makeQuestionPDU(challenge.getName(), i);
     >>>>>>> d9655d8b23d4bedcab8fb2c349f2b3f4a88b0172
     //@todo nesta parte de escolher perguntas, não podem haver repetidas
     >>>>>>> master

     challenge.getSubscribers().stream()
     .filter(user -> user.getIP() != null)
     .forEach((user) -> {
     socket.sendPDU(ans, user.getIP(), user.getPort());
     });

     //music: 
     q.loadMusic();

     i = 0;
     boolean b = true;
     while (b) {
     PDU musicPDU = new PDU(PDUType.REPLY);
     //@todo: ruioliveiras continuar aqui, esta merda precisa de continue se nao for o ultimo
     b = createMusicBlockPDU(musicPDU, q, i++);
     if (b) {
     challenge.getSubscribers().stream()
     .filter(user -> user.getIP() != null)
     .forEach((user) -> {
     socket.sendPDU(musicPDU, user.getIP(), user.getPort());
     });
     }
     };

     }
     } catch (Exception ex) {
     ex.printStackTrace();
     }

     }

     public PDU makeQuestionPDU(String challengeName, int number) {
     PDU questionPDU = new PDU(PDUType.REPLY);
     Question q = state.getChallenge(challengeName).getQuestion(number - 1);
     String questionText = q.getQuestion();
     String[] answers = q.getAnwser();
     int correct = q.getCorrect(), i;

     questionPDU.addParameter(PDUType.REPLY_CHALLE, challengeName);
     questionPDU.addParameter(PDUType.REPLY_NUM_QUESTION, (byte) number);
     questionPDU.addParameter(PDUType.REPLY_QUESTION, questionText);
     questionPDU.addParameter(PDUType.REPLY_CORRECT, (byte) q.getCorrect());
     for (i = 0; i < 3; i++) {
     questionPDU.addParameter(PDUType.REPLY_NUM_ANSWER, (byte) (i + 1));
     questionPDU.addParameter(PDUType.REPLY_ANSWER, answers[i]);
     }
     //image:
     q.loadImage();
     questionPDU.addParameter(PDUType.REPLY_IMG, q.getImageArray());
     questionPDU.addParameter(PDUType.CONTINUE,(byte) 0);
        
     return questionPDU;

     }*/
    /*
     private PDU startChallengeNow(String ip, String name, LocalDate date, LocalTime time) {
     PDU answer = new PDU(PDUType.REPLY);
     Challenge challenge = new Challenge(name, date, time);

     challenge.addSubscribers(state.getSession(ip));

     state.addChallenge(name, challenge);
     state.addOwner(name, "localhost");
     answer.addParameter(PDUType.REPLY_CHALLE, name);
     answer.addParameter(PDUType.REPLY_DATE, date);
     answer.addParameter(PDUType.REPLY_HOUR, time);

     executor.schedule(() -> {
     if (challenge.getSubscribers().size() > 1) {
     startChallengeNow(challenge);
     } else {
     answer.addParameter(PDUType.REPLY_ERRO, "Numero insuficiente de jogadores para iniciar desafio");
     }

     //  }, LocalDate.now().until(date.atTime(time), ChronoUnit.MILLIS), TimeUnit.MILLISECONDS);
     }, 5000, TimeUnit.MILLISECONDS);

     //@todo: criar thread que irá acordar quando for date,time
     //       esta thread irá verificar se existe pessoas suficientes, 
     //       caso exista envia a primeira questao.
     //              Após enviar a primeira questao aguarda tempo X por resposta do User
     //              caso  user nao responda resposta errada será assumida
     //              Envia proxima Questao, caso não seja a ultima.
     //       caso não exista dá erro. 
     // criar thread aqui que confirma a existencia de pessoas       
     // criar função sendQuestion(int numero da questao)
     //      caso seja a ultima questao:
     //           responder com o rating
     //      caso não seja a ultima questao:
     //          envia questao, espera X tempo, quem respondeu respondeu, quem nao respondeu marca errada
     //          /!\ sistema de lock para nao deixar responder a answer fora do tempo
     //          chama proxima questao
     // do lado do servidor
     // Quando se envia uma questão terá que se enviar 1 pdu com cenas normais (ver pagina 5 do enunciado)
     // e também tera que se enviar a musica criar função sendFramentedMusic:
     // que irá criar os framentos e enviar somehow...
     // do lado do cliente:
     // quando se fica a espera de uma questão, a seguir irá-se esperar a musica.
     // a seguir a ter estas duas coisas irá por na interface,
     // depois espera-se por a proxima questao, entretanto posse-se enviar a resposta.
     return answer;
     }*/
}
