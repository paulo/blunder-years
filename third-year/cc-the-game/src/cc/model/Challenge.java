/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package cc.model;

import cc.server.tcpServer.facade.TcpClient;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 *
 * @author ruioliveiras
 */
public class Challenge {

    /**
     * The Challenge name
     */
    private String name;
    /**
     * The date to start the Challenge.
     */
    private LocalDate date;
    /**
     * The time (in hours) to start the Challenge.
     */
    private LocalTime time;
    /**
     * The total number of users in all servers
     */
    private int userTotal = 0 ;
    /**
     * The current users that subscribe this challenge.
     */
    private final Set<User> subscribers;
    /**
     * Map from user.nick to his score.
     */
    private final Map<String, Integer> scores;
    /**
     * Set of the questions generated for the challenge
     */
    private final List<Question> questions;
    /**
     * This saves the index of the current Question
     */
    private int currentQuestion = 0;
    /**
     * Set of the Servers that are subscribed to this challenge
     */
    private final Set<TcpClient> servers;
    /**
     * Object used to wait the time for each question
     */
    private final Object questionWait = new Object();
    /**
     * The current number of answer to the actual question. after wait to
     * nextQuestion is set to zero
     */
    private int awnserCount;

    public Challenge() {
        subscribers = new HashSet<>();
        scores = new HashMap<>();
        questions = new ArrayList<>();
        servers = new HashSet<>();
    }

    /**
     * Main constructor
     *
     * @param name
     * @param date
     * @param time
     */
    public Challenge(String name, LocalDate date, LocalTime time) {
        this();
        this.name = name;
        this.date = date;
        this.time = time;
    }

    public String getName() {
        return name;
    }

    public LocalDate getDate() {
        return date;
    }

    public LocalTime getTime() {
        return time;
    }

    public LocalDateTime getDateTime() {
        return date.atTime(time);
    }

    public Set<User> getSubscribers() {
        return subscribers;
    }

    public void addSubscribers(User u) {
        userTotal++;
        subscribers.add(u);
        scores.put(u.getNick(), 0);
    }

    public int getScore(String userNick) {
        return scores.get(userNick);
    }

    public int answer(String userNick, boolean isCorrect) {
        synchronized (questionWait) {
            awnserCount++;
            if (awnserCount >= userTotal){
                questionWait.notify();
            }
            
            if (isCorrect) {
                scores.put(userNick, scores.get(userNick) + 2);
                return 2;
            } else {
                int actualScore = scores.get(userNick);
                if (actualScore - 1 > 0) {
                    scores.put(userNick, actualScore - 1);    
                    return -1;
                } else {
                    return 0;
                }
            }            
        }
    }

    public boolean hasQuestion(Question q) {
        return questions.contains(q);
    }

    public void addQuestion(Question q) {
        questions.add(q);
    }

    public Question getQuestion(int index) {
        return questions.get(index);
    }

    public void addServer(TcpClient u) {
        servers.add(u);
        userTotal++;
    }

    public Set<TcpClient> getServers() {
        return servers;
    }

    public void addScore(String userNick, int score) {
        scores.put(userNick, scores.getOrDefault(userNick, 0) + score);
    }

    public synchronized Question waitToNext(long timeout) throws InterruptedException {
        Question ret;
        synchronized (questionWait) {
            questionWait.wait(timeout);
            currentQuestion++;
            if (currentQuestion < questions.size()) {
                ret = questions.get(currentQuestion);
                ret.close();
            } else {
                ret = null;
            }
            
            awnserCount = 0;
        }
        return ret;
    }
}
