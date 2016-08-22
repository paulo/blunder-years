package cc.model;

import java.io.ByteArrayOutputStream;
import java.io.FileInputStream;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 *
 * @author ruioliveiras
 */
public class Question {

    /**
     * The text of the Question
     */
    public String question;
    /**
     * All the possibles answers
     */
    public String[] answer;
    /**
     * The index in the array answer of the correct answer
     */
    public int correct;
    /**
     * Image Path of this question, null if don't have one
     */
    public String imagePath;
    /**
     * Music path of the Music, null if don't have one
     */
    public String musicPath;

    /**
     * Byte array of this question's Image
     */
    public byte[] imageArray;

    /**
     * Byte array of this question's Music 
     */
    public List<byte[]> musicArray;

    /**
     * Boolean expression to save if the question was closed
     */
    public boolean questionClosed  =true;

    /**
     * Main constructor
     *
     * @param question
     * @param anwser
     * @param correct
     * @param imagePath
     * @param musicPath
     */
    public Question(String question, String[] anwser, int correct, String imagePath, String musicPath) {
        this.question = question;
        this.answer = anwser;
        this.correct = correct;
        this.imagePath = imagePath;
        this.musicPath = musicPath;
//        this.imageArray = imageArray;
//        this.musicArray = musicArray;
    }

    public Question(String question, String[] anwser, int correct, byte[] imageArray, List<byte[]> musicArray) {
        this.question = question;
        this.answer = anwser;
        this.correct = correct;
        this.imagePath = null;
        this.musicPath = null;
        this.imageArray = imageArray;
        this.musicArray = musicArray;
    }

    public void loadImage() {
        if (imageArray != null) {
            return;
        }
        try {
            Path path = Paths.get(imagePath);
            imageArray = Files.readAllBytes(path);
        } catch (IOException ex) {
            throw new RuntimeException("reading Image");
        }
    }

    public void loadMusic() {
        if (musicArray != null) {
            return;
        }
        musicArray = new ArrayList<>();
        try {
            //Path path = Paths.get(musicPath);
            FileInputStream fis = new FileInputStream(musicPath);
            while (fis.available() > 0) {
                int available = fis.available();
                byte[] buffer;
                if (available < 1024 * 48) {
                    buffer = new byte[available];
                } else {
                    buffer = new byte[1024 * 48];
                }
                fis.read(buffer);
                musicArray.add(buffer);
            }
            fis.close();

        } catch (IOException ex) {
            throw new RuntimeException("reading music");
        }
    }

    public String getQuestion() {
        return question;
    }

    public String[] getAnwser() {
        return answer;
    }

    public int getCorrect() {
        return correct;
    }

    public String getImagePath() {
        return imagePath;
    }

    public String getMusicPath() {
        return musicPath;
    }

    public List<byte[]> getMusicArray() {
        return musicArray;
    }

    public byte[] getByteMusicArray() {

        ByteArrayOutputStream outputStream = new ByteArrayOutputStream();

        for (byte[] b : this.musicArray) {
            try {
                outputStream.write(b);
            } catch (IOException ex) {
                Logger.getLogger(Question.class.getName()).log(Level.SEVERE, null, ex);
            }
        }

        return outputStream.toByteArray();
    }

    public byte[] getImageArray() {
        return imageArray;
    }

    public synchronized void close() {
        this.questionClosed = true;
    }

    public synchronized boolean isClosed() {
        return this.questionClosed;
    }

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append(question).append("\n")
                .append(answer[0]).append(" , ")
                .append(answer[1]).append(" , ")
                .append(answer[2]).append("\n")
                .append(correct).append("\n");
        return sb.toString();
    }

    @Override
    public int hashCode() {
        return getQuestion().hashCode(); //To change body of generated methods, choose Tools | Templates.
    }

    @Override
    public boolean equals(Object obj) {
        if (obj instanceof Question) {
            return ((Question) obj).getQuestion().equals(getQuestion());
        }
        return false;
    }
}
