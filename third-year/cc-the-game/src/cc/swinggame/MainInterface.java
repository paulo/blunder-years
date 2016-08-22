/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package cc.swinggame;

import cc.client.UDPClient;
import cc.model.Question;
import cc.server.udpServer.UDPChallengeProvider;
import java.awt.Color;
import java.io.IOException;
import java.util.Timer;
import java.util.TimerTask;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.swing.ImageIcon;
import javax.swing.JLabel;
import util.T2;

/**
 *
 * @author paulo
 */
public class MainInterface extends javax.swing.JFrame {

    private volatile int currentPoints, questionAnswered;
    private final Object obj = new Object();
    private UDPClient udp_client;
    private Thread current_song;
    private MusicRunnable music_runnable;
    private Timer timer;
    private String currentChallenge;
    private int currentQuestion, currentAnswer;
    private final Object waitAwswer = new Object();

    /**
     * Creates new form MainInterface
     */
    public MainInterface(UDPClient udpC) {
        initComponents();
        question_area.setLineWrap(true);
        currentQuestion = 0;
        current_song = null;
        currentPoints = 0;
        questionAnswered = 0;
        this.udp_client = udpC;
        setQuestionTheme();
    }

    //fechar a janela no fim do desafio ?
    //nao esta a usar um dos campos
    public void doChallenge(String desafio) {
        currentChallenge = desafio;
        new Thread(() -> {
            loadNextQuestion();
            currentQuestion = 0;

            do {
                 i_pergunta.setText((currentQuestion +1)+ "/" + UDPChallengeProvider.CHALLENGE_NUMQUESTION);
                try {
                    setTimer(29 * 1000);
                    synchronized (waitAwswer) {
                        waitAwswer.wait(29 * 1000);
                    }
                } catch (InterruptedException ex) {
                    throw new RuntimeException(ex);
                }
                stopMusic();
                r1_button.setEnabled(false);r2_button.setEnabled(false);r3_button.setEnabled(false);
                if (currentQuestion < UDPChallengeProvider.CHALLENGE_NUMQUESTION - 1) {
                    loadNextQuestion();
                    r1_button.setEnabled(true);r2_button.setEnabled(true);r3_button.setEnabled(true);    
                }
                currentQuestion++;
               
            } while (currentQuestion < UDPChallengeProvider.CHALLENGE_NUMQUESTION );
            stopMusic();
            udp_client.makeDatagramEnd();
            //setVisible(false);
        }).start();
//        for (currentQuestion = 0; currentQuestion < 10; currentQuestion++) {
//
//            actualQ = udp_client.getNextQuestion();
//            actualQ.getCorrect();
//            loadNextQuestion(actualQ);
//
//            correctAnswer_index = actualQ.getCorrect();
//
//        }
    }

    public void refreshFrame() {
        invalidate();
        validate();
        repaint();
    }

    public synchronized void setQuestionTheme() {
        r1_button.setBackground(Color.blue);
        r2_button.setBackground(Color.blue);
        r3_button.setBackground(Color.blue);
        r1_button.setEnabled(true);
        r2_button.setEnabled(true);
        r3_button.setEnabled(true);
    }

    //falta tratar para o caso em que nao respondeu
    public void showResult(int given, boolean correctAnswer) {
        Color c = correctAnswer ? Color.green : Color.red;
        
        if (given == 0) {
            r1_button.setBackground(c);
        } else if (given == 1) {
            r2_button.setBackground(c);
        } else if (given == 2) {
            r3_button.setBackground(c);
        }
    }

    public void loadNextQuestion() {
        Question quest = udp_client.getNextQuestion();
        setQuestionTexts(quest);
        questionAnswered = 0;
        setQuestionTheme();

        playMusic(quest.getByteMusicArray());

        try {
            setImage(quest.getImageArray());
        } catch (IOException ex) {
            System.out.println("Não foi possível imprimir imagem");
        }
    }

    public void setQuestionTexts(Question quest) {
        String[] answers = quest.getAnwser();
        String question_text = quest.getQuestion();

        r1_button.setText(answers[0]);
        r2_button.setText(answers[1]);
        r3_button.setText(answers[2]);

        question_area.setText(question_text);

    }

    public void stopMusic() {
        music_runnable.terminateMusic();
        try {
            current_song.join();
        } catch (InterruptedException ex) {
            Logger.getLogger(MainInterface.class.getName()).log(Level.SEVERE, null, ex);
        }
        timer.cancel();

    }
    
    

    //falta enviar o datagrama com os valores passados como bytes
    //valor escolhar é variavel answerGiven, valor questao é a variavel pergunta            
    public void answerQuestion(int answer) {
        stopMusic();
        questionAnswered = 1;
        T2<Integer,Boolean> ret =
                udp_client.makeDatagramAnswer((byte) answer, currentChallenge, (byte) currentQuestion);

        showResult(answer,ret.b);

        i_points.setText("" + ret.a);
        synchronized (waitAwswer) {
            waitAwswer.notify();
        }
    }

    public void setTimer(int time) {
        int timeBetween = 1000;
        timer = new Timer();
        timer.scheduleAtFixedRate(new TimerTask() {
            int i = 100;

            @Override
            public void run() {
                i -= 100 / ( time / timeBetween);
                progress_bar.setValue(i);
                progress_bar.setStringPainted(true);
                if (i <= 1) {
                    questionAnswered = 1;
                }
                if (questionAnswered != 0) {
                    timer.cancel();
                }
            }
        }, 0, timeBetween);
    }

    private void playMusic(byte[] music) {
        music_runnable = new MusicRunnable();
        music_runnable.setSong(music);

        current_song = new Thread(music_runnable);
        current_song.start();
    }

    public void setImage(byte[] iArray) throws IOException {

        ImageIcon imageI = new ImageIcon(iArray);
        JLabel image_label = new JLabel(imageI);

        image_panel.removeAll();
        image_panel.add(image_label);
        image_panel.revalidate();
        //image_panel.repaint();       
    }

    public void start(String name, String desafio) {
        new Thread(() -> {
            this.setVisible(true);
            this.setTitle(name);
            this.doChallenge(desafio);
        }).start();
    }

    /**
     * This method is called from within the constructor to initialize the form.
     * WARNING: Do NOT modify this code. The content of this method is always
     * regenerated by the Form Editor.
     */
    @SuppressWarnings("unchecked")
    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    private void initComponents() {

        backgound_panel = new javax.swing.JPanel();
        r1_button = new javax.swing.JButton();
        r2_button = new javax.swing.JButton();
        r3_button = new javax.swing.JButton();
        jScrollPane1 = new javax.swing.JScrollPane();
        question_area = new javax.swing.JTextArea();
        progress_bar = new javax.swing.JProgressBar();
        pontos_label = new javax.swing.JLabel();
        image_panel = new javax.swing.JPanel();
        i_points = new javax.swing.JLabel();
        jLabel1 = new javax.swing.JLabel();
        i_pergunta = new javax.swing.JLabel();

        setDefaultCloseOperation(javax.swing.WindowConstants.EXIT_ON_CLOSE);

        r1_button.setText("Resposta 1");
        r1_button.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                r1_buttonActionPerformed(evt);
            }
        });

        r2_button.setText("Resposta 2");
        r2_button.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                r2_buttonActionPerformed(evt);
            }
        });

        r3_button.setText("Resposta 3");
        r3_button.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                r3_buttonActionPerformed(evt);
            }
        });

        question_area.setEditable(false);
        question_area.setColumns(20);
        question_area.setLineWrap(true);
        question_area.setRows(5);
        jScrollPane1.setViewportView(question_area);

        progress_bar.setString("");

        pontos_label.setText("Pontos:");

        i_points.setFont(new java.awt.Font("Cantarell", 1, 24)); // NOI18N
        i_points.setText("0");

        jLabel1.setText("nº Pergunta:");

        i_pergunta.setFont(new java.awt.Font("Cantarell", 1, 24)); // NOI18N
        i_pergunta.setText("0");

        javax.swing.GroupLayout backgound_panelLayout = new javax.swing.GroupLayout(backgound_panel);
        backgound_panel.setLayout(backgound_panelLayout);
        backgound_panelLayout.setHorizontalGroup(
            backgound_panelLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(backgound_panelLayout.createSequentialGroup()
                .addContainerGap()
                .addGroup(backgound_panelLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addComponent(jScrollPane1)
                    .addGroup(backgound_panelLayout.createSequentialGroup()
                        .addComponent(pontos_label)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                        .addComponent(i_points, javax.swing.GroupLayout.PREFERRED_SIZE, 49, javax.swing.GroupLayout.PREFERRED_SIZE)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(jLabel1)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                        .addComponent(i_pergunta, javax.swing.GroupLayout.PREFERRED_SIZE, 49, javax.swing.GroupLayout.PREFERRED_SIZE)
                        .addGap(0, 0, Short.MAX_VALUE))
                    .addComponent(r1_button, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                    .addComponent(r2_button, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                    .addComponent(r3_button, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                    .addComponent(progress_bar, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                    .addComponent(image_panel, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
                .addContainerGap())
        );
        backgound_panelLayout.setVerticalGroup(
            backgound_panelLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(backgound_panelLayout.createSequentialGroup()
                .addContainerGap()
                .addGroup(backgound_panelLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(pontos_label, javax.swing.GroupLayout.PREFERRED_SIZE, 28, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addComponent(i_points, javax.swing.GroupLayout.PREFERRED_SIZE, 38, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addComponent(jLabel1)
                    .addComponent(i_pergunta, javax.swing.GroupLayout.PREFERRED_SIZE, 38, javax.swing.GroupLayout.PREFERRED_SIZE))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(jScrollPane1, javax.swing.GroupLayout.DEFAULT_SIZE, 110, Short.MAX_VALUE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(image_panel, javax.swing.GroupLayout.PREFERRED_SIZE, 401, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                .addComponent(r1_button, javax.swing.GroupLayout.PREFERRED_SIZE, 50, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(r2_button, javax.swing.GroupLayout.PREFERRED_SIZE, 50, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(r3_button, javax.swing.GroupLayout.PREFERRED_SIZE, 50, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                .addComponent(progress_bar, javax.swing.GroupLayout.PREFERRED_SIZE, 31, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addContainerGap())
        );

        javax.swing.GroupLayout layout = new javax.swing.GroupLayout(getContentPane());
        getContentPane().setLayout(layout);
        layout.setHorizontalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(layout.createSequentialGroup()
                .addComponent(backgound_panel, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                .addContainerGap())
        );
        layout.setVerticalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addComponent(backgound_panel, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
        );

        pack();
    }// </editor-fold>//GEN-END:initComponents

    private void r1_buttonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_r1_buttonActionPerformed
        //setAnswerState(1);
        r1_button.setBackground(Color.YELLOW);
        r2_button.setEnabled(false);
        r3_button.setEnabled(false);
        answerQuestion(0);
        //notifyObject();
    }//GEN-LAST:event_r1_buttonActionPerformed

    private void r2_buttonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_r2_buttonActionPerformed
        //setAnswerState(2);
        r2_button.setBackground(Color.YELLOW);
        r1_button.setEnabled(false);
        r3_button.setEnabled(false);
        answerQuestion(1);
        //notifyObject();        
    }//GEN-LAST:event_r2_buttonActionPerformed

    private void r3_buttonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_r3_buttonActionPerformed
        //setAnswerState(3);
        r3_button.setBackground(Color.YELLOW);
        r1_button.setEnabled(false);
        r2_button.setEnabled(false);
        answerQuestion(2);
        //notifyObject();
    }//GEN-LAST:event_r3_buttonActionPerformed

    /**
     * @param args the command line arguments
     */
    /*public static void main(String args[]) {
     /* Set the Nimbus look and feel */
        //<editor-fold defaultstate="collapsed" desc=" Look and feel setting code (optional) ">
        /* If Nimbus (introduced in Java SE 6) is not available, stay with the default look and feel.
     * For details see http://download.oracle.com/javase/tutorial/uiswing/lookandfeel/plaf.html 
     */
    /* try {
     for (javax.swing.UIManager.LookAndFeelInfo info : javax.swing.UIManager.getInstalledLookAndFeels()) {
     if ("Nimbus".equals(info.getName())) {
     javax.swing.UIManager.setLookAndFeel(info.getClassName());
     break;
     }
     }
     } catch (ClassNotFoundException ex) {
     java.util.logging.Logger.getLogger(MainInterface.class.getName()).log(java.util.logging.Level.SEVERE, null, ex);
     } catch (InstantiationException ex) {
     java.util.logging.Logger.getLogger(MainInterface.class.getName()).log(java.util.logging.Level.SEVERE, null, ex);
     } catch (IllegalAccessException ex) {
     java.util.logging.Logger.getLogger(MainInterface.class.getName()).log(java.util.logging.Level.SEVERE, null, ex);
     } catch (javax.swing.UnsupportedLookAndFeelException ex) {
     java.util.logging.Logger.getLogger(MainInterface.class.getName()).log(java.util.logging.Level.SEVERE, null, ex);
     }*/
        //</editor-fold>

    /* Create and display the form */
    /*java.awt.EventQueue.invokeLater(new Runnable() {
     public void run() {
     new MainInterface().setVisible(true);
     }
     });
     }*/

    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JPanel backgound_panel;
    private javax.swing.JLabel i_pergunta;
    private javax.swing.JLabel i_points;
    private javax.swing.JPanel image_panel;
    private javax.swing.JLabel jLabel1;
    private javax.swing.JScrollPane jScrollPane1;
    private javax.swing.JLabel pontos_label;
    private javax.swing.JProgressBar progress_bar;
    private javax.swing.JTextArea question_area;
    private javax.swing.JButton r1_button;
    private javax.swing.JButton r2_button;
    private javax.swing.JButton r3_button;
    // End of variables declaration//GEN-END:variables

}
