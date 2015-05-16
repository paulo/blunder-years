package cc.thegame;

import cc.client.UDPClient;
import cc.model.Question;
import java.awt.image.BufferedImage;
import java.io.ByteArrayInputStream;
import java.io.IOException;
import static java.lang.Thread.sleep;
import java.util.Timer;
import java.util.TimerTask;
import java.util.logging.Level;
import java.util.logging.Logger;
import javafx.embed.swing.SwingFXUtils;
import javafx.event.ActionEvent;
import javafx.fxml.FXML;
import javafx.scene.control.Button;
import javafx.scene.control.ProgressBar;
import javafx.scene.control.TextArea;
import javafx.scene.image.Image;
import javafx.scene.image.ImageView;
import javafx.scene.media.Media;
import javafx.scene.media.MediaPlayer;
import static javafx.scene.media.MediaPlayer.Status.PLAYING;
import javax.imageio.ImageIO;

/**
 * FXML Controller class
 *
 * @author paulo
 */
public class AppController {

    private Question actualQuestion;
    
    @FXML
    private Button r1_button, r2_button, r3_button;
    
    @FXML
    private TextArea question_text;
    
    @FXML
    private ImageView question_image; //provavelmente vai ter de chamar-se mesmo com uma imagem e nao com um url
    
    @FXML
    private ProgressBar time_bar;
    
    private static Timer time;
    
    private static int time_elapsed = 0;
    
    MediaPlayer music_player;
    
    
    public ImageView toImage(byte[] iArray) throws IOException{
        //byte[] bytearray = Base64.decode(base64String);
 
	BufferedImage bimage=ImageIO.read(new ByteArrayInputStream(iArray));
        Image image = SwingFXUtils.toFXImage(bimage, null);
	return new ImageView((Image) image);
    }
    
    public void createQuestion(Question quest){
        String[] answers = quest.getAnwser();
        
        r1_button.setText(answers[0]);
        r2_button.setText(answers[1]);
        r3_button.setText(answers[2]);
        
        setTimer(1F);
        
        /*question_text.setText(quest.getQuestion());
        try {
            question_image = toImage(quest.getImageArray());
        } catch (IOException ex) {
            System.out.println("Não foi possível converter a imagem.");
        } */       
    }  
    
    public void actualizeQuestion(Question quest){
        createQuestion(quest);
        setTimer(1F);
        //playMusic();    
    }
    
    public void cleanInterface(){
        r1_button.setText("");
        r2_button.setText("");
        r3_button.setText("");
        question_text.setText("");
        //question_image = null;
        //if(music_player.getStatus()==PLAYING) music_player.stop();   
    }
    
    
    /*Não esta a funcionar*/
    private void setBarStyleClass(ProgressBar bar,double t) {
        bar.getStyleClass().removeAll();

        if(t >= 0.5F){
            bar.getStyleClass().add("-fx-accent: green;");
        }
        if(t < 0.5F && t>0.25F){ 
            bar.getStyleClass().add("-fx-accent: yellow;");
        }
        if(t<=0.25F){
            bar.getStyleClass().add("-fx-accent: red;");
        } 
           
    }
    
    private void playMusic(){
                            
        /*File songfile = new File("./etc/musica/000001.mp3");
        Media media = new Media(songfile.toURI().toString());
        MediaPlayer music_player = new MediaPlayer(media);
        mp.play();
        */
    }
    
    
    public void setTimer(double inicial_value){
        final Timer timer = new Timer();
        timer.scheduleAtFixedRate(new TimerTask() {
            double i = inicial_value;
            @Override
            public void run() {
                i-=0.01F;
                setBarStyleClass(time_bar, i);
                time_bar.setProgress(i);
                if (i <= 0)
                    timer.cancel();
            }
        }, 0, 1000);
        
    }
    
    
    
    
    /**
     * Initializes the controller class.
     */
    public void initialize() {
        
        r1_button.setOnAction((event) -> {
            r1_button.setStyle("-fx-background-color: #3DA428; -fx-font-size: 14px;");
        });
            
        r2_button.setOnAction((event) -> {
            r2_button.setStyle("-fx-background-color: #3DA428; -fx-font-size: 14px;");
        });
           
        r3_button.setOnAction((event) -> {
            r3_button.setStyle("-fx-background-color: #3DA428; -fx-font-size: 14px;");
        });
            
        question_text.setDisable(true);
        question_text.setWrapText(true);
        
        //String[] answers = {"init1","init2","init3"};
            
        //Question q1 = new Question("Teste1", answers, 1, new byte[1], null);
                        
        //createQuestion(q1);
               
        //cleanInterface();
            
    }    
}
