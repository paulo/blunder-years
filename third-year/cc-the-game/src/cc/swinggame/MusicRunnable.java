/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package cc.swinggame;

import com.sun.xml.internal.messaging.saaj.util.ByteOutputStream;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.util.logging.Level;
import java.util.logging.Logger;
import javafx.embed.swing.JFXPanel;
import javafx.scene.media.Media;
import javafx.scene.media.MediaPlayer;
import sun.audio.AudioData;
import sun.audio.AudioDataStream;
import sun.audio.AudioPlayer;
import sun.audio.AudioStream;
import sun.audio.ContinuousAudioDataStream;

/**
 *
 * @author paulo
 */
public class MusicRunnable implements Runnable {
    private JFXPanel fxPanel;
    //private String music_url;
    private volatile boolean musicRunning;
    private volatile Object musicLock = new Object();
    
    private String path;
    
    
    public MusicRunnable(){
        fxPanel = new JFXPanel();
        //this.music_url = null;
        this.musicRunning = false;
//        this.musicArray = null;
    }
    
    public void terminateMusic(){
        this.musicRunning = false;
        synchronized(musicLock){
            musicLock.notifyAll();
        }
    }
    
    //alternativa...
    public void setSong(byte[] byteArray){
      musicRunning = true;
        FileOutputStream out = null;
        try {
            path = "temp/" + System.currentTimeMillis()+ ".mp3";
            out = new FileOutputStream(path);
            out.write(byteArray);
            
            // loop()
            /*ContinuousAudioDataStream continuousStream =
            new ContinuousAudioDataStream(audioStream);
            AudioPlayer.player.start(continuousStream);*/
        } catch (FileNotFoundException ex) {
            Logger.getLogger(MusicRunnable.class.getName()).log(Level.SEVERE, null, ex);
        } catch (IOException ex) {
            Logger.getLogger(MusicRunnable.class.getName()).log(Level.SEVERE, null, ex);
        } finally {
            try {
                out.close();
            } catch (IOException ex) {
                Logger.getLogger(MusicRunnable.class.getName()).log(Level.SEVERE, null, ex);
            }
        }

    }
    
    
    
    /*
    Quando for para tratar da musica em arrays de bytes, provavelmente vai ser necessario guardar as coisas em ficheiro
    */        
    @Override
    public void run() {
        File songfile = new File(path);
        try{
        Media media = new Media(songfile.toURI().toString());
        MediaPlayer music_player = new MediaPlayer(media);
        music_player.play();    
        
        while(musicRunning){
          synchronized(musicLock){
              try {
                  musicLock.wait();
              } catch (InterruptedException ex) {
                  new RuntimeException("interropted");
              }
          }
        }
        
        music_player.pause();
        music_player.stop();
        }catch(Exception ex){
            // smile and wave
        }
        
        
//        AudioData audiodata = new AudioData(musicArray);
//         
//        //AudioData audioData = new AudioStream(url.openStream()).getData();
//        AudioDataStream audioStream = new AudioDataStream(audiodata);
//        // play()
//        AudioPlayer.player.start(audioStream);
//        
//        while(musicRunning);      
//        
//        AudioPlayer.player.stop(audioStream);
    }
}
