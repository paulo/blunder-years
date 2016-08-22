/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package cc.swinggame;

import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.Image;
import javax.swing.ImageIcon;
import javax.swing.JPanel;

/**
 *
 * @author paulo
 */
public class ImagePanel extends JPanel{
    private Image img;
    
    public ImagePanel(Image ic){
        this.img = ic;
        Dimension size = new Dimension(img.getWidth(null), img.getHeight(null));
            setPreferredSize(size);
            setMinimumSize(size);
            setMaximumSize(size);
            setSize(size);
            setLayout(null);
    }
    
    
    @Override
    public void paintComponent(Graphics g) {
        g.drawImage(img, 0, 0, null);
    }
 
}