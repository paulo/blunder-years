/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package cc.play.features;

/**
 *
 * @author ruioliveiras
 */
public class FeatureExeption extends RuntimeException{
    public FeatureExeption(String message) {
        super(message);
    }
}
