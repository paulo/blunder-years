/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package cc.play;

import cc.play.features.Server;

/**
 *
 * @author ruioliveiras
 */
public abstract class FeatureServer extends Feature{
    protected Server facade;

    public FeatureServer(Server facade) {
        this.facade = facade;
    }
    
    
}
