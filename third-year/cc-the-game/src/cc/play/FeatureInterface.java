/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package cc.play;

import cc.pdu.PDUType;
import cc.play.features.login.Login;

/**
 *
 * @author ruioliveiras
 */
public interface FeatureInterface {
    public PDUType getType();

    public PDUType[] getRequest();

    
    public PDUType[] getResponse();
}
