/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package cc.play.features.login;

import cc.pdu.PDUType;
import cc.play.FeatureInterface;

/**
 *
 * @author ruioliveiras
 */
public interface Login extends FeatureInterface{
    public static PDUType[] post = {PDUType.LOGIN_NICK,PDUType.LOGIN_PASS};
    public static PDUType[] response = {PDUType.REPLY_NAME,PDUType.REPLY_NICK,PDUType.REPLY_POINTS};
    
    public default  PDUType getType() {
        return PDUType.LOGIN;
    }

    public default PDUType[] getRequest() {
        return Login.post;    
    }
    
    public default PDUType[] getResponse() {
        return Login.response;    
    } 
}
