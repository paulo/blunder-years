/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package cc.play.features.login;

import cc.pdu.PDUType;
import cc.play.FeatureClient;
import util.T3;

/**
 *
 * @author ruioliveiras
 */
public class LoginClient extends FeatureClient implements Login{

    public T3<String,String,Integer> go(String nick, String pass) {
        Object[] ret = super.go(nick,pass);
        return new T3<>((String) ret[0],(String) ret[1],(Integer) ret[2]);      
    }
}
