/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package cc.play.features.login;

import cc.pdu.PDUType;
import cc.play.FeatureServer;
import cc.play.features.Server;
import util.T3;

/**
 *
 * @author ruioliveiras
 */
public class LoginServer extends FeatureServer implements Login {

    public LoginServer(Server facade) {
        super(facade);
    }

    public T3<String, String, Integer> go(String nick, String pass) {
        // do some fucking login
        return null;
    }

    @Override
    public Object[] go(Object... args) {
        return go((String) args[0], (String) args[1]).toArray();
    }
}
