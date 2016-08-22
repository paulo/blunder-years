/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package cc.play.features;

import cc.play.FeatureContainer;
import cc.play.features.login.LoginServer;
import cc.server.tcpServer.ServerState;

/**
 *
 * @author ruioliveiras
 */
public class Server extends FeatureContainer{
    public final ServerState state = new ServerState(null, 0);
    public final LoginServer login = new LoginServer(this);

    public Server() {
        init(login);
    }

    

}
