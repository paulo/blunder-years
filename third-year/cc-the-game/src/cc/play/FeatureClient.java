/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package cc.play;

import cc.pdu.PDUType;

/**
 *
 * @author ruioliveiras
 */
abstract public class FeatureClient extends Feature {

    /**
     * in the case of the Feature in add case just send a PDU and get response
     * @see Feature.go(Object...args)
     */
    @Override
    public Object[] go(Object... args) {
        return send(args);
    }

    public Object[] send(Object... args) {
        getRequest();
        
        return null;
    }

}
