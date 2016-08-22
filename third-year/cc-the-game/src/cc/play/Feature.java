/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package cc.play;

import cc.pdu.PDU;
import cc.pdu.PDUType;

/**
 *
 * @author ruioliveiras
 */
public abstract class Feature implements FeatureInterface {

    abstract public Object[] go(Object... args);

    protected Object[] unbuildPDU(PDUType[] args, PDU pdu) {
        return null;
    }

    protected PDU buildPDU(PDUType[] args, Object... objs) {
        return null;
    }

    public Object[] unbuildPDU_res(PDU pdu) {
        return unbuildPDU(getResponse(), pdu);
    }

    public PDU buildPDU_res(Object... objs) {
        return buildPDU(getResponse(), objs);
    }

    public Object[] unbuildPDU_req(PDU pdu) {
        return unbuildPDU(getRequest(), pdu);
    }

    public PDU buildPDU_req(Object... objs) {
        return buildPDU(getRequest(), objs);
    }

}
