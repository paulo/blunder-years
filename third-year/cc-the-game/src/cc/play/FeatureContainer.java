/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package cc.play;

import cc.pdu.PDU;
import cc.pdu.PDUType;
import cc.play.features.FeatureExeption;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Stream;
import util.T2;

/**
 *
 * @author ruioliveiras
 */
public class FeatureContainer {

    protected final Map<PDUType, List<FeatureServer>> featuresHandler = new HashMap<>();
    
    protected void init(FeatureServer ... args){
        Stream.of(args)
                .peek(a -> {
                    if (!featuresHandler.containsKey(a.getType())) featuresHandler.put(a.getType(), new ArrayList<>());
                })
                .forEach(a -> featuresHandler.get(a.getType()).add(a));
    }
    
    protected PDU handle(PDU pdu) {
        List<FeatureServer> list = featuresHandler.get(pdu.getType());

        if (list != null) {
            PDU ret = list.stream()
                    .map(f -> new T2<>(f, f.unbuildPDU_req(pdu)))
                    .filter(t -> (t.b != null))
                    .findFirst()
                    .map(t -> t.a.buildPDU_res(t.a.go(t.b)))
                    .orElseThrow(() -> new FeatureExeption("don't exist"));
            return ret;
        } else {
            throw new FeatureExeption("don't exist");
        }
    }
}
