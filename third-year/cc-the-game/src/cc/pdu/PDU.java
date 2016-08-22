/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package cc.pdu;

import java.nio.ByteBuffer;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 *
 * @author ruioliveiras
 */
public class PDU {

    public final static int HEADER_SIZE_BYTES = 8;

    // header parameters
    private int version = 1; // 1 byte
    private boolean secure; // 1 byte
    private int label; // 2 byte
    private PDUType pduType; // 1 byte
    private int nField; //2 bytes
    private int sizeBytes; //1 byte
    // Other Parameters
    private Map<PDUType, List<Object>> parameters;
    private boolean pduContinue;

    public PDU() {
        this.parameters = new HashMap<>();
    }

    public PDU(int version, PDUType type) {
        this();
        this.version = version;
        this.pduType = type;
    }
    
    public PDU(PDUType type) {
        this();
        this.pduType = type;
    }

    public PDU(int version, boolean secure, int label, PDUType pduType, int nField) {
        this.version = version;
        this.secure = secure;
        this.label = label;
        this.pduType = pduType;
        this.nField = nField;
        this.parameters = new HashMap<>();
    }

    /**
     * This function initialize the headers of the PDU from a buffer of Bytes.
     * After call this function should call initParametersFromBytes, to
     * initialize the parameters
     *
     * @param b
     * @param offset
     * @return boolean with success
     */
    public boolean initHeaderFromBytes(byte[] b, int offset) {
        if (b.length < 8) {
            //throw new Exec
        }
        ByteBuffer bb = ByteBuffer.wrap(b, offset, b.length - offset);
        version = bb.get();
        secure = bb.get() != 0;
        label = bb.getShort();
        pduType = PDUType.getById(bb.get());
        nField = bb.get();
        short a = bb.getShort();
        sizeBytes = a & 0xFFFF;

        return true;
    }

    /**
     *
     * @param b
     * @param offset1
     */
    public void initParametersFromBytes(byte[] b, int offset1) {
        int offset = 0, i = 0;

        pduContinue = false;

        while (offset1 + offset < b.length && offset < sizeBytes && i < nField) {
            // the first byte are the identifier of the Parameter Type.
            int test = (int) (b[offset1 + offset++] & 0xFF);
            if (test < 0) {
                return;
            }
            PDUType parameter = pduType.getParameterById(test);

            short s = ByteBuffer.wrap(b, offset1 + offset, 2).getShort();
            int byteSize = s & 0xFFFF;
            // b[i]00101111 b[i+1] 10000001   0010111110000001  0 1111 1111 1000 0001

            offset += 2;
            Object obj = parameter.getDataType().read(b, offset + offset1, byteSize);

            offset += parameter.getDataType().getSize(obj);


            if (parameter.equals(PDUType.CONTINUE)) {
                pduContinue = true;
            }

            this.addParameter(parameter, obj);

            i++;
        }
        if (i != nField) {
            return;
        }
    }

    public byte[] toByte() {
        //calc the number of bytes need to the buffer
        sizeBytes = 0;
        nField = 0;
        parameters.entrySet().stream().forEach((entrySet) -> {
            PDUType key = entrySet.getKey();
            for (Object value : entrySet.getValue()) {
                sizeBytes += 3 + key.getDataType().getSize(value);
                nField++;
            }
//            sizeBytes += entrySet.getValue().stream()
//                    .reduce(0,
//                            (sum, b) -> sum + ,
//                            Integer::sum
//                    ).intValue();
        });
        //plus 8 because of header  
        final ByteBuffer b = ByteBuffer.allocate(sizeBytes + 8);
        //header
        b.put((byte) version);
        b.put((byte) ((secure) ? 1 : 0));
        b.putShort((short) label);
        b.put((byte) pduType.getId());
        b.put((byte) nField);
        b.putShort((short) sizeBytes);
        if(sizeBytes > Short.MAX_VALUE * 2){
            throw  new RuntimeException("max size of PDU");
        }
        
        //params:
        parameters.entrySet().stream()
                .forEach((entrySet) -> {
                    final PDUType key = entrySet.getKey();
                    entrySet.getValue().stream()
                    .forEach((value) -> {
                        b.put((byte) key.getId());
                        b.putShort((short) key.getDataType().getSize(value));
                        if (key.getDataType().getSize(value) > 1000) {
                            short a = (short) key.getDataType().getSize(value);
                            byte[] bbbb = ByteBuffer.allocate(10).putShort(a).array();
                            int asd = bbbb.length;
                            //return;
                        }
                        b.put(key.getDataType().toByte(value));
                    });
                });
        return b.array();
    }

    public int getParameterSizeBytes() {
        return sizeBytes;
    }

    public int getSizeBytes() {
        // plus header
        return sizeBytes + 8;
    }

    public int getFieldCount() {
        return nField;
    }

    public int getVersion() {
        return version;
    }

    public PDUType getType() {
        return pduType;
    }

    public boolean isSecure() {
        return secure;
    }

    public int getLabel() {
        return label;
    }

    public void setLabel(int label) {
        this.label = label;
    }

    public Object popParameter(PDUType p) {
        return parameters.get(p).remove(0);
    }

    public List<Object> getAllParameter(PDUType p) {
        return parameters.get(p);
    }

    public boolean hasParameter(PDUType p) {
        return parameters.containsKey(p)
                && parameters.get(p).size() > 0;
    }

    public boolean hasContinue() {
        return this.pduContinue;
    }

    // is fragmeted 
    public void addParameter(PDUType pduType, Object obj) {
        List<Object> l;
        if ((l = parameters.get(pduType)) == null) {
            l = new ArrayList<>();
            parameters.put(pduType, l);
        }
        l.add(obj);
    }

    //.. outrs funções uteis por index
    /*Versão [1 byte] (por defeito, 0)
     Segurança [1 byte] (por defeito, 0 – sem segurança)
     Label [2 bytes] (identificação que associa respostas a um pedido, 0 – broadcasting)
     Tipo [1 byte] (0 – REPLY, 1 – HELLO, 2 – REGISTER, ...)
     Número de Campos Seguintes [1 byte]
     Tamanho em bytes da Lista de Campos Seguintes [2 bytes]
     Lista de Campos Seguintes (dependente do Tipo, pode ser vazia)
     */
    @Override
    public String toString() {
        return "PDU,parameters:" + parameters.toString();
    }

}
