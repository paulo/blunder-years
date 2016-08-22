/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package cc.pdu;

/**
 * This is an hierarchy Enum (This is just a programming pattern).
 *
 * 1º the first level of the hierarchy is the getDataType of The PDU and uses
 the constructor " PDUType(int getId, int argSize)" where the getId is the
 identifier of the PDU, and the argSize is the number of parameter.

 2º the second level represents the possibles parameters of the PDU
 getDataType, for instance: an INFO PDU can have: an name (INFO_NAME) or nick
 (INFO_NICK); the constructor used are "PDUType(PDUType getParent, int getId,
 PDUDataType dataType)" where the Parent is the correspondent getDataType of
 the current parameter; the getId is the identifier of this parameter the
 getDataType is the DataType of this parameter (Ex: string, int32...)
 *
 * @author ruioliveiras
 */
public enum PDUType {

    HELLO(1, 0),
    REGISTER(2, 3),
        REGISTER_NAME(REGISTER,1,PDUDataType.string),
        REGISTER_NICK(REGISTER,2,PDUDataType.string),
        REGISTER_PASS(REGISTER,3,PDUDataType.byteBlock), // should not be string?
    LOGIN(3, 2),
        LOGIN_NICK(LOGIN,2,PDUDataType.string),
        LOGIN_PASS(LOGIN,3,PDUDataType.byteBlock),
    LOGOUT(4, 0),
    QUIT(5, 0),
    END(6, 0),
    LIST_CHALLENGES(7, 0),
    MAKE_CHALLENGE(8, 3),
        MAKE_CHALLENGE_CHALLENGE(MAKE_CHALLENGE,7,PDUDataType.string),
        MAKE_CHALLENGE_DATE(MAKE_CHALLENGE,4,PDUDataType.date),
        MAKE_CHALLENGE_HOUR(MAKE_CHALLENGE,5,PDUDataType.hour),
    ACCEPT_CHALLENGE(9, 1),
        ACCEPT_CHALLENGE_CHALLENGE(ACCEPT_CHALLENGE,7,PDUDataType.string),
    DELETE_CHALLENGE(10, 1),
        DELETE_CHALLENGE_CHALLENGE(DELETE_CHALLENGE,7,PDUDataType.string),
    ANSWER(11, 3),
        ANSWER_CHOOSE(ANSWER,6,PDUDataType.int8),
        ANSWER_CHALLENGE(ANSWER,7,PDUDataType.string),
        ANSWER_NQUESTION(ANSWER,10,PDUDataType.int8),
    RETRANSMIT(12, 3),
        RETRANSMIT_CHALLENGE(RETRANSMIT,7,PDUDataType.string),
        RETRANSMIT_NBLOCK(RETRANSMIT,10,PDUDataType.int8),
        RETRANSMIT_NQUESTION(RETRANSMIT,17,PDUDataType.int8),
    LIST_RANKING(13, 0),
    REPLY(0, 18),
        REPLY_OK(REPLY, 0 ,PDUDataType.nothing),
        REPLY_ERRO(REPLY, 255 ,PDUDataType.string),
        CONTINUE(REPLY, 254 ,PDUDataType.nothing),
        REPLY_NAME(REPLY, 1 ,PDUDataType.string),
        REPLY_NICK(REPLY, 2 ,PDUDataType.string),
        REPLY_DATE(REPLY, 4 ,PDUDataType.date),
        REPLY_HOUR(REPLY, 5 ,PDUDataType.hour),
        REPLY_CHALLE(REPLY, 7 ,PDUDataType.string),
        REPLY_NUM_QUESTION(REPLY, 10 ,PDUDataType.int8),
        REPLY_QUESTION(REPLY, 11 ,PDUDataType.string),
        REPLY_NUM_ANSWER(REPLY, 12 ,PDUDataType.int8),
        REPLY_ANSWER(REPLY, 13 ,PDUDataType.string),
        REPLY_CORRECT(REPLY, 14 ,PDUDataType.int8),
        REPLY_POINTS(REPLY, 15 ,PDUDataType.int8),
        REPLY_IMG(REPLY, 16 ,PDUDataType.byteBlock),
        REPLY_NUM_BLOCK(REPLY, 17 ,PDUDataType.int8),
        REPLY_BLOCK(REPLY, 18 ,PDUDataType.byteBlock),
        REPLY_SCORE(REPLY, 20 ,PDUDataType.int16),
    INFO(31, 10),
        INFO_NAME(INFO, 1, PDUDataType.string),
        INFO_NICK(INFO, 2, PDUDataType.string),
        INFO_CHALLE(INFO, 7, PDUDataType.string),
        INFO_DATE(INFO, 4, PDUDataType.date),
        INFO_HOUR(INFO, 5, PDUDataType.hour),
        INFO_NQUEST(INFO, 10, PDUDataType.int32),
        INFO_QUEST(INFO, 11, PDUDataType.string),
        INFO_NRESP(INFO, 12, PDUDataType.int32),
        INFO_RESP(INFO, 13, PDUDataType.string),
        INFO_CERT(INFO, 14, PDUDataType.string),
        INFO_IMAGE(INFO, 16, PDUDataType.string),
        INFO_MUSIC(INFO, 19, PDUDataType.string),
        INFO_SCORE(INFO, 20, PDUDataType.int32),
        INFO_IPSERVER(INFO, 30, PDUDataType.ip),
        INFO_PORT(INFO, 31, PDUDataType.port),
    TCP_CONTROL(500,0);
    //--------------------
    private int id, numParam;
    private PDUType parent;
    private PDUDataType dataType;

    private PDUType() {
    }

    private PDUType(int id, int numParam) {
        this.id = id;
        this.numParam = numParam;
        parent = null;
        dataType = null;

    }

    private PDUType(PDUType parent, int id, PDUDataType dataType) {
        this.parent = parent;
        this.dataType = dataType;
        this.id = id;
    }

    public int getId() {
        return id;
    }

    public PDUType getParent() {
        return parent;
    }

    public PDUDataType getDataType() {
        return dataType;
    }

    public int getNumParam() {
        return numParam;
    }

    /**
     * Get all the parameters of the current PDUType
     *
     * @return the array of the PDUType parameters
     */
    public PDUType[] getParameters() {
        int i = 0;
        PDUType[] res = new PDUType[numParam];
        // for all values of this enum (PDUType.values())
        for (PDUType value : PDUType.values()) {
            if (value.getParent() != null
                    && value.getParent().getId() == id) {
                res[i++] = value;
            }
        }
        return res;
    }

    /**
     * Get the parameter of the current PDUType with a certain Id
     *
     * @param paramId wanted id
     * @return the wanted PDUType Parameter or null if don't exists.
     */
    public PDUType getParameterById(int paramId) {
        for (PDUType value : values()) {
            if (value.getParent() != null
                    && value.getParent().getId() == this.id
                    && value.id == paramId) {
                return value;
            }
        }
        return null;
    }

    /**
     * Get a PDUType from all the values with a certain id
     *
     * @param id wanted id
     * @return the wanted PDUType or null if don't exists.
     */
    public static PDUType getById(int id) {
        for (PDUType value : values()) {
            if (value.getParent() == null 
                    && value.getId() == id) {
                return value;
            }
        }
        return null;
    }
}
