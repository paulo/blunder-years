/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package cc.pdu;

import java.io.UnsupportedEncodingException;
import java.net.InetAddress;
import java.net.UnknownHostException;
import java.nio.ByteBuffer;
import java.time.LocalDate;
import java.time.LocalTime;

/**
 *
 * @author ruioliveiras
 */
public enum PDUDataType {

    string {
                public Object read(byte[] b, int offset, int sizeByte) {
                    try {
                        // no need to calculate string size anymore  

                        String str = new String(b, offset, sizeByte, "UTF-8");

                        return str;
                    } catch (UnsupportedEncodingException ex) {
                        // stupid error here?
                    }
                    return null;
                }

                public int getSize(Object o) {
                    return ((String) o).getBytes().length;
                }

                public byte[] toByte(Object o) {
                    try {
                        return ((String) o).getBytes("UTF-8");
                    } catch (UnsupportedEncodingException ex) {
                        throw new RuntimeException(ex);
                    }
                }

            },
    int8 {
                public Byte read(byte[] b, int offset, int sizeByte) {
                    if (sizeByte != 1) {
                        //erro
                    }

                    return (Byte) b[offset];
                }

                public int getSize(Object o) {
                    return 1;
                }

                public byte[] toByte(Object o) {
                    return ByteBuffer.allocate(1).put((Byte) o).array();
                }

            },
    int16 {
                public Short read(byte[] b, int offset, int sizeByte) {
                    if (sizeByte != 2) {
                        //erro
                    }

                    return ByteBuffer.wrap(b, offset, 2).getShort();
                }

                public int getSize(Object o) {
                    return 2;
                }

                public byte[] toByte(Object o) {
                    return ByteBuffer.allocate(2).putShort((Short) o).array();
                }

            },
    int32 {
                public Object read(byte[] b, int offset, int sizeByte) {
                    if (sizeByte != 4) {
                        //erro
                    }
                    return ByteBuffer.wrap(b, offset, 4).getInt();
                }

                public int getSize(Object o) {
                    return 4;
                }

                public byte[] toByte(Object o) {
                    return ByteBuffer.allocate(4).putInt((Integer) o).array();
                }

            },
    date {
                public Object read(byte[] b, int offset, int sizeByte) {
                    ByteBuffer bb = ByteBuffer.wrap(b, offset, 6);

                    if (sizeByte != 6) {
                        //erro
                    }

                    int year = bb.getShort() + 2000;//2000 + (b[offset + 0] << 8) + (b[offset + 1]);
                    int month = bb.getShort();//(b[offset + 2] << 8) + (b[offset + 3]);
                    int day = bb.getShort();//(b[offset + 4] << 8) + (b[offset + 5]);

                    LocalDate ld = LocalDate.of(year, month, day);
                    return ld;
                }

                public int getSize(Object o) {
                    return 6;
                }

                public byte[] toByte(Object o) {
                    LocalDate lt = (LocalDate) o;
                    ByteBuffer b = ByteBuffer.allocate(6);
                    b.putShort((short) (lt.getYear() - 2000));
                    b.putShort((short) lt.getMonthValue());
                    b.putShort((short) lt.getDayOfMonth());
                    return b.array();
                }

            },
    hour {
                public Object read(byte[] b, int offset, int sizeByte) {
                    ByteBuffer bb = ByteBuffer.wrap(b, offset, 6);

                    if (sizeByte != 6) {
                        //erro
                    }

                    int hour = bb.getShort();// 2000 + (b[offset + 0] << 8) + (b[offset + 1]);
                    int min = bb.getShort();//(b[offset + 2] << 8) + (b[offset + 3]);
                    int sec = bb.getShort();// (b[offset + 4] << 8) + (b[offset + 5]);

                    LocalTime lt = LocalTime.of(hour, min, sec);
                    return lt;
                }

                public int getSize(Object o) {
                    return 6;
                }

                public byte[] toByte(Object o) {
                    LocalTime lt = (LocalTime) o;
                    ByteBuffer b = ByteBuffer.allocate(6);
                    b.putShort((short) lt.getHour());
                    b.putShort((short) lt.getMinute());
                    b.putShort((short) lt.getSecond());
                    return b.array();
                }

            },
    ip {
                public Object read(byte[] b, int offset, int sizeByte) {
                    if (sizeByte != 4) {
                        //erro
                    }

                    try {
                        byte[] buffer = new byte[4];
                        ByteBuffer.wrap(b, offset, 4).get(buffer);
                        return InetAddress.getByAddress(buffer);
                    } catch (UnknownHostException ex) {
                        throw new RuntimeException(ex.getCause());
                    }
                }

                public int getSize(Object o) {
                    return 4;
                }

                public byte[] toByte(Object o) {
                    return ((InetAddress) o).getAddress();
                }

            },
    port {
                public Object read(byte[] b, int offset, int sizeByte) {
                    if (sizeByte != 2) {
                        //erro
                    }

                    return ByteBuffer.wrap(b, offset, 2).getShort();
                }

                public int getSize(Object o) {
                    return 2;
                }

                public byte[] toByte(Object o) {
                    return ByteBuffer.allocate(2).putShort(((Integer) o).shortValue()).array();
                }

            },
    byteBlock {
                public Object read(byte[] b, int offset, int sizeByte) {
                    return ByteBuffer.allocate(sizeByte).put(b, offset, sizeByte).array();
                }

                public int getSize(Object o) {
                    return ((byte[]) o).length;
                }

                public byte[] toByte(Object o) {
                    return (byte[]) o;
                }

            },
    nothing {
                public Byte read(byte[] b, int offset, int sizeByte) {
                    return 0;
                }

                public int getSize(Object o) {
                    return 1;
                }

                public byte[] toByte(Object o) {
                    return ByteBuffer.allocate(1).put((byte) 0).array();
                }
            };

    /**
     * Parses an PDUDataType from a certain buffer
     *
     * @param b the buffer array
     * @param offset the offset of the buffer array
     * @return read object Integer is the number of read bytes
     */
    public abstract Object read(byte[] b, int offset, int sizeByte);

    /**
     * Get the size in bytes of the Object o, that is used to calculate buffers
     * offsets
     *
     * @param o
     * @return
     */
    public abstract int getSize(Object o);

    /**
     * Convert to bytes a certain Object.
     *
     * @param o
     * @return return the byte array result
     */
    public abstract byte[] toByte(Object o);
}
