/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package Utils;

/**
 *
 * @author SimaoDias
 */
public class Normalize {
    /**
     * TODO
     * @param size
     * @param number
     * @return 
     */
    public static String number(int size, int number){
        StringBuilder res = new StringBuilder();
        String num = Integer.toString(number);
        
        for(int i = 0; i < size - num.length(); i++){
            res.append(0);
        }
        res.append(number);
        return res.toString();
    }
}
