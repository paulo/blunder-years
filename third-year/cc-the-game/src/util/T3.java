/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package util;

import java.util.Arrays;

/**
 *
 * @author ruioliveiras
 * @param <A>
 * @param <B>
 * @param <C>
 */
public class T3<A,B,C> {
    public A a;
    public B b;
    public C c;

    public T3() {
    }

    public T3(A a, B b, C c) {
        this.a = a;
        this.b = b;
        this.c = c;
    }
    
    public Object[] toArray(){
        return new Object[]{a,b,c};
    }
}
