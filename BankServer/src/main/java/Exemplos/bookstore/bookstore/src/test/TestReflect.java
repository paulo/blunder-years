package test;

import java.lang.reflect.Method;

import bookstore.StoreIf;

public class TestReflect {
	public static void main(String[] args) throws Exception {
		for(Method m: StoreIf.class.getMethods()) {
			System.out.println("Metodo "+m.getName());
			for(Class c: m.getParameterTypes()) {
				System.out.println("  - "+c.getName());
			}
		}
	}
}
