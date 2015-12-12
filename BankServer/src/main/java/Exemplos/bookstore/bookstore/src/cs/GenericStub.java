package cs;

import java.net.InetAddress;

public class GenericStub {
	private InetAddress host;
	private int port;
	private Object id;
	
	public GenericStub(InetAddress host, int port, Object id) {
		super();
		this.host = host;
		this.port = port;
		this.id = id;
	}
	
	protected Object inv(Object req) {
		
	}
}
