/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package cc.offline;

import cc.client.ClientBash;
import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;

/**
 *
 * @author ruioliveiras
 */
public class Client2 {
        public static void main(String[] args) {

        ClientBash c_bash = new ClientBash("127.0.0.47",5050);
        BufferedReader in = new BufferedReader(new InputStreamReader(System.in));
        String command;

        try {
            while (!(command = in.readLine()).toUpperCase().equals("END")) {
                c_bash.execute(command.toUpperCase());
            }
            
            System.out.println("Obrigado e Até à Próxima!");
        } catch (IOException ex) {
            System.out.println("Erro ao ler do stdin na Bash");
        }

        c_bash.getUDPClient().closeCSocket();
    }
}
