/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package Model;

import java.rmi.RemoteException;

/**
 *
 * @author SimaoDias
 */
public interface TransactionIf {
    void deposit(int amount, AccountIf account) throws RemoteException;
    boolean withdraw(int amount, AccountIf account) throws RemoteException;
}
