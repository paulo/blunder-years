package Exceptions;

/**
 * @author Grupo 30
 */


public class UserIsNotLoggedInException extends Exception {
    public UserIsNotLoggedInException() {super();}
    public UserIsNotLoggedInException(String s){super(s);}
}