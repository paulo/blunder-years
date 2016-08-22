package Exceptions;

/**
 * @author Grupo 30
 */


public class UserIsAlreadyLoggedInException extends Exception{
    public UserIsAlreadyLoggedInException() {super();}
    public UserIsAlreadyLoggedInException(String s){super(s);}
}
