package pp.project.exception;

import java.util.List;

/** Exception class wrapping a list of error messages. */
public class ParseException extends Exception {


    public static final String ANSI_RESET = "\u001B[0m";
    public static final String ANSI_BLACK = "\u001B[30m";
    public static final String ANSI_RED = "\u001B[31m";
    public static final String ANSI_GREEN = "\u001B[32m";
    public static final String ANSI_YELLOW = "\u001B[33m";
    public static final String ANSI_BLUE = "\u001B[34m";
    public static final String ANSI_PURPLE = "\u001B[35m";
    public static final String ANSI_CYAN = "\u001B[36m";
    public static final String ANSI_WHITE = "\u001B[37m";



    private final List<String> messages;

    public ParseException(List<String> messages) {
        super(messages.toString());
        this.messages = messages;
    }

    /** Returns the error messages wrapped in this exception. */
    public List<String> getMessages() {
        return this.messages;
    }

    /** Prints all error messages to stdout, line by line. */
    public void print() {
        System.out.println(ANSI_RED + "ParseException");
        for (String error : getMessages()) {
            System.out.println(error);
        }
        System.out.println(ANSI_RESET);
    }
}