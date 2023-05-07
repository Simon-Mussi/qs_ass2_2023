package at.tugraz.ist.qs2023.messageboard;

/**
 * Exception class used to signal that the message type does not exist
 *
 */
public class UnknownMessageException extends Exception {
    /**
     * Constructs a new UnknownClientException with the specified detail message.
     *
     * @param message the detail message.
     */
    public UnknownMessageException(String message) {
        super(message);
    }
}
