package at.tugraz.ist.qs2023.messageboard.messagestoremessages;

/**
 * Message used to signal that a message should be edited.
 */
public class EditMessage extends MessageStoreMessage {

    /**
     * The messageId of the message to be edited
     */
    public final long messageId;

    /**
     * The name of the person who edits the message
     */
    public final String clientName;

    /**
     * New message which should replace the old
     */
    public final String newMessage;

    public EditMessage(long messageId, String clientName, String newMessage, long communicationId) {
        this.messageId = messageId;
        this.clientName = clientName;
        this.newMessage = newMessage;
        this.communicationId = communicationId;
    }
}
