package at.tugraz.ist.qs2023.messageboard.clientmessages;

/**
 * Message sent from client to worker to edit an existing message.
 */
public class Edit extends ClientMessage {

    /**
     * The messageId of the message to be edited
     */
    public final long messageId;

    /**
     * The name of the person who edits the message
     */
    public final String clientName;

    /**
     * The new message
     */
    public final String newMessage;


    public Edit(long messageId, String clientName, String newMessage, long communicationId) {
        super(communicationId);
        this.messageId = messageId;
        this.clientName = clientName;
        this.newMessage = newMessage;
    }

    @Override
    public int getDuration() {
        return 1;
    }
}
