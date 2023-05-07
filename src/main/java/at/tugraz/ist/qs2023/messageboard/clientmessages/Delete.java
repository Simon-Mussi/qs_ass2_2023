package at.tugraz.ist.qs2023.messageboard.clientmessages;

/**
* Message sent from client to worker to delete an existing message.
*/
public class Delete extends ClientMessage {

    /**
     * The user message id of the message to be deleted
     */
    public final long messageId;

    /**
     * The name of the person who deletes the message
     */
    public final String clientName;

    public Delete(long mId, String clientName, long communicationId) {
        super(communicationId);
        this.messageId = mId;
        this.clientName = clientName;
    }

    @Override
    public int getDuration() {
        return 1;
    }
}
