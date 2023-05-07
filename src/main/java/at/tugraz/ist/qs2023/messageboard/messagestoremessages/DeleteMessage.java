package at.tugraz.ist.qs2023.messageboard.messagestoremessages;

/**
 * Message used to signal that a UserMessage should be deleted.
 */
public class DeleteMessage extends MessageStoreMessage {
    /**
     * UserMessage id of the UserMessage which should be deleted
     */
    public final long messageId;
    /**
     * Name of the person who tries to delete the message
     */
    public final String clientName;

    public DeleteMessage(String clientName, long messageId, long commId) {
        this.clientName = clientName;
        this.messageId = messageId;
        this.communicationId = commId;
    }
}
