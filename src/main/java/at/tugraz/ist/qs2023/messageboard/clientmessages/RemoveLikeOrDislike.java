package at.tugraz.ist.qs2023.messageboard.clientmessages;


/**
 * Message used to signal that a like/dislike should be deleted from a message.
 */
public class RemoveLikeOrDislike extends ClientMessage {
    public enum Type {
        LIKE,
        DISLIKE
    }

    /**
     * User message id of the user message for which the like/dislike should be removed
     */
    public final long messageId;

    /**
     * Name of the person which removes the like/dislike
     */
    public final String clientName;

    /**
     * Type, either like or dislike, which should be deleted
     */
    public final Type typeToDelete;

    public RemoveLikeOrDislike(String clientName, long communicationId, long mId, Type typeToDelete) {
        super(communicationId);
        this.clientName = clientName;
        this.messageId = mId;
        this.communicationId = communicationId;
        this.typeToDelete = typeToDelete;
    }

    @Override
    public int getDuration() {
        return 1;
    }
}
