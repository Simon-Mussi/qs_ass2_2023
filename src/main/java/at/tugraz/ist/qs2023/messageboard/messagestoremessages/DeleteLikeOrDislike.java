package at.tugraz.ist.qs2023.messageboard.messagestoremessages;

import at.tugraz.ist.qs2023.messageboard.clientmessages.RemoveLikeOrDislike;

/**
 * Message used to signal that a like/dislike should be deleted from a message.
 */
public class DeleteLikeOrDislike extends MessageStoreMessage {

    /**
     * user message id of the user message for which the like/dislike should be removed
     */
    public final long messageId;

    /**
     * name of the person which removes the like/dislike
     */
    public final String clientName;

    /**
     * Type, either like or dislike, which should be deleted
     */
    public final RemoveLikeOrDislike.Type typeToDelete;

    public DeleteLikeOrDislike(String clientName, long commId, long messageId, RemoveLikeOrDislike.Type typeToDelete) {
        this.clientName = clientName;
        this.messageId = messageId;
        this.communicationId = commId;
        this.typeToDelete = typeToDelete;
    }
}
