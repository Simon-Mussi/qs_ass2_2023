package at.tugraz.ist.qs2023.messageboard.messagestoremessages;

import at.tugraz.ist.qs2023.messageboard.clientmessages.Reaction;

/**
 * Message used to signal that a reaction should be added to a message.
 */
public class AddReaction extends MessageStoreMessage {
    /**
     * User message id of the user message which should be reacted to
     */
    public final long messageId;

    /**
     * Name of the person who reacts to the message
     */
    public final String clientName;

    /**
     * The reaction
     */
    public final Reaction.Emoji reaction;

    public AddReaction(String clientName, long messageId, long commId, Reaction.Emoji reaction) {
        this.clientName = clientName;
        this.reaction = reaction;
        this.messageId = messageId;
        this.communicationId = commId;
    }
}
