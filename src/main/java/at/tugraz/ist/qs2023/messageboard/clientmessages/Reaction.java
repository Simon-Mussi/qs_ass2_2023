package at.tugraz.ist.qs2023.messageboard.clientmessages;

/**
 * Message sent from client to worker to signal that a reaction should be added to a given user message.
 */
public class Reaction extends ClientMessage {
    public enum Emoji {
        SMILEY,
        LAUGHING,
        FROWN,
        CRYING,
        HORROR,
        SURPRISE,
        SKEPTICAL,
        COOL
    }

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

    public Reaction(String clientName, long communicationId, long mId, Emoji reaction) {
        super(communicationId);
        this.clientName = clientName;
        this.messageId = mId;
        this.reaction = reaction;
    }

    @Override
    public int getDuration() {
        return 1;
    }
}
