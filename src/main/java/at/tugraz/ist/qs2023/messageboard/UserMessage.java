package at.tugraz.ist.qs2023.messageboard;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

import at.tugraz.ist.qs2023.messageboard.clientmessages.Reaction;

/**
 * This class represents actual messages posted by users (do not confuse
 * with message passed between actors).
 */
public class UserMessage {

    /**
     * ID for new messages
     */
    public final static long NEW_ID = -1;

    /**
     * The author of the message
     */
    private final String author;

    /**
     * The message posted by the author
     */
    private String message;

    /**
     * Likes for the message (initially empty).
     * The strings in the list are names of people who like the message.
     */
    private final List<String> likes;

    /**
     * Dislikes for the message (initially empty).
     * The strings in the list are names of people who dislike the message.
     */
    private final List<String> dislikes;

    /**
     * The points of the message.
     * Each like adds 2 points,
     * each dislike subtracts 1 point.
     * Initially it is 0 points.
     */
    private int points = 0;

    /**
     * Reactions or emojis for the message (initially empty).
     * The strings in the list are names of people who dislike the message.
     */
    private final Map<String, Set<Reaction.Emoji>> reactions;

    /**
     * Invariant, only NEW and positive IDs are used.
     * ID of the message to be able to refer to it.
     * Only positive numbers and <c>UserMessage.NEW</c>
     * are allowed as IDs.
     */
    private long messageId;

    /**
     * Constructs the new UserMessage object
     *
     * @param author  author of the message
     * @param message posted message string
     */
    public UserMessage(String author, String message) {
        this.author = author;
        this.message = message;
        this.likes = new ArrayList<>();
        this.dislikes = new ArrayList<>();
        this.reactions = new HashMap<>();
        this.messageId = NEW_ID;
    }

    /**
     * Newly added toString()-method, which returns a string representation
     * of user messages.
     *
     * @return string-representation of User Message
     */
    @Override
    public String toString() {
        return author + ": " + message + ", liked by : " + likes.stream().sorted().collect(
                Collectors.joining(",")) + ", disliked by : " + dislikes.stream().sorted().collect(
                Collectors.joining(","))
                + ", points: " + points;
    }

    public String getAuthor() {
        return author;
    }

    public String getMessage() {
        return message;
    }

    public void setMessage(String message) {
        this.message = message;
    }

    public List<String> getLikes() {
        return likes;
    }

    public List<String> getDislikes() {
        return dislikes;
    }

    public int getPoints() {
        return points;
    }

    public void setPoints(int newPoints) {
        points = newPoints;
    }

    public Map<String, Set<Reaction.Emoji>> getReactions() {
        return reactions;
    }

    public long getMessageId() {
        return messageId;
    }

    public void setMessageId(long messageId) {
        this.messageId = messageId;
    }
}
