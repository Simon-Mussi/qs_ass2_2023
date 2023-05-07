package at.tugraz.ist.qs2023.messageboard;

import at.tugraz.ist.qs2023.actorsystem.DeterministicChannel;
import at.tugraz.ist.qs2023.actorsystem.Message;
import at.tugraz.ist.qs2023.actorsystem.SimulatedActor;
import at.tugraz.ist.qs2023.messageboard.clientmessages.*;
import at.tugraz.ist.qs2023.messageboard.messagestoremessages.*;

import java.util.*;

/**
 * Actor responsible for storage and retrieval of user messages.
 */
public class MessageStore extends SimulatedActor {

    /**
     * the amount of reports a user is blocked at
     */
    public final static int USER_BLOCKED_AT_COUNT = 5;
    /**
     * All reports, the key in the dictionary corresponds to a
     * client name and the value is a set of client names that
     * have reported that user.
     */
    private final Map<String, HashSet<String>> reports;
    /**
     * All messages stored, the key of the dictionary corresponds to
     * the message ID of the user message stored as value.
     */
    protected Map<Long, UserMessage> messages;
    /**
     * integral number which is used to create new message IDs
     */
    private long currentId;

    /**
     * Constructs a new MessageStore object, the channel is set to a
     * deterministic channel with no delay to simulate a good connection to
     * the store.
     */
    public MessageStore() {
        this.messages = new HashMap<>();
        this.reports = new HashMap<>();

        this.currentId = 0;
        // good connection between WorkerHelper and MessageStore -> no delay
        this.channel = new DeterministicChannel(0);
    }

    /**
     * The message processing logic for the store.
     * <p>
     * If the message passed as parameter is of type <c>RetrieveFromStore</c>,
     * all messages of a given author are looked up and sent back to the client of the
     * store.
     * <p>
     * If the message passed as parameter is of type <c>AddLike</c>, a
     * like is added to the given message if the message exists and has not
     * already been liked by the given person.
     * <p>
     * If the message passed as parameter is of type <c>AddDislike</c>, a
     * dislike is added to the given message if the message exists and has not
     * already been disliked by the given person.
     * <p>
     * If the message passed as parameter is of type <c>UpdateMessageStore</c>,
     * a message is stored if the message is new and if the same message has not already
     * been stored by the same author.
     * <p>
     * If the message passed as parameter is of type <c>AddReport</c>, a
     * report is added to the specified user if he has not already been reported
     * by the same user. If a user has been reported by more than 5 other users,
     * he cannot like, dislike, delete, edit, report or update/publish any messages.
     * <p>
     * In case of success a OperationAck message is sent to the client, otherwise
     * an UserBanned message or an OperationFailed message is sent, depending
     * on if the user was reported too often.
     *
     * @param message Non-null message received
     */
    @Override
    public void receive(Message message) {
        if (message instanceof RetrieveFromStore) {
            RetrieveFromStore retrieve = (RetrieveFromStore) message;
            List<UserMessage> foundMessage = findByAuthor(retrieve.author);
            retrieve.storeClient.tell(new FoundMessages(foundMessage, retrieve.communicationId));
        } else if (message instanceof AddLike) {
            AddLike addLikeMessage = (AddLike) message;
            if (isBanned(addLikeMessage.clientName)) {
                addLikeMessage.storeClient.tell(new UserBanned(addLikeMessage.communicationId));
            } else if (addLike(addLikeMessage.clientName, addLikeMessage.messageId)) {
                UserMessage likedMessage = messages.get(addLikeMessage.messageId);
                addLikeMessage.storeClient.tell(new ReactionResponse(addLikeMessage.communicationId, likedMessage.getPoints()));
            } else {
                addLikeMessage.storeClient.tell(new OperationFailed(addLikeMessage.communicationId));
            }
        } else if (message instanceof AddDislike) {
            AddDislike addDislikeMessage = (AddDislike) message;
            if (isBanned(addDislikeMessage.clientName)) {
                addDislikeMessage.storeClient.tell(new UserBanned(addDislikeMessage.communicationId));
            } else if (addDislike(addDislikeMessage.clientName, addDislikeMessage.messageId)) {
                UserMessage dislikedMessage = messages.get(addDislikeMessage.messageId);
                addDislikeMessage.storeClient.tell(new ReactionResponse(addDislikeMessage.communicationId, dislikedMessage.getPoints()));
            } else {
                addDislikeMessage.storeClient.tell(new OperationFailed(addDislikeMessage.communicationId));
            }
        } else if (message instanceof DeleteLikeOrDislike) {
            DeleteLikeOrDislike deleteLikeOrDislikeMessage = (DeleteLikeOrDislike) message;
            if (isBanned(deleteLikeOrDislikeMessage.clientName)) {
                deleteLikeOrDislikeMessage.storeClient.tell(new UserBanned(deleteLikeOrDislikeMessage.communicationId));
            } else if (deleteLikeOrDislike(deleteLikeOrDislikeMessage.clientName, deleteLikeOrDislikeMessage.messageId,
                    deleteLikeOrDislikeMessage.typeToDelete)) {
                UserMessage likeOrDislikeMessageDeleted = messages.get(deleteLikeOrDislikeMessage.messageId);
                deleteLikeOrDislikeMessage.storeClient.tell(new ReactionResponse(deleteLikeOrDislikeMessage.communicationId, likeOrDislikeMessageDeleted.getPoints()));
            } else {
                deleteLikeOrDislikeMessage.storeClient.tell(new OperationFailed(deleteLikeOrDislikeMessage.communicationId));
            }
        } else if (message instanceof AddReaction) {
            AddReaction addReactionMessage = (AddReaction) message;
            if (isBanned(addReactionMessage.clientName)) {
                addReactionMessage.storeClient.tell(new UserBanned(addReactionMessage.communicationId));
            } else if (addReaction(addReactionMessage.clientName, addReactionMessage.messageId, addReactionMessage.reaction)) {
                addReactionMessage.storeClient.tell(new ReactionResponse(addReactionMessage.communicationId, addReactionMessage.reaction.toString()));
            } else {
                addReactionMessage.storeClient.tell(new OperationFailed(addReactionMessage.communicationId));
            }
        } else if (message instanceof UpdateMessageStore) {
            UpdateMessageStore updateMessage = (UpdateMessageStore) message;
            if (isBanned(updateMessage.message.getAuthor())) {
                updateMessage.storeClient.tell(new UserBanned(updateMessage.communicationId));
            } else if (update(updateMessage.message)) {
                updateMessage.storeClient.tell(new OperationAck(updateMessage.communicationId));
            } else {
                updateMessage.storeClient.tell(new OperationFailed(updateMessage.communicationId));
            }
        } else if (message instanceof AddReport) {
            AddReport reportMessage = (AddReport) message;
            if (isBanned(reportMessage.clientName)) {
                reportMessage.storeClient.tell(new UserBanned(reportMessage.communicationId));
            } else if (addReport(reportMessage.clientName, reportMessage.reportedClientName)) {
                reportMessage.storeClient.tell(new OperationAck(reportMessage.communicationId));
            } else {
                reportMessage.storeClient.tell(new OperationFailed(reportMessage.communicationId));
            }
        } else if (message instanceof SearchInStore) {
            SearchInStore searchMessage = (SearchInStore) message;
            List<UserMessage> foundMessage = findByAuthorOrText(searchMessage.searchText);
            searchMessage.storeClient.tell(new FoundMessages(foundMessage, searchMessage.communicationId));
        } else if (message instanceof EditMessage) {
            EditMessage editMessage = (EditMessage) message;
            if (isBanned(editMessage.clientName)) {
                editMessage.storeClient.tell(new UserBanned(editMessage.communicationId));
            } else if (editMessage(editMessage.messageId, editMessage.clientName, editMessage.newMessage)) {
                editMessage.storeClient.tell(new OperationAck(editMessage.communicationId));
            } else {
                editMessage.storeClient.tell(new OperationFailed(editMessage.communicationId));
            }
        } else if (message instanceof DeleteMessage) {
            DeleteMessage deleteMessage = (DeleteMessage) message;
            if (isBanned(deleteMessage.clientName)) {
                deleteMessage.storeClient.tell(new UserBanned(deleteMessage.communicationId));
            } else if (deleteMessage(deleteMessage.clientName, deleteMessage.messageId)) {
                deleteMessage.storeClient.tell(new OperationAck(deleteMessage.communicationId));
            } else {
                deleteMessage.storeClient.tell(new OperationFailed(deleteMessage.communicationId));
            }
        }
    }

    private boolean isBanned(String clientName) {
        HashSet<String> reporters = reports.getOrDefault(clientName, null);
        return reporters != null && reporters.size() > USER_BLOCKED_AT_COUNT;
    }

    /**
     * Internal helper method containing the update logic
     *
     * @param message the user message to be saved
     * @return true if successful, false otherwise
     */
    private boolean update(UserMessage message) {

        if (message.getMessageId() == UserMessage.NEW_ID) {
            boolean containsSameMessage = false;
            for (UserMessage m : messages.values()) {
                if (m.getAuthor().equals(message.getAuthor()) && m.getMessage().equals(message.getMessage())) {
                    containsSameMessage = true;
                    break; // added
                }
            }
            if (!containsSameMessage) {
                message.setMessageId(currentId++);
                messages.put(message.getMessageId(), message);
                return true;
            }
        }
        return false;
    }

    /**
     * Internal helper method containing the logic for looking up messages.
     *
     * @param author the name of the author of the returned messages
     * @return all messages posted by the given author
     */
    private List<UserMessage> findByAuthor(String author) {
        List<UserMessage> foundMessages = new ArrayList<>();
        for (UserMessage message : messages.values()) {
            if (message.getAuthor().equals(author))
                foundMessages.add(message);
        }
        return foundMessages;
    }

    /**
     * Internal helper method containing the logic for looking up messages
     * by Author or by their message text.
     *
     * @param searchText the name of the author of the returned messages
     * @return all messages containing the given Text
     */
    private List<UserMessage> findByAuthorOrText(String searchText) {
        List<UserMessage> foundMessages = new ArrayList<>();
        for (UserMessage message : messages.values()) {
            if (message.getAuthor().toLowerCase().contains(searchText.toLowerCase()) ||
                    message.getMessage().toLowerCase().contains(searchText.toLowerCase()))
                foundMessages.add(message);
        }
        return foundMessages;
    }

    /**
     * Internal helper method containing the logic for adding likes.
     *
     * @param clientName the name of the person who likes the message
     * @param messageId  the id of message to be liked
     * @return true if successful, false otherwise
     */
    private boolean addLike(String clientName, long messageId) {
        if (!messages.containsKey(messageId))
            return false;
        UserMessage message = messages.get(messageId);
        if (message.getLikes().contains(clientName))
            return false;
        message.getLikes().add(clientName);
        message.setPoints(message.getPoints() + 2);
        return true;
    }

    /**
     * Internal helper method containing the logic for deleting likes/dislikes.
     *
     * @param clientName the name of the person who wants to delete like/dislike
     * @param messageId  the id of the message
     * @return true if successful, false otherwise
     */
    private boolean deleteLikeOrDislike(String clientName, long messageId, RemoveLikeOrDislike.Type type) {
        if (!messages.containsKey(messageId))
            return false;
        UserMessage message = messages.get(messageId);
        if (type == RemoveLikeOrDislike.Type.LIKE) {
            if (!message.getLikes().contains(clientName))
                return false;
            message.getLikes().remove(clientName);
            message.setPoints(message.getPoints() - 2);
        } else if (type == RemoveLikeOrDislike.Type.DISLIKE) {
            if (!message.getDislikes().contains(clientName))
                return false;
            message.getDislikes().remove(clientName);
            message.setPoints(message.getPoints() + 1);
        } else
            throw new NullPointerException("Unknown delete type.");
        return true;
    }

    /**
     * Internal helper method containing the logic for adding dislikes.
     *
     * @param clientName the name of the person who dislikes the message
     * @param messageId  the id of message to be disliked
     * @return true if successful, false otherwise
     */
    private boolean addDislike(String clientName, long messageId) {
        if (!messages.containsKey(messageId))
            return false;
        UserMessage message = messages.get(messageId);
        if (message.getDislikes().contains(clientName))
            return false;
        message.getDislikes().add(clientName);
        message.setPoints(message.getPoints() - 1);

        return true;
    }

    /**
     * Internal helper method containing the logic for reporting users.
     *
     * @param clientName         the name of the person who reported the other user
     * @param reportedClientName the name of the user to be reported
     * @return true if successful, false otherwise
     */
    private boolean addReport(String clientName, String reportedClientName) {

        HashSet<String> reporters = reports.getOrDefault(reportedClientName, new HashSet<>());
        if (reporters.add(clientName)) {
            reports.put(reportedClientName, reporters);
            return true;
        } else {
            // reporter already reported the user
            return false;
        }
    }

    /**
     * Internal helper method containing the logic for adding reactions.
     *
     * @param clientName the name of the person who reacts to the message
     * @param messageId  the id of message to be reacted to
     * @return true if successful, false otherwise
     */
    private boolean addReaction(String clientName, long messageId, Reaction.Emoji reaction) {
        if (!messages.containsKey(messageId))
            return false;
        UserMessage message = messages.get(messageId);
        Set<Reaction.Emoji> reactionsSet = new HashSet<>();
        if (message.getReactions().containsKey(clientName))
            if (message.getReactions().get(clientName).contains(reaction))
                return false;
            else
                reactionsSet = message.getReactions().get(clientName);
        reactionsSet.add(reaction);
        message.getReactions().put(clientName, reactionsSet);
        return true;
    }

    /**
     * Internal helper method containing the logic for editing existing messages.
     *
     * @param messageId  the id of the message to be edited
     * @param clientName the client, which wants to edit message
     * @param newMessage the new message
     * @return true if successful, false otherwise
     */
    private boolean editMessage(long messageId, String clientName, String newMessage) {
        if (!messages.containsKey(messageId)) {
            return false;
        }
        for (UserMessage m : messages.values()) {
            if (m.getAuthor().equals(clientName) && m.getMessage().equals(newMessage)) {
                return false;
            }
        }
        UserMessage message = messages.get(messageId);
        if (!clientName.equals(message.getAuthor())) {
            return false;
        }
        message.setMessage(newMessage);
        messages.put(message.getMessageId(), message);
        return true;
    }

    /**
     * Internal helper method containing the logic for deleting a message.
     *
     * @param clientName the name of the person who tries to delete the message
     * @param messageId  the id of message to be deleted
     * @return true if successful, false otherwise
     */
    private boolean deleteMessage(String clientName, long messageId) {
        if (!messages.containsKey(messageId))
            return false;
        UserMessage message = messages.get(messageId);
        if (!clientName.equals(message.getAuthor()))
            return false;
        messages.remove(messageId);
        return true;
    }
}
