package at.tugraz.ist.qs2023.messageboard.messagestoremessages;

/**
 * Message used to signal that it should be searched for the given message
 * either in the Author or the Message and return all matching messages.
 */
public class SearchInStore extends MessageStoreMessage {
    /**
     * The author of the message which should be looked up
     */
    public final String searchText;

    public SearchInStore(String searchText, long commId) {
        this.searchText = searchText;
        this.communicationId = commId;
    }
}
