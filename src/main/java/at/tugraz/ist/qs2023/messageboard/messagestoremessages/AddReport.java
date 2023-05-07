package at.tugraz.ist.qs2023.messageboard.messagestoremessages;

/**
 * Message used to signal that a user should be reported.
 */
public class AddReport extends MessageStoreMessage {
    /**
     * User which should be reported
     */
    public final String reportedClientName;

    /**
     * Name of the person which reported the user
     */
    public final String clientName;

    public AddReport(String clientName, long commId, String reportedClientName) {
        this.clientName = clientName;
        this.communicationId = commId;
        this.reportedClientName = reportedClientName;
    }
}
