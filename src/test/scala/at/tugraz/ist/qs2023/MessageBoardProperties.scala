package at.tugraz.ist.qs2023

import at.tugraz.ist.qs2023.actorsystem.{SimulatedActor}
import at.tugraz.ist.qs2023.messageboard.UserMessage
import at.tugraz.ist.qs2023.messageboard.Worker.MAX_MESSAGE_LENGTH
import at.tugraz.ist.qs2023.messageboard.MessageStore.USER_BLOCKED_AT_COUNT
import at.tugraz.ist.qs2023.messageboard.clientmessages._
import org.junit.runner.RunWith
import org.scalacheck.Prop.{classify, forAll}
import org.scalacheck.{Gen, Properties}

@RunWith(classOf[ScalaCheckJUnitPropertiesRunner])
class MessageBoardProperties extends Properties("MessageBoardProperties") {

  // Generator for valid messages
  val validMessageGen: Gen[String] = Gen.asciiPrintableStr.map(s =>
    if (s.length <= MAX_MESSAGE_LENGTH) s else s.substring(0, MAX_MESSAGE_LENGTH)
  )

  property("message length: Publish + Ack [R1]") = forAll { (author: String, message: String) =>
    // arrange-  initialize the message board
    val sut = new SUTMessageBoard
    sut.getDispatcher.tell(new InitCommunication(sut.getClient, sut.getCommId))
    while (sut.getClient.receivedMessages.isEmpty)
      sut.getSystem.runFor(1)
    val initAck = sut.getClient.receivedMessages.remove.asInstanceOf[InitAck]
    val worker: SimulatedActor = initAck.worker

    // act - send and receive the messages
    worker.tell(new Publish(new UserMessage(author, message), sut.getCommId))
    while (sut.getClient.receivedMessages.isEmpty)
      sut.getSystem.runFor(1)
    val reply = sut.getClient.receivedMessages.remove()

    worker.tell(new FinishCommunication(sut.getCommId))
    while (sut.getClient.receivedMessages.isEmpty)
      sut.getSystem.runFor(1)
    sut.getClient.receivedMessages.remove.asInstanceOf[FinishAck]

    // assert - define your property and check against it
    // The following classify is optional, it prints stats on the generated values.
    // But the check inside is required.
    classify(message.length <= MAX_MESSAGE_LENGTH, "valid message length", "invalid message length") {
      // if operationAck is received, the message length should be smaller or equal to 10
      reply.isInstanceOf[OperationAck] == message.length <= MAX_MESSAGE_LENGTH
    }
  }
  // TODO: add another properties for requirements R1-R16

  property("user blocked after USER_BLOCKED_AT_COUNT reports: Publish + Ack [R2]") = forAll(Gen.alphaStr, validMessageGen) { (author: String, message: String) =>
    // arrange - initialize the message board
    val sut = new SUTMessageBoard
    sut.getDispatcher.tell(new InitCommunication(sut.getClient, sut.getCommId))
    while (sut.getClient.receivedMessages.isEmpty)
      sut.getSystem.runFor(1)
    val initAck = sut.getClient.receivedMessages.remove.asInstanceOf[InitAck]
    val worker: SimulatedActor = initAck.worker

    // act - publish a message and report the author USER_BLOCKED_AT_COUNT + 1 times
    worker.tell(new Publish(new UserMessage(author, message), sut.getCommId))
    while (sut.getClient.receivedMessages.isEmpty)
      sut.getSystem.runFor(1)
    sut.getClient.receivedMessages.remove()

    for (_ <- 1 to USER_BLOCKED_AT_COUNT + 1) {
      worker.tell(new Report(author, sut.getCommId, "reporter"))
      while (sut.getClient.receivedMessages.isEmpty)
        sut.getSystem.runFor(1)
      sut.getClient.receivedMessages.remove()
    }

    // act - try to publish another message
    worker.tell(new Publish(new UserMessage(author, "new message"), sut.getCommId))
    while (sut.getClient.receivedMessages.isEmpty)
      sut.getSystem.runFor(1)
    val reply = sut.getClient.receivedMessages.remove()

    // finish communication
    worker.tell(new FinishCommunication(sut.getCommId))
    while (sut.getClient.receivedMessages.isEmpty)
      sut.getSystem.runFor(1)
    sut.getClient.receivedMessages.remove.asInstanceOf[FinishAck]

    // assert - check if the publish operation failed
    reply.isInstanceOf[OperationFailed]
  }

  property("report user not previously banned: Publish + Ack [R8]") = forAll(Gen.listOfN(2, Gen.alphaStr), Gen.listOfN(2, validMessageGen)) { (authors: List[String], messages: List[String]) =>
    // arrange and initialize the message board
    val sut = new SUTMessageBoard
    val communication = new InitCommunication(sut.getClient, sut.getCommId)
    sut.getDispatcher.tell(communication)
    while (sut.getClient.receivedMessages.isEmpty)
      sut.getSystem.runFor(1)
    val initAck = sut.getClient.receivedMessages.remove.asInstanceOf[InitAck]
    val worker: SimulatedActor = initAck.worker

    // 1 act - send and receive the messages
    val userMsg = new UserMessage(authors(0), messages(0))
    worker.tell(new Publish(userMsg, sut.getCommId))
    while (sut.getClient.receivedMessages.isEmpty)
      sut.getSystem.runFor(1)
    val reply1 = sut.getClient.receivedMessages.remove()

    // 2 act - send and receive the messages
    val userMsg2 = new UserMessage(authors(1), messages(1))
    worker.tell(new Publish(userMsg2, sut.getCommId))
    while (sut.getClient.receivedMessages.isEmpty)
      sut.getSystem.runFor(1)
    val reply2 = sut.getClient.receivedMessages.remove()

    // 3 act - send and receive the messages
    // report the user once
    worker.tell(new Report(authors(0), sut.getCommId, authors(1)))
    while (sut.getClient.receivedMessages.isEmpty)
      sut.getSystem.runFor(1)
    val report1 = sut.getClient.receivedMessages.remove()

    // 4 act - send and receive the messages
    // report the user another time -> should fail
    worker.tell(new Report(authors(0), sut.getCommId, authors(1)))
    while (sut.getClient.receivedMessages.isEmpty)
      sut.getSystem.runFor(1)
    val report2 = sut.getClient.receivedMessages.remove()

    // finish communication
    worker.tell(new FinishCommunication(sut.getCommId))
    while (sut.getClient.receivedMessages.isEmpty)
      sut.getSystem.runFor(1)
    sut.getClient.receivedMessages.remove()

    report1.isInstanceOf[OperationAck] && report2.isInstanceOf[OperationFailed]
  }

  property("no identical messages: Publish + Ack [R3]") = forAll(Gen.alphaStr, validMessageGen) { (author: String, message: String) =>
    // arrange - initialize the message board
    val sut = new SUTMessageBoard
    sut.getDispatcher.tell(new InitCommunication(sut.getClient, sut.getCommId))
    while (sut.getClient.receivedMessages.isEmpty)
      sut.getSystem.runFor(1)
    val initAck = sut.getClient.receivedMessages.remove.asInstanceOf[InitAck]
    val worker: SimulatedActor = initAck.worker

    // act - publish a message
    worker.tell(new Publish(new UserMessage(author, message), sut.getCommId))
    while (sut.getClient.receivedMessages.isEmpty)
      sut.getSystem.runFor(1)
    val reply1 = sut.getClient.receivedMessages.remove()

    // act - publish the same message again
    worker.tell(new Publish(new UserMessage(author, message), sut.getCommId))
    while (sut.getClient.receivedMessages.isEmpty)
      sut.getSystem.runFor(1)
    val reply2 = sut.getClient.receivedMessages.remove()

    // finish communication
    worker.tell(new FinishCommunication(sut.getCommId))
    while (sut.getClient.receivedMessages.isEmpty)
      sut.getSystem.runFor(1)
    sut.getClient.receivedMessages.remove.asInstanceOf[FinishAck]

    // assert - check if the first publish operation was successful and the second one failed
    reply1.isInstanceOf[OperationAck] && reply2.isInstanceOf[OperationFailed]
  }

  property("operation on non-existent message:  Publish + Ack [R4]") = forAll(Gen.alphaStr, validMessageGen) { (author: String, message: String) =>
    // arrange - initialize the message board
    // arrange-  initialize the message board
    val sut = new SUTMessageBoard
    sut.getDispatcher.tell(new InitCommunication(sut.getClient, sut.getCommId))
    while (sut.getClient.receivedMessages.isEmpty)
      sut.getSystem.runFor(1)
    val initAck = sut.getClient.receivedMessages.remove.asInstanceOf[InitAck]
    val worker: SimulatedActor = initAck.worker

    // Failing
    val reactions = Reaction.Emoji.values();
    reactions.foreach((reaction) => {
      worker.tell(new Reaction(author, sut.getCommId, 8, reaction))
      while (sut.getClient.receivedMessages.isEmpty)
        sut.getSystem.runFor(1)
      var replyReactionFailed = sut.getClient.receivedMessages.remove()
    })

    worker.tell(new Like(author, sut.getCommId, 10))
    while (sut.getClient.receivedMessages.isEmpty)
      sut.getSystem.runFor(1)
    val likeReplyF = sut.getClient.receivedMessages.remove()

    worker.tell(new Dislike(author, sut.getCommId, 10))
    while (sut.getClient.receivedMessages.isEmpty)
      sut.getSystem.runFor(1)
    val dislikeReplyF = sut.getClient.receivedMessages.remove()

    worker.tell(new RemoveLikeOrDislike(author, sut.getCommId, 10, RemoveLikeOrDislike.Type.LIKE))
    while (sut.getClient.receivedMessages.isEmpty)
      sut.getSystem.runFor(1)
    val remLikeReplyF = sut.getClient.receivedMessages.remove()

    worker.tell(new RemoveLikeOrDislike(author, sut.getCommId, 10, RemoveLikeOrDislike.Type.DISLIKE))
    while (sut.getClient.receivedMessages.isEmpty)
      sut.getSystem.runFor(1)
    val remDislikeReplyF = sut.getClient.receivedMessages.remove()

    worker.tell(new Edit(10, author, "new message", sut.getCommId))
    while (sut.getClient.receivedMessages.isEmpty)
      sut.getSystem.runFor(1)
    val editReplyF = sut.getClient.receivedMessages.remove()

    worker.tell(new Delete(10, author, sut.getCommId))
    while (sut.getClient.receivedMessages.isEmpty)
      sut.getSystem.runFor(1)
    val deleteReplyF = sut.getClient.receivedMessages.remove()

    // succeeding
    val userMsg = new UserMessage(author, message);
    worker.tell(new Publish(userMsg, sut.getCommId))
    while (sut.getClient.receivedMessages.isEmpty)
      sut.getSystem.runFor(1)
    val reply1 = sut.getClient.receivedMessages.remove()

    reactions.foreach((reaction) => {
      worker.tell(new Reaction(author, sut.getCommId, userMsg.getMessageId, reaction))
      while (sut.getClient.receivedMessages.isEmpty)
        sut.getSystem.runFor(1)
      val reactionReplyS = sut.getClient.receivedMessages.remove()
    })

    worker.tell(new Like(author, sut.getCommId, userMsg.getMessageId))
    while (sut.getClient.receivedMessages.isEmpty)
      sut.getSystem.runFor(1)
    val likeReplyS = sut.getClient.receivedMessages.remove()

    worker.tell(new Dislike(author, sut.getCommId, userMsg.getMessageId))
    while (sut.getClient.receivedMessages.isEmpty)
      sut.getSystem.runFor(1)
    val dislikeReplyS = sut.getClient.receivedMessages.remove()

    worker.tell(new RemoveLikeOrDislike(author, sut.getCommId, 10, RemoveLikeOrDislike.Type.LIKE))
    while (sut.getClient.receivedMessages.isEmpty)
      sut.getSystem.runFor(1)
    val remLikeReplyS = sut.getClient.receivedMessages.remove()

    worker.tell(new RemoveLikeOrDislike(author, sut.getCommId, 10, RemoveLikeOrDislike.Type.DISLIKE))
    while (sut.getClient.receivedMessages.isEmpty)
      sut.getSystem.runFor(1)
    val remDislikeReplyS = sut.getClient.receivedMessages.remove()

    worker.tell(new Edit(userMsg.getMessageId, author, "new message", sut.getCommId))
    while (sut.getClient.receivedMessages.isEmpty)
      sut.getSystem.runFor(1)
    val editReplyS = sut.getClient.receivedMessages.remove()

    worker.tell(new Delete(userMsg.getMessageId, author, sut.getCommId))
    while (sut.getClient.receivedMessages.isEmpty)
      sut.getSystem.runFor(1)
    val deleteReplyS = sut.getClient.receivedMessages.remove()

    // finish communication
    worker.tell(new FinishCommunication(sut.getCommId))
    while (sut.getClient.receivedMessages.isEmpty)
      sut.getSystem.runFor(1)
    sut.getClient.receivedMessages.remove()

    likeReplyF.isInstanceOf[OperationFailed] && dislikeReplyF.isInstanceOf[OperationFailed] && likeReplyS.isInstanceOf[ReactionResponse] && dislikeReplyS.isInstanceOf[ReactionResponse]
  }

  property("message liked/disliked once: Like/Dislike + Ack [R5]") = forAll(Gen.alphaStr, validMessageGen) { (author: String, message: String) =>
    // arrange - initialize the message board
    val sut = new SUTMessageBoard
    sut.getDispatcher.tell(new InitCommunication(sut.getClient, sut.getCommId))
    while (sut.getClient.receivedMessages.isEmpty)
      sut.getSystem.runFor(1)
    val initAck = sut.getClient.receivedMessages.remove.asInstanceOf[InitAck]
    val worker: SimulatedActor = initAck.worker

    // act - publish a message
    val userMsg = new UserMessage(author, message);
    worker.tell(new Publish(userMsg, sut.getCommId))
    while (sut.getClient.receivedMessages.isEmpty)
      sut.getSystem.runFor(1)
    val reply = sut.getClient.receivedMessages.remove()

    // act - like the message
    worker.tell(new Like(author, sut.getCommId, userMsg.getMessageId))
    while (sut.getClient.receivedMessages.isEmpty)
      sut.getSystem.runFor(1)
    val likeReply1 = sut.getClient.receivedMessages.remove()

    // act - try to like the same message again
    worker.tell(new Like(author, sut.getCommId, userMsg.getMessageId))
    while (sut.getClient.receivedMessages.isEmpty)
      sut.getSystem.runFor(1)
    val likeReply2 = sut.getClient.receivedMessages.remove()

    // act - dislike the message
    worker.tell(new Dislike(author, sut.getCommId, userMsg.getMessageId))
    while (sut.getClient.receivedMessages.isEmpty)
      sut.getSystem.runFor(1)
    val dislikeReply1 = sut.getClient.receivedMessages.remove()

    // act - try to dislike the same message again
    worker.tell(new Dislike(author, sut.getCommId, userMsg.getMessageId))
    while (sut.getClient.receivedMessages.isEmpty)
      sut.getSystem.runFor(1)
    val dislikeReply2 = sut.getClient.receivedMessages.remove()

    // finish communication
    worker.tell(new FinishCommunication(sut.getCommId))
    while (sut.getClient.receivedMessages.isEmpty)
      sut.getSystem.runFor(1)
    sut.getClient.receivedMessages.remove.asInstanceOf[FinishAck]

    // assert - check if the first like/dislike operation was successful and the second ones failed
    likeReply1.isInstanceOf[ReactionResponse] && likeReply2.isInstanceOf[OperationFailed] &&
      dislikeReply1.isInstanceOf[ReactionResponse] && dislikeReply2.isInstanceOf[OperationFailed]
  }

  property("retrieve all messages of an author: GetAuthorMessages + Ack [R6]") = forAll(Gen.alphaStr, Gen.listOfN(5, validMessageGen)) { (author: String, messages: List[String]) =>
    // arrange - initialize the message board
    val sut = new SUTMessageBoard
    sut.getDispatcher.tell(new InitCommunication(sut.getClient, sut.getCommId))
    while (sut.getClient.receivedMessages.isEmpty)
      sut.getSystem.runFor(1)
    val initAck = sut.getClient.receivedMessages.remove.asInstanceOf[InitAck]
    val worker: SimulatedActor = initAck.worker

    // act - send and receive the messages
    messages.foreach((msg) => {
      val userMsg = new UserMessage(author, msg);
      worker.tell(new Publish(userMsg, sut.getCommId))
      while (sut.getClient.receivedMessages.isEmpty)
        sut.getSystem.runFor(1)
      val reply1 = sut.getClient.receivedMessages.remove()
    })

    // act - send and receive the messages
    worker.tell(new RetrieveMessages(author, sut.getCommId))
    while (sut.getClient.receivedMessages.isEmpty)
      sut.getSystem.runFor(1)
    val publishedMessages = sut.getClient.receivedMessages.remove()

    // finish communication
    worker.tell(new FinishCommunication(sut.getCommId))
    while (sut.getClient.receivedMessages.isEmpty)
      sut.getSystem.runFor(1)
    sut.getClient.receivedMessages.remove.asInstanceOf[FinishAck]
    // assert - check if all published messages are correctly retrieved
    publishedMessages.isInstanceOf[FoundMessages]
  }

  property("search for messages containing a given text: SearchMessages + Ack [R7]") = forAll(Gen.alphaStr, Gen.listOfN(5, validMessageGen)) { (author: String, messages: List[String]) =>
    // arrange - initialize the message board
    val sut = new SUTMessageBoard
    sut.getDispatcher.tell(new InitCommunication(sut.getClient, sut.getCommId))
    while (sut.getClient.receivedMessages.isEmpty)
      sut.getSystem.runFor(1)
    val initAck = sut.getClient.receivedMessages.remove.asInstanceOf[InitAck]
    val worker: SimulatedActor = initAck.worker

    val searchText = "search"

    // act - publish multiple messages
    val publishedMessages = messages.map { message =>
      worker.tell(new Publish(new UserMessage(author, message + searchText), sut.getCommId))
      while (sut.getClient.receivedMessages.isEmpty)
        sut.getSystem.runFor(1)
      sut.getClient.receivedMessages.remove()
    }

    // act - search for messages containing a given text
    worker.tell(new SearchMessages(searchText, sut.getCommId))
    while (sut.getClient.receivedMessages.isEmpty)
      sut.getSystem.runFor(1)
    val searchedMessages = sut.getClient.receivedMessages.remove()

    // finish communication
    worker.tell(new FinishCommunication(sut.getCommId))
    while (sut.getClient.receivedMessages.isEmpty)
      sut.getSystem.runFor(1)
    sut.getClient.receivedMessages.remove.asInstanceOf[FinishAck]

    // assert - check if all messages containing the search text are correctly retrieved
    searchedMessages.isInstanceOf[FoundMessages]
  }



  /*property("example property with generators") =
    forAll(Gen.alphaStr, Gen.nonEmptyListOf(validMessageGen)) { (author: String, messages: List[String]) =>
      val sut = new SUTMessageBoard
      sut.getDispatcher.tell(new InitCommunication(sut.getClient, sut.getCommId))
      while (sut.getClient.receivedMessages.isEmpty)
        sut.getSystem.runFor(1)
      val initAck = sut.getClient.receivedMessages.remove.asInstanceOf[InitAck]
      val worker: SimulatedActor = initAck.worker

      // here would be a worker.tell, e.g. in a loop

      worker.tell(new FinishCommunication(sut.getCommId))
      while (sut.getClient.receivedMessages.isEmpty)
        sut.getSystem.runFor(1)
      sut.getClient.receivedMessages.remove()

      // here would be a check
      false
    }*/


}
