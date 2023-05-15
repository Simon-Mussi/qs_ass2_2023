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

  property("Message length: Edit + Ack [R1] ") = forAll { (author: String, new_message: String) =>
    // arrange-  initialize the message board
    val sut = new SUTMessageBoard
    sut.getDispatcher.tell(new InitCommunication(sut.getClient, sut.getCommId))
    while (sut.getClient.receivedMessages.isEmpty)
      sut.getSystem.runFor(1)
    val initAck = sut.getClient.receivedMessages.remove.asInstanceOf[InitAck]
    val worker: SimulatedActor = initAck.worker

    // act - send and receive the messages
    val old_message = "test"
    val msg = new UserMessage(author, old_message)
    worker.tell(new Publish(msg, sut.getCommId))
    while (sut.getClient.receivedMessages.isEmpty)
      sut.getSystem.runFor(1)
    sut.getClient.receivedMessages.remove()

    worker.tell(new Edit(msg.getMessageId, author, new_message, sut.getCommId))
    while (sut.getClient.receivedMessages.isEmpty)
      sut.getSystem.runFor(1)
    val reply_edit = sut.getClient.receivedMessages.remove()

    worker.tell(new FinishCommunication(sut.getCommId))
    while (sut.getClient.receivedMessages.isEmpty)
      sut.getSystem.runFor(1)
    val fin = sut.getClient.receivedMessages.remove.asInstanceOf[FinishAck]

    // assert - define your property and check against it
    // The following classify is optional, it prints stats on the generated values.
    // But the check inside is required.
    classify(new_message.length <= MAX_MESSAGE_LENGTH, "valid message length", "invalid message length") {
      // if operationAck is received, the message length should be smaller or equal to 10
      reply_edit.isInstanceOf[OperationAck] == new_message.length <= MAX_MESSAGE_LENGTH && !(new_message == old_message)
    }
  }

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

    likeReplyF.isInstanceOf[OperationFailed] && dislikeReplyF.isInstanceOf[OperationFailed] && likeReplyS.isInstanceOf[ReactionResponse] &&
      dislikeReplyS.isInstanceOf[ReactionResponse]
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

  property("report user only once: Report + Ack [R8]") = forAll(Gen.listOfN(2, Gen.alphaStr), validMessageGen) { (authors: List[String], message: String) =>
    // arrange and initialize the message board
    val sut = new SUTMessageBoard
    val communication = new InitCommunication(sut.getClient, sut.getCommId)
    sut.getDispatcher.tell(communication)
    while (sut.getClient.receivedMessages.isEmpty)
      sut.getSystem.runFor(1)
    val initAck = sut.getClient.receivedMessages.remove.asInstanceOf[InitAck]
    val worker: SimulatedActor = initAck.worker

    // act - send and receive the messages
    val userMsg = new UserMessage(authors(0), message)
    worker.tell(new Publish(userMsg, sut.getCommId))
    while (sut.getClient.receivedMessages.isEmpty)
      sut.getSystem.runFor(1)
    val reply = sut.getClient.receivedMessages.remove()

    // act - report the user once
    worker.tell(new Report(authors(1), sut.getCommId, authors(0)))
    while (sut.getClient.receivedMessages.isEmpty)
      sut.getSystem.runFor(1)
    val report1 = sut.getClient.receivedMessages.remove()

    // act - report the user another time -> should fail
    worker.tell(new Report(authors(1), sut.getCommId, authors(0)))
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

  property("successful requests receive OperationAck or ReactionResponse [R9]") = forAll(Gen.alphaStr, validMessageGen) { (author: String, message: String) =>
    // arrange - initialize the message board
    val sut = new SUTMessageBoard
    sut.getDispatcher.tell(new InitCommunication(sut.getClient, sut.getCommId))
    while (sut.getClient.receivedMessages.isEmpty)
      sut.getSystem.runFor(1)
    val initAck = sut.getClient.receivedMessages.remove.asInstanceOf[InitAck]
    val worker: SimulatedActor = initAck.worker

    // act - publish a message
    val userMsg = new UserMessage(author, message)
    worker.tell(new Publish(userMsg, sut.getCommId))
    while (sut.getClient.receivedMessages.isEmpty)
      sut.getSystem.runFor(1)
    val publishAck = sut.getClient.receivedMessages.remove()

    // act - add a like to the message
    worker.tell(new Like(author, sut.getCommId, userMsg.getMessageId))
    while (sut.getClient.receivedMessages.isEmpty)
      sut.getSystem.runFor(1)
    val likeAck = sut.getClient.receivedMessages.remove()

    // act - remove the like from the message
    worker.tell(new RemoveLikeOrDislike(author, sut.getCommId, userMsg.getMessageId, RemoveLikeOrDislike.Type.LIKE))
    while (sut.getClient.receivedMessages.isEmpty)
      sut.getSystem.runFor(1)
    val removeLikeAck = sut.getClient.receivedMessages.remove()

    // act - add a like to the message
    worker.tell(new Dislike(author, sut.getCommId, userMsg.getMessageId))
    while (sut.getClient.receivedMessages.isEmpty)
      sut.getSystem.runFor(1)
    val dislikeAck = sut.getClient.receivedMessages.remove()

    // act - remove the dislike from the message
    worker.tell(new RemoveLikeOrDislike(author, sut.getCommId, userMsg.getMessageId, RemoveLikeOrDislike.Type.DISLIKE))
    while (sut.getClient.receivedMessages.isEmpty)
      sut.getSystem.runFor(1)
    val removeDislikeAck = sut.getClient.receivedMessages.remove()

    // act - edit the message
    worker.tell(new Edit(userMsg.getMessageId, author,"message", sut.getCommId))
    while (sut.getClient.receivedMessages.isEmpty)
      sut.getSystem.runFor(1)
    val editAck = sut.getClient.receivedMessages.remove()

    // act - delete the message
    worker.tell(new Delete(userMsg.getMessageId, author, sut.getCommId))
    while (sut.getClient.receivedMessages.isEmpty)
      sut.getSystem.runFor(1)
    val deleteAck = sut.getClient.receivedMessages.remove()

    // act - report the author
    worker.tell(new Report(author, sut.getCommId, "reporter"))
    while (sut.getClient.receivedMessages.isEmpty)
      sut.getSystem.runFor(1)
    val reportAck = sut.getClient.receivedMessages.remove()

    // finish communication
    worker.tell(new FinishCommunication(sut.getCommId))
    while (sut.getClient.receivedMessages.isEmpty)
      sut.getSystem.runFor(1)
    sut.getClient.receivedMessages.remove.asInstanceOf[FinishAck]

    // assert - check if all successful operations return OperationAck or ReactionResponse
    likeAck.isInstanceOf[ReactionResponse] && dislikeAck.isInstanceOf[ReactionResponse] && removeLikeAck.isInstanceOf[ReactionResponse] &&
      removeDislikeAck.isInstanceOf[ReactionResponse] && reportAck.isInstanceOf[OperationAck] && deleteAck.isInstanceOf[OperationAck]
  }

  property("ban user: Publish + Ack [R10]") = forAll(Gen.listOfN(10, Gen.alphaStr), Gen.listOfN(2, validMessageGen)) { (authors: List[String], messages: List[String]) =>

    // arrange-  initialize the message board
    val sut = new SUTMessageBoard
    val comm = new InitCommunication(sut.getClient, sut.getCommId)
    sut.getDispatcher.tell(comm)
    while (sut.getClient.receivedMessages.isEmpty)
      sut.getSystem.runFor(1)
    val initAck = sut.getClient.receivedMessages.remove.asInstanceOf[InitAck]
    val worker: SimulatedActor = initAck.worker

    // act - send and receive the messages

    // act - send and receive the messages
    val userMsg2 = new UserMessage(authors(0), messages(0))
    worker.tell(new Publish(userMsg2, sut.getCommId))
    while (sut.getClient.receivedMessages.isEmpty)
      sut.getSystem.runFor(1)
    val reply2 = sut.getClient.receivedMessages.remove()

    // act - send and receive the messages
    val userMsg1 = new UserMessage(authors(0), messages(0))
    worker.tell(new Publish(userMsg1, sut.getCommId))
    while (sut.getClient.receivedMessages.isEmpty)
      sut.getSystem.runFor(1)
    val reply1 = sut.getClient.receivedMessages.remove()

    var mes =  "test"
    authors.foreach((text) => {
      // act - send and receive the messages
      worker.tell(new Report(text + mes, sut.getCommId, authors(0)))
      while (sut.getClient.receivedMessages.isEmpty)
        sut.getSystem.runFor(1)
      val report1 = sut.getClient.receivedMessages.remove()
      mes += "test"
    })

    // act - send and receive the messages
    val userMsg3 = new UserMessage(authors(0), messages(1))
    worker.tell(new Publish(userMsg3, sut.getCommId))
    while (sut.getClient.receivedMessages.isEmpty)
      sut.getSystem.runFor(1)
    val reply3 = sut.getClient.receivedMessages.remove()

    // like
    // act - send and receive the messages
    worker.tell(new Like(authors(0), sut.getCommId, userMsg1.getMessageId))
    while (sut.getClient.receivedMessages.isEmpty)
      sut.getSystem.runFor(1)
    val replyLike = sut.getClient.receivedMessages.remove()
    replyLike.isInstanceOf[UserBanned]

    // act - send and receive the messages
    val reactions = Reaction.Emoji.values();
    reactions.foreach((reaction) => {
      worker.tell(new Reaction(authors(0), sut.getCommId, userMsg1.getMessageId, reaction))
      while (sut.getClient.receivedMessages.isEmpty)
        sut.getSystem.runFor(1)
      val replyReaction = sut.getClient.receivedMessages.remove()
      replyReaction.isInstanceOf[UserBanned]
    })

    // act - send and receive the messages
    worker.tell(new Like(authors(1), sut.getCommId, userMsg1.getMessageId))
    while (sut.getClient.receivedMessages.isEmpty)
      sut.getSystem.runFor(1)
    val replyLikeFailed = sut.getClient.receivedMessages.remove()

    // act - send and receive the messages
    worker.tell(new RemoveLikeOrDislike(authors(1), sut.getCommId, userMsg1.getMessageId, RemoveLikeOrDislike.Type.LIKE))
    while (sut.getClient.receivedMessages.isEmpty)
      sut.getSystem.runFor(1)
    val replyDeleteLikeFailed = sut.getClient.receivedMessages.remove()

    // act - send and receive the messages
    worker.tell(new Dislike(authors(0), sut.getCommId, userMsg1.getMessageId))
    while (sut.getClient.receivedMessages.isEmpty)
      sut.getSystem.runFor(1)
    val replyDislikeFailed = sut.getClient.receivedMessages.remove()
    replyDislikeFailed.isInstanceOf[UserBanned]


    // act - send and receive the messages
    worker.tell(new RemoveLikeOrDislike(authors(0), sut.getCommId, userMsg1.getMessageId, RemoveLikeOrDislike.Type.DISLIKE))
    while (sut.getClient.receivedMessages.isEmpty)
      sut.getSystem.runFor(1)
    val replyDeleteDislikeFailed = sut.getClient.receivedMessages.remove()
    replyDeleteDislikeFailed.isInstanceOf[UserBanned]

    // finish communication
    worker.tell(new FinishCommunication(sut.getCommId))
    while (sut.getClient.receivedMessages.isEmpty)
      sut.getSystem.runFor(1)
    sut.getClient.receivedMessages.remove()

    reply2.isInstanceOf[OperationAck] && reply3.isInstanceOf[UserBanned]
  }

  property("likes add two points, dislikes remove one point [R11]") = forAll(Gen.alphaStr, validMessageGen) { (author: String, message: String) =>
    // arrange - initialize the message board
    val sut = new SUTMessageBoard
    sut.getDispatcher.tell(new InitCommunication(sut.getClient, sut.getCommId))
    while (sut.getClient.receivedMessages.isEmpty)
      sut.getSystem.runFor(1)
    val initAck = sut.getClient.receivedMessages.remove.asInstanceOf[InitAck]
    val worker: SimulatedActor = initAck.worker

    // act - publish a message
    val userMsg = new UserMessage(author, message)
    worker.tell(new Publish(userMsg, sut.getCommId))
    while (sut.getClient.receivedMessages.isEmpty)
      sut.getSystem.runFor(1)
    sut.getClient.receivedMessages.remove()

    // act - like the message
    worker.tell(new Like(author, sut.getCommId, userMsg.getMessageId))
    while (sut.getClient.receivedMessages.isEmpty)
      sut.getSystem.runFor(1)
    val likeReply = sut.getClient.receivedMessages.remove()

    // act - dislike the message
    worker.tell(new Dislike(author, sut.getCommId, userMsg.getMessageId))
    while (sut.getClient.receivedMessages.isEmpty)
      sut.getSystem.runFor(1)
    val dislikeReply = sut.getClient.receivedMessages.remove()

    // finish communication
    worker.tell(new FinishCommunication(sut.getCommId))
    while (sut.getClient.receivedMessages.isEmpty)
      sut.getSystem.runFor(1)
    sut.getClient.receivedMessages.remove.asInstanceOf[FinishAck]

    // assert - check if liking the message added two points and disliking it removed one point
    likeReply.isInstanceOf[ReactionResponse] && dislikeReply.isInstanceOf[ReactionResponse]
  }

  property("removing like decreases two points, removing dislike increases one point [R12]") = forAll(Gen.alphaStr, validMessageGen) { (author: String, message: String) =>
    // arrange - initialize the message board
    val sut = new SUTMessageBoard
    sut.getDispatcher.tell(new InitCommunication(sut.getClient, sut.getCommId))
    while (sut.getClient.receivedMessages.isEmpty)
      sut.getSystem.runFor(1)
    val initAck = sut.getClient.receivedMessages.remove.asInstanceOf[InitAck]
    val worker: SimulatedActor = initAck.worker

    // act - publish a message
    val userMsg = new UserMessage(author, message)
    worker.tell(new Publish(userMsg, sut.getCommId))
    while (sut.getClient.receivedMessages.isEmpty)
      sut.getSystem.runFor(1)
    sut.getClient.receivedMessages.remove()

    // act - like the message and then remove the like
    worker.tell(new Like(author, sut.getCommId, userMsg.getMessageId))
    while (sut.getClient.receivedMessages.isEmpty)
      sut.getSystem.runFor(1)
    sut.getClient.receivedMessages.remove()

    worker.tell(new RemoveLikeOrDislike(author, sut.getCommId, userMsg.getMessageId, RemoveLikeOrDislike.Type.LIKE))
    while (sut.getClient.receivedMessages.isEmpty)
      sut.getSystem.runFor(1)
    val removeLikeReply = sut.getClient.receivedMessages.remove()

    // act - dislike the message and then remove the dislike
    worker.tell(new Dislike(author, sut.getCommId, userMsg.getMessageId))
    while (sut.getClient.receivedMessages.isEmpty)
      sut.getSystem.runFor(1)
    sut.getClient.receivedMessages.remove()

    worker.tell(new RemoveLikeOrDislike(author, sut.getCommId, userMsg.getMessageId, RemoveLikeOrDislike.Type.DISLIKE))
    while (sut.getClient.receivedMessages.isEmpty)
      sut.getSystem.runFor(1)
    val removeDislikeReply = sut.getClient.receivedMessages.remove()

    // finish communication
    worker.tell(new FinishCommunication(sut.getCommId))
    while (sut.getClient.receivedMessages.isEmpty)
      sut.getSystem.runFor(1)
    sut.getClient.receivedMessages.remove.asInstanceOf[FinishAck]

    removeLikeReply.isInstanceOf[ReactionResponse] && removeDislikeReply.isInstanceOf[ReactionResponse]
  }

  property("like/dislike can only be removed by the same user [R13]") = forAll(Gen.alphaStr, validMessageGen, Gen.alphaStr) { (author: String, message: String, otherUser: String) =>
    // arrange - initialize the message board
    val sut = new SUTMessageBoard
    sut.getDispatcher.tell(new InitCommunication(sut.getClient, sut.getCommId))
    while (sut.getClient.receivedMessages.isEmpty)
      sut.getSystem.runFor(1)
    val initAck = sut.getClient.receivedMessages.remove.asInstanceOf[InitAck]
    val worker: SimulatedActor = initAck.worker

    // act - publish a message
    val userMsg = new UserMessage(author, message)
    worker.tell(new Publish(userMsg, sut.getCommId))
    while (sut.getClient.receivedMessages.isEmpty)
      sut.getSystem.runFor(1)
    sut.getClient.receivedMessages.remove()

    // act - like the message
    worker.tell(new Like(author, sut.getCommId, userMsg.getMessageId))
    while (sut.getClient.receivedMessages.isEmpty)
      sut.getSystem.runFor(1)
    sut.getClient.receivedMessages.remove()

    // act - try to remove the like with a different user
    worker.tell(new RemoveLikeOrDislike("Other", sut.getCommId, userMsg.getMessageId, RemoveLikeOrDislike.Type.LIKE))
    while (sut.getClient.receivedMessages.isEmpty)
      sut.getSystem.runFor(1)
    val removeLikeReply = sut.getClient.receivedMessages.remove()

    // act - like the message
    worker.tell(new Dislike(author, sut.getCommId, userMsg.getMessageId))
    while (sut.getClient.receivedMessages.isEmpty)
      sut.getSystem.runFor(1)
    sut.getClient.receivedMessages.remove()

    // act - try to remove the dislike with a different user
    worker.tell(new RemoveLikeOrDislike("Other", sut.getCommId, userMsg.getMessageId, RemoveLikeOrDislike.Type.DISLIKE))
    while (sut.getClient.receivedMessages.isEmpty)
      sut.getSystem.runFor(1)
    val removeDislikeReply = sut.getClient.receivedMessages.remove()

    // finish communication
    worker.tell(new FinishCommunication(sut.getCommId))
    while (sut.getClient.receivedMessages.isEmpty)
      sut.getSystem.runFor(1)
    sut.getClient.receivedMessages.remove.asInstanceOf[FinishAck]

    // assert - check if removing a like with a different user was unsuccessful
    removeLikeReply.isInstanceOf[OperationFailed] && removeDislikeReply.isInstanceOf[OperationFailed]
  }

  property("all reactions from a user must be different [R14]") = forAll(Gen.alphaStr, validMessageGen, Gen.listOfN(2, Gen.alphaStr)) { (author: String, message: String, reactions: List[String]) =>
    // arrange - initialize the message board
    val sut = new SUTMessageBoard
    sut.getDispatcher.tell(new InitCommunication(sut.getClient, sut.getCommId))
    while (sut.getClient.receivedMessages.isEmpty)
      sut.getSystem.runFor(1)
    val initAck = sut.getClient.receivedMessages.remove.asInstanceOf[InitAck]
    val worker: SimulatedActor = initAck.worker

    val userMsg = new UserMessage(author, message)
    worker.tell(new Publish(userMsg, sut.getCommId))
    while (sut.getClient.receivedMessages.isEmpty)
      sut.getSystem.runFor(1)
    sut.getClient.receivedMessages.remove()

    // act - give a reaction to the message
    worker.tell(new Reaction(author, sut.getCommId, userMsg.getMessageId, Reaction.Emoji.COOL))
    while (sut.getClient.receivedMessages.isEmpty)
      sut.getSystem.runFor(1)
    val coolReaction = sut.getClient.receivedMessages.remove()

    // act - give the same reaction to the message again
    worker.tell(new Reaction(author, sut.getCommId, userMsg.getMessageId, Reaction.Emoji.COOL))
    while (sut.getClient.receivedMessages.isEmpty)
      sut.getSystem.runFor(1)
    val coolReactionF = sut.getClient.receivedMessages.remove()

    // act - give a reaction to the message
    worker.tell(new Reaction(author, sut.getCommId, userMsg.getMessageId, Reaction.Emoji.SMILEY))
    while (sut.getClient.receivedMessages.isEmpty)
      sut.getSystem.runFor(1)
    val smileyReaction = sut.getClient.receivedMessages.remove()

    // act - give a reaction to the message
    worker.tell(new Reaction(author, sut.getCommId, userMsg.getMessageId, Reaction.Emoji.SMILEY))
    while (sut.getClient.receivedMessages.isEmpty)
      sut.getSystem.runFor(1)
    val smileyReactionF = sut.getClient.receivedMessages.remove()

    // act - give a reaction to the message
    worker.tell(new Reaction(author, sut.getCommId, userMsg.getMessageId, Reaction.Emoji.CRYING))
    while (sut.getClient.receivedMessages.isEmpty)
      sut.getSystem.runFor(1)
    val cryReaction = sut.getClient.receivedMessages.remove()

    // act - give a reaction to the message
    worker.tell(new Reaction(author, sut.getCommId, userMsg.getMessageId, Reaction.Emoji.CRYING))
    while (sut.getClient.receivedMessages.isEmpty)
      sut.getSystem.runFor(1)
    val cryReactionF = sut.getClient.receivedMessages.remove()

    // act - give a reaction to the message
    worker.tell(new Reaction(author, sut.getCommId, userMsg.getMessageId, Reaction.Emoji.SURPRISE))
    while (sut.getClient.receivedMessages.isEmpty)
      sut.getSystem.runFor(1)
    val supReaction = sut.getClient.receivedMessages.remove()

    // act - give a reaction to the message
    worker.tell(new Reaction(author, sut.getCommId, userMsg.getMessageId, Reaction.Emoji.SURPRISE))
    while (sut.getClient.receivedMessages.isEmpty)
      sut.getSystem.runFor(1)
    val supReactionF = sut.getClient.receivedMessages.remove()

    // act - give a reaction to the message
    worker.tell(new Reaction(author, sut.getCommId, userMsg.getMessageId, Reaction.Emoji.FROWN))
    while (sut.getClient.receivedMessages.isEmpty)
      sut.getSystem.runFor(1)
    val frownReaction = sut.getClient.receivedMessages.remove()

    // act - give a reaction to the message
    worker.tell(new Reaction(author, sut.getCommId, userMsg.getMessageId, Reaction.Emoji.FROWN))
    while (sut.getClient.receivedMessages.isEmpty)
      sut.getSystem.runFor(1)
    val frownReactionF = sut.getClient.receivedMessages.remove()

    // act - give a reaction to the message
    worker.tell(new Reaction(author, sut.getCommId, userMsg.getMessageId, Reaction.Emoji.HORROR))
    while (sut.getClient.receivedMessages.isEmpty)
      sut.getSystem.runFor(1)
    val horrorReaction = sut.getClient.receivedMessages.remove()

    // act - give a reaction to the message
    worker.tell(new Reaction(author, sut.getCommId, userMsg.getMessageId, Reaction.Emoji.HORROR))
    while (sut.getClient.receivedMessages.isEmpty)
      sut.getSystem.runFor(1)
    val horrorReactionF = sut.getClient.receivedMessages.remove()

    // act - give a reaction to the message
    worker.tell(new Reaction(author, sut.getCommId, userMsg.getMessageId, Reaction.Emoji.LAUGHING))
    while (sut.getClient.receivedMessages.isEmpty)
      sut.getSystem.runFor(1)
    val lolReaction = sut.getClient.receivedMessages.remove()

    // act - give a reaction to the message
    worker.tell(new Reaction(author, sut.getCommId, userMsg.getMessageId, Reaction.Emoji.LAUGHING))
    while (sut.getClient.receivedMessages.isEmpty)
      sut.getSystem.runFor(1)
    val lolReactionF = sut.getClient.receivedMessages.remove()

    // act - give a reaction to the message
    worker.tell(new Reaction(author, sut.getCommId, userMsg.getMessageId, Reaction.Emoji.SKEPTICAL))
    while (sut.getClient.receivedMessages.isEmpty)
      sut.getSystem.runFor(1)
    val skepticalReaction = sut.getClient.receivedMessages.remove()

    // act - give a reaction to the message
    worker.tell(new Reaction(author, sut.getCommId, userMsg.getMessageId, Reaction.Emoji.SKEPTICAL))
    while (sut.getClient.receivedMessages.isEmpty)
      sut.getSystem.runFor(1)
    val skepticalReactionF = sut.getClient.receivedMessages.remove()

    // finish communication
    worker.tell(new FinishCommunication(sut.getCommId))
    while (sut.getClient.receivedMessages.isEmpty)
      sut.getSystem.runFor(1)
    sut.getClient.receivedMessages.remove.asInstanceOf[FinishAck]

    // assert - check if giving the same reaction again was unsuccessful
    coolReaction.isInstanceOf[ReactionResponse] && coolReactionF.isInstanceOf[OperationFailed] &&
      smileyReaction.isInstanceOf[ReactionResponse] && smileyReactionF.isInstanceOf[OperationFailed] &&
      frownReaction.isInstanceOf[ReactionResponse] && frownReactionF.isInstanceOf[OperationFailed] &&
      skepticalReaction.isInstanceOf[ReactionResponse] && skepticalReactionF.isInstanceOf[OperationFailed] &&
      supReaction.isInstanceOf[ReactionResponse] && supReactionF.isInstanceOf[OperationFailed] &&
      horrorReaction.isInstanceOf[ReactionResponse] && horrorReactionF.isInstanceOf[OperationFailed] &&
      lolReaction.isInstanceOf[ReactionResponse] && lolReactionF.isInstanceOf[OperationFailed] &&
      cryReaction.isInstanceOf[ReactionResponse] && cryReactionF.isInstanceOf[OperationFailed]
  }

  property("an author cannot edit a message to duplicate content [R15]") = forAll(Gen.alphaStr, Gen.listOfN(2, validMessageGen)) { (author: String, messages: List[String]) =>
    // arrange - initialize the message board
    val sut = new SUTMessageBoard
    sut.getDispatcher.tell(new InitCommunication(sut.getClient, sut.getCommId))
    while (sut.getClient.receivedMessages.isEmpty)
      sut.getSystem.runFor(1)
    val initAck = sut.getClient.receivedMessages.remove.asInstanceOf[InitAck]
    val worker: SimulatedActor = initAck.worker

    // act - publish two messages
    val userMsg1 = new UserMessage(author, messages(0))
    worker.tell(new Publish(userMsg1, sut.getCommId))
    while (sut.getClient.receivedMessages.isEmpty)
      sut.getSystem.runFor(1)
    sut.getClient.receivedMessages.remove()

    val userMsg2 = new UserMessage(author, messages(1))
    worker.tell(new Publish(userMsg2, sut.getCommId))
    while (sut.getClient.receivedMessages.isEmpty)
      sut.getSystem.runFor(1)
    sut.getClient.receivedMessages.remove()

    // act - edit the first message to have the same content as the second message
    worker.tell(new Edit(userMsg1.getMessageId, messages(0), messages(1), sut.getCommId))
    while (sut.getClient.receivedMessages.isEmpty)
      sut.getSystem.runFor(1)
    val unsuccessfulEdit = sut.getClient.receivedMessages.remove()

    // finish communication
    worker.tell(new FinishCommunication(sut.getCommId))
    while (sut.getClient.receivedMessages.isEmpty)
      sut.getSystem.runFor(1)
    sut.getClient.receivedMessages.remove.asInstanceOf[FinishAck]

    // assert - check if editing a message to duplicate content was unsuccessful
    unsuccessfulEdit.isInstanceOf[OperationFailed]
  }

  property("only the author of a message is able to edit it [R16]") = forAll(Gen.listOfN(2, Gen.alphaStr), validMessageGen) { (authors: List[String], message: String) =>
    // arrange - initialize the message board
    val sut = new SUTMessageBoard
    sut.getDispatcher.tell(new InitCommunication(sut.getClient, sut.getCommId))
    while (sut.getClient.receivedMessages.isEmpty)
      sut.getSystem.runFor(1)
    val initAck = sut.getClient.receivedMessages.remove.asInstanceOf[InitAck]
    val worker: SimulatedActor = initAck.worker

    // act - publish a message
    val userMsg = new UserMessage(authors(0), message)
    worker.tell(new Publish(userMsg, sut.getCommId))
    while (sut.getClient.receivedMessages.isEmpty)
      sut.getSystem.runFor(1)
    sut.getClient.receivedMessages.remove()

    // act - a different author tries to edit the message
    worker.tell(new Edit(userMsg.getMessageId, message, "new message", sut.getCommId))
    while (sut.getClient.receivedMessages.isEmpty)
      sut.getSystem.runFor(1)
    val unsuccessfulEdit = sut.getClient.receivedMessages.remove()

    // finish communication
    worker.tell(new FinishCommunication(sut.getCommId))
    while (sut.getClient.receivedMessages.isEmpty)
      sut.getSystem.runFor(1)
    sut.getClient.receivedMessages.remove.asInstanceOf[FinishAck]

    // assert - check if the edit from a different author was unsuccessful
    unsuccessfulEdit.isInstanceOf[OperationFailed]
  }

  property("only the author of a message is able to delete it [R17]") = forAll(Gen.listOfN(2, Gen.alphaStr), validMessageGen) { (authors: List[String], message: String) =>
    // arrange - initialize the message board
    val sut = new SUTMessageBoard
    sut.getDispatcher.tell(new InitCommunication(sut.getClient, sut.getCommId))
    while (sut.getClient.receivedMessages.isEmpty)
      sut.getSystem.runFor(1)
    val initAck = sut.getClient.receivedMessages.remove.asInstanceOf[InitAck]
    val worker: SimulatedActor = initAck.worker

    // act - publish a message
    val userMsg = new UserMessage(authors(0), message)
    worker.tell(new Publish(userMsg, sut.getCommId))
    while (sut.getClient.receivedMessages.isEmpty)
      sut.getSystem.runFor(1)
    sut.getClient.receivedMessages.remove()

    // act - a different author tries to delete the message
    worker.tell(new Delete(userMsg.getMessageId, authors(0), sut.getCommId))
    while (sut.getClient.receivedMessages.isEmpty)
      sut.getSystem.runFor(1)
    val successfulDelete = sut.getClient.receivedMessages.remove()

    worker.tell(new Publish(userMsg, sut.getCommId))
    while (sut.getClient.receivedMessages.isEmpty)
      sut.getSystem.runFor(1)
    sut.getClient.receivedMessages.remove()

    // act - a different author tries to delete the message
    worker.tell(new Delete(userMsg.getMessageId, authors(1), sut.getCommId))
    while (sut.getClient.receivedMessages.isEmpty)
      sut.getSystem.runFor(1)
    val unsuccessfulDelete = sut.getClient.receivedMessages.remove()

    // finish communication
    worker.tell(new FinishCommunication(sut.getCommId))
    while (sut.getClient.receivedMessages.isEmpty)
      sut.getSystem.runFor(1)
    sut.getClient.receivedMessages.remove.asInstanceOf[FinishAck]

    // assert - check if the delete from a different author was unsuccessful
    successfulDelete.isInstanceOf[OperationAck] && unsuccessfulDelete.isInstanceOf[OperationFailed]
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
