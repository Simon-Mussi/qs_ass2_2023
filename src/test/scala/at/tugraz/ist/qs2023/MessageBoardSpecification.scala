package at.tugraz.ist.qs2023

import at.tugraz.ist.qs2023.actorsystem.{Message, SimulatedActor}
import at.tugraz.ist.qs2023.messageboard.MessageStore.USER_BLOCKED_AT_COUNT
import at.tugraz.ist.qs2023.messageboard.UserMessage
import at.tugraz.ist.qs2023.messageboard.Worker.MAX_MESSAGE_LENGTH
import at.tugraz.ist.qs2023.messageboard.clientmessages._
import org.scalacheck.commands.Commands
import org.scalacheck.{Gen, Prop}

import scala.collection.mutable
import scala.jdk.CollectionConverters._
import scala.util.Try

// Documentation: https://github.com/typelevel/scalacheck/blob/master/doc/UserGuide.md#stateful-testing
object MessageBoardSpecification extends Commands {
  override type State = ModelMessageBoard
  override type Sut = SUTMessageBoard

  override def canCreateNewSut(newState: State, initSuts: Iterable[State], runningSuts: Iterable[Sut]): Boolean = {
    initSuts.isEmpty && runningSuts.isEmpty
  }

  override def newSut(state: State): Sut = new SUTMessageBoard

  override def destroySut(sut: Sut): Unit = ()

  override def initialPreCondition(state: State): Boolean = state.messages.isEmpty && state.reports.isEmpty

  override def genInitialState: Gen[State] = ModelMessageBoard(Nil, Nil, lastCommandSuccessful = false, userBanned = false)

  override def genCommand(state: State): Gen[Command] = Gen.oneOf(genPublish, genLike, genDislike, genReport, genReaction, genRetrieve, genSearch, genEdit, genRemoveLikeOrDislike, genDelete)

  val genAuthor: Gen[String] = Gen.oneOf("Alice", "Bob")
  val genReporter: Gen[String] = Gen.oneOf("Alice", "Bob", "Lena", "Lukas", "Simone", "Charles", "Gracie", "Patrick", "Laura", "Leon")
  val genMessage: Gen[String] = Gen.oneOf("msg_w_9ch", "msg_w_10ch", "msg_w_11ch_")
  val genEmoji: Gen[Reaction.Emoji] = Gen.oneOf(Reaction.Emoji.COOL, Reaction.Emoji.CRYING, Reaction.Emoji.LAUGHING, Reaction.Emoji.SMILEY, Reaction.Emoji.SURPRISE, Reaction.Emoji.SKEPTICAL)

  def genPublish: Gen[PublishCommand] = for {
    author <- genAuthor
    message <- genMessage
  } yield PublishCommand(author, message)

  //TODO general questions:
  // - where do I need to implement the restrictions??
  //   in the nextState, in the postCondition or i both?
  // _
  // general -> always edit run, nextState and postCondition


  case class PublishCommand(author: String, message: String) extends Command {
    type Result = Message

    def run(sut: Sut): Result = {
      // TODO
      /*
      Here you execute the command on the System-Under-Test. You should return a result that
      can be checked in the post-condition. We already suggested result types, but you are allowed
      to change them.
       */
      /* from MessageBoardProperties
      val sut = new SUTMessageBoard
      sut.getDispatcher.tell(new InitCommunication(sut.getClient, sut.getCommId))
      while (sut.getClient.receivedMessages.isEmpty)
        sut.getSystem.runFor(1)
      val initAck = sut.getClient.receivedMessages.remove.asInstanceOf[InitAck]
      val worker: SimulatedActor = initAck.worker
       */

      sut.getDispatcher.tell(new InitCommunication(sut.getClient, sut.getCommId))
      while(sut.getClient.receivedMessages.isEmpty){
        sut.getSystem.runFor(1)
      }
      val initAck = sut.getClient.receivedMessages.remove.asInstanceOf[InitAck]
      val worker: SimulatedActor = initAck.worker

      worker.tell(new Publish(new UserMessage(author, message), sut.getCommId))
      while (sut.getClient.receivedMessages.isEmpty) {
        sut.getSystem.runFor(1)
      }
      val res = sut.getClient.receivedMessages.remove()

      worker.tell(new FinishCommunication(sut.getCommId))
      while (sut.getClient.receivedMessages.isEmpty == true) {
        sut.getSystem.runFor(1)
      }
      sut.getClient.receivedMessages.remove()
      res
    }

    def nextState(state: State): State = {
      /*
      Here you execute the command on the model. Please pay attention: The model has to be
      immutable. You cannot modify the variables of the model directly; instead, you need to use
      copy() to return a modified instance.
       */
      //R1 less than or exactly
      if(message.length > MAX_MESSAGE_LENGTH) {
          return state.copy(lastCommandSuccessful = false)
      }

      //R2 more than USER BLOCKED AT COUNT (=5)
      val num_reports = state.reports.count(report => report.reportedClientName == author)

      if (num_reports >USER_BLOCKED_AT_COUNT) {
        return state.copy(lastCommandSuccessful = false, userBanned = true)
      }

      // R3 -> saving messages
      val index_eq_msg = state.messages.indexWhere(mes => (mes.message == message) && (mes.author == author))
      if(index_eq_msg != -1) {
        return state.copy(lastCommandSuccessful = false)
      }


      // R4 -> like/dislike
      // R5 -> like/dislike
      // R6 -> get a list of all possible messages
      // R7 -> search for messages
      // R8 -> reporting
      // R9 -> unsuccessful requests //TODO?
      // R10 -> handling of unsuccessful requests // TODO implement this here?
      // R11 -> handling of likes/dislikes
      // R12 -> remove of a like
      // R13 -> like/dislike
      // R14 -> reactions
      // R15 -> edit
      // R16 -> edit
      // R17 -> deleting

      // => no other requirement is relevant, meaning if those two did not catch
      // smth it is true

      val messages = ModelUserMessage(author, message, Nil, Nil, collection.mutable.Map(), 0) :: state.messages
      state.copy(messages = messages, lastCommandSuccessful = true, userBanned = false)
    }

    override def preCondition(state: State): Boolean = true

    override def postCondition(state: State, result: Try[Message]): Prop = {
      if (result.isSuccess) {
        val reply: Message = result.get
        val newState: State = nextState(state)
        /*
        Here you need to check if the behaviour of the SUT and the model is the same. Please pay attention:
        The state passed as the argument is the state before the invocation of nextState(),
        so you need to use nextState(state) to get the actual model after processing the command.
         */
        //if(reply == newState) true
        if(reply.isInstanceOf[OperationAck] && newState.lastCommandSuccessful)  true
        else false
      } else {
        false
      }
    }

    override def toString: String = s"Publish($author, $message)"
  }

  def genReaction: Gen[ReactionCommand] = for {
    author <- genAuthor
    message <- genMessage
    rName <- genAuthor
    reactionType <- genEmoji
  } yield ReactionCommand(author, message, rName, reactionType)

  case class ReactionCommand(author: String, message: String, rName: String, reactionType: Reaction.Emoji) extends Command {
    type Result = Message

    def run(sut: Sut): Result = {
      sut.getDispatcher.tell(new InitCommunication(sut.getClient, sut.getCommId))
      while (sut.getClient.receivedMessages.isEmpty) {
        sut.getSystem.runFor(1)
      }
      val initAck = sut.getClient.receivedMessages.remove.asInstanceOf[InitAck]
      val worker: SimulatedActor = initAck.worker

      worker.tell(new RetrieveMessages(author, sut.getCommId))
      while (sut.getClient.receivedMessages.isEmpty) {
        sut.getSystem.runFor(1)
      }
      val res = sut.getClient.receivedMessages.remove().asInstanceOf[FoundMessages]

      val msgID = res.messages.asScala.find(msg => msg.getMessage == message).orNull.getMessageId

      worker.tell(new Reaction(rName, sut.getCommId, msgID, reactionType))
      while (sut.getClient.receivedMessages.isEmpty) {
        sut.getSystem.runFor(1)
      }
      val res1 = sut.getClient.receivedMessages.remove() //.asInstanceOf[FoundMessages]

      worker.tell(new FinishCommunication(sut.getCommId))
      while (sut.getClient.receivedMessages.isEmpty == true) {
        sut.getSystem.runFor(1)
      }
      sut.getClient.receivedMessages.remove()

      res1
    }

    def nextState(state: State): State = {
      //R2 more than USER BLOCKED AT COUNT (=5)
      val num_reports = state.reports.count(report => report.reportedClientName == author)
      if (num_reports > USER_BLOCKED_AT_COUNT) {
        return state.copy(lastCommandSuccessful = false, userBanned = true)
      }

      // R4 In order to  react to or delete a message, the message must exist
      val exists = state.messages.exists(m => (m.message == message && m.author == author))
      if(!exists) {
        return state.copy(lastCommandSuccessful = false)
      }

      // R14 A message can get more than one reaction from a user, but all reactions to a message from
      // the same user should be different (set semantics).
      val equal_reaction_exists = state.messages.exists(m => ((m.message == message && m.author == author)
        && m.reactions.exists(reaction => (reaction._1 == rName && reaction._2.contains(reactionType)))))
      if(equal_reaction_exists) {
        return state.copy(lastCommandSuccessful = false)
      }


      val messages = state.messages.map(
        msg => {
          if (msg.message == message && msg.author == author) {
            if(msg.reactions.get(rName) != null){
              val cloned_reactions = msg.reactions.clone()
              cloned_reactions.put (rName, msg.reactions(rName).clone() += reactionType)
              msg.copy(reactions = cloned_reactions.clone())
            }
            else {
              val cloned_reaction = msg.reactions.clone()
              cloned_reaction.put(rName, collection.mutable.Set[Reaction.Emoji](reactionType))
              msg.copy( reactions = cloned_reaction.clone())
            }
          } else {
            msg
          }
        }
      )
      state.copy(messages = messages, lastCommandSuccessful = true, userBanned = false)
    }

    override def preCondition(state: State): Boolean = true

    override def postCondition(state: State, result: Try[Message]): Prop = {
      if (result.isSuccess) {
        val reply: Message = result.get
        val newState: State = nextState(state)
        if(reply.isInstanceOf[ReactionResponse] && newState.lastCommandSuccessful)
          true
        else
          false
      } else {
        val reply: Message = result.get
        //if(reply.isInstanceOf[OperationFailed] && !newState.lastCommandSuccessful)  true
        false
      }
    }

    override def toString: String = s"Reaction($author, $message, $rName, $reactionType)"
  }

  def genLike: Gen[LikeCommand] = for {
    author <- genAuthor
    message <- genMessage
    likeName <- genAuthor
  } yield LikeCommand(author, message, likeName)

  case class LikeCommand(author: String, message: String, likeName: String) extends Command {
    type Result = Message

    def run(sut: Sut): Result = {
      sut.getDispatcher.tell(new InitCommunication(sut.getClient, sut.getCommId))
      while (sut.getClient.receivedMessages.isEmpty) {
        sut.getSystem.runFor(1)
      }
      val initAck = sut.getClient.receivedMessages.remove.asInstanceOf[InitAck]
      val worker: SimulatedActor = initAck.worker

      worker.tell(new RetrieveMessages(message, sut.getCommId))
      while (sut.getClient.receivedMessages.isEmpty) {
        sut.getSystem.runFor(1)
      }
      val res = sut.getClient.receivedMessages.remove().asInstanceOf[FoundMessages]

      val msgID = res.messages.asScala.find(msg => msg.getMessage == message).orNull.getMessageId

      worker.tell(new Like(likeName, sut.getCommId, msgID))
      while (sut.getClient.receivedMessages.isEmpty) {
        sut.getSystem.runFor(1)
      }
      val res1 = sut.getClient.receivedMessages.remove() //.asInstanceOf[FoundMessages]

      worker.tell(new FinishCommunication(sut.getCommId))
      while (sut.getClient.receivedMessages.isEmpty == true) {
        sut.getSystem.runFor(1)
      }
      sut.getClient.receivedMessages.remove()

      res1
    }

    def nextState(state: State): State = {
      //R2 more than USER BLOCKED AT COUNT (=5)
      val num_reports = state.reports.count(report => report.reportedClientName == author)
      if (num_reports > USER_BLOCKED_AT_COUNT) {
        return state.copy(lastCommandSuccessful = false, userBanned = true)
      }

      // R4 In order to like/dislike edit, react to or delete a message, the message must exist.
      val exists = state.messages.exists(m => (m.message == message && m.author == author))
      if (!exists) {
        return state.copy(lastCommandSuccessful = false)
      }

      // R5 A message may only be liked/disliked by users who have not yet liked/disliked the correspon-
      //ding message.
      val already_liked = state.messages.exists(m => ((m.message == message) &&
         m.likes.contains(likeName)))
      if (already_liked) {
        return state.copy(lastCommandSuccessful = false)
      }

      val messages = state.messages.map (
        msg => {
          if(msg.message == message && msg.author == author){
            msg.copy(likes = msg.likes:+likeName,
                    points = msg.points + 2 )
          }
          else {
            msg
          }
        }
      )

      state.copy(messages = messages, lastCommandSuccessful = true, userBanned = false)
    }

    override def preCondition(state: State): Boolean = true

    override def postCondition(state: State, result: Try[Message]): Prop = {
      if (result.isSuccess) {
        val reply: Message = result.get
        val newState: State = nextState(state)
        // f the system replies with ReactionResponse, the SUT
        //was successful.
        if (reply.isInstanceOf[ReactionResponse] && newState.lastCommandSuccessful)
          true
        else
          false
      } else {
        false
      }
    }

    override def toString: String = s"Like($author, $message, $likeName)"
  }

  def genDislike: Gen[DislikeCommand] = for {
    author <- genAuthor
    message <- genMessage
    dislikeName <- genAuthor
  } yield DislikeCommand(author, message, dislikeName)

  case class DislikeCommand(author: String, message: String, dislikeName: String) extends Command {
    type Result = Message

    def run(sut: Sut): Result = {
      sut.getDispatcher.tell(new InitCommunication(sut.getClient, sut.getCommId))
      while (sut.getClient.receivedMessages.isEmpty) {
        sut.getSystem.runFor(1)
      }
      val initAck = sut.getClient.receivedMessages.remove.asInstanceOf[InitAck]
      val worker: SimulatedActor = initAck.worker

      worker.tell(new RetrieveMessages(message, sut.getCommId))
      while (sut.getClient.receivedMessages.isEmpty) {
        sut.getSystem.runFor(1)
      }
      val res = sut.getClient.receivedMessages.remove().asInstanceOf[FoundMessages]

      val msgID = res.messages.asScala.find(msg => msg.getMessage == message).orNull.getMessageId

      worker.tell(new Dislike(dislikeName, sut.getCommId, msgID))
      while (sut.getClient.receivedMessages.isEmpty) {
        sut.getSystem.runFor(1)
      }
      val res1 = sut.getClient.receivedMessages.remove() //.asInstanceOf[FoundMessages]

      worker.tell(new FinishCommunication(sut.getCommId))
      while (sut.getClient.receivedMessages.isEmpty == true) {
        sut.getSystem.runFor(1)
      }
      sut.getClient.receivedMessages.remove()

      res1
    }

    def nextState(state: State): State = {

      //R2 more than USER BLOCKED AT COUNT (=5)
      val num_reports = state.reports.count(report => report.reportedClientName == author)

      if (num_reports > USER_BLOCKED_AT_COUNT) {
        return state.copy(lastCommandSuccessful = false, userBanned = true)
      }

      //R4
      val exists = state.messages.exists(m => (m.message == message && m.author == author))
      if (!exists) {
        return state.copy(lastCommandSuccessful = false)
      }

      // R5 A message may only be liked/disliked by users who have not yet liked/disliked the correspon-
      //ding message.
      val already_liked = state.messages.exists(m => ((m.message == message) &&
        m.likes.contains(dislikeName)))
      if (already_liked) {
        return state.copy(lastCommandSuccessful = false)
      }

      val messages = state.messages.map(
        msg => {
          if (msg.message == message && msg.author == author) {
            msg.copy(dislikes = msg.dislikes :+ dislikeName,
              points = msg.points - 1)
          }
          else {
            msg
          }
        }
      )

      state.copy(messages = messages, lastCommandSuccessful = true, userBanned = false)
    }

    override def preCondition(state: State): Boolean = true

    override def postCondition(state: State, result: Try[Message]): Prop = {
      if (result.isSuccess) {
        val reply: Message = result.get
        val newState: State = nextState(state)
        if (reply.isInstanceOf[ReactionResponse] && newState.lastCommandSuccessful)
          true
        else
          false
      } else {
        false
      }
    }

    override def toString: String = s"Dislike($author, $message, $dislikeName)"
  }

  def genReport: Gen[ReportCommand] = for {
    reporter <- genReporter
    reported <- genAuthor
  } yield ReportCommand(reporter, reported)

  case class ReportCommand(reporter: String, reported: String) extends Command {
    type Result = Message

    def run(sut: Sut): Result = {
      sut.getDispatcher.tell(new InitCommunication(sut.getClient, sut.getCommId))
      while (sut.getClient.receivedMessages.isEmpty) {
        sut.getSystem.runFor(1)
      }
      val initAck = sut.getClient.receivedMessages.remove.asInstanceOf[InitAck]
      val worker: SimulatedActor = initAck.worker

      worker.tell(new Report(reporter, sut.getCommId, reported))
      while (sut.getClient.receivedMessages.isEmpty) {
        sut.getSystem.runFor(1)
      }

      val res = sut.getClient.receivedMessages.remove()

      worker.tell(new FinishCommunication(sut.getCommId))
      while (sut.getClient.receivedMessages.isEmpty == true) {
        sut.getSystem.runFor(1)
      }
      sut.getClient.receivedMessages.remove()

      res
    }

    def nextState(state: State): State = {
      //R2 more than USER BLOCKED AT COUNT (=5)
      val num_reports = state.reports.count(report => report.reportedClientName == reporter)
      if (num_reports > USER_BLOCKED_AT_COUNT) {
        return state.copy(lastCommandSuccessful = false, userBanned = true)
      }

      // R8 A user may report another user only if they have not previously reported the user in question.
      val alreadyReported = state.reports.find(rep => rep.clientName == reporter && rep.reportedClientName == reported)
      if(alreadyReported != None){
        return state.copy(lastCommandSuccessful = false)

      }

      val reportsVal = ModelReport(reporter, reported)::state.reports

      state.copy(lastCommandSuccessful = true, userBanned = false, reports = reportsVal)
    }

    override def preCondition(state: State): Boolean = true

    override def postCondition(state: State, result: Try[Message]): Prop = {
      if (result.isSuccess) {
        val reply: Message = result.get
        val newState: State = nextState(state)
        if (reply.isInstanceOf[OperationAck] && newState.lastCommandSuccessful)
          true
        else
          false
      } else {
        false
      }
    }

    override def toString: String = s"Report($reporter, $reported)"
  }

  def genRetrieve: Gen[RetrieveCommand] = for {
    author <- genAuthor
  } yield RetrieveCommand(author)

  // just a suggestion, change it according to your needs.
  case class RetrieveCommandResult(success: Boolean, messages: List[String])

  /*
  (UserMessage.toString()) and in the model
  (ModelUserMessage.toString()) to compare the individual messages.
   */

  case class RetrieveCommand(author: String) extends Command {
    type Result = RetrieveCommandResult

    def run(sut: Sut): Result = {
      sut.getDispatcher.tell(new InitCommunication(sut.getClient, sut.getCommId))
      while (sut.getClient.receivedMessages.isEmpty) {
        sut.getSystem.runFor(1)
      }
      val initAck = sut.getClient.receivedMessages.remove.asInstanceOf[InitAck]
      val worker: SimulatedActor = initAck.worker

      worker.tell(new RetrieveMessages(author, sut.getCommId))
      while (sut.getClient.receivedMessages.isEmpty) {
        sut.getSystem.runFor(1)
      }
      val res = sut.getClient.receivedMessages.remove().asInstanceOf[FoundMessages]

      val notSuccessful = res.messages.isEmpty
      val messages = {
        if(notSuccessful) Nil
        else res.messages.asScala.map(m => m.getMessage).toList
      }

      val res1 = RetrieveCommandResult(!notSuccessful, messages)

      worker.tell(new FinishCommunication(sut.getCommId))
      while (sut.getClient.receivedMessages.isEmpty == true) {
        sut.getSystem.runFor(1)
      }
      sut.getClient.receivedMessages.remove()

      res1
    }

    def nextState(state: State): State = {
      state.copy(lastCommandSuccessful = true)
    }

    override def preCondition(state: State): Boolean = true

    override def postCondition(state: State, result: Try[Result]): Prop = {
      if (result.isSuccess) {
        val reply: Result = result.get
        if(reply.success) {
          val newState: State = nextState(state)
          val msges = newState.messages.filter(m => m.author == author).map(m => m.message)
          val model_msges = reply.messages
          // R6 It should be possible to retrieve a list of all existing messages of an author.
          // could not find anything to search for
          if(msges == model_msges) true //TODO is this enough/does it work good enough
          else false
        }
        else {
          false
        }
      } else {
        false
      }
    }

    override def toString: String = s"Retrieve($author)"
  }

  def genSearch: Gen[SearchCommand] = for {
    searchText <- genMessage
  } yield SearchCommand(searchText)

  case class SearchCommandResult(success: Boolean, messages: List[String])

  case class SearchCommand(searchText: String) extends Command {
    type Result = SearchCommandResult

    def run(sut: Sut): Result = {
      sut.getDispatcher.tell(new InitCommunication(sut.getClient, sut.getCommId))
      while (sut.getClient.receivedMessages.isEmpty) {
        sut.getSystem.runFor(1)
      }
      val initAck = sut.getClient.receivedMessages.remove.asInstanceOf[InitAck]
      val worker: SimulatedActor = initAck.worker

      worker.tell(new SearchMessages(searchText, sut.getCommId))
      while (sut.getClient.receivedMessages.isEmpty) {
        sut.getSystem.runFor(1)
      }
      val res = sut.getClient.receivedMessages.remove().asInstanceOf[FoundMessages]
  // TODO can I test this with author as well (from res.author)
      val notSuccessful = res.messages.isEmpty()
      val messages = {
        if (notSuccessful) Nil
        else res.messages.asScala.map(m => m.getMessage).toList
      }

      val res1 = SearchCommandResult(!notSuccessful, messages)

      worker.tell(new FinishCommunication(sut.getCommId))
      while (sut.getClient.receivedMessages.isEmpty == true) {
        sut.getSystem.runFor(1)
      }
      sut.getClient.receivedMessages.remove()

      res1
    }

    def nextState(state: State): State = {
      state.copy(lastCommandSuccessful = true)
    }

    override def preCondition(state: State): Boolean = true

    override def postCondition(state: State, result: Try[Result]): Prop = {
      if (result.isSuccess) {
        /*
        R7 It should be possible to search for messages containing a given text (case-insensitive) in the
        message text itself or the author’s name and get back a list of those messages.
        */
        val reply: Result = result.get

        if (reply.success) {
          val newState: State = nextState(state)
          val msges = newState.messages.filter(m => m.message.contains(searchText)).map(m => m.message)
          val model_msges = reply.messages

          if(msges == model_msges) true
          else false
        }
        else false
      } else {
        false
      }
    }

    override def toString: String = s"Search($searchText)"
  }

  def genEdit: Gen[EditCommand] = for {
    author <- genAuthor
    oldMessage <- genMessage
    newMessage <- genMessage
  } yield EditCommand(author, oldMessage, newMessage)

  case class EditCommand(author: String, oldMessage: String, newMessage: String) extends Command {
    type Result = Message

    def run(sut: Sut): Result = {
      sut.getDispatcher.tell(new InitCommunication(sut.getClient, sut.getCommId))
      while (sut.getClient.receivedMessages.isEmpty) {
        sut.getSystem.runFor(1)
      }
      val initAck = sut.getClient.receivedMessages.remove.asInstanceOf[InitAck]
      val worker: SimulatedActor = initAck.worker

      worker.tell(new RetrieveMessages(author, sut.getCommId))
      while (sut.getClient.receivedMessages.isEmpty) {
        sut.getSystem.runFor(1)
      }
      val res = sut.getClient.receivedMessages.remove().asInstanceOf[FoundMessages]

      val msgID = res.messages.asScala.find(msg => msg.getMessage == oldMessage).orNull.getMessageId

      worker.tell(new Edit(msgID, author, newMessage, sut.getCommId))
      while (sut.getClient.receivedMessages.isEmpty) {
        sut.getSystem.runFor(1)
      }

      val res1 = sut.getClient.receivedMessages.remove()

      worker.tell(new FinishCommunication(sut.getCommId))
      while (sut.getClient.receivedMessages.isEmpty == true) {
        sut.getSystem.runFor(1)
      }
      sut.getClient.receivedMessages.remove()

      res1
    }

    def nextState(state: State): State = {
      //R2 more than USER BLOCKED AT COUNT (=5)
      val num_reports = state.reports.count(report => report.reportedClientName == author)
      if (num_reports > USER_BLOCKED_AT_COUNT) {
        return state.copy(lastCommandSuccessful = false, userBanned = true)
      }

      //R15 An author is unable to edit a message to have the
      // same content as one of their existing messages
      if (oldMessage == newMessage) {
        return state.copy(lastCommandSuccessful = false)
      }

      //R16 Only the author of a message, who published it, is able to edit it.
      val msgFromAuthor = state.messages.find(msg => (msg.message == oldMessage && msg.author == author))
      if (msgFromAuthor == None) {
        return state.copy(lastCommandSuccessful = false)
      }

      state.copy(lastCommandSuccessful = true, userBanned = false)
    }

    override def preCondition(state: State): Boolean = true

    override def postCondition(state: State, result: Try[Message]): Prop = {
      if (result.isSuccess) {
        val reply: Result = result.get
        false // TODO
      } else {
        false
      }
    }

    override def toString: String = s"Edit($author, $oldMessage, $newMessage)"
  }

  def genRemoveLikeOrDislike: Gen[RemoveLikeOrDislikeCommand] = for {
    author <- genAuthor
    message <- genMessage
    removeType <- Gen.choose(0, 1)
  } yield RemoveLikeOrDislikeCommand(author, message, removeType)

  case class RemoveLikeOrDislikeCommand(author: String, message: String, removeType: Int) extends Command {
    type Result = Message

    def run(sut: Sut): Result = {
      sut.getDispatcher.tell(new InitCommunication(sut.getClient, sut.getCommId))
      while (sut.getClient.receivedMessages.isEmpty) {
        sut.getSystem.runFor(1)
      }
      val initAck = sut.getClient.receivedMessages.remove.asInstanceOf[InitAck]
      val worker: SimulatedActor = initAck.worker

      worker.tell(new SearchMessages(message, sut.getCommId))
      while (sut.getClient.receivedMessages.isEmpty) {
        sut.getSystem.runFor(1)
      }
      val res = sut.getClient.receivedMessages.remove().asInstanceOf[FoundMessages]

      val msgID = res.messages.asScala.find(msg => msg.getMessage == message).orNull.getMessageId

      worker.tell(new RemoveLikeOrDislike(author, sut.getCommId, msgID, removeType.asInstanceOf[RemoveLikeOrDislike.Type]))
      while (sut.getClient.receivedMessages.isEmpty) {
        sut.getSystem.runFor(1)
      }

      val res1 = sut.getClient.receivedMessages.remove()

      worker.tell(new FinishCommunication(sut.getCommId))
      while (sut.getClient.receivedMessages.isEmpty == true) {
        sut.getSystem.runFor(1)
      }
      sut.getClient.receivedMessages.remove()

      res1
    }

    def nextState(state: State): State = {
      // TODO
      state
    }

    override def preCondition(state: State): Boolean = true

    override def postCondition(state: State, result: Try[Message]): Prop = {
      if (result.isSuccess) {
        val reply: Result = result.get
        false // TODO
      } else {
        false
      }
    }

    override def toString: String = s"RemoveLikeOrDislike($author, $message, $removeType)"
  }
  
    def genDelete: Gen[DeleteCommand] = for {
    author <- genAuthor
    message <- genMessage
    deletingUser <- genAuthor
  } yield DeleteCommand(author, message, deletingUser)

  case class DeleteCommand(author: String, message: String, deletingUser: String) extends Command {
    type Result = Message

    def run(sut: Sut): Result = {
      sut.getDispatcher.tell(new InitCommunication(sut.getClient, sut.getCommId))
      while (sut.getClient.receivedMessages.isEmpty) {
        sut.getSystem.runFor(1)
      }
      val initAck = sut.getClient.receivedMessages.remove.asInstanceOf[InitAck]
      val worker: SimulatedActor = initAck.worker

      worker.tell(new RetrieveMessages(message, sut.getCommId))
      while (sut.getClient.receivedMessages.isEmpty) {
        sut.getSystem.runFor(1)
      }
      val res = sut.getClient.receivedMessages.remove().asInstanceOf[FoundMessages]

      val msgID = res.messages.asScala.find(msg => msg.getMessage == message).orNull.getMessageId

      worker.tell(new Delete(msgID, deletingUser, sut.getCommId))
      while (sut.getClient.receivedMessages.isEmpty) {
        sut.getSystem.runFor(1)
      }
      val res1 = sut.getClient.receivedMessages.remove() //.asInstanceOf[FoundMessages]

      worker.tell(new FinishCommunication(sut.getCommId))
      while (sut.getClient.receivedMessages.isEmpty == true) {
        sut.getSystem.runFor(1)
      }
      sut.getClient.receivedMessages.remove()

      res1
    }

    def nextState(state: State): State = {
      // TODO
      state
    }

    override def preCondition(state: State): Boolean = true

    override def postCondition(state: State, result: Try[Message]): Prop = {
      if (result.isSuccess) {
        val reply: Result = result.get
        false // TODO
      } else {
        false
      }
    }

    override def toString: String = s"Delete($author, $message, $deletingUser)"
  }
}
