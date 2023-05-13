package at.tugraz.ist.qs2023

import at.tugraz.ist.qs2023.actorsystem.{Message, SimulatedActor}
import at.tugraz.ist.qs2023.messageboard.MessageStore.USER_BLOCKED_AT_COUNT
import at.tugraz.ist.qs2023.messageboard.UserMessage
import at.tugraz.ist.qs2023.messageboard.Worker.MAX_MESSAGE_LENGTH
import at.tugraz.ist.qs2023.messageboard.clientmessages._
import org.scalacheck.commands.Commands
import org.scalacheck.{Gen, Prop}

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
      // TODO
      /*
      Here you execute the command on the model. Please pay attention: The model has to be
      immutable. You cannot modify the variables of the model directly; instead, you need to use
      copy() to return a modified instance.
       */
      //R1 less than or exactly
      if(message.length > MAX_MESSAGE_LENGTH) return state.copy(lastCommandSuccessful = false)

      //R2 more than USER BLOCKED AT COUNT (=5)
      val num_reports = state.reports.count(report => report.reportedClientName == author)

      else if (num_reports >USER_BLOCKED_AT_COUNT) return state.copy(lastCommandSuccessful = false, userBanned = true)

      // R3 -> saving messages = irrelavant TODO do I need this here as well?


      // R4 -> like/dislike
      // R5 -> like/dislike
      // R6 -> get a list of all possible messages
      // R7 -> search for messages
      // R8 -> reporting
      // R9 -> unsuccessful requests
      // R10 -> handling of unsuccessful requests
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


      //state
    }

    override def preCondition(state: State): Boolean = true

    override def postCondition(state: State, result: Try[Message]): Prop = {
      if (result.isSuccess) {
        val reply: Message = result.get
        val newState: State = nextState(state)
        // TODO
        /*
        Here you need to check if the behaviour of the SUT and the model is the same. Please pay attention:
        The state passed as the argument is the state before the invocation of nextState(),
        so you need to use nextState(state) to get the actual model after processing the command.
         */
        //if(reply == newState) true
        if(reply.isInstanceOf[OperationAck] && newState.lastCommandSuccessful)  true
        // TODO can it still be userBanned or would isSuccess be false then
        else if(reply.isInstanceOf[UserBanned] && newState.userBanned)  true
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
      // TODO
      throw new java.lang.UnsupportedOperationException("Not implemented yet.")
    }

    def nextState(state: State): State = {
      // TODO
      state
    }

    override def preCondition(state: State): Boolean = true

    override def postCondition(state: State, result: Try[Message]): Prop = {
      if (result.isSuccess) {
        val reply: Message = result.get
        val newState: State = nextState(state)
        false // TODO
      } else {
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
      // TODO
      throw new java.lang.UnsupportedOperationException("Not implemented yet.")
    }

    def nextState(state: State): State = {
      // TODO
      state
    }

    override def preCondition(state: State): Boolean = true

    override def postCondition(state: State, result: Try[Message]): Prop = {
      if (result.isSuccess) {
        val reply: Message = result.get
        val newState: State = nextState(state)
        false // TODO
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
      // TODO
      throw new java.lang.UnsupportedOperationException("Not implemented yet.")
    }

    def nextState(state: State): State = {
      // TODO
      state
    }

    override def preCondition(state: State): Boolean = true

    override def postCondition(state: State, result: Try[Message]): Prop = {
      if (result.isSuccess) {
        val reply: Message = result.get
        val newState: State = nextState(state)
        false // TODO
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
      // TODO
      throw new java.lang.UnsupportedOperationException("Not implemented yet.")
    }

    def nextState(state: State): State = {
      // TODO
      state
    }

    override def preCondition(state: State): Boolean = true

    override def postCondition(state: State, result: Try[Message]): Prop = {
      if (result.isSuccess) {
        val reply: Message = result.get
        val newState: State = nextState(state)
        false // TODO
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

  case class RetrieveCommand(author: String) extends Command {
    type Result = RetrieveCommandResult

    def run(sut: Sut): Result = {
      // TODO
      throw new java.lang.UnsupportedOperationException("Not implemented yet.")
    }

    def nextState(state: State): State = {
      // TODO
      state
    }

    override def preCondition(state: State): Boolean = true

    override def postCondition(state: State, result: Try[Result]): Prop = {
      if (result.isSuccess) {
        val reply: Result = result.get
        false // TODO
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
      // TODO
      throw new java.lang.UnsupportedOperationException("Not implemented yet.")
    }

    def nextState(state: State): State = {
      // TODO
      state
    }

    override def preCondition(state: State): Boolean = true

    override def postCondition(state: State, result: Try[Result]): Prop = {
      if (result.isSuccess) {
        val reply: Result = result.get
        false // TODO
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
      // TODO
      throw new java.lang.UnsupportedOperationException("Not implemented yet.")
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
      // TODO
      throw new java.lang.UnsupportedOperationException("Not implemented yet.")
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
      // TODO
      throw new java.lang.UnsupportedOperationException("Not implemented yet.")
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
