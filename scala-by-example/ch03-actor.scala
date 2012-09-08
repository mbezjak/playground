import scala.actors.Actor
import scala.actors.TIMEOUT
import java.util.Date

abstract class AuctionMessage
case class Offer(bid: Int, client: Actor) extends AuctionMessage
case class Inquire(client: Actor)         extends AuctionMessage

abstract class AuctionReply
case class  Status(asked: Int, expire: Date)               extends AuctionReply
case object BestOffer                                      extends AuctionReply
case class  BeatenOffer(maxBid: Int)                       extends AuctionReply
case class  AuctionConcluded(seller: Actor, client: Actor) extends AuctionReply
case object AuctionFailed                                  extends AuctionReply
case object AuctionOver                                    extends AuctionReply

class Auction(seller: Actor, minBid: Int, closing: Date) extends Actor {
  val timeToShutdown = 10 * 1000 // msec
  val bidIncrement   = 0

  def act() {
    var maxBid = minBid - bidIncrement
    var maxBidder: Actor = null
    var running = true
    while (running) {
      receiveWithin((closing.getTime() - new Date().getTime())) {
        case Offer(bid, client) =>
          if (bid >= maxBid + bidIncrement) {
            if (maxBid >= minBid && maxBidder != null) maxBidder ! BeatenOffer(bid)
            maxBid = bid
            maxBidder = client
            client ! BestOffer
          } else {
            client ! BeatenOffer(maxBid)
          }
        case Inquire(client) => client ! Status(maxBid, closing)
        case TIMEOUT =>
          if (maxBid >= minBid) {
            val reply = AuctionConcluded(seller, maxBidder)
            maxBidder ! reply
            seller ! reply
          } else {
            seller ! AuctionFailed
          }

          receiveWithin(timeToShutdown) {
            case Offer(_, client) => client ! AuctionOver
            case TIMEOUT => running = false
          }
      }
    }
  }
}

class Seller extends Actor {

  def act() = {
    receive {
      case AuctionConcluded(_, client) => println("Seller: Sold to " + client)
      case AuctionFailed => println("Seller: Sold to no one")
    }
  }

}

val tenSecFromNow = new Date(new Date().getTime() + 10 * 1000)
val seller  = new Seller().start()
val auction = new Auction(seller, 10, tenSecFromNow).start()


class Buyer(name: String, maxBid: Int) extends Actor {

  def act() {
    var currentBid = 5
    var actionOver = false
    auction ! Offer(currentBid, this)

    while(currentBid < maxBid && !actionOver) {

      receive {
        case BestOffer => println(name + ": I have best offer at " + currentBid)
        case BeatenOffer(bid) =>
          println(name + ": Minimum bid should be " + bid + " but mine is " + currentBid)
          currentBid += 1
          auction ! Offer(currentBid, this)
        case AuctionConcluded(seller, maxBidder) =>
          println(name + ": Action concluded with max bidder " + maxBidder)
        case AuctionOver =>
          actionOver = true
          println(name + ": Action is over")
      }
    }
  }

  override def toString = name
}

val john    = new Buyer("John", 20).start()
val jane    = new Buyer("Jane", 25).start()
