package objsets

import org.scalatest._

class CallCounter extends Function1[Tweet, Unit] {
  var count = 0
  def apply(tweet: Tweet) = { count += 1 }
}

class TweetSetSpec extends FunSpec with Matchers {

  describe("A TweetSet") {

    describe("has a filterAcc method") {

      it("which returns an empty set if the TweetSet is empty") {
        val counter = new CallCounter
        new Empty().filterAcc((_) => true, new Empty).foreach(counter)
        counter.count should be (0)
      }

      it("which returns an empty set if the predicate accepts nothing") {
        val counter = new CallCounter
        val set = new Empty().incl(new Tweet(null, "foo", 0))

        set.filterAcc((_) => false, new Empty).foreach(counter)

        counter.count should be (0)
      }

      it("which returns the same set if the accumulator is empty and the predicate accepts everything") {
        val counter = new CallCounter
        val tweet = new Tweet(null, "foo", 0)
        val set = new Empty().incl(tweet)

        val filtered = set.filterAcc((_) => true, new Empty)
        filtered.foreach(counter)

        counter.count should be (1)
        filtered contains tweet should be (true)
      }

      it("which returns only those tweets that match the predicate") {
        val goodTweets = Seq(
          new Tweet(null, "msg1", 1),
          new Tweet("user1", "msg2", 2),
          new Tweet(null, "msg3", 3)
        )
        val badTweets = Seq(
          new Tweet(null, "msg4", 0),
          new Tweet("user2", "msg5", 0),
          new Tweet(null, "msg6", 0)
        )

        var set: TweetSet = new Empty
        goodTweets.foreach((tweet: Tweet) => { set = set.incl(tweet) })
        badTweets. foreach((tweet: Tweet) => { set = set.incl(tweet) })

        val filtered = set.filterAcc(_.retweets > 0, new Empty)

        val counter = new CallCounter
        filtered.foreach(counter)
        counter.count should be (3)

        goodTweets.foreach(filtered contains _ should be (true))
        badTweets. foreach(filtered contains _ should be (false))
      }

      it("which retains the tweets in the accumulator") {
        val tweet = new Tweet(null, "foo", 0)
        val acc = new Empty().incl(tweet)

        val filtered = new Empty().filterAcc((_) => true, acc)

        val counter = new CallCounter
        filtered.foreach(counter)
        counter.count should be (1)

        filtered contains tweet should be (true)
      }

    }

  }

}
