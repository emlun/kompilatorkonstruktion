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

      it("which retains the tweets in the accumulator even if some tweets in the set match") {
        val tweet = new Tweet(null, "foo", 0)
        val acc = new Empty().incl(tweet)

        val filtered = new Empty().incl(new Tweet(null, "bar", 0)).filterAcc((_) => true, acc)

        val counter = new CallCounter
        filtered.foreach(counter)
        counter.count should be (2)

        filtered contains tweet should be (true)
      }

    }

    describe("has a filter method") {

      it("which returns an empty set if the TweetSet is empty") {
        val counter = new CallCounter
        new Empty().filter((_) => true).foreach(counter)
        counter.count should be (0)
      }

      it("which returns an empty set if the predicate accepts nothing") {
        val counter = new CallCounter
        val set = new Empty().incl(new Tweet(null, "foo", 0))

        set.filter((_) => false).foreach(counter)

        counter.count should be (0)
      }

      it("which returns the same set if the predicate accepts everything") {
        val counter = new CallCounter
        val tweet = new Tweet(null, "foo", 0)
        val set = new Empty().incl(tweet)

        val filtered = set.filter((_) => true)
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

        val filtered = set.filter(_.retweets > 0)

        val counter = new CallCounter
        filtered.foreach(counter)
        counter.count should be (3)

        goodTweets.foreach(filtered contains _ should be (true))
        badTweets. foreach(filtered contains _ should be (false))
      }

    }

    describe("has a union method") {

      it("which returns the empty set if both operands are empty") {
        val counter = new CallCounter

        new Empty().union(new Empty).foreach(counter)

        counter.count should be (0)
      }

      it("which returns the first operand if the second operand is empty") {
        val counter = new CallCounter
        val tweet = new Tweet(null, "foo", 0)

        val set = new Empty().incl(tweet)

        val united = set.union(new Empty)
        united.foreach(counter)

        counter.count should be (1)
        united contains tweet should be (true)
      }

      it("which returns the second operand if the first operand is empty") {
        val counter = new CallCounter
        val tweet = new Tweet(null, "foo", 0)

        val set = new Empty().incl(tweet)

        val united = new Empty().union(set)
        united.foreach(counter)

        counter.count should be (1)
        united contains tweet should be (true)
      }

      it("which returns a set containing the sum of the sets if operands are disjoint") {
        val counter = new CallCounter
        val tweet1 = new Tweet(null, "foo", 0)
        val tweet2 = new Tweet(null, "bar", 0)

        val set1 = new Empty().incl(tweet1)
        val set2 = new Empty().incl(tweet2)

        val united = set1.union(set2)
        united.foreach(counter)

        counter.count should be (2)
        united contains tweet1 should be (true)
        united contains tweet2 should be (true)
      }

      it("which eliminates duplicates if operands are conjoint") {
        val counter = new CallCounter
        val tweet = new Tweet(null, "foo", 0)

        val set1 = new Empty().incl(tweet)
        val set2 = new Empty().incl(tweet)

        val united = set1.union(set2)
        united.foreach(counter)

        counter.count should be (1)
        united contains tweet should be (true)
      }

    }

    describe("has a mostRetweeted method") {
      it("which returns a tweet with maximum retweets in the set") {
        val tweets = Seq(
          new Tweet(null, "foo", 1),
          new Tweet(null, "bar", 1),
          new Tweet(null, "boo", 2),
          new Tweet(null, "far", 2)
        )
        var set: TweetSet = new Empty
        tweets.foreach((tweet: Tweet) => { set = set.incl(tweet) })

        set.mostRetweeted.retweets should be (2)
      }
    }

    describe("has a descendingByRetweet method") {
      it("which returns a list sorted in descending order of retweets") {
        val tweets = Seq(
          new Tweet(null, "a", 1),
          new Tweet(null, "b", 2),
          new Tweet(null, "c", 3),
          new Tweet(null, "d", 1),
          new Tweet(null, "e", 2),
          new Tweet(null, "f", 3)
        )
        var set: TweetSet = new Empty
        tweets.foreach((tweet: Tweet) => { set = set.incl(tweet) })

        var sorted = set.descendingByRetweet

        sorted.head.retweets should be (3); sorted = sorted.tail
        sorted.head.retweets should be (3); sorted = sorted.tail
        sorted.head.retweets should be (2); sorted = sorted.tail
        sorted.head.retweets should be (2); sorted = sorted.tail
        sorted.head.retweets should be (1); sorted = sorted.tail
        sorted.head.retweets should be (1)
      }
    }

  }

}
