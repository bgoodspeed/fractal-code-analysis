/* The Computer Language Shootout 
   http://shootout.alioth.debian.org/
   contributed by Isaac Gouy (Scala novice)
*/

object ackermann {
   def main(args: Array[String]) = {
      val n = toPositiveInt(args);
      Console println("Ack(3," + n + "): " + ack(3,n));
   }

   def ack(m: Int, n: Int): Int = 
      if (m == 0) n + 1;
      else if (n == 0) ack(m-1, 1);
      else ack(m-1, ack(m, n-1));

   private def toPositiveInt(s: Array[String]) = {
      val i = 
         try { Integer.parseInt(s(0)); } 
         catch { case _ => 1 }
      if (i>0) i; else 1;
   }
}
/* The Computer Language Shootout 
   http://shootout.alioth.debian.org/
   contributed by Isaac Gouy (Scala novice)
*/

object ary3 {
   def main(args: Array[String]) = {
      val n = toPositiveInt(args);
      var j = 0;

      val x = new Array[Int](n);
      for (val i <- Iterator.range(0,n)) x(i)=i+1;

      val y = new Array[Int](n);
      for (val j <- Iterator.range(0,1000);
           val i <- Iterator.range(0,n))
         y(i)=y(i)+x(i);

      Console.println(y(0) + " " +  y(n-1)); 
   }


   private def toPositiveInt(s: Array[String]) = {
      val i = 
         try { Integer.parseInt(s(0)); } 
         catch { case _ => 1 }
      if (i>0) i; else 1;
   }
}



/* The Computer Language Shootout
   http://shootout.alioth.debian.org/

   contributed by Kannan Goundan
   modified by Isaac Gouy
   optimized by David Pollak
   updated to 2.8 by Rex Kerr
   modified by Piotr Tarsa
*/

sealed abstract class Node(i: Int, left: Node, right: Node) {
  def isum: Int
}
case class NonLeaf(i: Int, left: Node, right: Node) extends Node(i, left, right) {
  def isum: Int = i + left.isum - right.isum
}
case class Leaf(i: Int) extends Node(i, NullNode, NullNode) {
  def isum: Int = i
}
case object NullNode extends Node(0, new Leaf(0), new Leaf(0)) {
  def isum: Int = 0
}

object Tree {
  def apply(i: Int, depth: Int): Node = {
    if (depth > 0) NonLeaf(i, Tree(i * 2 - 1, depth - 1), Tree(i * 2, depth - 1))
    else Leaf(i)
  }
}

object binarytrees {
  def main(args: Array[String]) = {
    val n = try{ args(0).toInt } catch { case _ => 1 }
    val minDepth = 4
    val maxDepth = n max (minDepth + 2)

    def print(name: String, depth: Int, check: Int) =
      println(name + " of depth " + depth + "\t check: " + check)

    print("stretch tree", maxDepth + 1, Tree(0, maxDepth + 1).isum)
    val longLivedTree = Tree(0, maxDepth)
    minDepth to maxDepth by 2 foreach {
      depth =>
      val iterations = 1 << (maxDepth - depth + minDepth)
      var i, sum = 0
      while (i < iterations) {
        i += 1
        sum += Tree(i, depth).isum + Tree(-i, depth).isum
      }
      print(iterations *2  + "\t trees", depth, sum)
    }
    print("long lived tree", maxDepth, longLivedTree.isum)
  }
}
/* The Computer Language Shootout
   http://shootout.alioth.debian.org/

   contributed by Kannan Goundan
   modified by Isaac Gouy
   optimized by David Pollak
   updated for 2.8 and parallelized by Rex Kerr
*/

import scala.actors.Futures._

object binarytrees {
  def report(name: String, depth: Int, check: Int) =
    println(name + " of depth " + depth + "\t check: " + check)

  def main(args: Array[String]) = {
    val n = try{ args(0).toInt } catch { case _ => 1 }
    val minDepth = 4
    val maxDepth = n max (minDepth+2)
    val threads = 3  // More than 3 tends to overwhelm GC

    report("stretch tree", maxDepth+1, Tree(0,maxDepth+1).isum)
    val longLivedTree = Tree(0,maxDepth)
    var depth = minDepth
    while (depth <= maxDepth) {
      val iterations = 1 << (maxDepth - depth + minDepth)
      val limits = (0 to threads).map(_*iterations/threads).sliding(2).toList
      val check = limits.map(i => future(Go(i(0)+1,i(1),depth).calc))
      report(iterations*2 + "\t trees", depth, check.map(_()).sum)
      depth += 2
    }
    report("long lived tree", maxDepth, longLivedTree.isum)
  }
}

case class Sum(var sum: Int) {
  def +=(i: Int) = { sum+=i; this }
}

case class Go(i0: Int, i1: Int, depth: Int) {
  def calc = (Sum(0) /: (i0 to i1))((s,i) =>
    s += Tree(i,depth).isum + Tree(-i,depth).isum
  ).sum
}

final class Tree(i: Int, left: Tree, right: Tree) {
  def isum: Int = if (left eq null) i else i + left.isum - right.isum
}
object Tree {
  def apply(i: Int, depth: Int): Tree = {
    if (depth > 0) new Tree(i, Tree(i*2-1, depth-1), Tree(i*2, depth-1))
    else new Tree(i, null, null)
  }
}

/* The Computer Language Shootout
   http://shootout.alioth.debian.org/

   contributed by Kannan Goundan
   modified by Isaac Gouy
   optimized by David Pollak
   updated to 2.8 by Rex Kerr
*/

object binarytrees {
  def main(args: Array[String]) = {
    val n = try{ args(0).toInt } catch { case _ => 1 }
    val minDepth = 4
    val maxDepth = n max (minDepth+2)

    def print(name: String, depth: Int, check: Int) =
      println(name + " of depth " + depth + "\t check: " + check)

    print("stretch tree", maxDepth+1, Tree(0,maxDepth+1).isum)
    val longLivedTree = Tree(0,maxDepth)
    var depth = minDepth
    while (depth <= maxDepth) {
      val iterations = 1 << (maxDepth - depth + minDepth)
      var i,sum = 0
      while (i < iterations) {
        i += 1
        sum += Tree(i,depth).isum + Tree(-i,depth).isum
      }
      print(iterations*2 + "\t trees", depth, sum)
      depth += 2
    }
    print("long lived tree", maxDepth, longLivedTree.isum)
  }
}

final class Tree(i: Int, left: Tree, right: Tree) {
  def isum: Int = {
    val tl = left
    if (tl eq null) i
    else i + tl.isum - right.isum
  }
}
object Tree {
  def apply(i: Int, depth: Int): Tree = {
    if (depth > 0) new Tree(i, Tree(i*2-1, depth-1), Tree(i*2, depth-1))
    else new Tree(i, null, null)
  }
}
/* The Computer Language Benchmarks Game
   http://shootout.alioth.debian.org/
   contributed by Julien Gaugaz
   inspired by the version contributed by Yura Taras and modified by Isaac Gouy
*/
  
import scala.actors.Actor
import scala.actors.Actor._
import scala.actors.Exit

object chameneos {
  
  abstract class Colour
  case object RED extends Colour
  case object YELLOW extends Colour
  case object BLUE extends Colour
  case object FADED extends Colour
  
  val colours = Array(BLUE, RED, YELLOW)
  
  case class Meet(colour:Colour)
  case class Change(colour:Colour)
  case class MeetingCount(count:int)
  
  
  class Mall(var n: int, numChameneos: int) extends Actor {
    var waitingChameneo:Option[Actor] = None
    startChameneos()
    start()
    
    def startChameneos(): Unit = {
      var i = 0
      while(i < numChameneos) {
        Chameneo(this, colours(i%3), i).start()
        i = i + 1
      }
    }
    
    def act() {
      var sumMeetings = 0
      var numFaded = 0
      loop {
        react {
          
          case MeetingCount(i) => {
            numFaded = numFaded + 1
            sumMeetings = sumMeetings + i
            if(numFaded == numChameneos) {
              println(sumMeetings)
              exit()
            }
          }
          
          case msg@Meet(c) => {
            if(n > 0) {
	      waitingChameneo match {
                case Some(chameneo) =>
                  n = n-1
                  chameneo.forward(msg)
                  waitingChameneo = None
                case None =>
                  waitingChameneo = Some(sender)
              }
            } else {
              waitingChameneo match {
                case Some(chameneo) =>
                  chameneo!Exit(this, "normal")
                case None => 
              }
              sender!Exit(this, "normal")
            }
          }
          
        }
      }
    }
  }
  
  case class Chameneo(var mall: Mall, var colour: Colour, id:int) extends Actor {
    var meetings = 0
    def act() {
      loop {
        mall!Meet(colour)	
        react {
          case Meet(otherColour) =>
            colour = complement(otherColour)
            meetings = meetings +1
            sender!Change(colour)
          case Change(newColour) =>
            colour = newColour
            meetings = meetings +1
          case Exit(_,_) =>
	    colour = FADED
            sender!MeetingCount(meetings)
            exit()
        }
      }
    }
    
    def complement(otherColour:Colour): Colour = {
      colour match {
      case RED => otherColour match {
        case RED => RED
        case YELLOW => BLUE
        case BLUE => YELLOW
        case FADED => FADED
      }
      case YELLOW => otherColour match {
        case RED => BLUE
        case YELLOW => YELLOW
        case BLUE => RED
        case FADED => FADED
      }
      case BLUE => otherColour match {
        case RED => YELLOW
        case YELLOW => RED
        case BLUE => BLUE
        case FADED => FADED
      }
      case FADED => FADED
      }
    }
    override def toString() = id+"("+colour+")"
  }
  
  def main(args : Array[String]) : Unit = {
    if(args.length < 1) throw new IllegalArgumentException("Syntax: scala chameneos N [numChameneos]")
    val N = Integer.parseInt(args(0))
    var numChameneos = 4
    if(args.length == 2)
      numChameneos = Integer.parseInt(args(1))
    new Mall(N, numChameneos)
  }
}
/* The Computer Language Benchmarks Game
   http://shootout.alioth.debian.org/
   contributed by Yura Taras
   modified by Isaac Gouy
   modified by Julien Gaugaz
*/


object chameneos {
  abstract class Colour
  case object RED extends Colour
  case object YELLOW extends Colour
  case object BLUE extends Colour
  case object FADED extends Colour
  val colours = Array(BLUE, RED, YELLOW)
  class MeetingPlace(var n: int) {
    var other: Creature = _
    def meet(c: Creature) = synchronized {
      if(n > 0) {
          if(other == null) {
            other = c;
            this.wait()
          } else {
            other.setOther(c.colour)
            c.setOther(other.colour)
            other = null
            n = n - 1
            this.notify()
          }
        } else {
          c.setOther(FADED)
      }
    }
  }
  class Creature(private val mp: MeetingPlace, var colour: Colour) extends Thread {
    private var met = 0
    var other: Colour = _
    def setOther(_o: Colour) {
      other = _o
    }
    def getCreaturesMet = met
    override def run() {
      try {
        while(colour != FADED) {
          mp.meet(this)
          if(other == FADED) {
            colour = FADED
          } else {
            met = met + 1
            colour = complement(other)
          }
        }
      } catch {
        case e:InterruptedException => () // Let the thread exit
      }
    }

    def complement(other: Colour) = Pair(colour,other) match {
          case Pair(RED,YELLOW) => BLUE
          case Pair(RED,BLUE)   => YELLOW
          case Pair(RED,RED)    => RED
          case Pair(YELLOW,BLUE)=> RED
          case Pair(YELLOW,RED)    => BLUE
          case Pair(YELLOW,YELLOW) => YELLOW
          case Pair(BLUE,RED)      => YELLOW
          case Pair(BLUE,YELLOW)   => RED
          case Pair(BLUE,BLUE)     => BLUE
          case Pair(FADED, _)      => FADED
    }
  }

  def apply(n: int, numChameneos: int) {
      val mp = new MeetingPlace(n)
      val creatureColors:Array[Colour] = new Array(numChameneos)
      var i = 0;
      while(i < numChameneos) {
        creatureColors(i) = colours(i%3)
        i = i + 1
      }
      val creatures = for(val x <- creatureColors) yield {
        val cr = new Creature(mp, x);
        cr.start();
        cr
      }
      creatures.foreach(x => x.join)
      val meetings = (creatures foldLeft 0) {(x, y) => (x + y.getCreaturesMet)}
      Console.println(meetings)
  }

  def main(args: Array[String]) {
    if(args.length < 1) throw new IllegalArgumentException("Syntax: scala chameneos N [numChameneos]")
    val N = Integer.parseInt(args(0))
    var numChameneos = 4
    if(args.length == 2)
      numChameneos = Integer.parseInt(args(1))
    chameneos(N, numChameneos)
  }
}



/* The Computer Language Benchmarks Game
   http://shootout.alioth.debian.org/

   Scala translation contributed by Rex Kerr;
   based on Java original by Michael Barker.
*/


import java.util.concurrent._;
import atomic._;

object chameneosredux {
  object Color extends Enumeration(0,"blue","red","yellow") {
    val Blue,Red,Yellow = Value
    def doComplement(c1: Value, c2: Value) = c1 match {
      case Blue => c2 match {
        case Blue => Blue
        case Red => Yellow
        case Yellow => Red
      }
      case Red => c2 match {
        case Blue => Yellow
        case Red => Red
        case Yellow => Blue
      }
      case Yellow => c2 match {
        case Blue => Red
        case Red => Blue
        case Yellow => Yellow
      }
    }
    def printColors {
      Color.foreach(c1 => Color.foreach(c2 => {
        println(c1 + " + " + c2 + " -> " + doComplement(c1,c2))
      }))
    }
  }

  object Verbalize {
    val digit = Map('0'->"zero",'1'->"one",'2'->"two",'3'->"three",'4'->"four",
                    '5'->"five",'6'->"six",'7'->"seven",'8'->"eight",'9'->"nine")
    def apply(n: Int) = n.toString.toList.map(c=>" "+digit(c)).mkString
  }

  class MeetingPlace(meetings: Int) {
    private val meetingsLeft = new atomic.AtomicInteger(meetings)
    private val creatureRef = new atomic.AtomicReference[Creature]

    def meet(incoming: Creature) {
      val existing = creatureRef.get
      if (existing == null) {
        if (!creatureRef.compareAndSet(existing,incoming)) meet(incoming);
      }
      else {
        val newColor = Color.doComplement(incoming.color,existing.color)      
        if (!creatureRef.compareAndSet(existing,null)) meet(incoming);
        else {
          meetingsLeft.getAndDecrement() match {
            case x if (x>0) =>
              existing.setColor(incoming.id, newColor, x==1);
              incoming.setColor(existing.id, newColor, x==1);
            case _ =>
              existing.finish()
              incoming.finish()
          }
        }
      }
    }
  }

  class Dispatcher(bqc: BlockingQueue[Creature]) extends Runnable {
    def run() {
      try {
        while(true) bqc.take().run()
      }
      catch { case e: InterruptedException => }
    }
  }

  class Creature(place: MeetingPlace, var color: Color.Value, bqc: BlockingQueue[Creature], latch: CountDownLatch) {
    val id = System.identityHashCode(this)
    var count = 0
    private var sameCount = 0

    def finish() { latch.countDown() }
    def setColor(id2: Int, newColor: Color.Value, complete: Boolean) {
      color = newColor
      count += 1
      if (id==id2) sameCount += 1
      if (!complete) bqc.add(this)
      else finish()
    }
    def run() { place.meet(this) }
    override def toString() = count + Verbalize(sameCount)
  }

  def run(n: Int, colors: Color.Value*) {
    val place = new MeetingPlace(n)
    val bqc = new ArrayBlockingQueue[Creature](colors.length)
    val latch = new CountDownLatch(colors.length - 1)

    val creatures = colors.map(c => { print(" "+c) ; new Creature(place, c, bqc, latch) })
    println

    val threads = colors.map(_ => new Thread(new Dispatcher(bqc)))
    threads.foreach(_.start())
    creatures.foreach(c => bqc.add(c))

    try { latch.await }
    catch { case ie: InterruptedException => println("Exiting with error: " + ie) }

    threads.foreach(_.interrupt())

    println( Verbalize ( (0 /: creatures)((sum,c) => { println(c) ; sum + c.count }) ) )
    println
  }

  def main(args: Array[String]) {
    import Color._

    val n = try { args(0).toInt } catch { case _ => 600 }
    
    printColors
    println

    run(n,Blue,Red,Yellow)
    run(n,Blue,Red,Yellow,Red,Yellow,Blue,Red,Yellow,Red,Blue)
  }
}
/*   The Computer Language Benchmarks Game
   http://shootout.alioth.debian.org/

   Contributed by Eric Willigers
   Port of Java implementation by Michael Barker and Luzius Meisser
*/

object Colours extends Enumeration {
   val Blue = Value("blue")
   val Red = Value("red")
   val Yellow = Value("yellow")
}

import Colours.{Blue, Red, Yellow, Value => Colour}

final class Creature(place: MeetingPlace, var colour: Colour) extends Runnable {
   val id = System.identityHashCode(this)
   var sameCount = 0
   var count = 0

   def run() = try {
      while (true) {
         val p = place.meet(id, colour)
         colour = p.colour
         if (p.sameId)
            sameCount += 1
         count +=1
      }
   } catch {
      case _: Exception => ()
   }

   override def toString = String.valueOf(count)+" "+chameneosredux.getNumber(sameCount)
}

final class MeetingPlace(var meetingsLeft: Int) {
   var firstColour: Option[Colour] = None
   var firstId = 0
   var current: Future = _

   def meet(id: Int, c: Colour) = synchronized {
      if (meetingsLeft == 0) {
         throw new Exception("Finished")
      } else {
         if (firstColour.isEmpty) {
            firstColour = Some(c)
            firstId = id
            current = new Future()
         } else {
            current.setItem(new Pair(id == firstId, chameneosredux.doCompliment(c, firstColour.get)))
            firstColour = None
            meetingsLeft -= 1
         }

         current
      }
   }.getItem()
}

final class Future {
   @volatile var p: Pair = _

   def getItem() = {
      while (p == null)
         Thread.`yield`()   
      p
   }

   def setItem(_p: Pair) {
      this.p = _p
   }
}

final case class Pair(sameId: Boolean, colour: Colour)

object chameneosredux {
   def doCompliment(c1: Colour, c2: Colour) = (c1, c2) match {
      case (Blue, Blue) => Blue   
      case (Blue, Red) => Yellow   
      case (Blue, Yellow) => Red   
      case (Red, Blue) => Yellow
      case (Red, Red) => Red
      case (Red, Yellow) => Blue   
      case (Yellow, Blue) => Red   
      case (Yellow, Red) => Blue
      case (Yellow, Yellow) => Yellow
   }

   def run(n: Int, colours: Colour*) {
      val place = new MeetingPlace(n)
      colours.foreach { c => print(" "+c) }
      val creatures = colours.map { new Creature(place, _) }.toArray
      println()
      val ts = creatures.map { new Thread(_) }
      ts.foreach { _.start() }
      ts.foreach { _.join() }
      creatures.foreach { println(_) }
      println(getNumber(creatures.foldLeft(0){_ + _.count}))
      println()
   }

   def main(args: Array[String]) {
      val n = if (args.isEmpty) 600 else Integer.parseInt(args(0))
      printColours()
      println()
      run(n, Blue, Red, Yellow)
      run(n, Blue, Red, Yellow, Red, Yellow,
            Blue, Red, Yellow, Red, Blue)
   }

   val Numbers = Array[String]("zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine")

   def getNumber(n: Int) = String.valueOf(n).toList.map { ch => Numbers(Character.getNumericValue(ch)) } .mkString(" ")

   def printColours() {
      printColours(Blue, Blue)
      printColours(Blue, Red)
      printColours(Blue, Yellow)
      printColours(Red, Blue)   
      printColours(Red, Red)
      printColours(Red, Yellow)
      printColours(Yellow, Blue)
      printColours(Yellow, Red)
      printColours(Yellow, Yellow)
   }

   def printColours(c1: Colour, c2: Colour) {
      println(c1+" + "+c2+" -> "+doCompliment(c1, c2))   
   }
}

/* The Computer Language Shootout 
   http://shootout.alioth.debian.org/
   contributed by Isaac Gouy (Scala novice)
*/

object except {

   var Lo = 0;
   var Hi = 0;

   def main(args: Array[String]) = {
      val n = toPositiveInt(args);

      for (val i <- Iterator.range(0,n)) 
         someFunction(i);

      Console print("Exceptions: HI=" + Hi);
      Console println(" / LO=" + Lo);
   }


   def blowup(n: Int) = {
      if ((n % 2) == 0)
         throw new LoException();
      else 
         throw new HiException();   }


   def loFunction(n: Int) = {
      try { blowup(n); } 
      catch { case _: LoException => Lo = Lo + 1; }
   }


   def hiFunction(n: Int) = {
      try { loFunction(n); } 
      catch { case _: HiException => Hi = Hi + 1; }
   }


   def someFunction(n: Int) = {
      try { hiFunction(n); } 
      catch { case e: Exception =>  
         Console println("We shouldn't get here: " + e);
      }
   }


   def toPositiveInt(s: Array[String]) = {
      val i = 
         try { Integer.parseInt(s(0)); } 
         catch { case _ => 1 }
      if (i>0) i; else 1;
   }

}

private class LoException extends Exception {}
private class HiException extends Exception {}
/*
 * The Computer Language Shootout
 * http://shootout.alioth.debian.org/
 *
 * contributed by Andrei Formiga
 */

object fannkuch
{
  var permN : int = 0
  var maxFlips : int = 0

  def flips(l: List[int]): int = l match {
    case 1 :: ls => 0
    case n :: ls => flips((l take n reverse) ::: (l drop n)) + 1
  }

  def rotateLeft(l: List[int]) = 
    l match { case List() => List() case x :: xs => xs ::: List(x) }

  def printPerm(perm: List[int]) = 
    { perm foreach(x => Console.print(x.toString())); Console.println; }

  def processPerm(perm: List[int]) = {
    val f = flips(perm)
    if (f > maxFlips) maxFlips = f
    if (permN < 30) { printPerm(perm); permN = permN + 1; }
  }

  def permutations(l: List[int], n: int, i: int): unit = {
    if (i < n) {
      if (n == 1)
	processPerm(l)
      else { 
	permutations(l, n - 1, 0)
	permutations(rotateLeft(l take n) ::: (l drop n), n, i + 1)
      }
    }
  }

  def main(args: Array[String]) = 
  {
    val n = Integer.parseInt(args(0))

    permutations(List.range(1, n + 1), n, 0)
    Console.println("Pfannkuchen(" + n + ") = " + maxFlips)
  }
}
/*
 * The Computer Language Shootout
 * http://shootout.alioth.debian.org/
 *
 * contributed by Andrei Formiga
 */

object fannkuch
{
  def main(args: Array[String]) = {
    val n = Integer.parseInt(args(0))
    var maxFlips = 0
    var permN = 0
    var k: int = 0
    var temp: int = 0
    var first: int = 0
    var flips: int = 0
    var perm0: Array[int] = new Array[int](n)
    var perm: Array[int] = new Array[int](n)
    var rot: Array[int] = new Array[int](n)

    while (k < n) { perm(k) = k + 1; rot(k) = 0; k = k + 1; }
    while (rot(n - 1) < n) {
      if (permN < 30) {
	k = 0
	while (k < n) {
	  Console.print(perm(k).toString())
	  k = k + 1
	}
	Console.println
	permN = permN + 1
      }

      flips = 0
      k = 0
      while (k < n) { perm0(k) = perm(k); k = k + 1; }
      first = perm0(0)
      while(first != 1) {
	k = 0
	while (k < first / 2) {
	  temp = perm0(k); perm0(k) = perm0(first - 1 - k); perm0(first - 1 - k) = temp;
	  k = k + 1
	}
	first = perm0(0)
	flips = flips + 1
      }

      if (flips > maxFlips) maxFlips = flips

      temp = perm(0); perm(0) = perm(1); perm(1) = temp;
      rot(1) = rot(1) + 1
      var j = 1
      while (j < n - 1 && rot(j) > j) {
	rot(j) = 0
	j = j + 1

	k = 0
  	while (k < j) {
	  temp = perm(k); perm(k) = perm(k + 1); perm(k + 1) = temp;
	  k = k + 1
	}
	rot(j) = rot(j) + 1
      }
    }

    Console.println("Pfannkuchen(" + n + ") = " + maxFlips)
  }
}

/* The Computer Language Benchmarks Game
 * http://shootout.alioth.debian.org/
 * contributed by Rex Kerr
 */

import scala.actors._

object fannkuch {  
  case class Job(start:Array[Int],n:Int)
  case class Found(who:Kucher,k:Int)
  case class Stop()

  class Kucher(val foreman:Fanner,len:Int,num:Int) extends Actor {
    var go = true
    val perm = new Array[Int](len)
    val temp = new Array[Int](len)
    val flip = new Array[Int](len)

    def permute(n:Int) : Boolean = {
      var i = 0
      while (i < n-1 && flip(i)==0) {
        var j = 0
        val t = perm(0)
        while (j<=i) {
          perm(j) = perm(j+1)
          j = j+1
        }
        perm(j) = t
        i = i+1
      }
      flip(i) -= 1;
      while (i>0) {
        i -= 1
        flip(i)=i
      }
      flip(n-1)>=0
    }

    def count() = {
      var K = 0
      var i = 0
      while (i<perm.length) {
        temp(i) = perm(i)
        i += 1
      }
      while (temp(0)!=0) {
        var m = temp(0)
        i = 0
        while (i<m) {
          val t = temp(i)
          temp(i) = temp(m)
          temp(m) = t
          i += 1
          m -= 1
        }
        K += 1
      }
      K
    }

    def act() { Actor.loopWhile(go) { react {
      case x : Job =>
        var K = 0
        var verbose = 30
        var i = 0
        while (i<perm.length) {
          perm(i) = x.start(i)
          if (perm(i) != i) verbose = 0 
          flip(i)=i
          i += 1
        }
        do {
          if (verbose > 0) {
            println( perm.map(_+1).mkString("") )
            verbose -= 1
          }
          K = K max count
        } while (permute(x.n))
        foreman ! Found(this,K)
      case s : Stop =>
        go = false
    }}}
  }
  
  class Fanner(work:Array[Job], workers:Array[Kucher], N:Int) extends Actor {
    var next = 0
    var K = 0
    var done = 0
    def act() { Actor.loop { react {
      case msg : Found =>
        K = K max msg.k
        if (msg.k>=0) done += 1
        if (next < work.length) {
          msg.who ! work(next)
          next += 1
        }
        else if (done==work.length) {
          println("Pfannkuchen(" + N + ") = " + K)
          workers.foreach(_ ! Stop())
          exit()
        }
    }}}
  }
  
  def swapped(a:Array[Int],i:Int,j:Int) = {
    val b  = java.util.Arrays.copyOf(a,a.length)
    b(i) = a(j)
    b(j) = a(i)
    b
  }

  def main(args:Array[String]) {
    val N = args(0).toInt
    val base = (0 until N).toArray

    // Split full problem into subsets for parallelization by swapping
    // Nth entry into i<N spot and then finding permutations of first (N-1)
    // (Do it recursively down to length 8 to help with load balacing.)
    val work = new collection.mutable.ListBuffer[Job]()
    work += Job(base,N min 8)
    for (i <- N-1 to 8 by -1) {
      for (j <- 0 until i) {
        work += Job(swapped(base,i,j) , i)
      }
    }
    // Uncomment this to see which permutations are tested in each work block:
    // work.foreach( w => println("Permute first " + w.n + " of " + w.start.mkString(",")) )

    val workers = new Array[Kucher]( Runtime.getRuntime.availableProcessors );
    val foreman = new Fanner( work.toArray , workers , N );
    foreman.start()
    for (i <- 0 until workers.length) {
      workers(i) = new Kucher(foreman,N,i)
      workers(i).start
      foreman ! Found(workers(i),-1)
    }
  }
}
/* The Computer Language Benchmarks Game
   http://shootout.alioth.debian.org/

   conversion to Scala by Rex Kerr
   from Java version by Oleg Mazurov and Isaac Gouy
*/

object fannkuchredux
{
  def fannkuch(n: Int): Int = {
    val perm1 = Array.range(0,n)
    val perm, count = new Array[Int](n)
    var f, i, k, r, flips, nperm, checksum = 0

    r = n
    while (r>0) {
      i = 0
      while (r != 1) { count(r-1) = r; r -= 1 }
      while (i < n) { perm(i) = perm1(i); i += 1 }
      
      // Count flips and update max  and checksum
      f = 0
      k = perm(0)
      while (k != 0) {
        i = 0
        while (2*i < k) {
          val t = perm(i); perm(i) = perm(k-i); perm(k-i) = t
          i += 1
        }
        k = perm(0)
        f += 1
      }
      if (f>flips) flips = f
      if ((nperm&0x1)==0) checksum += f
      else checksum -= f

      // Use incremental change to generate another permutation
      var go = true
      while (go) {
        if (r == n) {
          println(checksum)
          return flips
        }
        val p0 = perm1(0)
        i = 0
        while (i < r) {
          val j = i+1
          perm1(i) = perm1(j)
          i = j
        }
        perm1(r) = p0

        count(r) -= 1
        if (count(r) > 0) go = false
        else r += 1
      }
      nperm += 1
    }
    flips
  }

  def main(args: Array[String]) {
    val n = (if (args.length > 0) args(0).toInt else 7)
    println("Pfannkuchen("+n+") = "+fannkuch(n))
  }
}
/*
 * The Computer Language Benchmarks Game
 * http://shootout.alioth.debian.org/
 *
 * Scala version contributed by Rex Kerr
 * translated from Java version by Oleg Mazurov, June 2010
 * 
 */

object fannkuchredux {
  def fac(x: Int): Long = if (x < 2) 1L else x*fac(x-1)
  val F = (0 to 20).map(fac).toArray
  var chunk = 0L
  var ntasks = 0
  val taskId = new java.util.concurrent.atomic.AtomicInteger(0)
    
  class Fannkuch(n: Int) extends Thread {
    val p, pp, count = new Array[Int](n)
    var flips, cksum = 0

    def direct(idx: Long, i: Int) {
       if (i > 0) {
        val d = (idx / F(i)).toInt
        count(i) = d
        var j = 0
        while (j < d) { pp(j) = p(j); j += 1 }
        j = 0
        while (j+d <= i) { p(j) = p(j+d); j += 1 }
        while (j <= i) { p(j) = pp(j+d-i-1); j += 1 }
        direct(idx%F(i), i-1)
      }
    }
      
    def permute() {
      var first = p(1)
      p(1) = p(0)
      p(0) = first
      var i = 1
      count(i) += 1
      while (count(i) > i ) {
        count(i) = 0
        i += 1
        p(0) = p(1)
        val next = p(1)
        var j = 1
        while (j < i) { p(j) = p(j+1); j += 1 }
        p(i) = first
        first = next
        count(i) += 1
      }
    }

    def fcount() = {
      var flips = 1
      var first = p(0)
      if (p(first) != 0) {
        var i = 0
        while (i < n) { pp(i) = p(i); i += 1 }
        do {
          flips += 1
          var lo = 1
          var hi = first -1
          while (lo < hi) {
            val t = pp(lo)
            pp(lo) = pp(hi)
            pp(hi) = t
            lo += 1
            hi -= 1
          }
          val t = pp(first)
          pp(first) = first
          first = t
        } while (pp(first) != 0);
      }
      flips
    }
    
    def runTask(task: Int) {
      val lo = task*chunk
      val hi = F(n) min (lo+chunk)
      var j = 0
      while (j < p.length) { p(j) = j; j += 1 }
      direct(lo,p.length-1)
      var i = lo
      while (true) {
        if (p(0) != 0) {
          val f = fcount
          flips = Math.max(flips,f)
          cksum += (if ((i%2)==0) f else -f)
        }
        i += 1
        if (i == hi) return
        permute
      }
    }
    
    override def run() { while (true) {
      val task = taskId.getAndIncrement()
      if (task >= ntasks) return
      runTask(task)
    }}
  }
    
  def announce(n: Int, f: Int, ck: Int) {
    printf("%d\nPfannkuchen(%d) = %d\n",ck,n,f)
  }
  
  def main(args: Array[String]) {
    val n = (if (args.length > 0) args(0).toInt else 7)
    if (n < 0 || n > 20) announce(n,-1,-1)
    else if (n <= 1) announce(n,0,0)
    else {
      val nthreads = Runtime.getRuntime.availableProcessors
      def split(i: Long) = (F(n)+i-1)/i
      chunk = split(nthreads*50)
      ntasks = split(chunk).toInt
      val threads = Array.range(0,nthreads).map(_ => new Fannkuch(n))
      threads.foreach(_.start)
      threads.foreach(_.join)
      announce(n, (0/:threads)(_ max _.flips), (0/:threads)(_ + _.cksum))
    }
  }
}

/* The Computer Language Shootout
  http://shootout.alioth.debian.org/
  contributed by Isaac Gouy
  updated for 2.8 and modified by Rex Kerr
*/

import java.io._

object fasta {
  val ALU =
    "GGCCGGGCGCGGTGGCTCACGCCTGTAATCCCAGCACTTTGG" +
    "GAGGCCGAGGCGGGCGGATCACCTGAGGTCAGGAGTTCGAGA" +
    "CCAGCCTGGCCAACATGGTGAAACCCCGTCTCTACTAAAAAT" +
    "ACAAAAATTAGCCGGGCGTGGTGGCGCGCGCCTGTAATCCCA" +
    "GCTACTCGGGAGGCTGAGGCAGGAGAATCGCTTGAACCCGGG" +
    "AGGCGGAGGTTGCAGTGAGCCGAGATCGCGCCACTGCACTCC" +
    "AGCCTGGGCGACAGAGCGAGACTCCGTCTCAAAAA"

  val IUB = (Array( ('a',0.27), ('c',0.12), ('g',0.12), ('t',0.27) ) ++
    "BDHKMNRSVWY".map(c => (c,0.02))
  ).scanLeft( (0:Byte,0.0) )( (l,r) => (r._1.toByte, l._2+r._2) ).tail

  val HomoSapiens = Array(
    ('a', 0.3029549426680),
    ('c', 0.1979883004921),
    ('g', 0.1975473066391),
    ('t', 0.3015094502008)
  ).scanLeft( (0:Byte,0.0) )( (l,r) => (r._1.toByte, l._2+r._2) ).tail

  def main(args: Array[String]) = {
    val n = args(0).toInt
    val s = new FastaOutputStream(System.out)

    s.writeDescription("ONE Homo sapiens alu")
    s.writeRepeatingSequence(ALU,n*2)

    s.writeDescription("TWO IUB ambiguity codes")
    s.writeRandomSequence(IUB,n*3)

    s.writeDescription("THREE Homo sapiens frequency")
    s.writeRandomSequence(HomoSapiens,n*5)

    s.close
  }
}


// extend the Java BufferedOutputStream class

class FastaOutputStream(out: OutputStream) extends BufferedOutputStream(out) {
  private val LineLength = 60
  private val nl = '\n'.toByte

  def writeDescription(desc: String) = { write( (">" + desc + "\n").getBytes ) }

  def writeRepeatingSequence(_alu: String, length: Int) = {
    val alu = _alu.getBytes
    var n = length; var k = 0; val kn = alu.length;

    while (n > 0) {
      val m = if (n < LineLength) n else LineLength

      var i = 0
      while (i < m){
        if (k == kn) k = 0
        val b = alu(k)
        if (count < buf.length){ buf(count) = b; count += 1 }
        else { write(b) } // flush buffer
        k += 1
        i += 1
      }

      write(nl)
      n -= LineLength
    }
  }

  def writeRandomSequence(distribution: Array[(Byte,Double)], length: Int) = {
    var n = length
    while (n > 0) {
      val m = if (n < LineLength) n else LineLength

      var i = 0
      while (i < m){
        val b = selectRandom(distribution)
        if (count < buf.length) { buf(count) = b; count += 1 }
        else { write(b) } // flush buffer
        i += 1
      }

      if (count < buf.length){ buf(count) = nl; count += 1 }
      else { write(nl) } // flush buffer
      n -= LineLength
    }
  }

  private def selectRandom(distribution: Array[(Byte,Double)]): Byte = {
    val n = distribution.length
    val r = RandomNumber scaledTo(1.0)

    var i = 0
    while (i < n) {
      if (r < distribution(i)._2) return distribution(i)._1
      i = i+1
    }
    return distribution(n-1)._1
  }
}


object RandomNumber {
  val IM = 139968
  val IA = 3877
  val IC = 29573
  private var seed = 42

  def scaledTo(max: Double) = {
    seed = (seed * IA + IC) % IM
    max * seed / IM
  }
}
/* The Computer Language Benchmarks Game
  http://shootout.alioth.debian.org/
  contributed by Rex Kerr
  based on Scala version by Isaac Gouy
  with optimization tricks from C version by Petr Prokhorenkov
*/

import java.io._

object fasta {
  val ALU =
    "GGCCGGGCGCGGTGGCTCACGCCTGTAATCCCAGCACTTTGG" +
    "GAGGCCGAGGCGGGCGGATCACCTGAGGTCAGGAGTTCGAGA" +
    "CCAGCCTGGCCAACATGGTGAAACCCCGTCTCTACTAAAAAT" +
    "ACAAAAATTAGCCGGGCGTGGTGGCGCGCGCCTGTAATCCCA" +
    "GCTACTCGGGAGGCTGAGGCAGGAGAATCGCTTGAACCCGGG" +
    "AGGCGGAGGTTGCAGTGAGCCGAGATCGCGCCACTGCACTCC" +
    "AGCCTGGGCGACAGAGCGAGACTCCGTCTCAAAAA"

  val IUBs = "acgtBDHKMNRSVWY"
  val IUBp = (
    Array(0.27,0.12,0.12,0.27) ++ Array.fill(11)(0.02)
  ).scanLeft(0.0)(_ + _).tail

  val HSs = "acgt"
  val HSp = Array(
    0.3029549426680, 0.1979883004921, 0.1975473066391, 0.3015094502008
  ).scanLeft(0.0)(_ + _).tail

  def main(args: Array[String]) = {
    val n = args(0).toInt
    val s = new FastaOutputStream(System.out)

    s.writeRepeating(ALU, n*2, "ONE Homo sapiens alu")
    s.writeRandom(IUBs, IUBp, n*3, "TWO IUB ambiguity codes")
    s.writeRandom(HSs, HSp, n*5, "THREE Homo sapiens frequency")

    s.close
  }
}


// extend the Java BufferedOutputStream class
class FastaOutputStream(out: OutputStream) extends BufferedOutputStream(out) {
  private val TableSize = 4096
  private val N = 60
  private val chunk = new Array[Byte](N+1)
  private val nl = '\n'.toByte

  // Tail-recursive; can check by prepending @annotation.tailrec
  private def writeRep(seq: Array[Byte], n: Int, off: Int = 0) {
    if (n > N) {
      val remains = seq.length - off
      // Assume seq.length >= N!
      if (remains>=N) {
        write(seq,off,N); write(nl)
        writeRep(seq, n-N, if (remains==N) 0 else off+N)
      }
      else {
        write(seq,off,remains); write(seq,0,N-remains); write(nl)
        writeRep(seq, n-N, 0+N-remains)
      }
    }
    else {
      for (i <- 0 until n) write(seq((i+off)%seq.length))
      write(nl)
    }
  }
  def writeRepeating(seq: String, n: Int, desc: String) {
    write( (">"+desc+"\n").getBytes )
    writeRep(seq.getBytes,n)
  }

  // Tail-recursive (check with @annotation.tailrec)
  private def writeRand(tab: Table, n: Int) {
    val m = if (n < N) { chunk(n) = nl; n } else N
    var i = 0
    while (i<m) {
      chunk(i) = tab.next
      i += 1
    }
    write(chunk,0,m+1)
    if (n > N) writeRand(tab, n-N)
  }
  def writeRandom(seq: String, dist: Array[Double], n: Int, desc: String) {
    write( (">"+desc+"\n").getBytes )
    chunk(N) = nl
    val tab = new Table(seq.getBytes, dist, TableSize)
    writeRand(tab,n)
  }

  // Constant time lookup table, assumes (1/size) < p(rarest entry)
  private class Table(bytes: Array[Byte], dist: Array[Double], size: Int) {
    abstract class X { def pick(d: Double): Byte }
    class B(b: Byte) extends X { def pick(d: Double) = b }
    class P(b0 : Byte, p: Double, b1: Byte) extends X {
      def pick(d: Double) = if (d < p) b0 else b1
    }

    def seek(p: Double): Int = {
      var i = 0
      while (i+1<dist.length && p >= dist(i)) i += 1
      i
    }
    var lastj = -1
    var lastX = null:X
    val lookup: Array[X] = (0 until size).map(i => {
      val (j0,j1) = (seek(i.toDouble/size), seek((i+1).toDouble/size))
      if (j0==j1) {
        if (lastj==j0) lastX
        else {
          lastX = new B(bytes(j0))
          lastj = j0
          lastX
        }
      }
      else {
        lastj = -1
        new P(bytes(j0),dist(j0),bytes(j1))
      }
    }).toArray
    
    def next = {
      val p = RandomNumber.next
      lookup((p*size).toInt).pick(p)
    }
  }

  private object RandomNumber {
    val (im,ia,ic) = (139968,3877,29573)
    val scale = 1.0/im
    var seed = 42
    def next = { seed = (seed * ia + ic) % im; seed*scale }
  }
}
/* The Computer Language Shootout 
   http://shootout.alioth.debian.org/
   contributed by Isaac Gouy (Scala novice)
*/

object fibo {
   def main(args: Array[String]) = 
      Console.println( fib( toPositiveInt(args) ));


   def fib(n: Int): Int = 
      if (n < 2) 1; else fib(n-2) + fib(n-1);


   private def toPositiveInt(s: Array[String]) = {
      val i = 
         try { Integer.parseInt(s(0)); } 
         catch { case _ => 1 }
      if (i>0) i; else 1;
   }
}



/* ------------------------------------------------------------------ */
/* The Great Computer Language Shootout                               */
/* http://shootout.alioth.debian.org/                                 */
/*                                                                    */
/* Contributed by Anthony Borla                                       */
/* ------------------------------------------------------------------ */

import java.text.DecimalFormat;
import java.text.FieldPosition;

object harmonic
{
  def main(args: Array[String]): unit =
  {
    var n = Integer.parseInt(args(0));

    var value = harmonic(n, 0.0);

    val formatter = new DecimalFormat("#.000000000");
    var formattedValue = formatter.format(value, new StringBuffer(), new FieldPosition(0));

    System.out.println(formattedValue);
  }

  final def harmonic(n: int, a: double): double =
  {
    if (n == 0) return a;
    return harmonic(n - 1, a + 1.0 / n);
  }
}

/* The Computer Language Shootout
   http://shootout.alioth.debian.org/
   contributed by Isaac Gouy (Scala novice)
*/

object harmonic {
   def main(args: Array[String]) = {
      val n = Integer.parseInt(args(0));
      var partialSum = 0.0;

      for (val i <- Iterator.range(1,n+1)) partialSum = partialSum + 1.0/i;
      Console.printf("{0,number,#.000000000}\n")(partialSum);
   }
}
/* The Computer Language Shootout
   http://shootout.alioth.debian.org/
   contributed by Isaac Gouy (Scala novice)
*/

object harmonic {
   def main(args: Array[String]) = {
      val n = Integer.parseInt(args(0));
      var partialSum = 0.0;
      var i = 1;

      while (i < n){ partialSum = partialSum + 1.0/i; i = i + 1; }
      Console.printf("{0,number,#.000000000}\n")(partialSum);
   }
}
/* The Computer Language Shootout 
   http://shootout.alioth.debian.org/
   contributed by Isaac Gouy (Scala novice)
*/

import scala.collection.mutable.HashMap;

object hash {
   def main(args: Array[String]) = {
      val n = toPositiveInt(args);
      var count = 0;
      val table = new HashMap[String,Int]();

      for (val i <- Iterator.range(1,n+1)) 
         table += Integer.toString(i, 16) -> i;

      for (val i <- Iterator.range(1,n+1)) 
         if (table contains Integer.toString(i, 10)) 
            count = count + 1;

      Console println(count);
   }


   private def toPositiveInt(s: Array[String]) = {
      val i = 
        try { Integer.parseInt(s(0)); } 
        catch { case e: Exception => 1 }

      if (i>0) i; else 1;
   }

}



/* The Computer Language Shootout 
   http://shootout.alioth.debian.org/
   contributed by Isaac Gouy (Scala novice)
*/

import scala.collection.mutable.HashMap;

object hash2 {
   def main(args: Array[String]) = {

      def printValue[A,B](table: HashMap[A,Cell[B]], key: A) = 
         table get(key) match {
            case Some(c) => Console print(c value);
            case None => Console print(None)
      }

      var n = toPositiveInt(args);
      var nKeys = 10000;

      val table1 = new HashMap[String,Cell[Int]]();
      val table2 = new HashMap[String,Cell[Int]]();

      for (val i <- Iterator.range(0,nKeys)) 
         table1 += ("foo_" + i) -> new Cell(i);


      while (n>0) {
         for (val each <- table1.elements){
            val key = each._1;
            val c1 = each._2;

            table2 get(key) match {
               case Some(c2) => 
                  c2.value = c2.value + c1.value;
               case None => 
                  table2 += key -> new Cell(c1.value);
            }
         }
         n = n-1;
      }

      printValue(table1,"foo_1");    Console print(" ");
      printValue(table1,"foo_9999"); Console print(" ");
      printValue(table2,"foo_1");    Console print(" ");
      printValue(table2,"foo_9999"); Console print("\n");

   }


   private def toPositiveInt(s: Array[String]) = {
      val i = 
         try { Integer.parseInt(s(0)); } 
         catch { case _ => 1 }
      if (i>0) i; else 1;
   }
}


private class Cell[T](v: T) extends Object { 
   var value: T = v; 
}


/* The Computer Language Shootout 
   http://shootout.alioth.debian.org/
   contributed by Isaac Gouy (Scala novice)
*/

object heapsort {
   def main(args: Array[String]) = {
      val n = toPositiveInt(args);

      val numbers = new Array[Double](n+1);
      for (val i <- Iterator.range(1,n+1)) 
         numbers(i) = generate(100.0);

      heapsort(n, numbers);

      Console.printf("{0,number,#.000000000}\n")(numbers(n));   
   }


   def heapsort(n: Int, ra: Array[Double]): Unit = {
      var l = 0; var j = 0; var ir = 0; var i = 0; 
      var rra = 0.0d;

      if (n < 2) return;
      l = (n >> 1) + 1;
      ir = n;
      while (true) {
         if (l > 1) { l = l-1; rra = ra(l); }
         else {
            rra = ra(ir);
            ra(ir) = ra(1);
            ir = ir-1;
            if (ir == 1) {
               ra(1) = rra;
               return;
            }
         }
         i = l;
         j = l << 1;
         while (j <= ir) {
            if (j < ir && ra(j) < ra(j+1)) { j = j+1; }
            if (rra < ra(j)) {
               ra(i) = ra(j);
               i = j;
               j = j + i;
            } 
            else j = ir + 1;
         }
         ra(i) = rra;
      }
   }


   private val IM = 139968;
   private val IA = 3877;
   private val IC = 29573;
   private var seed = 42;

   private def generate(max: Double) = {
      seed = (seed * IA + IC) % IM;
      max * seed / IM;
   }


   private def toPositiveInt(s: Array[String]) = {
      val i = 
         try { Integer.parseInt(s(0)); } 
         catch { case _ => 1 }
      if (i>0) i; else 1;
   }

}
/* The Computer Language Shootout
   http://shootout.alioth.debian.org/
   contributed by Isaac Gouy (Scala novice)
   modified for Scala 2.x by Anthony Borla
*/

object hello extends Application {
  Console.println("hello world")
}

/*
   The Computer Language Benchmarks Game
   http://shootout.alioth.debian.org/
   Based partially on the single-threaded version by Isaac Gouy with fixes from
   Iulian Dragos and Meiko Rachimow.
   Based on Scala contribution of Rex Kerr
   Based on bit encoding idea of C++ contribution of Andrew Moon
   Contributed by The Anh Tran
   Updated for 2.8 by Rex Kerr
*/

import scala.actors.Futures.future
import scala.actors.Future
import scala.collection.mutable.HashMap
import java.io._

final
object knucleotide
{
   def main(args : Array[String]) =
   {
      // read all data from inputstream
      val data = Helper.readAll

      // hand out tasks, each task will be picked up by scala threadpool
      val tables = List(1, 2, 3, 4, 6, 12, 18)
         .map( sz => queueBuildHashtable(data, sz) )
         .splitAt(2)

      // print frequency of each nucleotide
      tables._1.foreach(printFreq(_, data.length))

      // print how many times a specific pattern appears in input data
      val codeSeq = List("GGT", "GGTA", "GGTATT", "GGTATTTTAATT", "GGTATTTTAATTTATAGT");
      (tables._2 zip codeSeq).foreach( e => printFreq(e._1, e._2) )
   }

   private
   def printFreq(lht: List[Future[KnuHashMap]], data_length : Int) =
   {
      // have to merge (whole list of results) into a hashmap
      val ht = mergeTables(lht)

      // sort by decending frequencies
      val sorted_list = ht.toArray.sortWith((a, b) => !(a._2 < b._2) )

      val total = data_length.toFloat

      sorted_list foreach ( a =>   printf("%s %.3f\n",
                              a._1.decode,
                              (a._2.toFloat * 100.0f / total))   )

      println
   }

   private
   def printFreq(lht : List[Future[KnuHashMap]], pt : String) =
   {
      val ht = mergeTables(lht)

      val k = new KnuKey(pt.length)
      k.encode(pt.toCharArray, 0)
      assert(pt == k.decode)

      val v = ht.getOrElse(k, 0)
      printf("%d\t%s\n", v, pt)
   }

   private
   def queueBuildHashtable(data : Array[Char], frameSize : Int) =
   {
      // each task will hash whole input data, at a specific offset
      // with (0 <= offset < frameSize)
      (0 until frameSize)
         .map( offset => future(buildHashtable(data, frameSize, offset)) )
         .toList
   }

   private
   def buildHashtable(data : Array[Char], frameSize : Int, startOffset : Int) : KnuHashMap =
   {
      val ht = new KnuHashMap
      val key = new KnuKey(frameSize)

      val i_end = data.length - frameSize + 1
      var i_beg = startOffset

      while (i_beg < i_end)
      {
         key.encode(data, i_beg)
         ht.incCounter(key)

         i_beg += frameSize
      }

      ht
   }

   private
   def mergeTables(list_hashtable: List[Future[KnuHashMap]]) =
      list_hashtable
         .map( _() )
         .reduceLeft( (t1, t2) => {
            t2.foreach(e => t1.addCounter(e._1, e._2))
            t1
         })
}

private final
class KnuKey(var key : Long, val hash_length : Int)
{
   def this(hlen: Int) = this(0, hlen)

   @inline
   override def clone() = new KnuKey(key, hash_length)

   @inline
   override def hashCode() : Int = key.toInt

   @inline
   override def equals(other : Any) = other match {
      case that: KnuKey => this.key == that.key
      case _            => false
   }

   @inline
   def encode(data : Array[Char], offset : Int) = {
      var mkey = 0L
      var index = 0
      var shift = 0

      while (index < hash_length)
      {
         mkey |= (Helper(data(offset + index)) << shift)

         shift += Helper.bit_per_code
         index += 1
      }

      key = mkey
      this
   }

   def decode() = {
      val sb = new StringBuilder(hash_length)

      var index = 0
      var extract_mask = Helper.bit_mask

      while (index < hash_length)
      {
         val extract_value = ((key & extract_mask) >>> (index * Helper.bit_per_code)).toInt
         sb append( Helper(extract_value) )

         extract_mask <<= Helper.bit_per_code
         index += 1
      }

      sb toString
   }
}


private final
object Helper {
   val bit_mask      = 3L
   val bit_per_code   = 2

   @inline
   def apply(c : Char) : Long = (c: @annotation.switch) match {
      case 'a'   => 0
      case 't'   => 1
      case 'c'   => 2
      case 'g'   => 3

      case 'A'   => 0
      case 'T'   => 1
      case 'C'   => 2
      case 'G'   => 3

      case _      => assert(false); -1
   }

   private
   val Int2Iub = Array('A', 'T', 'C', 'G')

   @inline
   def apply(c : Int) : Char = Int2Iub(c)


   def readAll() = {
      val reader = new BufferedReader(new InputStreamReader (System.in, "US-ASCII"), 4*1024*1024)

      var line = reader readLine()
      while ((line != null) && ((line(0) != '>') || (line.startsWith(">THREE") == false)))
         line = reader readLine

      val sb = new StringBuilder(32*1024*1024)

      line = reader readLine()
      while (line != null)
      {
         sb append line
         line = reader readLine
      }

      // Read a char by "String.charAt(index)" is much slower than "Array(index)"
      // => use Array[Char]
      sb.toString.toCharArray
   }
}


private final
class KnuHashMap extends HashMap[KnuKey, Int]
{
   @inline
   def incCounter(key : KnuKey) : Unit = addCounter(key, 1)

   @inline
   def addCounter(key : KnuKey, valToAdd: Int) {
      // directly look up entry inside hashtable
      var e  = table(index(key.hashCode)).asInstanceOf[Entry]
      while (e != null)
      {
         if (e.key == key)
         {
            e.value += valToAdd
            return
         }
         else
            e = e.next
      }

      addEntry(new Entry(key.clone, valToAdd))
   }
}
/* 
 * The Computer Language Benchmarks Game
 * http://shootout.alioth.debian.org/
 * contributed by Rex Kerr
 * (based partially on the single-threaded version by Isaac Gouy
 * with fixes from Iulian Dragos and Meiko Rachimow)
 */

import java.io._
import scala.collection.mutable._
import scala.actors._
import Actor.loop

object knucleotide {
  class ShiftScan(offset:Int, stride:Int) extends Actor {
    val bag = new HashBag
    def findFreq(s:String, j:Int) = {
      val n = s.length - j + 1
      var i = offset
      while (i < n) {
        bag(s.substring(i,i+j)).inc
        i += j
      }
      bag
    }
    def act() { loop { react {
      case seq : String =>
        sender ! findFreq(seq,stride)
        exit
    }}}
  }

  class SizeScan(length:Int, boss:Actor) extends Actor {
    var bags = List[HashBag]()
    val workers = (0 until length).map(new ShiftScan(_,length))
    def act() { loop { react {
      case b : HashBag =>
        bags = b :: bags
        if (bags.length==workers.length) {
          boss ! (bags.head /: bags.tail)(_ ++ _)
          exit
        }
      case seq : String =>
        workers.foreach(_.start ! seq)
    }}}
  }

  class Scan(work : String*) extends Actor {
    val lengths = work.map(_.length)
    val freqs = work.filter(_(0)=='*').map(_.length)
    val counts = work.filter(_(0)!='*')
    val scans = new HashMap[Int,HashBag]()
    var seq_len = 0
     
    def writeFreqs(j : Int) {
      val n  = 1.0 + seq_len - j
      val sorted = scans(j).elements.toList.sort(
        (a,b) => (a.value>b.value) || (a.value==b.value && a.key > b.key)
      )
      sorted.foreach(a => printf(a.key + " %.3f\n", a.value * 100.0 / n))
      println
    }
     
    def act() { loop { react {
      case seq : String =>
        seq_len = seq.length
        lengths.foreach(l => new SizeScan(l,this).start() ! seq)
      case h : HashBag =>
        val n = h.elements.next.key.length
        scans(n) = h
        if (lengths.forall(scans contains _)) {
          freqs.foreach(writeFreqs(_))
          counts.foreach(s => println(scans(s.length)(s).value + "\t" + s))
          exit
        }
    }}}
  }

  def readSequence(id: String, r: BufferedReader) = {
    val b = new java.lang.StringBuilder()
     
    var line = r.readLine
    while (line != null) {
      if (line(0)=='>' && line.startsWith(id)) line = null
      else line = r.readLine
    }
     
    line = r.readLine
    while (line != null) {
      if (line(0)=='>') line = null
      else {
        b.append(line.toUpperCase)
        line = r.readLine
      }
    }
    b.toString
  }
  
  def main(args: Array[String]) {
    val r = new BufferedReader(new InputStreamReader(System.in))
    val seq = readSequence(">THREE",r)
    r.close
      
    val scanner = new Scan("*","**","GGT","GGTA","GGTATT",
                           "GGTATTTTAATT","GGTATTTTAATTTATAGT")
    scanner.start() ! seq
  }
}

class HashBag extends HashTable[String] {
  class Counter(val key: String, var value: Int)
        extends HashEntry[String,Counter]
  {
    def inc { value += 1 }
    def +=(i:Int) { value += i }
  }

  protected type Entry = Counter
  protected def entryKey(e: Entry) = e.key
  def elements = entries
   
  def apply(key:String): Counter = {
    var bucket = table(index(elemHashCode(key))).asInstanceOf[Entry]
    while (bucket ne null) {
      if (elemEquals(entryKey(bucket),key)) return bucket
      bucket = bucket.next
    }
    bucket = new Entry(key,0)
    addEntry(bucket)
    bucket
  }
     
  def ++(h : HashBag) : HashBag = {
    h.elements.foreach(kv => this(kv.key) += kv.value)
    this
  }
}
/* The Computer Language Benchmarks Game
   http://shootout.alioth.debian.org/
   
   Contributed by Rex Kerr
   (inspired by the C++ version by Andrew Moon)
*/

import java.io._
import actors.Futures._

object knucleotide {
  val BlockSize = 1024*1024
  
  // Iterators are not specialized so we need our own
  abstract class LongIter {
    def hasNext: Boolean
    def next: Long
    def foreach(f: Long => Unit) { while (hasNext) f(next) }
  }
  
  val table = Array.tabulate[Byte](256) {
    case 'a' | 'A' => 0
    case 't' | 'T' => 1
    case 'g' | 'G' => 2
    case 'c' | 'C' => 3
    case '\n' => -3
    case '>' => -2
    case _ => -1
  }

  // More efficient to store DNA sequence data as bits instead of bytes
  class Bits(val data: Array[Int]) {
    self =>
    var size = 0
    var index = 0
    var n = 0
    
    def add2(b: Byte) {
      size += 1
      if (n>30) { index += 1; n = 0 }
      data(index) |= (b&0x3)<<n
      n += 2
    }
    
    def addLots(bs: Bits) {
      if (n==0 || n>30) {
        if (n>30) { index += 1; n = 0 }
        System.arraycopy(bs.data,0,data,index,bs.index)
        index += bs.index
        if (index > 0 && n == 0) { index -= 1; n = 32 }
      }
      else {
        var i = 0
        while (i < bs.index) {
          val j = bs.data(i)
          data(index) |= j << n
          index += 1
          data(index) |= j >>> (32-n)
          i += 1
        }
        size
      }
      size += bs.index*16
      if (bs.n != 0) {
        var n = bs.n
        var i = bs.data(bs.index)
        while (n > 0) {
          add2( i.toByte )
          i >>>= 2
          n -= 2
        }
      }
    }
    
    def scan(n: Int, offset: Int) = new LongIter {
      var i = offset % 16
      var j = offset / 16
      val mask = (1L << (2*n)) - 1
      def hasNext = j*16 + i + n <= self.size
      def next = {
        if (i+n <= 16) {
          val l = ((data(j) >>> (2*i)) & mask)
          i += n
          if (i>=16) { j += 1; i -= 16 }
          l
        }
        else {
          val l = (((data(j) >>> (2*i))).toLong | (data(j+1).toLong << 2*(16-i))) & mask
          j += 1
          i += n - 16
          if (i>=16) { j += 1; i -= 16 }
          l
        }
      }
    }
  }
  
  // Load a UTF-8 DNA file from standard in, picking out requested sequence
  def load(is: InputStream, target: Array[Byte]) = {
    var need = 1
    var found,nl,done = false
    def read: Bits = {
      val data = new Array[Byte](BlockSize)
      val n = is.read(data)
      var i = 0
      while (i<n && need<target.length) {
        if (data(i)==target(need)) need += 1 else need = 0
        i += 1
      }
      if (need >= target.length && !found) {
        while (i<n && data(i)!='\n') i += 1
        if (i<n) found = true
      }
      if (found && !done)
      {
        val bits = new Bits(new Array[Int](1+((n-i)>>4)))
        while (i < n) {
          val x = table(data(i)&0xFF)
          if (x >= 0) { bits.add2(x); nl = false }
          else if (x == -3) nl = true
          else if (nl && x == -2) { i = n; done = true }
          i += 1
        }
        bits
      }
      else if (n==BlockSize && !done) read
      else new Bits(new Array[Int](0))
    }
    val data = Iterator.continually(read).takeWhile(_.size > 0).toArray
    val all = new Bits(new Array[Int](data.map(_.size).sum/16+1))
    data.foreach(all.addLots)
    all
  }
  
  // Utility to go from binary to text representation
  val decode = Map(0L->"A", 1L->"T", 2L->"G", 3L->"C")
  def l2s(l: Long, n: Int): String = {
    if (n <= 0) ""
    else decode(l&0x3) + l2s(l>>>2, n-1)
  }
    
  // Custom counted hash set (neither Java nor Scala provides one)
  class DnaHash(z: Int) {
    var size = 16
    var n = 0
    var keys = new Array[Long](size)
    var counts = new Array[Int](size)
    final def hc(l: Long) = (l.toInt + (l>>17).toInt) & (size-1)
    final def nx(i: Int) = (i+1) & (size - 1)
    def +=(key: Long, count: Int = 1) {
      val index = hc(key)
      if (counts(index) == 0) {
        keys(index) = key
        counts(index) = count
        n += 1
      }
      else if (keys(index) == key) counts(index) += count
      else if (6*n > size) {
        val (oldk, oldc, olds) = (keys, counts, size)
        size *= 2
        keys = new Array[Long](size)
        counts = new Array[Int](size)
        n = 0
        var i = 0
        while (i < olds) {
          if (oldc(i) > 0) this += (oldk(i), oldc(i))
          i += 1
        }
        this += key
      }
      else {
        var i = nx(index)
        while (counts(i) != 0 && keys(i) != key) i = nx(i)
        if (counts(i) == 0) {
          keys(i) = key
          counts(i) = count
          n += 1
        }
        else counts(i) += count
      }
    }
    def apply(key: Long) = {
      var i = hc(key)
      while (counts(i) > 0 && keys(i) != key) i = nx(i)
      counts(i)
    }
    def printSorted {
      val factor = 100.0/counts.sum
      (counts.map(_*factor) zip keys.map(l2s(_,z))).filter(_._1 > 0).sortWith((a,b) =>
        a._1 > b._1 || (a._1 == b._1 && a._2 < b._2)
      ).foreach{ case (freq, label) => printf("%s %.3f\n",label,freq) }
      println
    }
    def print(s: String) {
      val key = s.getBytes.map(x => table(x & 0xFF).toLong).reduceRight((l,r) => 4*r + l)
      printf("%-7d %s\n",this(key),s)
    }
  }
  
  // Required function that adds data with offset to hash set
  def addToHash(data: Bits, hash: DnaHash, n: Int, offset: Int) = data.scan(n,offset).foreach(hash += _)
  
  def main(args: Array[String]) {
    val sizes = List(1,2,3,4,6,12,18)
    val sequence = "GGTATTTTAATTTATAGT"
    val data = load(System.in, "\n>THREE".getBytes)
    val answers = sizes.map(n => n -> future {
      val h = new DnaHash(n)
      for (i <- 0 until n) addToHash(data,h,n,i)
      h
    }).toMap
    answers(1)().printSorted
    answers(2)().printSorted
    sizes.drop(2).foreach(n => answers(n)().print(sequence.substring(0,n)))
  }
}
/* The Computer Language Shootout 
   http://shootout.alioth.debian.org/
   contributed by Isaac Gouy (Scala novice)
*/

import collection.mutable.ListBuffer;

object lists {
   def main(args: Array[String]) = {
      var n = toPositiveInt(args);
      val nSize = 10;
      var L1Count = 0;
      var i = 0;

      while (n > 0) {
         var L1 = new ListBuffer[Int]();
         i=0; while (i < nSize){ L1 + i; i=i+1; } 
         var L2 = L1.clone().asInstanceOf[ListBuffer[Int]];
         var L3 = new ListBuffer[Int]();
         while (L2.length > 0) L3 + L2.remove(0); 
         i = L3.length;
         while (i > 0){ i=i-1; L2 + L3.remove(i); }

         Console println(L2 length);
         n = n - 1;
      }
   }

   private def toPositiveInt(s: Array[String]) = {
      val i = 
         try { Integer.parseInt(s(0)); } 
         catch { case _ => 1 }
      if (i>0) i; else 1;
   }
}
/* The Computer Language Benchmarks Game
   http://shootout.alioth.debian.org/
   original contributed by Isaac Gouy
   made to use single array and parallelized by Stephen Marsh
   converted to Scala 2.8 by Rex Kerr
*/

import java.io.BufferedOutputStream

object mandelbrot {
  var size: Int = 0
  var bytesPerRow: Int = 0
  var bitmap: Array[Byte] = _
  var donerows: Array[Boolean] = _
  var nextRow = 0
  val limitSquared = 4.0
  val max = 50

  def getNextRow: Int = synchronized {
    notify() // wakes up main thread
    if (nextRow == size) return -1
    nextRow += 1
    return nextRow - 1
  }

  def main(args: Array[String]) {
    size = args(0).toInt
    bytesPerRow = (size+7)/8 // ceiling of (size / 8)
    bitmap = new Array(bytesPerRow*size)
    donerows = new Array(size)

    for (i <- 0 until Runtime.getRuntime().availableProcessors())
      new Thread(new CalcThread()).start()

    // main thread prints rows as they finish
    println("P4\n" + size + " " + size)
    val w = new BufferedOutputStream(System.out)
    var y = 0
    while (y < size) {
      while (!donerows(y)) synchronized{wait()}
      w.write(bitmap, y * bytesPerRow, bytesPerRow)
      y += 1
    }
    w.close
  }

  class CalcThread extends Runnable {
    def run () {
      while (true) {
	var y = getNextRow
	if (y == -1) return

	var bits = 0
	var bitnum = 0
	var x = 0
	var aindex = y * bytesPerRow

	while (x < size) {

        val cr = 2.0 * x / size - 1.5
        val ci = 2.0 * y / size - 1.0

        var zr, tr, zi, ti = 0.0

        var j = max
            do {
              zi = 2.0 * zr * zi + ci
              zr = tr - ti + cr
              ti = zi*zi
              tr = zr*zr

              j = j - 1
            } while (!(tr + ti > limitSquared) && j > 0)

         bits = bits << 1
         if (!(tr + ti > limitSquared)) bits += 1
         bitnum += 1

         if (x == size - 1) {
           bits = bits << (8 - bitnum)
           bitnum = 8
         }

         if (bitnum == 8) {
           bitmap(aindex) = bits.toByte
	   aindex += 1
           bits = 0
           bitnum = 0
         }

         x += 1
	}
	donerows(y) = true
      }
    }
  }
}
/* The Computer Language Shootout
   http://shootout.alioth.debian.org/
   contributed by Isaac Gouy
*/

import java.io.BufferedOutputStream

object mandelbrot { 
   def main(args: Array[String]) = {
      val side = Integer.parseInt(args(0))
      val limitSquared = 4.0
      val max = 50
      var bits = 0
      var bitnum = 0
      val w = new BufferedOutputStream(System.out)

      Console.println("P4\n" + side + " " + side)

      var y = 0
      while (y < side){

         var x = 0
         while (x < side){

            val cr = 2.0 * x / side - 1.5
            val ci = 2.0 * y / side - 1.0

            var zr = 0.0; var zi = 0.0
            var tr = 0.0; var ti = 0.0

            var j = max
            do {
               zi = 2.0 * zr * zi + ci
               zr = tr - ti + cr
               ti = zi*zi
               tr = zr*zr

               j = j - 1
            } while (!(tr + ti > limitSquared) && j > 0)


            bits = bits << 1
            if (!(tr + ti > limitSquared)) bits = bits + 1
            bitnum = bitnum + 1

            if (x == side - 1){
               bits = bits << (8 - bitnum)
               bitnum = 8
            }

            if (bitnum == 8){
               w.write(bits.toByte)
               bits = 0
               bitnum = 0
            }

            x = x + 1
         }
         y = y + 1
      }
      w.close
   } 
}
/* The Computer Language Benchmarks Game
 * http://shootout.alioth.debian.org/
 * original contributed by Kenneth Jonsson
 */

import scala.actors.Actor
import scala.actors.Actor._

class Worker(size: Int) extends Actor {
    private val bytesPerRow = (size + 7) >> 3
    private val maxIterations = 50
    private val limitSquared = 4.0

    // Calculate all pixes for one row [-i..i], the real-part
    // coordinate is constant throughout this method
    private def calcRow(rowNum: Int): (Actor, Int, Array[Byte]) = {
	var rowBitmap = new Array[Byte](bytesPerRow)
	var column = 0
	val ci = 2.0 * rowNum / size - 1.0

	while (column < size) {
	    val cr = 2.0 * column / size - 1.5
	    var zr, tr, zi, ti = 0.0
            var iterations = 0

            do {
		zi = 2 * zr * zi + ci
		zr = tr - ti + cr
		ti = zi * zi
		tr = zr * zr
		iterations += 1
            } while (tr + ti <= limitSquared && iterations < maxIterations)

	    if (tr + ti <= limitSquared)
		rowBitmap(column >> 3) = (rowBitmap(column >> 3)
					  | (0x80 >> (column & 7))).toByte

            column += 1
	}
	return (self, rowNum, rowBitmap)
    }

    def act() {
	while (true) {
	    receive {
		case rowNum: Int =>
		    reply(calcRow(rowNum))
		case "EXIT" =>
		    exit()
	    }
	}
    }
}

class MandelbrotCoordinator(size: Int) extends Actor {

    private var nextRowNum = 0
    private var rowsRemaining = size
    private var bitmap = new Array[Array[Byte]](size)

    private def calcNextRow(worker: Actor) {
	if (nextRowNum == size)
	    // All rows has been dispatched, tell the worker to exit
	    worker ! "EXIT"
	else {
	    worker ! nextRowNum
	    nextRowNum += 1
	}
    }

    def act() {
	for (i <- 1 to Runtime.getRuntime().availableProcessors()) {
	    val worker = new Worker(size)
	    // Keep two rows in flight per worker to avoid any worker
	    // idle time, probably not neccessary on a quad-core
	    // machine but might help at higher core count...
	    calcNextRow(worker)
	    calcNextRow(worker)
	    worker.start
	}

	while (true) {
	    receive {
		case (sender: Actor, rowNum: Int, rowBitmap: Array[Byte]) =>
		    calcNextRow(sender)
		    bitmap(rowNum) = rowBitmap
		    rowsRemaining -= 1
		    if (rowsRemaining == 0) {
			// The image is finished, write it to stdout and exit
			println("P4\n" + size + " " + size)
			bitmap.foreach(row => System.out.write(row, 0, row.length))
			exit()
		    }
	    }
	}
    }
}

object mandelbrot {
    def main(args: Array[String]) {
	val coordinator = new MandelbrotCoordinator(args(0).toInt)
	coordinator.start
    }
}
/* The Computer Language Shootout
   http://shootout.alioth.debian.org/
   contributed by Isaac Gouy
*/


import scala.concurrent._ 

object message {
   def main(args: Array[String]) = {
      val n = Integer.parseInt(args(0))
      val nActors = 500
      val finalSum = n * nActors

      case class Message(value: Int)

      class Incrementor(next: Pid) extends Actor {
         var sum = 0

         override def run() = {
            while (true) {
               receive { 
                  case Message(value) => 
                     val j = value + 1 
                     if (null != next){ 
                        next ! Message(j) 
                     } else { 
                        sum = sum + j
                        if (sum >= finalSum){ 
                           Console.println(sum); 
                           System.exit(0) // exit without cleaning up
                        }
                     } 
               }
            }
         }

         def pid() = { this.start; this.self }
      }

      def actorChain(i: Int, a: Pid): Pid = 
         if (i > 0) actorChain(i-1, new Incrementor(a).pid ) else a

      val firstActor = actorChain(nActors, null)
      var i = n; while (i > 0){ firstActor ! Message(0); i = i-1 }
   }
}
/* The Computer Language Shootout
   http://shootout.alioth.debian.org/
   contributed by Philipp Haller
*/

import scala.actors._; import scala.actors.Actor._

object message {
  def main(args: Array[String]) = {
    val n = Integer.parseInt(args(0)); val nActors = 500; val finalSum = n * nActors
    Scheduler.impl = new SingleThreadedScheduler

    def beh(next: Actor, sum: int): unit =
      react {
        case value: int =>
          val j = value + 1; val nsum = sum + j
          if (next == null && nsum >= finalSum) {
            Console.println(nsum)
            System.exit(0)
          }
          else {
            if (next != null) next ! j
            beh(next, nsum)
          }
      }

    def actorChain(i: Int, a: Actor): Actor =
      if (i > 0) actorChain(i-1, actor(beh(a, 0))) else a

    val firstActor = actorChain(nActors, null)
    var i = n; while (i > 0) { firstActor ! 0; i = i-1 }
  }
}
/* The Computer Language Shootout
   http://shootout.alioth.debian.org/
   contributed by Isaac Gouy
   updated for 2.8 by Rex Kerr
*/

// Most for-comprehension replaced by while loops
// BoardCells occupied by each Piece orientation are cached
// Piece orientations are cached

import scala.collection.mutable._

object meteor {
   def main(args: Array[String]) = {
      val solver = new Solver( args(0).toInt )
      solver.findSolutions
      solver.printSolutions
   }
}


final class Solver (n: Int) {
   private var countdown = n
   private var first: String = _
   private var last: String = _

   private val board = new Board()

   val pieces = Array.tabulate(10)(i => new Piece(i))

   val unplaced = new BitSet(pieces.length)

   { unplaced ++= (0 until pieces.length) }


   def findSolutions(): Unit = {
      if (countdown == 0) return

      if (unplaced.size > 0){
         val emptyCellIndex = board.firstEmptyCellIndex

         var k = 0
         while (k < pieces.length){
            if (unplaced.contains(k)){
               unplaced -= k

               var i = 0
               while (i < Piece.orientations){
                  val piece = pieces(k).nextOrientation

                  var j = 0
                  while (j < Piece.size){
                     if (board.add(j,emptyCellIndex,piece)) {

                        if (!shouldPrune) findSolutions

                        board.remove(piece)
                     }
                     j = j + 1
                  }
                  i = i + 1
               }
               unplaced += k
            }
            k = k + 1
         }
      }
      else {
         puzzleSolved
      }
   }

   private def puzzleSolved() = {
      val b = board.asString
      if (first == null){
         first = b; last = b
      } else {
         if (b < first){ first = b } else { if (b > last){ last = b } }
      }
      countdown = countdown - 1
   }

   private def shouldPrune(): Boolean = {
      board.unmark
      var i = 0
      while (i < board.cells.length){
         if (board.cells(i).contiguousEmptyCells % Piece.size != 0) return true
         i = i + 1
      }
      false
   }


   def printSolutions() = {

      def printBoard(s: String) = {
         var indent = false
         var i = 0
         while (i < s.length){
            if (indent) print(' ')
            var j = 0
            while (j < Board.cols){
               print(s.charAt(i)); print(' ')
               j = j + 1
               i = i + 1
            }
            print('\n')
            indent = !indent
         }
         print('\n')
      }

      print(n + " solutions found\n\n")
      printBoard(first)
      printBoard(last)
   }

/*
   def printPieces() =
      for (i <- Iterator.range(0,Board.pieces)) pieces(i).print
*/

}



// Board.scala
// import scala.collection.mutable._

object Board {
   val cols = 5
   val rows = 10
   val size = rows * cols
   val pieces = 10
   val noFit = new Array[BoardCell](0)
}

final class Board {
   val cells = boardCells()

   val cellsPieceWillFill = new Array[BoardCell](Piece.size)
   var cellCount = 0

   def unmark() = {
      var i = 0
      while (i < cells.length){
         cells(i).unmark
         i = i + 1
      }
   }

   def asString() =
      new String( cells map(
         c => if (c.piece == null) '-'.toByte
              else (c.piece.number + 48).toByte ))

   def firstEmptyCellIndex() = cells.findIndexOf(c => c.isEmpty)


   private val cache = Array.fill(
     Board.pieces,Piece.orientations,Piece.size,Board.size
   )(null: Array[BoardCell])

   def add(pieceIndex: Int, boardIndex: Int, p: Piece): Boolean = {
      var a = cache(p.number)(p.orientation)(pieceIndex)(boardIndex)

      cellCount = 0
      p.unmark

      if (a == null){
         find(p.cells(pieceIndex), cells(boardIndex))

         if (cellCount != Piece.size){
            cache(p.number)(p.orientation)(pieceIndex)(boardIndex) = Board.noFit
            return false
         }

         a = cellsPieceWillFill .filter(c => true)
         cache(p.number)(p.orientation)(pieceIndex)(boardIndex) = a
      }
      else {
         if (a == Board.noFit) return false
      }

      var i = 0
      while (i < a.length){
         if (!a(i).isEmpty) return false
         i = i + 1
      }

      i = 0
      while (i < a.length){
         a(i).piece = p
         i = i + 1
      }

      true
   }


   def remove(piece: Piece) = {
      var i = 0
      while (i < cells.length){
         if (cells(i).piece == piece) cells(i).empty
         i = i + 1
      }
   }


   private def find(p: PieceCell, b: BoardCell): Unit = {
      if (p != null && !p.marked && b != null){
         cellsPieceWillFill(cellCount) = b
         cellCount = cellCount + 1
         p.mark

         var i = 0
         while (i < Cell.sides){
            find(p.next(i), b.next(i))
            i = i + 1
         }
      }
   }


   private def boardCells() = {
      val a = Array.tabulate(Board.size)(i => new BoardCell(i))
      val m = (Board.size / Board.cols) - 1

      for (i <- 0 until a.length) {
         val row = i / Board.cols
         val isFirst = i % Board.cols == 0
         val isLast = (i+1) % Board.cols == 0
         val c = a(i)

         if (row % 2 == 1) {
            if (!isLast) c.next(Cell.NE) = a(i-(Board.cols-1))
            c.next(Cell.NW) = a(i-Board.cols)
            if (row != m) {
               if (!isLast) c.next(Cell.SE) = a(i+(Board.cols+1))
               c.next(Cell.SW) = a(i+Board.cols)
            }
         } else {
            if (row != 0) {
               if (!isFirst) c.next(Cell.NW) = a(i-(Board.cols+1))
               c.next(Cell.NE) = a(i-Board.cols)
            }
            if (row != m) {
               if (!isFirst) c.next(Cell.SW) = a(i+(Board.cols-1))
               c.next(Cell.SE) = a(i+Board.cols)
            }
         }
         if (!isFirst) c.next(Cell.W) = a(i-1)
         if (!isLast) c.next(Cell.E) = a(i+1)
      }
      a
   }


/*
// Printing all the board cells and their neighbours
// helps check that they are connected properly

   def printBoardCellsAndNeighbours() = {
      println("cell\tNW NE W  E  SW SE")
      for (i <- 0 until Board.size) {
         print(i + "\t")
         for (j <- 0 until Cell.sides) {
            val c = cells(i).next(j)
            if (c == null)
               print("-- ")
            else
               printf("{0,number,00} ")(c.number)
         }
         println("")
      }
      println("")
   }
*/

}




// Piece.scala

object Piece {
   val size = 5
   val rotations = Cell.sides
   val flips = 2
   val orientations = rotations * flips
}

final class Piece(_number: Int) {
   val number = _number

   def unmark() = {
      val c = cache(orientation)
      var i = 0
      while (i < c.length){
         c(i).unmark
         i = i + 1
      }
   }

   def cells = cache(orientation)

   private val cache = Array.tabulate(Piece.orientations)(pieceOrientation _)

   var orientation = 0

   def nextOrientation() = {
      orientation = (orientation + 1) % Piece.orientations
      this
   }


   private def pieceOrientation(k: Int) = {
      val cells = Array.fill(Piece.size)(new PieceCell())
      makePiece(number,cells)

      var i = 0
      while (i < k){
         if (i % Piece.rotations == 0)
            cells.foreach(_.flip)
         else
            cells.foreach(_.rotate)

         i = i + 1
      }
      cells
   }

   private def makePiece(number: Int, cells: Array[PieceCell]) = {
      number match {
         case 0 => make0(cells)
         case 1 => make1(cells)
         case 2 => make2(cells)
         case 3 => make3(cells)
         case 4 => make4(cells)
         case 5 => make5(cells)
         case 6 => make6(cells)
         case 7 => make7(cells)
         case 8 => make8(cells)
         case 9 => make9(cells)
      }
   }

   private def make0(a: Array[PieceCell]) = {
      a(0).next(Cell.E) = a(1)
      a(1).next(Cell.W) = a(0)
      a(1).next(Cell.E) = a(2)
      a(2).next(Cell.W) = a(1)
      a(2).next(Cell.E) = a(3)
      a(3).next(Cell.W) = a(2)
      a(3).next(Cell.SE) = a(4)
      a(4).next(Cell.NW) = a(3)
   }

   private def make1(a: Array[PieceCell]) = {
      a(0).next(Cell.SE) = a(1)
      a(1).next(Cell.NW) = a(0)
      a(1).next(Cell.SW) = a(2)
      a(2).next(Cell.NE) = a(1)
      a(2).next(Cell.W) = a(3)
      a(3).next(Cell.E) = a(2)
      a(3).next(Cell.SW) = a(4)
      a(4).next(Cell.NE) = a(3)
   }

   private def make2(a: Array[PieceCell]) = {
      a(0).next(Cell.W) = a(1)
      a(1).next(Cell.E) = a(0)
      a(1).next(Cell.SW) = a(2)
      a(2).next(Cell.NE) = a(1)
      a(2).next(Cell.SE) = a(3)
      a(3).next(Cell.NW) = a(2)
      a(3).next(Cell.SE) = a(4)
      a(4).next(Cell.NW) = a(3)
   }

   private def make3(a: Array[PieceCell]) = {
      a(0).next(Cell.SW) = a(1)
      a(1).next(Cell.NE) = a(0)
      a(1).next(Cell.W) = a(2)
      a(2).next(Cell.E) = a(1)
      a(1).next(Cell.SW) = a(3)
      a(3).next(Cell.NE) = a(1)
      a(2).next(Cell.SE) = a(3)
      a(3).next(Cell.NW) = a(2)
      a(3).next(Cell.SE) = a(4)
      a(4).next(Cell.NW) = a(3)
   }

   private def make4(a: Array[PieceCell]) = {
      a(0).next(Cell.SE) = a(1)
      a(1).next(Cell.NW) = a(0)
      a(1).next(Cell.SW) = a(2)
      a(2).next(Cell.NE) = a(1)
      a(1).next(Cell.E) = a(3)
      a(3).next(Cell.W) = a(1)
      a(3).next(Cell.SE) = a(4)
      a(4).next(Cell.NW) = a(3)
   }

   private def make5(a: Array[PieceCell]) = {
      a(0).next(Cell.SW) = a(1)
      a(1).next(Cell.NE) = a(0)
      a(0).next(Cell.SE) = a(2)
      a(2).next(Cell.NW) = a(0)
      a(1).next(Cell.SE) = a(3)
      a(3).next(Cell.NW) = a(1)
      a(2).next(Cell.SW) = a(3)
      a(3).next(Cell.NE) = a(2)
      a(3).next(Cell.SW) = a(4)
      a(4).next(Cell.NE) = a(3)
   }

   private def make6(a: Array[PieceCell]) = {
      a(0).next(Cell.SW) = a(1)
      a(1).next(Cell.NE) = a(0)
      a(2).next(Cell.SE) = a(1)
      a(1).next(Cell.NW) = a(2)
      a(1).next(Cell.SE) = a(3)
      a(3).next(Cell.NW) = a(1)
      a(3).next(Cell.SW) = a(4)
      a(4).next(Cell.NE) = a(3)
   }

   private def make7(a: Array[PieceCell]) = {
      a(0).next(Cell.SE) = a(1)
      a(1).next(Cell.NW) = a(0)
      a(0).next(Cell.SW) = a(2)
      a(2).next(Cell.NE) = a(0)
      a(2).next(Cell.SW) = a(3)
      a(3).next(Cell.NE) = a(2)
      a(3).next(Cell.SE) = a(4)
      a(4).next(Cell.NW) = a(3)
   }

   private def make8(a: Array[PieceCell]) = {
      a(0).next(Cell.E) = a(1)
      a(1).next(Cell.W) = a(0)
      a(1).next(Cell.E) = a(2)
      a(2).next(Cell.W) = a(1)
      a(2).next(Cell.NE) = a(3)
      a(3).next(Cell.SW) = a(2)
      a(3).next(Cell.E) = a(4)
      a(4).next(Cell.W) = a(3)
   }

   private def make9(a: Array[PieceCell]) = {
      a(0).next(Cell.E) = a(1)
      a(1).next(Cell.W) = a(0)
      a(1).next(Cell.E) = a(2)
      a(2).next(Cell.W) = a(1)
      a(2).next(Cell.NE) = a(3)
      a(3).next(Cell.SW) = a(2)
      a(2).next(Cell.E) = a(4)
      a(4).next(Cell.W) = a(2)
      a(4).next(Cell.NW) = a(3)
      a(3).next(Cell.SE) = a(4)
   }

/*
   def printMe() = {
      println("Piece # " + number)
      println("cell\tNW NE W  E  SW SE")
      for (i <- Iterator.range(0,Piece.size)){
         print(i + "\t")
         for (j <- Iterator.range(0,Cell.sides)){
            val c = cells(i).next(j)
            if (c == null)
               print("-- ")
            else
               for (k <- Iterator.range(0,Piece.size)){
                  if (cells(k) == c) printf(" {0,number,0} ")(k)
               }
         }
         println("")
      }
      println("")
   }
*/
}





// Cell.scala

object Cell {
   val NW = 0; val NE = 1
   val W  = 2; val E  = 3
   val SW = 4; val SE = 5

   val sides = 6
}

abstract class Cell {
   var marked = false

   def mark() = marked = true
   def unmark() = marked = false
}




// BoardCell.scala

final class BoardCell(val number: Int) extends Cell {
   val next = new Array[BoardCell](Cell.sides)
   var piece: Piece = _

   def isEmpty() = piece == null
   def empty() = piece = null

   def contiguousEmptyCells(): Int = {
      if (!marked && isEmpty){
         mark
         var count = 1

         var i = 0
         while (i < next.length){
            if (next(i) != null && next(i).isEmpty)
               count = count + next(i).contiguousEmptyCells
            i = i + 1
         }

         count } else { 0 }
   }
}




// PieceCell.scala

final class PieceCell extends Cell {
   val next = new Array[PieceCell](Cell.sides)

   def flip = {
      var swap = next(Cell.NE)
      next(Cell.NE) = next(Cell.NW)
      next(Cell.NW) = swap

      swap = next(Cell.E)
      next(Cell.E) = next(Cell.W)
      next(Cell.W) = swap

      swap = next(Cell.SE)
      next(Cell.SE) = next(Cell.SW)
      next(Cell.SW) = swap
   }

   def rotate = {
      var swap = next(Cell.E)
      next(Cell.E) = next(Cell.NE)
      next(Cell.NE) = next(Cell.NW)
      next(Cell.NW) = next(Cell.W)
      next(Cell.W) = next(Cell.SW)
      next(Cell.SW) = next(Cell.SE)
      next(Cell.SE) = swap
   }
}
/* The Computer Language Shootout 
   http://shootout.alioth.debian.org/
   contributed by Isaac Gouy (Scala novice)
*/

object methcall {
   def main(args: Array[String]) = {
      var n = toPositiveInt(args);
      var v: Boolean = _;

      val toggle = new Toggle(true);
      for (val i <- Iterator.range(1,n)) v = toggle.activate.value;         

      Console println( toggle.activate.value );

      val ntoggle = new NToggle(true,3);
      for (val i <- Iterator.range(1,n)) v = ntoggle.activate.value;    
     
      Console println( ntoggle.activate.value );
   }


   private def toPositiveInt(s: Array[String]) = {
      val i = 
         try { Integer.parseInt(s(0)); } 
         catch { case _ => 1 }
      if (i>0) i; else 1;
   }
}


private class Toggle(b: Boolean) {
   var state = b;

   def value = state;

   def activate = {
      state = !state;
      this 
   }
}


private class NToggle(b: Boolean, trigger: Int) 
extends Toggle(b) {

   val toggleTrigger = trigger;
   var count = 0;

   override def activate = {
      count = count + 1;
      if (count >= toggleTrigger) {
         state = !state;
         count = 0;
      }
      this
   }
}
/* The Computer Language Benchmarks Game
   http://shootout.alioth.debian.org/
   contributed by Isaac Gouy
   modified by Meiko Rachimow
   updated for 2.8 by Rex Kerr
*/

import math._

object nbody {
  def main(args: Array[String]) = {
    var n = args(0).toInt

    printf("%.9f\n", JovianSystem.energy )
    while (n > 0) { JovianSystem.advance(0.01); n -= 1 }
    printf("%.9f\n", JovianSystem.energy )
  }
}


abstract class NBodySystem {

  def energy() = {
    var e = 0.0
    for (i <- 0 until bodies.length) {
      e += 0.5 * bodies(i).mass * bodies(i).speedSq
      
      for (j <- i+1 until bodies.length) {
        val dx = bodies(i).x - bodies(j).x
        val dy = bodies(i).y - bodies(j).y
        val dz = bodies(i).z - bodies(j).z
        val distance = sqrt(dx*dx + dy*dy + dz*dz)
        e -= (bodies(i).mass * bodies(j).mass) / distance
      }
    }
    e
  }

  def advance(dt: Double) = {
    var i = 0
    while (i < bodies.length){
      var j = i+1
      while (j < bodies.length){
        val dx = bodies(i).x - bodies(j).x
        val dy = bodies(i).y - bodies(j).y
        val dz = bodies(i).z - bodies(j).z

        val distance = sqrt(dx*dx + dy*dy + dz*dz)
        val mag = dt / (distance * distance * distance)

        bodies(i).advance(dx,dy,dz,-bodies(j).mass*mag)
        bodies(j).advance(dx,dy,dz,bodies(i).mass*mag)

        j += 1
      }
      i += 1
    }

    i = 0
    while (i < bodies.length){
      bodies(i).move(dt)
      i += 1
    }
  }

  protected val bodies: Array[Body]

  class Body(){
    var x,y,z = 0.0
    var vx,vy,vz = 0.0
    var mass = 0.0
    def speedSq = vx*vx + vy*vy + vz*vz
    def move(dt: Double) {
      x += dt*vx
      y += dt*vy
      z += dt*vz
    }
    def advance(dx: Double, dy: Double, dz: Double, delta: Double) {
      vx += dx*delta
      vy += dy*delta
      vz += dz*delta
    }
  }
}

object JovianSystem extends NBodySystem {
   protected val bodies = initialValues

   private def initialValues() = {
      val SOLAR_MASS = 4 * Pi * Pi
      val DAYS_PER_YEAR = 365.24

      val sun = new Body
      sun.mass = SOLAR_MASS

      val jupiter = new Body
      jupiter.x = 4.84143144246472090e+00
      jupiter.y = -1.16032004402742839e+00
      jupiter.z = -1.03622044471123109e-01
      jupiter.vx = 1.66007664274403694e-03 * DAYS_PER_YEAR
      jupiter.vy = 7.69901118419740425e-03 * DAYS_PER_YEAR
      jupiter.vz = -6.90460016972063023e-05 * DAYS_PER_YEAR
      jupiter.mass = 9.54791938424326609e-04 * SOLAR_MASS

      val saturn = new Body
      saturn.x = 8.34336671824457987e+00
      saturn.y = 4.12479856412430479e+00
      saturn.z = -4.03523417114321381e-01
      saturn.vx = -2.76742510726862411e-03 * DAYS_PER_YEAR
      saturn.vy = 4.99852801234917238e-03 * DAYS_PER_YEAR
      saturn.vz = 2.30417297573763929e-05 * DAYS_PER_YEAR
      saturn.mass = 2.85885980666130812e-04 * SOLAR_MASS

      val uranus = new Body
      uranus.x = 1.28943695621391310e+01
      uranus.y = -1.51111514016986312e+01
      uranus.z = -2.23307578892655734e-01
      uranus.vx = 2.96460137564761618e-03 * DAYS_PER_YEAR
      uranus.vy = 2.37847173959480950e-03 * DAYS_PER_YEAR
      uranus.vz = -2.96589568540237556e-05 * DAYS_PER_YEAR
      uranus.mass = 4.36624404335156298e-05 * SOLAR_MASS

      val neptune = new Body
      neptune.x = 1.53796971148509165e+01
      neptune.y = -2.59193146099879641e+01
      neptune.z = 1.79258772950371181e-01
      neptune.vx = 2.68067772490389322e-03 * DAYS_PER_YEAR
      neptune.vy = 1.62824170038242295e-03 * DAYS_PER_YEAR
      neptune.vz = -9.51592254519715870e-05 * DAYS_PER_YEAR
      neptune.mass = 5.15138902046611451e-05  * SOLAR_MASS


      val initialValues = Array ( sun, jupiter, saturn, uranus, neptune )

      var px = 0.0; var py = 0.0; var pz = 0.0;
      for (b <- initialValues){
         px += (b.vx * b.mass)
         py += (b.vy * b.mass)
         pz += (b.vz * b.mass)
      }
      sun.vx = -px / SOLAR_MASS
      sun.vy = -py / SOLAR_MASS
      sun.vz = -pz / SOLAR_MASS

      initialValues
   }
}
/* The Computer Language Shootout 
   http://shootout.alioth.debian.org/
   contributed by Isaac Gouy (Scala novice)
*/

object nestedloop {
   def main(args: Array[String]) = {
      val n = toPositiveInt(args);
      var count = 0;

      for (val a <- Iterator.range(0,n); 
           val b <- Iterator.range(0,n); 
           val c <- Iterator.range(0,n); 
           val d <- Iterator.range(0,n); 
           val e <- Iterator.range(0,n); 
           val f <- Iterator.range(0,n)
         ) 
         count = count + 1;

      Console println(count);
   }


   private def toPositiveInt(s: Array[String]) = {
      val i = 
         try { Integer.parseInt(s(0)); } 
         catch { case _ => 1 }
      if (i>0) i; else 1;
   }
}
/* The Computer Language Shootout
   http://shootout.alioth.debian.org/
   contributed by Isaac Gouy
*/


object nsieve { 

   def nsieve(m: int, isPrime: Array[boolean]) = {
      var i = 2
      while (i < m){ isPrime(i) = true; i = i+1 }
      var count = 0

      i = 2
      while (i < m){
         if (isPrime(i)){
            var k = i+i
            while (k < m){ isPrime(k) = false; k = k+i }
            count = count + 1
         }

         i = i+1
      }
      count
   }


   def main(args: Array[String]) = {
      val n = Integer.parseInt(args(0))
      val m = (1<<n)*10000
      val flags = new Array[boolean](m+1)

      def printPrimes(m: int) = {

         def pad(i: int, width: int) = {
            val s = i.toString
            List.range(0, width - s.length)
               .map((i) => " ") .foldLeft("")((a,b) => a+b) + s 
         }

         Console.println("Primes up to " +  pad(m,8) + pad(nsieve(m,flags),9))
      }


      printPrimes(m)
      printPrimes( (1<<n-1)*10000 )
      printPrimes( (1<<n-2)*10000 )
   } 
}
/* The Computer Language Shootout
   http://shootout.alioth.debian.org/
   contributed by Isaac Gouy
*/


object nsieve { 

   def nsieve(m: int, isPrime: Array[boolean]) = {
      for (val i <- Iterator.range(2, m)) isPrime(i) = true
      var count = 0

      for (val i <- Iterator.range(2, m)){
         if (isPrime(i)){
            var k = i+i
            while (k < m){ isPrime(k) = false; k = k+i }
            count = count + 1
         }
      }
      count
   }


   def main(args: Array[String]) = {
      val n = Integer.parseInt(args(0))
      val m = (1<<n)*10000
      val flags = new Array[boolean](m+1)

      def printPrimes(m: int) = {

         def pad(i: int, width: int) = {
            val s = i.toString
            List.range(0, width - s.length)
               .map((i) => " ") .foldLeft("")((a,b) => a+b) + s 
         }

         Console.println("Primes up to " +  pad(m,8) + pad(nsieve(m,flags),9))
      }


      printPrimes(m)
      printPrimes( (1<<n-1)*10000 )
      printPrimes( (1<<n-2)*10000 )
   } 
}
/* The Computer Language Shootout
   http://shootout.alioth.debian.org/
   contributed by Isaac Gouy
*/


object nsieve { 

   def nsieve(m: int, isPrime: Array[boolean]) = {
      for (val i <- List.range(2, m)) isPrime(i) = true
      var count = 0

      for (val i <- List.range(2, m)){
         if (isPrime(i)){
            var k = i+i
            while (k < m){ isPrime(k) = false; k = k+i }
            count = count + 1
         }
      }
      count
   }


   def main(args: Array[String]) = {
      val n = Integer.parseInt(args(0))
      val m = (1<<n)*10000
      val flags = new Array[boolean](m+1)

      def printPrimes(m: int) = {

         def pad(i: int, width: int) = {
            val s = i.toString
            List.range(0, width - s.length)
               .map((i) => " ") .foldLeft("")((a,b) => a+b) + s 
         }

         Console.println("Primes up to " +  pad(m,8) + pad(nsieve(m,flags),9))
      }


      printPrimes(m)
      printPrimes( (1<<n-1)*10000 )
      printPrimes( (1<<n-2)*10000 )
   } 
}
/* The Computer Language Shootout
   http://shootout.alioth.debian.org/
   contributed by Isaac Gouy
*/

import scala.collection.mutable.BitSet

object nsievebits { 

   def nsieve(m: int) = {
      val notPrime = new BitSet(m+1)
      notPrime += 1

      var i = 2
      while (i <= m){
         if (!notPrime.contains(i)){
            var k = i+i
            while (k <= m){ 
               if (!notPrime.contains(k)) notPrime += k 
               k = k+i 
            }
         }

         i = i+1
      }
      m - notPrime.size
   }


   def main(args: Array[String]) = {

      def printPrimes(m: int) = {

         def pad(i: int, width: int) = {
            val s = i.toString
            List.range(0, width - s.length)
               .map((i) => " ") .foldLeft("")((a,b) => a+b) + s 
         }

         Console.println("Primes up to " +  pad(m,8) + pad(nsieve(m),9))
      }

      val n = Integer.parseInt(args(0))
      printPrimes( (1<<n  )*10000 )
      printPrimes( (1<<n-1)*10000 )
      printPrimes( (1<<n-2)*10000 )
   } 
}
/* The Computer Language Shootout
   http://shootout.alioth.debian.org/
   contributed by Isaac Gouy
*/

import scala.collection.mutable.BitSet

object nsievebits { 

   def nsieve(m: int) = {
      val notPrime = new BitSet(m+1)
      notPrime += 1

      var i = 2
      while (i <= m){
         if (!notPrime.contains(i)){
            var k = i+i
            while (k <= m){ notPrime += k; k = k+i }
         }

         i = i+1
      }
      m - notPrime.size
   }


   def main(args: Array[String]) = {

      def printPrimes(m: int) = {

         def pad(i: int, width: int) = {
            val s = i.toString
            List.range(0, width - s.length)
               .map((i) => " ") .foldLeft("")((a,b) => a+b) + s 
         }

         Console.println("Primes up to " +  pad(m,8) + pad(nsieve(m),9))
      }

      val n = Integer.parseInt(args(0))
      printPrimes( (1<<n  )*10000 )
      printPrimes( (1<<n-1)*10000 )
      printPrimes( (1<<n-2)*10000 )
   } 
}
/* The Computer Language Shootout
   http://shootout.alioth.debian.org/
   contributed by Isaac Gouy
*/

import scala.collection.mutable.BitSet

object nsievebits { 

   def nsieve(m: int) = {
      val isPrime = new BitSet(m+1)
      isPrime ++= Iterator.range(2,m+1)

      for (val i <- Iterator.range(2,m+1)){
         if (isPrime.contains(i)){
            var k = i+i
            while (k <= m){ isPrime -= k; k = k+i }
         }
      }
      isPrime.size
   }


   def main(args: Array[String]) = {

      def printPrimes(m: int) = {

         def pad(i: int, width: int) = {
            val s = i.toString
            List.range(0, width - s.length)
               .map((i) => " ") .foldLeft("")((a,b) => a+b) + s  
         }

         Console.println("Primes up to " +  pad(m,8) + pad(nsieve(m),9))
      }

      val n = Integer.parseInt(args(0))
      printPrimes( (1<<n  )*10000 )
      printPrimes( (1<<n-1)*10000 )
      printPrimes( (1<<n-2)*10000 )
   } 
}
/* The Computer Language Shootout 
   http://shootout.alioth.debian.org/
   contributed by Isaac Gouy (Scala novice)
*/

object objinst {
   def main(args: Array[String]) = {
      var n = toPositiveInt(args);

      var toggle = new Toggle(true);
      for (val i <- Iterator.range(0,5)) 
         Console println( toggle.activate.value );
      for (val i <- Iterator.range(0,n)) 
         toggle = new Toggle(true);

      Console print("\n");

      var ntoggle = new NToggle(true,3);
      for (val i <- Iterator.range(0,8)) 
         Console println( ntoggle.activate.value );   
      for (val i <- Iterator.range(0,n)) 
         ntoggle = new NToggle(true,3);  
   }


   private def toPositiveInt(s: Array[String]) = {
      val i = 
         try { Integer.parseInt(s(0)); } 
         catch { case _ => 1 }
      if (i>0) i; else 1;
   }
}


private class Toggle(b: Boolean) {
   var state = b;

   def value = state;

   def activate = {
      state = !state;
      this 
   }
}


private class NToggle(b: Boolean, trigger: Int) 
extends Toggle(b) {

   val toggleTrigger = trigger;
   var count = 0;

   override def activate = {
      count = count + 1;
      if (count >= toggleTrigger) {
         state = !state;
         count = 0;
      }
      this
   }
}
/*
   The Computer Language Benchmarks Game
   http://shootout.alioth.debian.org/
   contributed by Andrei Formiga
   modified by Isaac Gouy
   modified by Meiko Rachimow
*/

object partialsums {

   def main(args: Array[String]) = {
      accumulate(0,0,0,0,0,0,0,0,0, Integer.parseInt(args(0))+1, 1,1)
   }

   val twothirds = 2.0 / 3.0

   def accumulate(a1: double, a2: double, a3: double, a4: double, a5: double,
                  a6: double, a7: double, a8: double, a9: double,
                  n: double, alt: double, k: double) : Unit = {

      if (k < n) {

         val k2 =  Math.pow(k, 2.0)
         val k3 = k2 * k
         val sk = Math.sin(k)
         val ck = Math.cos(k)

         accumulate(
             a1 + Math.pow(twothirds, k - 1.0)
            ,a2 + 1.0 / Math.sqrt(k)
            ,a3 + 1.0 / (k * (k + 1.0))
            ,a4 + 1.0 / (k3 * sk*sk)
            ,a5 + 1.0 / (k3 * ck*ck)
            ,a6 + 1.0 / k
            ,a7 + 1.0 / k2
            ,a8 + alt / k
            ,a9 + alt / (2.0 * k - 1.0)
            ,n
            ,-alt
            ,k + 1.0
            )

      } else {

         val f = "%.9f\t"
         Console.printf( f + "(2/3)^k\n", a1)
         Console.printf( f + "k^-0.5\n", a2)
         Console.printf( f + "1/k(k+1)\n", a3)
         Console.printf( f + "Flint Hills\n", a4)
         Console.printf( f + "Cookson Hills\n", a5)
         Console.printf( f + "Harmonic\n", a6)
         Console.printf( f + "Riemann Zeta\n", a7)
         Console.printf( f + "Alternating Harmonic\n", a8)
         Console.printf( f + "Gregory\n", a9)

      }
   }
}
/* 
   The Computer Language Shootout
   http://shootout.alioth.debian.org/
   contributed by Andrei Formiga 
   modified by Isaac Gouy 
*/

object partialsums {

   def main(args: Array[String]) = {
      accumulate(0,0,0,0,0,0,0,0,0, Integer.parseInt(args(0))+1, 1,1)
   }

   val twothirds = 2.0 / 3.0

   def accumulate(a1: double, a2: double, a3: double, a4: double, a5: double, 
                  a6: double, a7: double, a8: double, a9: double,
                  n: double, alt: double, k: double) : Unit = {

      if (k < n) {

         val k2 =  Math.pow(k, 2.0)
         val k3 = k2 * k
         val sk = Math.sin(k)
         val ck = Math.cos(k)

         accumulate(
             a1 + Math.pow(twothirds, k - 1.0)
            ,a2 + 1.0 / Math.sqrt(k)
            ,a3 + 1.0 / (k * (k + 1.0))
            ,a4 + 1.0 / (k3 * sk*sk)
            ,a5 + 1.0 / (k3 * ck*ck)
            ,a6 + 1.0 / k
            ,a7 + 1.0 / k2
            ,a8 + alt / k
            ,a9 + alt / (2.0 * k - 1.0)
            ,n
            ,-alt
            ,k + 1.0
            )

      } else {

         val f = "{0,number,0.000000000}\t"
         Console.printf( f + "(2/3)^k\n", a1)
         Console.printf( f + "k^-0.5\n", a2)
         Console.printf( f + "1/k(k+1)\n", a3)
         Console.printf( f + "Flint Hills\n", a4)
         Console.printf( f + "Cookson Hills\n", a5)
         Console.printf( f + "Harmonic\n", a6)
         Console.printf( f + "Riemann Zeta\n", a7)
         Console.printf( f + "Alternating Harmonic\n", a8)
         Console.printf( f + "Gregory\n", a9)

      }
   }
}

/* The Computer Language Benchmarks Game 
   http://shootout.alioth.debian.org/   

   Contributed by John Nilsson 
   Major performance improvement by Geoff Reedy  
*/

object pidigits {
    type I = BigInt
    import BigInt._
    val List(_0,_1,_10) = List[I](0,1,10)
  
    class LFT(val q:I, val r:I, val t:I) {
        def compose(o:LFT) = new LFT(q * o.q, (q * o.r) + (r * o.t), t * o.t)
        def extractDigit = {
            val (y,rem) = (3*q + r) /% t
            if((rem + q) < t) Some(y) else None
        }
        def next(y:I) = new LFT(_10*q, _10*(r-(y*t)), t)
        def reduce = {
            val d = (q>>q.lowestSetBit).gcd(r).gcd(t)
            new LFT(q/d,r/d,t/d)
        }
    }

    def pi_digits = {
        def _lfts = Stream from 1 map { k => new LFT(k, k * 4 + 2, k * 2 + 1) }
        def _pi_digits(z:LFT, lfts:Stream[LFT],n:Int): Stream[(Int,I)] = {
            val _z = if(lfts.head.q % 5000 == 0) z reduce else z
            _z extractDigit match {
                case Some(y) => Stream.cons((n,y),_pi_digits(_z next y, lfts,n+1))
                case None    => _pi_digits(_z compose lfts.head, lfts.tail,n)
            }
        }
        _pi_digits(new LFT(_1,_0,_1),_lfts,1)
    }
  
    def by[T](s: Stream[T], n: Int): Stream[Stream[T]] =
        if(s.isEmpty) Stream.empty
        else Stream.cons(s take n, by(s drop n, n))

    def main(args: Array[String]): Unit =
        for (d <- by(pi_digits take args(0).toInt, 10))
            println("%-10s\t:%d".format(d.map(_._2).mkString(""),d.last._1))
}
/* 
 * The Computer Language Benchmarks Game
 * http://shootout.alioth.debian.org/
 * contributed by Rex Kerr
 * based on version by John Nilsson as modified by Geoff Reedy
*/

object pidigits {
  type I = BigInt
  import BigInt._
    
  class LFT(q:I, r:I, t:I) {
    def compose(k: Int) = new LFT(q*k, (q*(4*k+2))+(r*(2*k+1)), t*(2*k+1))
    def extract = {
      val (y,rem) = (q*3 + r) /% t
      if((rem + q) < t) Some(y.intValue) else None
    }
    def next(y: Int) = new LFT(q*10, (r-(t*y))*10, t)
  }

  def pi_digits = {
    def digits(z: LFT, k: Int): Stream[Int] = z extract match {
      case Some(y) => Stream.cons(y,digits(z next y,k))
      case None    => digits(z compose k,k+1)
    }
    digits(new LFT(1,0,1),1)
  }

  def by[T](s: Stream[T], n: Int): Stream[Stream[T]] =
    if (s.isEmpty) Stream.empty
    else Stream.cons(s take n, by(s drop n, n))

  def main(args: Array[String]): Unit =
    for ((d,n) <- by(pi_digits take args(0).toInt, 10).zipWithIndex)
      printf("%-10s\t:%d\n",d.mkString,10*n+d.length)
}
/* 
 * The Computer Language Benchmarks Game
 * http://shootout.alioth.debian.org/
 * contributed by Rex Kerr
 * based on version by John Nilsson as modified by Geoff Reedy
 * GMP wrapping based on Java version by Pall, Kraus, & Sassa
*/

object pidigits {
  import Gmp._
    
  class LFT(q:I, r:I, val t:I) {
    def use(z: LFT) = { ~q; ~r; if (t ne z.t) ~t; z }
    def compose(k: Int) = use(new LFT(q*k!, (q*(4*k+2))+*=(r,(2*k+1))!, t*(2*k+1)!))
    def extract = {
      val (y,rem) = (r + q*3) /% t !!
      val x = if((rem + q) < t) Some(y.toInt) else None
      ~y; ~rem
      x
    }
    def next(y: Int) = use(new LFT(q*10!, (r*10 -*= (t,10*y))!, t))
  }

  def pi_digits = {
    def digits(z: LFT, k: Int): Stream[Int] = z extract match {
      case Some(y) => Stream.cons(y,digits(z next y,k))
      case None    => digits(z compose k,k+1)
    }
    digits(new LFT(I(1),I(0),I(1)),1)
  }

  def by[T](s: Stream[T], n: Int): Stream[Stream[T]] =
    if (s.isEmpty) Stream.empty
    else Stream.cons(s take n, by(s drop n, n))

  def main(args: Array[String]): Unit =
    for ((d,n) <- by(pi_digits take args(0).toInt, 10).zipWithIndex)
      printf("%-10s\t:%d\n",d.mkString,10*n+d.length)
}

/*
 * Partial GMP wrapper for Scala.
 * Write math like normal.
 * Use ! to pull the result off the temporary stack
 * Use ~ to return a value to the temporary stack
 * Be careful with weird +*= GMP functions that destroy argument
*/
class GmpUtil {
  System.loadLibrary("jpargmp")
  @native def mpz_init(): Long
  @native def mpz_clear(src: Long)
  @native def mpz_set_si(lhs: Long, a: Int)
  @native def mpz_get_si(a: Long): Int
  @native def mpz_cmp(a: Long, b: Long): Int
  @native def mpz_add(sum: Long, a: Long, b: Long)
  @native def mpz_sub(sum: Long, a: Long, b: Long)
  @native def mpz_mul_si(prod: Long, a: Long, b: Int)
  @native def mpz_addmul_ui(lhs: Long, a: Long, b: Int)
  @native def mpz_submul_ui(lhs: Long, a: Long, b: Int)
  @native def mpz_tdiv_qr(quot: Long, rem: Long, n: Long, d: Long)
}
object Gmp {
  val gmp = new GmpUtil
  private var stack = Nil:List[I]
  private var defunct = Nil:List[I]
  class I {
    private val z = gmp.mpz_init()
    def !() = stack match {
      case i :: rest if (i eq this) =>
        stack = Nil
        defunct = rest ::: defunct
        i
      case _ => I.die
    }
    def !!() = stack match {
      case i :: j :: rest if (i eq this) =>
        stack = Nil
        defunct = rest ::: defunct
        (i,j)
      case _ => I.die
    }
    def toInt = gmp.mpz_get_si(z)
    def <(i: I) = gmp.mpz_cmp(z, i.z) < 0
    def +(i: I) = { gmp.mpz_add(I.ans.z, z, i.z); I.get }
    def -(i: I) = { gmp.mpz_sub(I.ans.z, z, i.z); I.get }
    def *(n: Int) = { gmp.mpz_mul_si(I.ans.z, z, n); I.get }
    def +*=(i: I, n: Int) = { gmp.mpz_addmul_ui(z, i.z, n); this }
    def -*=(i: I, n: Int) = { gmp.mpz_submul_ui(z, i.z, n); this }
    def /%(i: I) = { val r = I.ans.z; gmp.mpz_tdiv_qr(I.ans.z, r, z, i.z); I.get }
    def unary_~() = { defunct ::= this }
    override def finalize() { gmp.mpz_clear(z); super.finalize }
  }
  object I {
    def apply(n:Int) = defunct match {
      case i :: rest =>
        defunct = rest
        gmp.mpz_set_si(i.z,n)
        i
      case _ =>
        val i = new I
        if (n != 0) gmp.mpz_set_si(i.z,n)
        i
    }
    def ans() = { val i = I(0); stack ::= i; i }
    def die: Nothing = throw new IndexOutOfBoundsException
    def get() = stack match { case i :: rest => i ; case _ => die }
  }  
}
/* The Computer Language Shootout 
   http://shootout.alioth.debian.org/
   contributed by Isaac Gouy (Scala novice)
*/

import concurrent.SyncVar;
import concurrent.ops._;

object prodcons {
   def main(args: Array[String]) = {
      val n = toPositiveInt(args);
      val buffer = new SharedBuffer();
      var p = 0;
      var c = 0;
      val cDone = new SyncVar[Boolean];

      spawn { 
         while(p<n) { p=p+1; buffer put(p); }
      }

      spawn { 
         var v: Int = _;
         while(c<n) { c=c+1; v = buffer.get; }
         cDone set true;
      }

      cDone.get;
      Console println(p + " " + c); 
   }


   private def toPositiveInt(s: Array[String]) = {
      val i = 
         try { Integer.parseInt(s(0)); } 
         catch { case _ => 1 }
      if (i>0) i; else 1;
   }
}


private class SharedBuffer() {
   var contents: Int = _;
   var available = false;

   def get = synchronized {
      while (available == false) wait();
      available = false;
         // Console println("\t" + "get " + contents);
      notifyAll();
      contents
   }

   def put(value: Int) = synchronized {
      while (available == true) wait();
      contents = value;
      available = true;
         // Console println("put " + value);
      notifyAll();
   }
}




/* The Computer Language Shootout 
   http://shootout.alioth.debian.org/
   contributed by Isaac Gouy (Scala novice)
*/

object random {
   def main(args: Array[String]) = {
      var n = toPositiveInt(args);
      var result: Double = _;

      while (n>0) { result=generate(100.0); n=n-1; }

      Console.printf("{0,number,#.000000000}\n")(result);
   }

   private val IM = 139968;
   private val IA = 3877;
   private val IC = 29573;
   private var seed = 42;

   def generate(max: Double) = {
      seed = (seed * IA + IC) % IM;
      max * seed / IM;
   }

   private def toPositiveInt(s: Array[String]) = {
      val i = 
         try { Integer.parseInt(s(0)); } 
         catch { case _ => 1 }
      if (i>0) i; else 1;
   }
}
/*
 * The Computer Language Shootout
 * http://shootout.alioth.debian.org/
 *
 * contributed by Andrei Formiga
 */

object recursive
{
  def ack(x:int, y:int): int = if (x == 0) y + 1 
			       else if (y == 0) ack(x - 1, 1) 
			       else ack(x - 1, ack(x, y - 1))

  def fib(n:int): int = if (n < 2) 1 else fib(n - 2) + fib(n - 1)
  
  def fib(n:double): double = if (n < 2.0) 1.0 else fib(n - 2.0) + fib(n - 1.0)

  def tak(x:int, y:int, z:int): int = if (y < x) tak(tak(x - 1, y, z),
						     tak(y - 1, z, x),
						     tak(z - 1, x, y))
				      else z

  def tak(x:double, y:double, z:double): double = 
    if (y < x)  tak(tak(x - 1.0, y, z), tak(y - 1.0, z, x), tak(z - 1.0, x, y)) 
    else z

  def main(args: Array[String]) = {
    var n = Integer.parseInt(args(0))
    Console.println("Ack(3," + n + "): " + ack(3, n))
    Console.printf("Fib({0,number,.#}): {1,number,.#}\n", (27.0+n), fib(27.0+n))
    n = n - 1
    Console.println("Tak(" + (3*n) + "," + (2*n) + "," + n + "): " + tak(3*n, 2*n, n))
    Console.println("Fib(3): " + fib(3))
    Console.println("Tak(3.0,2.0,1.0): " + tak(3.0,2.0,1.0))
  }
}
/*
 * The Computer Language Benchmarks Game
 * http://shootout.alioth.debian.org/
 *
 * contributed by Andrei Formiga
 * modified by Meiko Rachimow
 */

object recursive
{
  def ack(x:int, y:int): int = if (x == 0) y + 1
			       else if (y == 0) ack(x - 1, 1)
			       else ack(x - 1, ack(x, y - 1))

  def fib(n:int): int = if (n < 2) 1 else fib(n - 2) + fib(n - 1)

  def fib(n:double): double = if (n < 2.0) 1.0 else fib(n - 2.0) + fib(n - 1.0)

  def tak(x:int, y:int, z:int): int = if (y < x) tak(tak(x - 1, y, z),
						     tak(y - 1, z, x),
						     tak(z - 1, x, y))
				      else z

  def tak(x:double, y:double, z:double): double =
    if (y < x)  tak(tak(x - 1.0, y, z), tak(y - 1.0, z, x), tak(z - 1.0, x, y))
    else z

  def main(args: Array[String]) = {
    var n = Integer.parseInt(args(0))
    Console.println("Ack(3," + n + "): " + ack(3, n))
    Console.printf("Fib(%.1f): %.1f\n", (27.0+n), fib(27.0+n))
    n = n - 1
    Console.println("Tak(" + (3*n) + "," + (2*n) + "," + n + "): " + tak(3*n, 2*n, n))
    Console.println("Fib(3): " + fib(3))
    Console.println("Tak(3.0,2.0,1.0): " + tak(3.0,2.0,1.0))
  }
}
/* The Computer Language Shootout
  http://shootout.alioth.debian.org/
  contributed by Isaac Gouy
  modified and updated for 2.8 by Rex Kerr
*/

import java.io._

object regexdna {
  def main(args: Array[String]) {

    var sequence = readFully()
    val initialLength = sequence.length

    def matching(s: String) = java.util.regex.Pattern.compile(s).matcher(sequence)

    // remove FASTA sequence descriptions and new-lines
    sequence = matching(">.*\n|\n").replaceAll("")
    val codeLength = sequence.length

    // regex match
    Array(
      "agggtaaa|tttaccct",
      "[cgt]gggtaaa|tttaccc[acg]",
      "a[act]ggtaaa|tttacc[agt]t",
      "ag[act]gtaaa|tttac[agt]ct",
      "agg[act]taaa|ttta[agt]cct",
      "aggg[acg]aaa|ttt[cgt]ccct",
      "agggt[cgt]aa|tt[acg]accct",
      "agggta[cgt]a|t[acg]taccct",
      "agggtaa[cgt]|[acg]ttaccct"
    ).map(v => {
      var count = 0
      val m = matching(v)
      while (m.find()) count += 1
      println(v + " " + count)
    })

    // regex substitution
    Array(
      ("B", "(c|g|t)"),
      ("D", "(a|g|t)"),
      ("H", "(a|c|t)"),
      ("K", "(g|t)"),
      ("M", "(a|c)"),
      ("N", "(a|c|g|t)"),
      ("R", "(a|g)"),
      ("S", "(c|g)"),
      ("V", "(a|c|g)"),
      ("W", "(a|t)"),
      ("Y", "(c|t)")
    ).foreach(iub => sequence = matching(iub._1).replaceAll(iub._2) )

    println("\n" + initialLength + "\n" + codeLength + "\n" + sequence.length)
  }

  def readFully() = {
    val block = new Array[Char](10240)
    val buffer = new StringBuffer
    val r = new InputStreamReader(System.in)

    Iterator.
      continually(r.read(block)).
      takeWhile(_ > -1).
      foreach(n => buffer.append(block,0,n))

   r.close
   buffer.toString
  }
}
// The Computer Language Benchmarks Game
// http://shootout.alioth.debian.org/

// Contributed by The Anh Tran
// Updated for 2.8 by Rex Kerr

import scala.io.Source
import java.util.regex.Pattern
import scala.collection.immutable.HashMap
import scala.actors.Futures.future

object regexdna {

  def main(args : Array[String]) {

    // load data from stdin
    var input = Source.stdin.mkString
    val init_len = input length

    // strip header & newline
    input = ">.*\n|\n".r replaceAllIn(input, "")
    val strip_len = input length

    // counting patterns
    val patterns  = Array(
      "agggtaaa|tttaccct" ,
      "[cgt]gggtaaa|tttaccc[acg]",
      "a[act]ggtaaa|tttacc[agt]t",
      "ag[act]gtaaa|tttac[agt]ct",
      "agg[act]taaa|ttta[agt]cct",
      "aggg[acg]aaa|ttt[cgt]ccct",
      "agggt[cgt]aa|tt[acg]accct",
      "agggta[cgt]a|t[acg]taccct",
      "agggtaa[cgt]|[acg]ttaccct")

    // queue tasks, each task is handled in a separate thread
    val count_results  = patterns map( pt => 
      future(pt.r.findAllIn(input).toArray.length)
    )

    // replace IUB
    val replace_result  = future {
      val iub = HashMap(
        'B' -> "(c|g|t)",
        'D' -> "(a|g|t)",
        'H' -> "(a|c|t)",
        'K' -> "(g|t)",
        'M' -> "(a|c)",
        'N' -> "(a|c|g|t)",
        'R' -> "(a|g)",
        'S' -> "(c|g)",
        'V' -> "(a|c|g)",
        'W' -> "(a|t)",
        'Y' -> "(c|t)"  )

      val buffer  = new StringBuffer((input.length * 3) / 2)
      val matcher  = Pattern compile "[BDHKMNRSVWY]" matcher input

      while ( matcher find )
        matcher appendReplacement( buffer, iub(input(matcher start))  )

      matcher appendTail buffer
      buffer length
    }


    // print results
    for ((pt, cres) <- patterns zip count_results)
      printf( "%s %d\n", pt, cres() )

    printf( "\n%d\n%d\n%d\n", init_len, strip_len, replace_result() )
  }
}
// The Computer Language Benchmarks Game
// http://shootout.alioth.debian.org/

// Contributed by The Anh Tran
// Updated for 2.8 by Rex Kerr

import scala.actors.Futures.future

object regexdna {

  def main(args: Array[String]) {
    var input = readAll
    val init_len = input length

    // strip header & newline
    input = """>.*\n|\n""".r replaceAllIn(input, "")
    val strip_len  = input length

    // counting patterns
    val patterns  = Array(
      "agggtaaa|tttaccct" ,
      "[cgt]gggtaaa|tttaccc[acg]",
      "a[act]ggtaaa|tttacc[agt]t",
      "ag[act]gtaaa|tttac[agt]ct",
      "agg[act]taaa|ttta[agt]cct",
      "aggg[acg]aaa|ttt[cgt]ccct",
      "agggt[cgt]aa|tt[acg]accct",
      "agggta[cgt]a|t[acg]taccct",
      "agggtaa[cgt]|[acg]ttaccct")

    // queue tasks, each task is handled in a separate thread
    val count_results = patterns map( pt => future(pt.r.findAllIn(input).length) )

    // replace IUB
    val replace_result = future {
    val iub = Array(
      "", "(c|g|t)", "", "(a|g|t)", "", "", "", "(a|c|t)",
      "", "", "(g|t)", "", "(a|c)", "(a|c|g|t)", "", "",
      "", "(a|g)", "(c|g)", "", "", "(a|c|g)", "(a|t)", "",
      "(c|t)"  )

      val buffer = new StringBuffer(input.length + (input.length >>> 1)) // input.len * 1.5
      val matcher = java.util.regex.Pattern compile "[BDHKMNRSVWY]" matcher input

      while ( matcher find )
        matcher appendReplacement( buffer, iub(input(matcher start) - 'A')  )

      matcher appendTail buffer
      buffer length
    }

    // print results
    for ((pt, cres) <- patterns zip count_results)
      printf( "%s %d\n", pt, cres() )

    printf( "\n%d\n%d\n%d\n", init_len, strip_len, replace_result() )
  }

  def readAll() = {
    // load data from stdin
    val reader = new java.io.InputStreamReader(System.in);

    val sb = new StringBuilder(64*1024*1024)
    val buf = new Array[Char](4 *1024*1024)
 
    Iterator.
      continually(reader read buf).
      takeWhile(_ != -1).
      foreach(n => sb.appendAll(buf, 0, n))

    sb toString
  }
}
/*
 * The Computer Language Benchmarks Game
 * http://shootout.alioth.debian.org/
 * contributed by Rex Kerr
 * algorithm follows Java version #4 by Anthony Donnefort
 */

object revcomp extends java.io.ByteArrayOutputStream {
  val input = new Array[Byte](8192)

  val table = new Array[Byte](128)
  for (i <- 0 to 127) { table(i) = i.toByte }
  for ((i,o) <- "ACGTUMRWSYKVHDB".toList zip "TGCAAKYWSRMBDHVN".toList) {
    table(i) = o.toByte
    table(i.toLowerCase) = o.toByte
  }

  def rcOut = {
    if (count > 0) {
      var begin = 0
      var end = count-1
      while (buf(begin) != '\n' && begin < count) { begin += 1 }
      while (begin <= end) {
        if (buf(begin) == '\n') begin += 1
        if (buf(end) == '\n') end -= 1
        if (begin<=end) {
          val temp = buf(begin)
          buf(begin) = table(buf(end))
          buf(end) = table(temp)
          begin += 1
          end -= 1
        }
      }
      System.out.write(buf,0,count)
    }
  }
  
  def main(args:Array[String]) = {
    var n = 0
    do {
      n = System.in.read(input)
      if (n > 0) {
        var i = 0
        var i0 = 0
        while (i < n) {
          if (input(i)=='>') {
            if (i>i0) write(input,i0,i-i0)
            rcOut
            reset
            i0 = i
          }
          i += 1
        }
        if (i0<n) write(input,i0,n-i0)
      }
    } while (n != -1)
    rcOut
  }
}
/*
 * The Computer Language Benchmarks Game
 * http://shootout.alioth.debian.org/
 * contributed by Rex Kerr
 */

object revcomp {
  def hl(s: String) = s + s.toLowerCase
  val table = Map( (hl("ACGTUMRWSYKVHDBN") zip ("TGCAAKYWSRMBDHVN"*2)): _* )

  val buf = new collection.mutable.ArrayBuffer[Char]
  def out {
    buf.reverseIterator.grouped(60).foreach( s => println(s.mkString) )
    buf clear
  }

  def main(args:Array[String]) = {
    io.Source.stdin.getLines().foreach(s => {
      if (s startsWith ">") {
        out
        println(s)
      }
      else buf ++= s.map(table(_))
    })
    out
  }
}
/* The Computer Language Shootout 
   http://shootout.alioth.debian.org/
   contributed by Isaac Gouy (Scala novice)
*/

object sieve {
   def main(args: Array[String]) = {
      var n = toPositiveInt(args);
      val start = 2;
      val stop = 8192;
      val isPrime = new Array[Boolean](stop+1);
      var count: Int = _;

      while (n>0) { 
         count = 0;

         for (val i <- Iterator.range(start,stop+1)) 
            isPrime(i)=true;

         for (val i <- Iterator.range(start,stop+1)) {
            if( isPrime(i) ) {
               var k = i+i;
               while (k<=stop) { isPrime(k)=false; k=k+i; }
               count = count+1;
            }
         }
         n=n-1; 
      }

      Console.println("Count: " + count);
   }


   private def toPositiveInt(s: Array[String]) = {
      val i = 
         try { Integer.parseInt(s(0)); } 
         catch { case _ => 1 }
      if (i>0) i; else 1;
   }
}



/* The Computer Language Benchmarks Game
   http://shootout.alioth.debian.org/
   contributed by Isaac Gouy
   modified by Meiko Rachimow
   updated for 2.8 by Rex Kerr
*/

object spectralnorm {
  def main(args: Array[String]) = {
    val n = (if (args.length>0) args(0).toInt else 100)
    printf("%.09f\n", (new SpectralNorm(n)).approximate())
  }
}

class SpectralNorm(n: Int) {

  // Ordinary and transposed versions of infinite matrix
  val A = (i: Int, j: Int) => 1.0/((i+j)*(i+j+1)/2 +i+1)
  val At = (j: Int, i: Int) => 1.0/((i+j)*(i+j+1)/2 +i+1)

  // Matrix multiplication w <- M*v
  def mult(v: Array[Double], w: Array[Double], M: (Int,Int)=> Double ) {
    var i = 0
    while (i < n) {
     var s = 0.0
     var j = 0
     while (j < n) { s += M(i,j)*v(j); j += 1 }
     w(i) =  s
     i += 1
    }
  }

  def approximate() = {
    val u,v,w = Array.fill(n)(1.0)

    var i = 0
    while (i < 10) {
      // Multiply by matrix & transpose
      mult(u,w,A)
      mult(w,v,At)
      mult(v,w,A)
      mult(w,u,At)
      i += 1
    }

    var vbv,vv = 0.0
    i = 0
    while (i < n) {
      vbv += u(i)*v(i)
      vv += v(i)*v(i)
      i += 1
    }

    math.sqrt(vbv/vv)
  }
}
/*   The Computer Language Benchmarks Game
   http://shootout.alioth.debian.org/
   contributed by Eric Willigers
   based on Java version by The Anh Tran
*/

object spectralnorm {
   val nthread = Runtime.getRuntime.availableProcessors
   val barrier = new java.util.concurrent.CyclicBarrier(nthread)

   def main(args: Array[String]) {
      val n = if (args.length > 0) Integer parseInt args(0)  else 1000
      val fmt = new java.text.DecimalFormat("#.000000000")
      println(fmt format run(n))
   }

   def run(n: Int) = {
      val u = new Array[Double](n)
      val v = new Array[Double](n)
      val tmp = new Array[Double](n)
      val chunk = n / nthread
      var vBv = 0.
      var vv = 0.
      java.util.Arrays.fill(u, 1.)
      Array range(0, nthread) map { i =>
         val rbegin = i * chunk
         val rend = if (i < (nthread -1)) rbegin + chunk else n
         new Approximate(u, v, tmp, rbegin, rend)         
      } foreach { a=>
         a join()
         vBv += a.m_vBv
         vv += a.m_vv
      }
      Math sqrt(vBv/vv)
   }

   final class Approximate(u: Array[Double], v: Array[Double], tmp: Array[Double], rbegin: Int, rend: Int) extends Thread {
      var m_vBv = 0.
      var m_vv = 0.
      start()

      override def run() {
         for (i <- 0 until 10) {
            MultiplyAtAv(u, tmp, v)
            MultiplyAtAv(v, tmp, u)            
         }
         for (i <- rbegin until rend) {
            m_vBv += u(i) * v(i)
            m_vv  += v(i) * v(i)
         }
      }

      @inline
      def eval_A(i: Int, j: Int) = 1.0 / ( ((i+j) * (i+j+1) >>> 1) +i+1 )

      def MultiplyAv(v: Array[Double], Av: Array[Double]) {
         for (i <- rbegin until rend) {
            var sum = 0.
            var j = 0
            while (j < v.length) {
               sum += eval_A(i, j) * v(j)
               j += 1
            }
            Av(i) = sum
         }
      }

      def MultiplyAtv(v: Array[Double], Atv: Array[Double]) {
         for (i <- rbegin until rend) {
            var sum = 0.
            var j = 0
            while (j < v.length) {
               sum += eval_A(j, i) * v(j)
               j += 1
            }
            Atv(i) = sum
         }
      }

      def MultiplyAtAv(v: Array[Double], tmp: Array[Double], AtAv: Array[Double]) {
         MultiplyAv(v, tmp)
         barrier await()
         MultiplyAtv(tmp, AtAv)
         barrier await()
      }
   }
}
/* The Computer Language Shootout 
   http://shootout.alioth.debian.org/
   contributed by Isaac Gouy (Scala novice)
*/

object strcat {
   def main(args: Array[String]) = {
      var n = toPositiveInt(args);
      val s = "hello\n";
      val b = new StringBuffer(32);

      while (n>0) { b.append(s); n=n-1; }

      Console.println( b.length() );
   }


   private def toPositiveInt(s: Array[String]) = {
      val i = 
         try { Integer.parseInt(s(0)); } 
         catch { case _ => 1 }
      if (i>0) i; else 1;
   }
}



/*
 * The Computer Language Shootout
 * http://shootout.alioth.debian.org/
 *
 * contributed by Andrei Formiga
 */

/* imperative version */
object sumcol
{
  def main(args: Array[String]) = 
  {
    var sum = 0
    var line = Console.readLine

    while (line != null)
    {
      sum = sum + Integer.parseInt(line)
      line = Console.readLine
    }

    Console.println(sum.toString())
  }
}
/*
 * The Computer Language Shootout
 * http://shootout.alioth.debian.org/
 *
 * contributed by Andrei Formiga
 */

/* functional version */
object sumcol
{
  def sumFile(res: int): int = 
  {
    val line = Console.readLine
    if (line == null) res else sumFile(res + Integer.parseInt(line))
  }

  def main(args: Array[String]) = 
  {
    Console.println(sumFile(0).toString())
  }
}
/* The Computer Language Benchmarks Game
 * http://shootout.alioth.debian.org/
 * contributed by meiko rachimow
 */

import java.io.{BufferedReader, InputStreamReader}

object sumcol {

  def main(args: Array[String]) {
  
    val in = new BufferedReader(
      new InputStreamReader(java.lang.System.in))
    
    var sum = 0
    var line = in.readLine
    
    while (line != null) {
      sum = sum + line.toInt
      line = in.readLine
    }
    
    println(sum)
  }
}
/* ------------------------------------------------------------------ */
/* The Great Computer Language Shootout                               */
/* http://shootout.alioth.debian.org/                                 */
/*                                                                    */
/* Contributed by Anthony Borla                                       */
/* ------------------------------------------------------------------ */

object takfp
{
  def main(args: Array[String]): unit =
  {
    var n = Integer.parseInt(args(0));

    System.out.println(tak(n * 3.0f, n * 2.0f, n * 1.0f));
  }

  final def tak(x: float, y: float, z: float): float =
  {
    if (y >= x) return z;
    return tak(tak(x - 1.0f, y, z), tak(y - 1.0f, z, x), tak(z - 1.0f, x, y));
  }
}

/* The Computer Language Benchmarks Game
   http://shootout.alioth.debian.org/

   Contributed by Vincent Kraeutler
   updated for 2.8 by Rex Kerr
*/
import scala.actors.Actor
import scala.actors.Actor._

object threadring {

  class Thread(val label: Int) extends Actor {
    var next: Thread = null
    def act() { loop { react {
      case 0 => println(label); System.exit(0)
      case n: Int => next ! n - 1
    }}}
  }

  // create the threads
  val ring = Array.tabulate(503)(i => new Thread(i + 1))

  // hook them up
  ring.foreach(t => {
    t.next = ring( t.label % ring.length )
    t.start
  })

  def main(args : Array[String]) {
    val nHops = args(0).toInt
    ring(0) ! nHops
  }

}
