// $Id: ackermann.java,v 1.2 2005-05-13 16:24:17 igouy-guest Exp $
// http://www.bagley.org/~doug/shootout/ 

public class ackermann {
    public static void main(String[] args) {
	int num = Integer.parseInt(args[0]);
	System.out.println("Ack(3," + num + "): " + Ack(3, num));
    }
    public static int Ack(int m, int n) {
	return (m == 0) ? (n + 1) : ((n == 0) ? Ack(m-1, 1) :
				     Ack(m-1, Ack(m, n - 1)));
    }
}
// $Id: ary.java,v 1.1 2004-05-22 07:27:00 bfulgham Exp $
// http://www.bagley.org/~doug/shootout/

// this program is modified from:
//   http://cm.bell-labs.com/cm/cs/who/bwk/interps/pap.html
// Timing Trials, or, the Trials of Timing: Experiments with Scripting
// and User-Interface Languages</a> by Brian W. Kernighan and
// Christopher J. Van Wyk.

import java.io.*;
import java.util.*;

public class ary {
    public static void main(String args[]) {
	int i, j, k, n = Integer.parseInt(args[0]);
	int x[] = new int[n];
	int y[] = new int[n];

	for (i = 0; i < n; i++)
	    x[i] = i + 1;
	for (k = 0; k < 1000; k++ )
	    for (j = n-1; j >= 0; j--)
		y[j] += x[j];

	System.out.println(y[0] + " " + y[n-1]);
    }
}
/* The Computer Language Benchmarks Game
   http://shootout.alioth.debian.org/
 
   contributed by Leonhard Holz
   based on contribution by Jarkko Miettinen
*/

public class binarytrees {

   private final static int minDepth = 4;
   private final static int threadCount = Runtime.getRuntime().availableProcessors() > 1 ? 2 : 1;
   private final static TreeGenerator[] threads = new TreeGenerator[threadCount + 1];
   
   public static void main(String[] args)
   {
      int n = 0;
      if (args.length > 0) n = Integer.parseInt(args[0]);
      int maxDepth = (minDepth + 2 > n) ? minDepth + 2 : n;

      for (int i = 0; i < threadCount + 1; i++) {
         threads[i] = new TreeGenerator();
         threads[i].start();
      }
      
      TreeGenerator lastThread = threads[threadCount];
      lastThread.depth = maxDepth + 1;
      lastThread.run = true;
      try {
         synchronized(lastThread) {
            lastThread.notify();
            lastThread.wait();
         }
      } catch (InterruptedException e) {}

      System.out.println("stretch tree of depth " + lastThread.depth + "\t check: " + lastThread.result);

      lastThread.depth = maxDepth;
      lastThread.run = true;
      try {
         synchronized(lastThread) {
            lastThread.notify();
            lastThread.wait();
         }
      } catch (InterruptedException e) {}

      for (int depth = minDepth; depth <= maxDepth; depth+=2 ) {

         int check = 0;
         int iterations = 1 << (maxDepth - depth + minDepth);
         int length = iterations / threadCount;

         for (int i = 0; i < threadCount; i++) synchronized(threads[i]) {
            threads[i].depth = depth;
            threads[i].start = i * length;
            threads[i].end = (i + 1) * length;
            threads[i].run = true;
            threads[i].notify();
         }
         for (int i = 0; i < threadCount; i++) try {
            synchronized(threads[i]) {
               if (threads[i].run) threads[i].wait();
            }
            check += threads[i].result;
         } catch (InterruptedException e) {}

         System.out.println((iterations * 2) + "\t trees of depth " + depth + "\t check: " + check);
      }

      System.out.println("long lived tree of depth " + maxDepth + "\t check: "+ lastThread.result);

      for (int i = 0; i < threadCount + 1; i++) {
         threads[i].terminate = true;
         synchronized(threads[i]) {
            threads[i].notify();
         }
      }
   }

   private static class TreeGenerator extends Thread
   {
      private boolean run = false;
      private boolean terminate = false;

      private int start = 0;
      private int end = 0;
      private int result = 0;
      private int depth;
      
      private static TreeNode bottomUpTree(int item, int depth)
      {
         TreeNode node = new TreeNode();
         node.item = item;
         if (depth > 0) {
            node.left = bottomUpTree(2 * item - 1, depth - 1);
            node.right = bottomUpTree(2 * item, depth - 1);
         } else {
            node.left = null;
         }
         return node;
      }

      private static int checkItems(TreeNode node)
      {
         if (node.left == null) {
            return node.item;
         } else {
            return node.item + checkItems(node.left) - checkItems(node.right);
         }
      }
      
      
      public synchronized void run()
      {
         while (!terminate) {
            if (run) {
               result = 0;
               if (start == end) {
                  result += checkItems(bottomUpTree(start, depth));
               } else for (int i = start; i < end; i++) {
                  result += checkItems(bottomUpTree(i, depth)) + checkItems(bottomUpTree(-i, depth));
               }
               run = false;
               notify();
            }
            try {
               wait();
            } catch (InterruptedException e) {}
         }
      }
   }
   
   private static class TreeNode
   {
      private int item;
      private TreeNode left, right;
   }
}
/* The Great Computer Language Shootout
   http://shootout.alioth.debian.org/
 
   contributed by Jarkko Miettinen
*/

public class binarytrees {

	private final static int minDepth = 4;
	
	public static void main(String[] args){
		int n = 0;
		if (args.length > 0) n = Integer.parseInt(args[0]);
		
		int maxDepth = (minDepth + 2 > n) ? minDepth + 2 : n;
		int stretchDepth = maxDepth + 1;
		
		int check = (TreeNode.bottomUpTree(0,stretchDepth)).itemCheck();
		System.out.println("stretch tree of depth "+stretchDepth+"\t check: " + check);
		
		TreeNode longLivedTree = TreeNode.bottomUpTree(0,maxDepth);
		
		for (int depth=minDepth; depth<=maxDepth; depth+=2){
			int iterations = 1 << (maxDepth - depth + minDepth);
			check = 0;
			
			for (int i=1; i<=iterations; i++){
				check += (TreeNode.bottomUpTree(i,depth)).itemCheck();
				check += (TreeNode.bottomUpTree(-i,depth)).itemCheck();
			}
			System.out.println((iterations*2) + "\t trees of depth " + depth + "\t check: " + check);
		}	
		System.out.println("long lived tree of depth " + maxDepth + "\t check: "+ longLivedTree.itemCheck());
	}
	
	
	private static class TreeNode
	{
		private TreeNode left, right;
		private int item;
		
		TreeNode(int item){
			this.item = item;
		}
		
		private static TreeNode bottomUpTree(int item, int depth){
			if (depth>0){
				return new TreeNode(
						bottomUpTree(2*item-1, depth-1)
						, bottomUpTree(2*item, depth-1)
						, item
				);
			}
			else {
				return new TreeNode(item);
			}
		}
		
		TreeNode(TreeNode left, TreeNode right, int item){
			this.left = left;
			this.right = right;
			this.item = item;
		}
		
		private int itemCheck(){
			// if necessary deallocate here
			if (left==null) return item;
			else return item + left.itemCheck() - right.itemCheck();
		}
	}
}
/* The Computer Language Shootout
   http://shootout.alioth.debian.org/

   contributed by Keenan Tims
   modified by Michael Barker
*/


public class chameneos {

	private MeetingPlace mp;

	public static final Colour[] COLOURS = { Colour.BLUE, Colour.RED, Colour.YELLOW, Colour.BLUE };

	private Creature[] creatures = new Creature[COLOURS.length];

	public enum Colour {
		RED, BLUE, YELLOW, FADED
	}

	public class Creature extends Thread {

		private MeetingPlace mp;
		private Colour colour;
		private int met = 0;
		private Colour other;
		
		public Creature(Colour c, MeetingPlace mp) {
			this.colour = c;
			this.mp = mp;
		}

		public void run() {
			try {
				while (colour != Colour.FADED) {
					mp.meet(this);
					if (other == Colour.FADED)
						colour = Colour.FADED;
					else {
						met++;
						colour = complement(other);
					}					
				}
			} catch (InterruptedException e) {
				// Let the thread exit.
			}
		}
		
		private Colour complement(Colour other) {
			if (colour == other)
				return colour;
			switch (colour) {
			case BLUE:
				return other == Colour.RED ? Colour.YELLOW : Colour.RED;
			case RED:
				return other == Colour.BLUE ? Colour.YELLOW : Colour.BLUE;
			case YELLOW:
				return other == Colour.BLUE ? Colour.RED : Colour.BLUE;
			default:
				return colour;
			}
		}

		public int getCreaturesMet() {
			return met;
		}

		public Colour getColour() {
			return colour;
		}

		public void setOther(Colour other) throws InterruptedException {
			this.other = other;
		}
	}

	public class MeetingPlace {
		
		int n;

		public MeetingPlace(int n) {
			this.n = n;
		}
		
		Creature other = null;
		public void meet(Creature c) throws InterruptedException {
			
			synchronized (this) {
				if (n > 0) {
					if (other == null) {
						other = c;
						this.wait();
					} else {
						other.setOther(c.getColour());
						c.setOther(other.getColour());
						other = null;
						n--;
						this.notify();
					}
				} else {
					c.setOther(Colour.FADED);
				}
			}
		}
	}

	public chameneos(int n) throws InterruptedException {
		int meetings = 0;
		mp = new MeetingPlace(n);
		
		for (int i = 0; i < COLOURS.length; i++) {
			creatures[i] = new Creature(COLOURS[i], mp);
			creatures[i].start();
		}

		// wait for all threads to complete
		for (int i = 0; i < COLOURS.length; i++)
			creatures[i].join();
		
		// sum all the meetings
		for (int i = 0; i < COLOURS.length; i++) {
			meetings += creatures[i].getCreaturesMet();			
		}

		System.out.println(meetings);
	}

	public static void main(String[] args) throws Exception {
		if (args.length < 1)
			throw new IllegalArgumentException();
		new chameneos(Integer.parseInt(args[0]));
	}
}
/* The Computer Language Benchmarks Game
 http://shootout.alioth.debian.org/

 contributed by Luzius Meisser
 based on a contribution by Keenan Tims
 that was modified by Michael Barker
 */

public class chameneos {

    public enum Colour {
        RED, BLUE, YELLOW, FADED;

        public Colour complement(Colour other) {
            if (this == other) {
                return this;
            } else if (this == Colour.BLUE) {
                return other == Colour.RED ? Colour.YELLOW : Colour.RED;
            } else if (this == Colour.YELLOW) {
                return other == Colour.BLUE ? Colour.RED : Colour.BLUE;
            } else {
                return other == Colour.YELLOW ? Colour.BLUE : Colour.YELLOW;
            }
        }
    }

    public class Future<T> {

        private volatile T t;

        public T getItem() {
            while (t == null) {
                Thread.yield();
            }
            return t;
        }

        // no synchronization necessary as assignment is atomic
        public void setItem(T t) {
            this.t = t;
        }
    }

    class Creature extends Thread {

        private MeetingPlace mp;
        private Colour colour;
        private int met;

        public Creature(Colour initialColour, MeetingPlace mp) {
            this.colour = initialColour;
            this.mp = mp;
            this.met = 0;
        }

        public void run() {
            try {
                while (true) {
                    colour = mp.meet(colour);
                    met++;
                }
            } catch (InterruptedException e) {
                colour = Colour.FADED;
            }
        }

        public int getCreaturesMet() {
            return met;
        }

        public Colour getColour() {
            return colour;
        }

    }

    public class MeetingPlace {

        private int meetingsLeft;
        private Colour first = null;
        private Future<Colour> current;

        public MeetingPlace(int meetings) {
            this.meetingsLeft = meetings;
        }

        public Colour meet(Colour myColor) throws InterruptedException {
            Future<Colour> newColor;
            synchronized (this) {
                if (meetingsLeft == 0) {
                    throw new InterruptedException();
                } else {
                    if (first == null) {
                        first = myColor;
                        current = new Future<Colour>();
                    } else {
                        current.setItem(myColor.complement(first));
                        first = null;
                        meetingsLeft--;
                    }
                    newColor = current;
                }
            }
            return newColor.getItem();
        }

    }

    public static final Colour[] COLOURS = { Colour.BLUE, Colour.RED, Colour.YELLOW, Colour.BLUE };

    private MeetingPlace mp;
    private Creature[] creatures;

    public chameneos(int meetings) {
        this.mp = new MeetingPlace(meetings);
        this.creatures = new Creature[COLOURS.length];
    }

    public void run() throws InterruptedException {
        for (int i = 0; i < COLOURS.length; i++) {
            creatures[i] = new Creature(COLOURS[i], mp);
            creatures[i].start();
        }

        for (int i = 0; i < COLOURS.length; i++) {
            creatures[i].join();
        }
    }

    public void printResult() {
        int meetings = 0;
        for (int i = 0; i < COLOURS.length; i++) {
            meetings += creatures[i].getCreaturesMet();
            // System.out.println(creatures[i].getCreaturesMet() + ", " +
            // creatures[i].getColour());
        }
        System.out.println(meetings);
    }

    public static void main(String[] args) throws Exception {
        if (args.length < 1) {
            throw new IllegalArgumentException();
        } else {
//            long t0 = System.nanoTime();
            chameneos cham = new chameneos(Integer.parseInt(args[0]));
            cham.run();
            cham.printResult();
//            long t1 = System.nanoTime();
//            System.out.println((t1 - t0) / 1000000);
        }
    }
}
/* The Computer Language Benchmarks Game
   http://shootout.alioth.debian.org/

   contributed by Michael Barker
*/


import java.util.concurrent.ArrayBlockingQueue;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicReference;


/**
 * This implementation uses the java.util.concurrent.atomic library
 * i.e. (compare and set) to avoid locking.  Real threads are used, but
 * are set up as a thread pool and meeting requests are pushed onto a
 * queue that feeds the thread pool.
 */
public final class chameneosredux {

    enum Colour {
        blue,
        red,
        yellow
    }

    private static Colour doCompliment(final Colour c1, final Colour c2) {
        switch (c1) {
        case blue:
            switch (c2) {
            case blue:
                return Colour.blue;
            case red:
                return Colour.yellow;
            case yellow:
                return Colour.red;
            }
        case red:
            switch (c2) {
            case blue:
                return Colour.yellow;
            case red:
                return Colour.red;
            case yellow:
                return Colour.blue;
            }
        case yellow:
            switch (c2) {
            case blue:
                return Colour.red;
            case red:
                return Colour.blue;
            case yellow:
                return Colour.yellow;
            }
        }

        throw new RuntimeException("Error");
    }

    static final class MeetingPlace {

        private final AtomicInteger meetingsLeft;
        private final AtomicReference<Creature> creatureRef = new AtomicReference<Creature>();

        public MeetingPlace(final int meetings) {
            meetingsLeft = new AtomicInteger(meetings);
        }

        public void meet(final Creature incoming) {
            Colour newColour = null;
            Creature first = null;
            Creature next = null;
            do {
                first = creatureRef.get();
                next = incoming;
                if (first != null) {
                    newColour = doCompliment(incoming.colour, first.colour);
                    next = null;
                }
            } while (!creatureRef.compareAndSet(first, next));

            if (first != null) {
                final int meetings = meetingsLeft.decrementAndGet();
                if (meetings >= 0) {
                    first.setColour(incoming.id, newColour);
                    incoming.setColour(first.id, newColour);
                } else {
                    first.complete();
                    incoming.complete();
                }
            }
        }
    }

    static final class Dispatcher implements Runnable {
        private final BlockingQueue<Creature> q;

        public Dispatcher(final BlockingQueue<Creature> q) {
            this.q = q;
        }

        public void run() {
            try {
                while (true) {
                    q.take().run();
                }
            } catch (final InterruptedException e) {
            }
        }
    }

    static final class Creature {

        private final int id;
        private final MeetingPlace place;
        private final BlockingQueue<Creature> q;
        private final CountDownLatch latch;
        private int count = 0;
        private int sameCount = 0;
        private Colour colour;

        public Creature(final MeetingPlace place, final Colour colour,
                        final BlockingQueue<Creature> q, final CountDownLatch latch) {
            this.id = System.identityHashCode(this);
            this.place = place;
            this.latch = latch;
            this.colour = colour;
            this.q = q;
        }

        public void complete() {
            latch.countDown();
        }

        public void setColour(final int id, final Colour newColour) {
            this.colour = newColour;
            count++;
            sameCount += 1 ^ Integer.signum(abs(this.id - id));
            q.add(this);
        }

        private int abs(final int x) {
            final int y = x >> 31;
            return (x ^ y) - y;
        }

        public void run() {
            place.meet(this);
        }

        public int getCount() {
            return count;
        }

        @Override
        public String toString() {
            return String.valueOf(count) + getNumber(sameCount);
        }
    }

    private static void run(final int n, final Colour...colours) {
        final int len = colours.length;
        final MeetingPlace place = new MeetingPlace(n);
        final Creature[] creatures = new Creature[len];
        final BlockingQueue<Creature> q = new ArrayBlockingQueue<Creature>(len);
        final CountDownLatch latch = new CountDownLatch(len - 1);

        for (int i = 0; i < len; i++) {
            System.out.print(" " + colours[i]);
            creatures[i] = new Creature(place, colours[i], q, latch);
        }

        System.out.println();
        final Thread[] ts = new Thread[len];
        for (int i = 0, h = ts.length; i < h; i++) {
            ts[i] = new Thread(new Dispatcher(q));
            ts[i].setDaemon(true);
            ts[i].start();
        }

        for (final Creature creature : creatures) {
            q.add(creature);
        }

        try {
            latch.await();
            for (final Thread t : ts) {
                t.interrupt();
            }
            for (final Thread t : ts) {
                t.join();
            }
        } catch (final InterruptedException e1) {
            System.err.println("Existing with error: " + e1);
        }

        int total = 0;
        for (final Creature creature : creatures) {
            System.out.println(creature);
            total += creature.getCount();
        }
        System.out.println(getNumber(total));
        System.out.println();
    }

    public static void main(final String[] args){
        chameneosredux.program_main(args,true);
    }

    public static void program_main(final String[] args, final boolean isWarm) {

        int n = 600;
        try {
            n = Integer.parseInt(args[0]);
        } catch (final Exception e) {
        }

        printColours();
        System.out.println();
        run(n, Colour.blue, Colour.red, Colour.yellow);
        run(n, Colour.blue, Colour.red, Colour.yellow, Colour.red, Colour.yellow,
               Colour.blue, Colour.red, Colour.yellow, Colour.red, Colour.blue);
    }

    private static final String[] NUMBERS = {
        "zero", "one", "two", "three", "four", "five",
        "six", "seven", "eight", "nine"
    };

    private static String getNumber(final int n) {
        final StringBuilder sb = new StringBuilder();
        final String nStr = String.valueOf(n);
        for (int i = 0; i < nStr.length(); i++) {
            sb.append(" ");
            sb.append(NUMBERS[Character.getNumericValue(nStr.charAt(i))]);
        }

        return sb.toString();
    }

    private static void printColours() {
        printColours(Colour.blue, Colour.blue);
        printColours(Colour.blue, Colour.red);
        printColours(Colour.blue, Colour.yellow);
        printColours(Colour.red, Colour.blue);
        printColours(Colour.red, Colour.red);
        printColours(Colour.red, Colour.yellow);
        printColours(Colour.yellow, Colour.blue);
        printColours(Colour.yellow, Colour.red);
        printColours(Colour.yellow, Colour.yellow);
    }

    private static void printColours(final Colour c1, final Colour c2) {
        System.out.println(c1 + " + " + c2 + " -> " + doCompliment(c1, c2));
    }


}
/* The Computer Language Benchmarks Game
   http://shootout.alioth.debian.org/

   contributed by Michael Barker
   based on a contribution by Luzius Meisser
*/

/**
 * This implementation uses standard Java threading (native threads).
 * 
 * This implementation simply adds the new functionality to the orginal 
 * implementation by Luzius Meisser from old chameneos shootout.  The interesting
 * part of this implementation, is that while a creature is waiting it does not
 * block its thread, rather it spins in a loop using a Thread.yield().
 */
public class chameneosredux {

    enum Colour {
        blue,
        red,
        yellow
    }
        
    private static Colour doCompliment(Colour c1, Colour c2) {
        switch (c1) {
        case blue:
            switch (c2) {
            case blue:
                return Colour.blue;
            case red:
                return Colour.yellow;
            case yellow:
                return Colour.red;
            }
        case red:
            switch (c2) {
            case blue:
                return Colour.yellow;
            case red:
                return Colour.red;
            case yellow:
                return Colour.blue;
            }
        case yellow:
            switch (c2) {
            case blue:
                return Colour.red;
            case red:
                return Colour.blue;
            case yellow:
                return Colour.yellow;
            }
        }
        
        throw new RuntimeException("Error");
    }

    static class MeetingPlace {
        
        private int meetingsLeft;

        public MeetingPlace(int meetings) {
            this.meetingsLeft = meetings;
        }
        
        private Colour firstColour = null;
        private int firstId = 0;
        Future<Pair> current;
        
        public Pair meet(int id, Colour c) throws Exception {
            Future<Pair> newPair;
            synchronized (this) {
                if (meetingsLeft == 0) {
                    throw new Exception("Finished");
                } else {
                    if (firstColour == null) {
                        firstColour = c;
                        firstId = id;
                        current = new Future<Pair>();
                    } else {
                        Colour newColour = doCompliment(c, firstColour);
                        current.setItem(new Pair(id == firstId, newColour));
                        firstColour = null;
                        meetingsLeft--;
                    }
                    newPair = current;
                }
            }
            return newPair.getItem();
            
        }
    }
        
    public static class Future<T> {

        private volatile T t;

        public T getItem() {
            while (t == null) {
                Thread.yield();
            }
            return t;
        }

        // no synchronization necessary as assignment is atomic
        public void setItem(T t) {
            this.t = t;
        }
    }    
    
    static class Creature implements Runnable {

        private final MeetingPlace place;
        private int count = 0;
        private int sameCount = 0;
        private Colour colour;
        private int id;

        public Creature(MeetingPlace place, Colour colour) {
            this.place = place;
            this.id = System.identityHashCode(this);
            this.colour = colour;
        }
        
        public void run() {
            try {
                
                while (true) {
                    Pair p = place.meet(id, colour);
                    colour = p.colour;
                    if (p.sameId) {
                        sameCount++;
                    }
                    count++;
                }
                
            } catch (Exception e) {}
        }
        
        public int getCount() {
            return count;
        }
        
        public String toString() {
            return String.valueOf(count) + getNumber(sameCount);
        }
    }    
    
    private static void run(int n, Colour...colours) {
        MeetingPlace place = new MeetingPlace(n);
        Creature[] creatures = new Creature[colours.length];
        for (int i = 0; i < colours.length; i++) {
            System.out.print(" " + colours[i]);
            creatures[i] = new Creature(place, colours[i]);
        }
        System.out.println();
        Thread[] ts = new Thread[colours.length];
        for (int i = 0; i < colours.length; i++) {
            ts[i] = new Thread(creatures[i]);
            ts[i].start();
        }
        
        for (Thread t : ts) {
            try {
                t.join();
            } catch (InterruptedException e) {
            }
        }
        
        int total = 0;
        for (Creature creature : creatures) {
            System.out.println(creature);
            total += creature.getCount();
        }
        System.out.println(getNumber(total));
        System.out.println();
    }
    
    public static void main(String[] args) {
        
        int n = 600;
        try {
            n = Integer.parseInt(args[0]);
        } catch (Exception e) {
        }
        
        printColours();
        System.out.println();
        run(n, Colour.blue, Colour.red, Colour.yellow);
        run(n, Colour.blue, Colour.red, Colour.yellow, Colour.red, Colour.yellow, 
                Colour.blue, Colour.red, Colour.yellow, Colour.red, Colour.blue);
    }    

    public static class Pair {
        public final boolean sameId;
        public final Colour colour;

        public Pair(boolean sameId, Colour c) {
            this.sameId = sameId;
            this.colour = c;
        }
    }
    
    private static final String[] NUMBERS = {
        "zero", "one", "two", "three", "four", "five", 
        "six", "seven", "eight", "nine"
    };
    
    private static String getNumber(int n) {
        StringBuilder sb = new StringBuilder();
        String nStr = String.valueOf(n);
        for (int i = 0; i < nStr.length(); i++) {
            sb.append(" ");
            sb.append(NUMBERS[Character.getNumericValue(nStr.charAt(i))]);
        }
        
        return sb.toString();
    }
    
    private static void printColours() {
        printColours(Colour.blue, Colour.blue);
        printColours(Colour.blue, Colour.red);
        printColours(Colour.blue, Colour.yellow);
        printColours(Colour.red, Colour.blue);
        printColours(Colour.red, Colour.red);
        printColours(Colour.red, Colour.yellow);
        printColours(Colour.yellow, Colour.blue);
        printColours(Colour.yellow, Colour.red);
        printColours(Colour.yellow, Colour.yellow);
    }
    
    private static void printColours(Colour c1, Colour c2) {
        System.out.println(c1 + " + " + c2 + " -> " + doCompliment(c1, c2));
    }
    
    
}
// $Id: echo.java,v 1.1 2004-05-22 07:57:50 bfulgham Exp $
// http://www.bagley.org/~doug/shootout/
// author: Dirus@programmer.net

import java.io.*;
import java.net.*;

public class echo {
    public static void main(String[] args) throws Exception {
	int iIterations = 1;
	try {
	    iIterations = Integer.parseInt(args[0]);
	} catch(Exception e) { }

	EchoServer esServer = new EchoServer(0);
	new EchoClient(InetAddress.getLocalHost(), esServer.getPort(), iIterations);
    }
}

class EchoClient extends Thread {
    private static final String GREETING = "Hello there sailor\n";
    private final InetAddress inetaServer;
    private final int         iPort;
    private final int         iIterations;

    public EchoClient(InetAddress inetaServer, int iPort, int iIterations) {
	this.inetaServer = inetaServer;
	this.iPort = iPort;
	this.iIterations = iIterations;
	start();
    }

    public void run() {
	Socket socketFromServer = null;
	try {
	    socketFromServer = new Socket(inetaServer, iPort);
	    BufferedReader in = new BufferedReader(new InputStreamReader(socketFromServer.getInputStream()));
	    OutputStream out = socketFromServer.getOutputStream();

	    byte[] bytesOut = GREETING.getBytes();
	    String strIn = GREETING.trim();
	    for(int i = 0; i < iIterations; ++i) {
		out.write(bytesOut);
		out.flush();
		String strRead = in.readLine();
		if(!strRead.equals(strIn))
		    throw new RuntimeException("client: \"" + strIn + "\" ne \"" + strRead + "\"");
	    }
	} catch(Exception e) {
	    e.printStackTrace();
	}

	try {
	    socketFromServer.close();
	} catch(Exception e) { }
    }
}

class EchoServer extends Thread {
    private static final int   BUFFER_SIZE = 1024;
    private final ServerSocket ssAccepting;
    private final int          iPort;

    public EchoServer(int iPort) throws IOException {
	ssAccepting = new ServerSocket(iPort);
	this.iPort = ssAccepting.getLocalPort();
	start();
    }

    public final int getPort() {
	return iPort;
    }

    public void run() {
	byte bytesIn[] = new byte[BUFFER_SIZE];
	try {
	    Socket socketClient = ssAccepting.accept();
	    InputStream in = socketClient.getInputStream();
	    OutputStream out = socketClient.getOutputStream();
	    int iLength, iCount = 0;
	    while ((iLength = in.read(bytesIn)) != -1) {
		out.write(bytesIn, 0, iLength);
		out.flush();
		iCount += iLength;
	    }
	    System.out.println("server processed " + iCount + " bytes");
	} catch (Exception e) {
	    e.printStackTrace();
	}
    }
}
// $Id: except.java,v 1.1 2004-05-23 04:36:29 bfulgham Exp $
// http://www.bagley.org/~doug/shootout/
// Collection class code is from my friend Phil Chu, Thanks Phil!

import java.io.*;
import java.util.*;
import java.text.*;

class Lo_Exception extends Exception {
    int num = 0;
    public Lo_Exception(int num) {
	this.num = num;
    }
    public String toString() {
	return "Lo_Exception, num = " + this.num;
    }
}

class Hi_Exception extends Exception {
    int num = 0;
    public Hi_Exception(int num) {
	this.num = num;
    }
    public String toString() {
	return "Hi_Exception, num = " + this.num;
    }
}

public class except {
    static int Lo = 0;
    static int Hi = 0;

    public static void main(String args[]) throws IOException {
	int n = Integer.parseInt(args[0]);

	for (int i=0; i<n; i++) {
	    some_function(i);
	}
	System.out.println("Exceptions: HI=" + Hi + " / LO=" + Lo);
    }

    public static void some_function(int n) {
	try {
	    hi_function(n);
	} catch (Exception e) {
	    System.out.println("We shouldn't get here: " + e);
	}
    }

    public static void hi_function(int n) throws Hi_Exception, Lo_Exception {
	try {
	    lo_function(n);
	} catch (Hi_Exception e) {
	    Hi++;
	}
    }

    public static void lo_function(int n) throws Hi_Exception, Lo_Exception {
	try {
	    blowup(n);
	} catch (Lo_Exception e) {
	    Lo++;
	}
    }

    public static void blowup(int n) throws Hi_Exception, Lo_Exception {
	if ((n % 2) == 0) {
	    throw new Lo_Exception(n);
	} else {
	    throw new Hi_Exception(n);
	}
    }
}
/*
* The Computer Language Benchmarks Game
* http://shootout.alioth.debian.org/
*
* Based on contribution of Eckehard Berns
* Based on code by Heiner Marxen
* and the ATS version by Hongwei Xi
* convert to Java by The Anh Tran
*/

import java.util.concurrent.atomic.AtomicInteger;

public final class fannkuch implements Runnable
{
    private final int n;
    private final int[] flip_max_arr;
    private final AtomicInteger remain_task = new AtomicInteger(0);
    
    public static void main(String[] args)
    {
        int x = (args.length > 0) ? Integer.parseInt(args[0]) : 7;
        fannkuch f = new fannkuch(x);
        System.out.format("Pfannkuchen(%d) = %d\n", x, f.fank_game());
    }
    
    public fannkuch(int N)
    {
        n = N;
        // hold flip_count result for each swap index
        flip_max_arr = new int[n];
    }
    
    private final int fank_game()
    {
        Thread[] th = new Thread[Runtime.getRuntime().availableProcessors()];
        for (int i = 0; i < th.length; i++)
        {
            th[i] = new Thread(this);
            th[i].start();
        }
        
        print_30_permut();
        
        for (Thread t : th)
        {
            try {
                t.join();
            }
            catch (InterruptedException ie)
            {   }
        }
        
        int mx = 0;
        for (int i : flip_max_arr)
            if (mx < i)
                mx = i;
        return mx;
    }
    
    // In order to divide tasks 'equally' for many threads, permut generation
    // strategy is different than that of original single thread.
    // this function will 'correctly' print first 30 permutations
    private final void print_30_permut()
    {
        // declare and initialize
        final int[] permutation = new int[n];
        for ( int i = 0; i < n; i++ )
        {
            permutation[i] = i;
            System.out.print((1 + i));
        }
        System.out.println();
        
        final int[] perm_remain = new int[n];
        for ( int i = 1; i <= n; i++ )
            perm_remain[i -1] = i;
        
        int numPermutationsPrinted = 1;
        for ( int pos_right = 2; pos_right <= n; pos_right++ )
        {
            int pos_left = pos_right -1;
            do
            {
                // rotate down perm[0..prev] by one
                next_perm(permutation, pos_left);
                
                if (--perm_remain[pos_left] > 0)
                {
                    if (numPermutationsPrinted++ < 30)
                    {
                        for (int i = 0; i < n; ++i)
                            System.out.print((1 + permutation[i]));
                        System.out.println();
                    }
                    else
                        return;
                    
                    for ( ; pos_left != 1; --pos_left)
                        perm_remain[pos_left -1] = pos_left;
                }
                else
                    ++pos_left;
            } while (pos_left < pos_right);
        }
    }
    
    public void run()
    {
        final int[] permutation = new int[n];
        final int[] perm_remain = new int[n];
        final int[] perm_flip = new int[n];

        int pos_right;
        while ((pos_right = remain_task.getAndIncrement()) < (n - 1))
        {
            int flip_max = 0;

            for (int i = 0; i < n - 1; i++)
                permutation[i] = i;

            permutation[pos_right] = (n - 1);
            permutation[n - 1] = (pos_right);

            for (int i = 1; i <= n; i++)
                perm_remain[i - 1] = i;

            int pos_left = n - 2;
            while (pos_left < n - 1)
            {
                // rotate down perm[0..r] by one
                next_perm(permutation, pos_left);

                if (--perm_remain[pos_left] > 0)
                {
                    for (; pos_left != 1; --pos_left)
                        perm_remain[pos_left - 1] = pos_left;

                    if ((permutation[0] != 0) && (permutation[n - 1] != (n - 1)))
                    {
                        System.arraycopy(permutation, 0, perm_flip, 0, n);
                        int flipcount = count_flip(perm_flip);
                        if (flip_max < flipcount)
                            flip_max = flipcount;
                    }
                }
                else
                    pos_left++;
            }

            // update max_flip foreach flipping position
            flip_max_arr[pos_right] = flip_max;
        }
    }


    // Take a permut array, continuously flipping until first element is '1'
    // Return flipping times
    private static final int count_flip(final int[] perm_flip)
    {
        // cache first element, avoid swapping perm[0] and perm[k]
        int v0 = perm_flip[0];
        int tmp;

        int flip_count = 0;
        do
        {
            for (int i = 1, j = v0 - 1; i < j; ++i, --j)
            {
                tmp = perm_flip[i];
                perm_flip[i] = perm_flip[j];
                perm_flip[j] = tmp;
            }

            tmp = perm_flip[v0];
            perm_flip[v0] = v0;
            v0 = tmp;

            flip_count++;
        } while (v0 != 0); // first element == '1' ?

        return flip_count;
    }

    // Return next permut, by rotating elements [0 - position] one 'step'
    // next_perm('1234', 2) -> '2314'
    private static final void next_perm(final int[] permutation, int position)
    {
        int perm0 = permutation[0];

        for (int i = 0; i < position; ++i)
            permutation[i] = permutation[i + 1];
        permutation[position] = perm0;
    }
}
/*
 * The Computer Language Benchmarks Game
 * http://shootout.alioth.debian.org/
 *
 * Contributed by Oleg Mazurov, May 2010
 *
 */

import java.util.concurrent.atomic.AtomicInteger;

public final class fannkuch implements Runnable
{
    private static final int TOPRINT = 30;
    private static final int CHUNKSZ = 5040*4;
    private static int n;
    private static int nfact;
    private static int[] maxFlips;
    private static AtomicInteger taskId;
    
    int[] p, pp, count;

    fannkuch()
    {
        p = new int[n];
        pp = new int[n];
        count = new int[n];        
    }

    void print()
    {
        for ( int i = 0; i < p.length; i++ ) {
            System.out.print( p[i] + 1 );
        }
        System.out.println();
    }

    int countFlips()
    {
        int last = p[0];
        if ( last == 0 )
            return 0;
        if ( p[last] == 0 )
            return 1;

        int flips = 1;
        int len = pp.length;
        System.arraycopy( p, 0, pp, 0, len );
        do {
             ++flips;
             for ( int lo = 1, hi = last - 1; lo < hi; ++lo, --hi ) {
                int t = pp[lo];
                pp[lo] = pp[hi];
                pp[hi] = t;
             }
             int t = pp[last];
             pp[last] = last;
             last = t;
        } while ( pp[last] != 0 );
        return flips;
    }

    void firstPermutation( int idx )
    {
        for ( int i=0; i<p.length; ++i ) {
           p[i] = i;
        }
        
        int curFact = nfact;
        for ( int i=count.length; i>0; --i ) {
            curFact /= i;
            int d = idx / curFact;
            count[i-1] = d;
            idx = idx % curFact;

            System.arraycopy( p, 0, pp, 0, i );
            for ( int j=0; j<i; ++j ) {
                p[j] = j+d < i ? pp[j+d] : pp[j+d-i];
            }
        }
    }

    boolean nextPermutation()
    {
        int first = p[1];
        p[1] = p[0];
        p[0] = first;
        
        int i=1; 
        while ( ++count[i] > i ) {
            count[i] = 0;
            if ( ++i == count.length ) {
                return false;
            }
            int next = p[0] = p[1];
            for ( int j=1; j<i; ++j ) {
                p[j] = p[j+1];
            }
            p[i] = first;
            first = next;
        }
        return true;
    }

    public void run()
    {        
        for (;;) {
            int task = taskId.getAndIncrement();
            if ( task >= maxFlips.length ) {
                break;
            }
                
            int idxMin = task*CHUNKSZ;
            int idxMax = Math.min( nfact, idxMin+CHUNKSZ );

            firstPermutation( idxMin );
            
            int mflips = 0;
            while ( idxMin < TOPRINT ) {
                print();
                if ( !nextPermutation() ) {
                    maxFlips[task] = mflips;
                    return;
                }
                mflips = Math.max( mflips, countFlips() );
                idxMin += 1;
            }
            
            for ( int idx=idxMin; idx<idxMax; ++idx ) {
                if ( !nextPermutation() ) {
                    maxFlips[task] = mflips;
                    return;
                }
                mflips = Math.max( mflips, countFlips() );
            }

            maxFlips[task] = mflips;
        }
    }

    static void printResult( int n, int res )
    {
        System.out.println( "Pfannkuchen("+n+") = "+res );
    }

    public static void main( String[] args )
    {        
        n = args.length > 0 ? Integer.parseInt( args[0] ) : 12;
        if ( n <= 1 ) {
            printResult( n, 0 );
            return;
        }

        nfact = 1;
        for ( int i=2; i<=n; ++i ) {
            nfact *= i;
        }
        
        int nchunks = (nfact + CHUNKSZ - 1) / CHUNKSZ;
        maxFlips = new int[nchunks];
        taskId = new AtomicInteger(0);
        
        int nthreads = Runtime.getRuntime().availableProcessors();
        Thread[] threads = new Thread[nthreads];
        for ( int i=0; i<nthreads; ++i ) {
            threads[i] = new Thread( new fannkuch() );
            threads[i].start();
        }
        for ( Thread t : threads ) {
            try {
                t.join();
            }
            catch ( InterruptedException e ) {}
        }
        
        int res = 0;
        for ( int v : maxFlips ) {
            res = Math.max( res, v );
        }
        printResult( n, res );
    }
}
/* The Computer Language Benchmarks Game
   http://shootout.alioth.debian.org/

   contributed by Klaus Friedel

   will only shine on a 64Bit JVM
*/

public class fannkuch {
  final static class Permutation{
    private long p;
    private int n;

    Permutation(int n) {
      this.n = n;
    }

    void copyTo(Permutation p2){
      p2.n = n;
      p2.p = p;
    }

    void init(){
      for(int i = 0; i < n; i++) set(i, i);
    }

    void set(int idx, int value){
      int shift = idx * 4;
      p &= ~(0xFL << shift);
      p |= ((long)value) << shift;
    }

    int get(int idx){
      int shift = idx * 4;
      return (int)(p >> shift) & 0xF;
    }

    int first(){
      return (int)p & 0xF;
    }

    // rotate digit 0...r by one
    void rotate(final int r){
      final long mask = (16L << 4*r) - 1;
      long x = (p & mask) >>> 4;
      x |= (p & 0xFL) << (4*r);
      p = (p & ~mask) | x;
    }

    void reverse(final int count){
      // do a complete reversal first
      long r = p;
      r = (r & 0x0F0F0F0F0F0F0F0FL) << 4  | (r & 0xF0F0F0F0F0F0F0F0L) >>> 4;
      r = (r & 0x00FF00FF00FF00FFL) << 8  | (r & 0xFF00FF00FF00FF00L) >>> 8;
      r = (r & 0x0000FFFF0000FFFFL) << 16 | (r & 0xFFFF0000FFFF0000L) >>> 16;
      r = (r & 0x00000000FFFFFFFFL) << 32 | (r & 0xFFFFFFFF00000000L) >>> 32;
      // select the relevant part:
      final int shift = 4*(16 - count);
      r >>= shift;
      // replace count of them:
      final long mask = (1L << 4*count) - 1;
      p = (p & ~mask) | (r & mask);
    }

    int flipUntilDone(){
      for(int flips = 0;;flips++){
        final int f = first();
        if(f == 0) return flips;
        reverse(f + 1);
      }
    }

    public String toString() {
      StringBuilder s = new StringBuilder();
      for(int i = 0; i < n; i++){
        s.append(get(i) + 1);
      }
      return s.toString();
    }
  }

  static long fannkuch(final int n) {
    Permutation perm = new Permutation(n);
    Permutation perm1 = new Permutation(n);
    int[] count = new int[n];
    final int n1 = n - 1;

    if (n < 1) return 0;
    perm1.init();

    int r = n;
    int didpr = 0;
    int flipsMax = 0;
    for (; ;) {
      if (didpr < 30) {
        System.out.println(perm1.toString());
        didpr++;
      }

      for (; r != 1; --r) count[r - 1] = r;

      if (!(perm1.first() == 0 || perm1.get(n1) == n1)) {
        perm1.copyTo(perm);
        int flips = perm.flipUntilDone();
        if (flipsMax < flips) {
          flipsMax = flips;
        }
      }

      for (; ;r++) {
        if (r == n) return flipsMax;
        /* rotate down perm1[0..r] by one */
        perm1.rotate(r);
        count[r]--;
        if (count[r] > 0) break;
      }
    }
  }


  public static void main(String[] args) {
    int n = 11;
    if(args.length == 1) n = Integer.parseInt(args[0]);
    System.out.printf("Pfannkuchen(%d) = %d\n", n, fannkuch(n));
  }
}
/* The Computer Language Benchmarks Game
   http://shootout.alioth.debian.org/
   C program contributed by Heiner Marxen
   Transliterated to Java by Amir K aka Razii
*/


public final class fannkuch
{
 public static void main(String[] args)
 {
  int n = 11;
  if(args.length == 1) n = Integer.parseInt(args[0]);
  System.out.println("Pfannkuchen(" + n + ") = " + fannkuch(n));
 }
 
 static int fannkuch(final int n)
 {
  int[] perm = new int[n];
  int[] perm1 = new int[n];
  int[] count = new int[n];
  int flips;
  int flipsMax;
  int r;
  int i;
  int k;
  int didpr;
  final int n1 = n - 1;
  if( n < 1 ) return 0;

  for( i=0;i<n;++i ) perm1[i] = i;
  /* initial (trivial) permu */ 
  r = n;
  didpr = 0;
  flipsMax = 0;
  for(;;)
  {
   if( didpr < 30 )
   {
    for( i=0;i<n;++i ) System.out.print (1+perm1[i]);
    System.out.print("\n");
    ++didpr;
   }
   for(;r!=1;--r)
   {
    count[r-1] = r;
   }
   if(!(perm1[0]==0 || perm1[n1]==n1) )
   {
    flips = 0;
    for( i=1;i<n;++i )
    {
     perm[i] = perm1[i];
    }
    k = perm1[0];
    
    /* cache perm[0] in k */ 
    do
    {
     /* k!=0 ==> k>0 */ 
     int j;
     for( i=1, j=k-1;i<j;++i, --j )
     {
      int t_mp = perm[i];
      perm[i] = perm[j];
      perm[j] = t_mp;
     }
     ++flips;
     /* * Now exchange k (caching perm[0]) and perm[k] */ 
     j=perm[k];
     perm[k]=k;
     k=j;
    }
    while(k != 0);
    if( flipsMax < flips )
    {
     flipsMax = flips;
    }
   }
   for(;;)
   {
    if( r == n )
    {
     return flipsMax;
    }
    /* rotate down perm[0..r] by one */
     int perm0 = perm1[0];
     i = 0;
     while( i < r )
     {
      k = i+1;
      perm1[i] = perm1[k];
      i = k;
     }
     perm1[r] = perm0;
    
    if( (count[r] -= 1) > 0 )
    {
     break;
    }
    ++r;
   }
  }
 }
}
/*
 * The Computer Language Benchmarks Game
 * http://shootout.alioth.debian.org/
 *
 * Contributed by Oleg Mazurov, May 2010
 *
 */

import java.util.concurrent.atomic.AtomicInteger;

public final class fannkuch implements Runnable
{
    private static final int TOPRINT = 30;
    private static final int NCHUNKS = 75;
    private static       int CHUNKSZ;
    private static int n;
    private static int[] Fact;
    private static int[] maxFlips;
    private static AtomicInteger taskId;
    
    int[] p, pp, count;

    void print()
    {
        for ( int i = 0; i < p.length; i++ ) {
            System.out.print( p[i] + 1 );
        }
        System.out.println();
    }

    int procInterval( int idxMin, int idxMax )
    {
        // First permutation
        for ( int i=0; i<p.length; ++i ) {
           p[i] = i;
        }

        int idx = idxMin;
        for ( int i=count.length-1; i>0; --i ) {
            int d = idx / Fact[i];
            count[i] = d;
            idx = idx % Fact[i];

            System.arraycopy( p, 0, pp, 0, i+1 );
            for ( int j=0; j<=i; ++j ) {
                p[j] = j+d <= i ? pp[j+d] : pp[j+d-i-1];
            }
        }
        if ( idxMin < TOPRINT ) {
            print();
        }

        int maxflips = 1;
        loop: for ( int i=idxMin; i<idxMax; ++i ) {

            // Count flips
            int first = p[0];
            if ( p[first] != 0 ) {
                int flips = 1;
                System.arraycopy( p, 0, pp, 0, pp.length );
                do {
                     ++flips;
                     for ( int lo = 1, hi = first - 1; lo < hi; ++lo, --hi ) {
                        int t = pp[lo];
                        pp[lo] = pp[hi];
                        pp[hi] = t;
                     }
                     int t = pp[first];
                     pp[first] = first;
                     first = t;
                } while ( pp[first] != 0 );
                maxflips = Math.max( maxflips, flips );
            }

            // Next permutation
            next: for ( int k=1;;) {
                int t = p[0];
                for ( int l=0; l<k; ++l ) {
                    p[l] = p[l+1];
                }
                p[k] = t;
                if ( ++count[k] > k ) {
                    count[k] = 0;
                    if ( ++k == count.length ) {
                        break loop;
                    }
                }
                else if ( i < TOPRINT-1 ) {
                    print();
                    break;
                }
                else {
                    for ( int l=k; l>0; --l ) {
                        if ( p[l] == l ) {
                            i += Fact[l];
                            k = Math.max( l, 1 );
                            continue next;
                        }
                    }
                    break;
                }
            }
        }

        return maxflips;
    }

    public void run()
    {
        p     = new int[n];
        pp    = new int[n];
        count = new int[n];        

        int task;
        while ( ( task = taskId.getAndIncrement() ) < maxFlips.length ) {
            int idxMin = task*CHUNKSZ;
            int idxMax = Math.min( Fact[n], idxMin+CHUNKSZ );
            if ( idxMin > 0 && idxMax <= Fact[n-1] ) continue;
            maxFlips[task] = procInterval( idxMin, idxMax );
        }
    }

    static void printResult( int n, int res )
    {
        System.out.println( "Pfannkuchen("+n+") = "+res );
    }

    public static void main( String[] args )
    {        
        n = args.length > 0 ? Integer.parseInt( args[0] ) : 12;
        if ( n <= 1 ) {
            printResult( n, 0 );
            return;
        }
        if ( n > 12 ) {         // 13! won't fit into int
            printResult( n, -1 );
            return;
        }

        Fact = new int[n+1];
        Fact[0] = 1;
        for ( int i=1; i<Fact.length; ++i ) {
            Fact[i] = Fact[i-1] * i;
        }
        
        CHUNKSZ = Math.max( (Fact[n] + NCHUNKS - 1) / NCHUNKS, TOPRINT );
        maxFlips = new int[(Fact[n] + CHUNKSZ - 1) / CHUNKSZ];
        taskId = new AtomicInteger(0);

        int nthreads = Runtime.getRuntime().availableProcessors();
        Thread[] threads = new Thread[nthreads];
        for ( int i=0; i<nthreads; ++i ) {
            threads[i] = new Thread( new fannkuch() );
            threads[i].start();
        }
        for ( Thread t : threads ) {
            try {
                t.join();
            }
            catch ( InterruptedException e ) {}
        }
        
        int res = 0;
        for ( int v : maxFlips ) {
            res = Math.max( res, v );
        }
        printResult( n, res );
    }
}
/*
 * The Computer Language Benchmarks Game
 * http://shootout.alioth.debian.org/
 *
 * Contributed by Oleg Mazurov, June 2010
 *
 */

import java.util.concurrent.atomic.AtomicInteger;

public final class fannkuchredux implements Runnable
{
    private static final int NCHUNKS = 150;
    private static       int CHUNKSZ;
    private static       int NTASKS;
    private static int n;
    private static int[] Fact;
    private static int[] maxFlips;
    private static int[] chkSums;
    private static AtomicInteger taskId;
    
    int[] p, pp, count;

    void print()
    {
        for ( int i = 0; i < p.length; i++ ) {
            System.out.print( p[i] + 1 );
        }
        System.out.println();
    }

    void firstPermutation( int idx )
    {
        for ( int i=0; i<p.length; ++i ) {
           p[i] = i;
        }

        for ( int i=count.length-1; i>0; --i ) {
            int d = idx / Fact[i];
            count[i] = d;
            idx = idx % Fact[i];

            System.arraycopy( p, 0, pp, 0, i+1 );
            for ( int j=0; j<=i; ++j ) {
                p[j] = j+d <= i ? pp[j+d] : pp[j+d-i-1];
            }
        }
    }

    boolean nextPermutation()
    {
        int first = p[1];
        p[1] = p[0];
        p[0] = first;
        
        int i=1; 
        while ( ++count[i] > i ) {
            count[i++] = 0;
            int next = p[0] = p[1];
            for ( int j=1; j<i; ++j ) {
                p[j] = p[j+1];
            }
            p[i] = first;
            first = next;
        }
        return true;
    }

    int countFlips()
    {
        int flips = 1;
	int first = p[0];
        if ( p[first] != 0 ) {
            System.arraycopy( p, 0, pp, 0, pp.length );
            do {
                 ++flips;
                 for ( int lo = 1, hi = first - 1; lo < hi; ++lo, --hi ) {
                    int t = pp[lo];
                    pp[lo] = pp[hi];
                    pp[hi] = t;
                 }
                 int t = pp[first];
                 pp[first] = first;
                 first = t;
            } while ( pp[first] != 0 );
        }
	return flips;
    }

    void runTask( int task )
    {
        int idxMin = task*CHUNKSZ;
        int idxMax = Math.min( Fact[n], idxMin+CHUNKSZ );

	firstPermutation( idxMin );

        int maxflips = 1;
        int chksum = 0;
        for ( int i=idxMin;; ) {

            if ( p[0] != 0 ) {
                int flips = countFlips();
                maxflips = Math.max( maxflips, flips );
		chksum += i%2 ==0 ? flips : -flips;
            }

	    if ( ++i == idxMax ) {
	        break;
	    }

            nextPermutation();
        }
	maxFlips[task] = maxflips;
	chkSums[task]  = chksum;
    }

    public void run()
    {
        p     = new int[n];
        pp    = new int[n];
        count = new int[n];        

        int task;
        while ( ( task = taskId.getAndIncrement() ) < NTASKS ) {
	    runTask( task );
        }
    }

    static void printResult( int n, int res, int chk )
    {
        System.out.println( chk+"\nPfannkuchen("+n+") = "+res );
    }

    public static void main( String[] args )
    {        
        n = args.length > 0 ? Integer.parseInt( args[0] ) : 12;
        if ( n < 0 || n > 12 ) {         // 13! won't fit into int
            printResult( n, -1, -1 );
            return;
        }
        if ( n <= 1 ) {
            printResult( n, 0, 0 );
            return;
        }

        Fact = new int[n+1];
        Fact[0] = 1;
        for ( int i=1; i<Fact.length; ++i ) {
            Fact[i] = Fact[i-1] * i;
        }
        
        CHUNKSZ = (Fact[n] + NCHUNKS - 1) / NCHUNKS;
	NTASKS = (Fact[n] + CHUNKSZ - 1) / CHUNKSZ;
        maxFlips = new int[NTASKS];
        chkSums  = new int[NTASKS];
        taskId = new AtomicInteger(0);

        int nthreads = Runtime.getRuntime().availableProcessors();
        Thread[] threads = new Thread[nthreads];
        for ( int i=0; i<nthreads; ++i ) {
            threads[i] = new Thread( new fannkuchredux() );
            threads[i].start();
        }
        for ( Thread t : threads ) {
            try {
                t.join();
            }
            catch ( InterruptedException e ) {}
        }
        
        int res = 0;
        for ( int v : maxFlips ) {
            res = Math.max( res, v );
        }
        int chk = 0;
        for ( int v : chkSums ) {
            chk += v;
        }
        
        printResult( n, res, chk );
    }
}
/* The Computer Language Benchmarks Game
   http://shootout.alioth.debian.org/

   contributed by Isaac Gouy
   converted to Java by Oleg Mazurov
*/

public class fannkuchredux
{
   public static int fannkuch(int n) {
      int[] perm = new int[n];
      int[] perm1 = new int[n];
      int[] count = new int[n];
      int maxFlipsCount = 0;
      int permCount = 0;
      int checksum = 0;

      for(int i=0; i<n; i++) perm1[i] = i;
      int r = n;

      while (true) {

         while (r != 1){ count[r-1] = r; r--; }

         for(int i=0; i<n; i++) perm[i] = perm1[i];
         int flipsCount = 0;
         int k;

         while ( !((k=perm[0]) == 0) ) {
            int k2 = (k+1) >> 1;
            for(int i=0; i<k2; i++) {
               int temp = perm[i]; perm[i] = perm[k-i]; perm[k-i] = temp;
            }
            flipsCount++;
         }

         maxFlipsCount = Math.max(maxFlipsCount, flipsCount);
         checksum += permCount%2 == 0 ? flipsCount : -flipsCount;

         // Use incremental change to generate another permutation
         while (true) {
            if (r == n) {
	       System.out.println( checksum );
	       return maxFlipsCount;
	    }
            int perm0 = perm1[0];
            int i = 0;
            while (i < r) {
               int j = i + 1;
               perm1[i] = perm1[j];
               i = j;
            }
            perm1[r] = perm0;

            count[r] = count[r] - 1;
            if (count[r] > 0) break;
            r++;
         }

         permCount++;
      }
   }

   public static void main(String[] args){
      int n = 7;
      if (args.length > 0) n = Integer.parseInt(args[0]);
      System.out.println("Pfannkuchen("+n+") = "+fannkuch(n));
   }
}
/*
 * The Computer Language Benchmarks Game
 * http://shootout.alioth.debian.org/
 *
 * Contributed by Oleg Mazurov, October 2010
 *
 * This version is submitted for comparison purpose only.
 * It's intended to show the effect of array bound check elimination in Java.
 */

import java.util.concurrent.atomic.AtomicInteger;

public final class fannkuchredux implements Runnable
{
    private static final int NCHUNKS = 240;
    private static       int CHUNKSZ;
    private static       int NTASKS;
    private static int n;
    private static int[] Fact;
    private static int[] maxFlips;
    private static int[] chkSums;
    private static AtomicInteger taskId;
    
    int[] p, pp, count;

    public fannkuchredux()
    {
        p     = new int[n];
        pp    = new int[n];
        count = new int[n+1];
    }

    void runTask( int task )
    {
        int idxMin = task*CHUNKSZ;
        int idxMax = Math.min( Fact[n], idxMin+CHUNKSZ );

	// first permutation
        for ( int i=0; i<n; ++i ) {
           p[i] = i;
        }
        for ( int i=n-1, idx=idxMin; i>0; --i ) {
            int d = idx / Fact[i];
            count[i] = d;
            idx = idx % Fact[i];

            System.arraycopy( p, 0, pp, 0, i+1 );
            for ( int j=0; j<=i; ++j ) {
                p[j] = j+d <= i ? pp[j+d] : pp[j+d-i-1];
            }
        }

        int p0=0,p1=0,p2=0,p3=0,p4=0,p5=0,p6=0,p7=0,p8=0,p9=0,p10=0,p11=0;
	switch ( n ) {
	case 12:  p11 = p[11];
	case 11:  p10 = p[10];
	case 10:  p9 = p[9];
	case 9:   p8 = p[8];
	case 8:   p7 = p[7];
	case 7:   p6 = p[6];
	case 6:   p5 = p[5];
	case 5:   p4 = p[4];
	case 4:   p3 = p[3];
	case 3:   p2 = p[2];
	case 2:   p1 = p[1];
	case 1:   p0 = p[0];
	}

        int maxflips = 0;
        int chksum = 0;

        for ( int i=idxMin; i<idxMax; ++i ) {

	    // count flips
            if ( p0 != 0 ) {
		int pp0 = p0, pp1 = p1, pp2 = p2, pp3 = p3, pp4 = p4, pp5 = p5,
		    pp6 = p6, pp7 = p7, pp8 = p8, pp9 = p9, pp10 = p10, pp11 = p11;
	        int flips = 1;
		for ( ;; ++flips ) {
		    int t = pp0;
		    switch ( t ) {
		    case 1: pp0 = pp1; pp1 = t; break;
		    case 2: pp0 = pp2; pp2 = t; break;
		    case 3: pp0 = pp3; pp3 = t;
	        	    t = pp2; pp2 = pp1; pp1 = t;
			    break;
		    case 4: pp0 = pp4; pp4 = t;
			    t = pp3; pp3 = pp1; pp1 = t;
			    break;
		    case 5: pp0 = pp5; pp5 = t;
			    t = pp4; pp4 = pp1; pp1 = t;
			    t = pp3; pp3 = pp2; pp2 = t;
			    break;
		    case 6: pp0 = pp6; pp6 = t;
			    t = pp5; pp5 = pp1; pp1 = t;
			    t = pp4; pp4 = pp2; pp2 = t;
			    break;
		    case 7: pp0 = pp7; pp7 = t;
			    t = pp6; pp6 = pp1; pp1 = t;
			    t = pp5; pp5 = pp2; pp2 = t;
			    t = pp4; pp4 = pp3; pp3 = t;
			    break;
		    case 8: pp0 = pp8; pp8 = t;
			    t = pp7; pp7 = pp1; pp1 = t;
			    t = pp6; pp6 = pp2; pp2 = t;
			    t = pp5; pp5 = pp3; pp3 = t;
			    break;
		    case 9: pp0 = pp9; pp9 = t;
			    t = pp8; pp8 = pp1; pp1 = t;
			    t = pp7; pp7 = pp2; pp2 = t;
			    t = pp6; pp6 = pp3; pp3 = t;
			    t = pp5; pp5 = pp4; pp4 = t;
			    break;
		    case 10: pp0 = pp10; pp10 = t;
			    t = pp9; pp9 = pp1; pp1 = t;
			    t = pp8; pp8 = pp2; pp2 = t;
			    t = pp7; pp7 = pp3; pp3 = t;
			    t = pp6; pp6 = pp4; pp4 = t;
			    break;
		    case 11: pp0 = pp11; pp11 = t;
			    t = pp10; pp10 = pp1; pp1 = t;
			    t = pp9; pp9 = pp2; pp2 = t;
			    t = pp8; pp8 = pp3; pp3 = t;
			    t = pp7; pp7 = pp4; pp4 = t;
			    t = pp6; pp6 = pp5; pp5 = t;
			    break;
		    }
		    if ( pp0 == 0 ) break;
		}

                maxflips = Math.max( maxflips, flips );
		chksum += (i&1)==0 ? flips : -flips;
            }

	    // next permutation
	    int t = p0; p0 = p1; p1 = t;
            int k=1; 
            while ( ++count[k] > k ) {
		count[k++] = 0;
		switch ( k ) {
		case 11: t = p11; p11 = p0; p0 = t;
		case 10: t = p10; p10 = p0; p0 = t;
		case 9:  t = p9; p9 = p0; p0 = t;
		case 8:  t = p8; p8 = p0; p0 = t;
		case 7:  t = p7; p7 = p0; p0 = t;
		case 6:  t = p6; p6 = p0; p0 = t;
		case 5:  t = p5; p5 = p0; p0 = t;
		case 4:  t = p4; p4 = p0; p0 = t;
		case 3:  t = p3; p3 = p0; p0 = t;
		case 2:  t = p2; p2 = p0; p0 = p1; p1 = t;
		}
            }
        }

	maxFlips[task] = maxflips;
	chkSums[task]  = chksum;
    }

    public void run()
    {
        int task;
        while ( ( task = taskId.getAndIncrement() ) < NTASKS ) {
	    runTask( task );
        }
    }

    static void printResult( int n, int res, int chk )
    {
        System.out.println( chk+"\nPfannkuchen("+n+") = "+res );
    }

    public static void main( String[] args )
    {
	// Inititalize
        n = args.length > 0 ? Integer.parseInt( args[0] ) : 12;
        if ( n <= 0 || n > 12 ) {         // 13! won't fit into int
            printResult( n, -1, -1 );
            return;
        }

        Fact = new int[n+1];
        Fact[0] = 1;
        for ( int i=1; i<Fact.length; ++i ) {
            Fact[i] = Fact[i-1] * i;
        }
        
        CHUNKSZ = (Fact[n] + NCHUNKS - 1) / NCHUNKS;
	NTASKS = (Fact[n] + CHUNKSZ - 1) / CHUNKSZ;
        maxFlips = new int[NTASKS];
        chkSums  = new int[NTASKS];
        taskId = new AtomicInteger(0);

	// Go parallel 
        int nthreads = Runtime.getRuntime().availableProcessors()-1;
        Thread[] threads = new Thread[nthreads];
        for ( int i=0; i<nthreads; ++i ) {
            threads[i] = new Thread( new fannkuchredux() );
            threads[i].start();
        }
	new fannkuchredux().run();
        for ( Thread t : threads ) {
            try {
                t.join();
            }
            catch ( InterruptedException e ) {}
        }
        
	// Reduce the results
        int res = 0;
        for ( int v : maxFlips ) {
            res = Math.max( res, v );
        }
        int chk = 0;
        for ( int v : chkSums ) {
            chk += v;
        }
        
        printResult( n, res, chk );
    }
}
/*
 * The Great Computer Language Shootout 
 * http://shootout.alioth.debian.org/
 * 
 * modified by Mehmet D. AKIN
 *
 */

import java.io.IOException;
import java.io.OutputStream;

class fasta {
    public static final int IM = 139968;
    public static final int IA = 3877;
    public static final int IC = 29573;
    public static int last = 42;

    public static final int LINE_LENGTH = 60;

    // pseudo-random number generator
    public static final double random(double max) {
        last = (last * IA + IC) % IM;
        return max * last / IM;
    }

    // Weighted selection from alphabet
    public static String ALU = 
              "GGCCGGGCGCGGTGGCTCACGCCTGTAATCCCAGCACTTTGG"
            + "GAGGCCGAGGCGGGCGGATCACCTGAGGTCAGGAGTTCGAGA"
            + "CCAGCCTGGCCAACATGGTGAAACCCCGTCTCTACTAAAAAT"
            + "ACAAAAATTAGCCGGGCGTGGTGGCGCGCGCCTGTAATCCCA"
            + "GCTACTCGGGAGGCTGAGGCAGGAGAATCGCTTGAACCCGGG"
            + "AGGCGGAGGTTGCAGTGAGCCGAGATCGCGCCACTGCACTCC"
            + "AGCCTGGGCGACAGAGCGAGACTCCGTCTCAAAAA";
    public static byte[] ALUB = ALU.getBytes(); 

    public static final frequency[] IUB = new frequency[] {
            new frequency('a', 0.27), 
            new frequency('c', 0.12),
            new frequency('g', 0.12), 
            new frequency('t', 0.27),
            
            new frequency('B', 0.02), 
            new frequency('D', 0.02),
            new frequency('H', 0.02), 
            new frequency('K', 0.02),
            new frequency('M', 0.02), 
            new frequency('N', 0.02),
            new frequency('R', 0.02), 
            new frequency('S', 0.02),
            new frequency('V', 0.02), 
            new frequency('W', 0.02),
            new frequency('Y', 0.02) };

    public static final frequency[] HomoSapiens = new frequency[] {
            new frequency('a', 0.3029549426680d),
            new frequency('c', 0.1979883004921d),
            new frequency('g', 0.1975473066391d),
            new frequency('t', 0.3015094502008d)};

    public static void makeCumulative(frequency[] a) {
        double cp = 0.0;
        for (int i = 0; i < a.length; i++) {
            cp += a[i].p;
            a[i].p = cp;
        }
    }

    // naive
    public final static byte selectRandom(frequency[] a) {
        int len = a.length;
        double r = random(1.0);
        for (int i = 0; i < len; i++)
            if (r < a[i].p)
                return a[i].c;
        return a[len - 1].c;
    }

    static int BUFFER_SIZE = 1024;
    static int index = 0;
    static byte[] bbuffer = new byte[BUFFER_SIZE];
    static final void makeRandomFasta(String id, String desc,frequency[] a, int n, OutputStream writer) throws IOException
    {
        index = 0;
        int m = 0;
        String descStr = ">" + id + " " + desc + '\n'; 
        writer.write(descStr.getBytes());
        while (n > 0) {
            if (n < LINE_LENGTH) m = n;  else m = LINE_LENGTH;
            if(BUFFER_SIZE - index < m){
                writer.write(bbuffer, 0, index);
                index = 0;
            }
            for (int i = 0; i < m; i++) {
                bbuffer[index++] = selectRandom(a);
            }
            bbuffer[index++] = '\n';
            n -= LINE_LENGTH;
        }
        if(index != 0) writer.write(bbuffer, 0, index);
    }    
    
    static final void makeRepeatFasta(String id, String desc, String alu, int n, OutputStream writer) throws IOException
    {
        index = 0;
        int m = 0;
        int k = 0;
        int kn = ALUB.length;
        String descStr = ">" + id + " " + desc + '\n'; 
        writer.write(descStr.getBytes());
        while (n > 0) {
            if (n < LINE_LENGTH) m = n; else m = LINE_LENGTH;
            if(BUFFER_SIZE - index < m){
                writer.write(bbuffer, 0, index);
                index = 0;
            }
            for (int i = 0; i < m; i++) {
                if (k == kn) k = 0;
                bbuffer[index++] = ALUB[k];
                k++;
            }
            bbuffer[index++] = '\n';
            n -= LINE_LENGTH;
        }
        if(index != 0) writer.write(bbuffer, 0, index);
    }
    
    public static void main(String[] args) throws IOException {
        makeCumulative(HomoSapiens);
        makeCumulative(IUB);
        int n = 2500000;
        if (args.length > 0)
            n = Integer.parseInt(args[0]);
        OutputStream out = System.out;
        makeRepeatFasta("ONE", "Homo sapiens alu", ALU, n * 2, out);
        makeRandomFasta("TWO", "IUB ambiguity codes", IUB, n * 3, out);
        makeRandomFasta("THREE", "Homo sapiens frequency", HomoSapiens, n * 5, out);
        out.close();
    }

    public static class frequency {
        public byte c;
        public double p;

        public frequency(char c, double p) {
            this.c = (byte)c;
            this.p = p;
        }
    }
}
/*
 * The Computer Language Benchmarks Game
 * http://shootout.alioth.debian.org/
 *
 * modified by Enotus
 *
 */

import java.io.*;

public class fasta {

    static final int LINE_LENGTH = 60;
    static final int OUT_BUFFER_SIZE = 256*1024;
    static final int LOOKUP_SIZE = 4*1024;
    static final double LOOKUP_SCALE = LOOKUP_SIZE - 1;

    static final class Freq {
        byte c;
        double p;
        Freq(char cc, double pp) {c = (byte) cc;p = pp;}
    }

    static final String ALU =
            "GGCCGGGCGCGGTGGCTCACGCCTGTAATCCCAGCACTTTGG"
            + "GAGGCCGAGGCGGGCGGATCACCTGAGGTCAGGAGTTCGAGA"
            + "CCAGCCTGGCCAACATGGTGAAACCCCGTCTCTACTAAAAAT"
            + "ACAAAAATTAGCCGGGCGTGGTGGCGCGCGCCTGTAATCCCA"
            + "GCTACTCGGGAGGCTGAGGCAGGAGAATCGCTTGAACCCGGG"
            + "AGGCGGAGGTTGCAGTGAGCCGAGATCGCGCCACTGCACTCC"
            + "AGCCTGGGCGACAGAGCGAGACTCCGTCTCAAAAA";
    static final Freq[] IUB = {
        new Freq('a', 0.27),
        new Freq('c', 0.12),
        new Freq('g', 0.12),
        new Freq('t', 0.27),
        new Freq('B', 0.02),
        new Freq('D', 0.02),
        new Freq('H', 0.02),
        new Freq('K', 0.02),
        new Freq('M', 0.02),
        new Freq('N', 0.02),
        new Freq('R', 0.02),
        new Freq('S', 0.02),
        new Freq('V', 0.02),
        new Freq('W', 0.02),
        new Freq('Y', 0.02)};
    static final Freq[] HomoSapiens = {
        new Freq('a', 0.3029549426680),
        new Freq('c', 0.1979883004921),
        new Freq('g', 0.1975473066391),
        new Freq('t', 0.3015094502008)};

    static void sumAndScale(Freq[] a) {
        double p = 0;
        for (int i = 0; i < a.length; i++)
            a[i].p = (p += a[i].p) * LOOKUP_SCALE;
        a[a.length - 1].p = LOOKUP_SCALE;
    }

    static final class Random {
    
        static final int IM = 139968;
        static final int IA = 3877;
        static final int IC = 29573;
        static final double SCALE = LOOKUP_SCALE / IM;
        static int last = 42;

        static double next() {
            return SCALE * (last = (last * IA + IC) % IM);
        }
    }

    static final class Out {
    
        static final byte buf[] = new byte[OUT_BUFFER_SIZE];
        static final int lim = OUT_BUFFER_SIZE - 2*LINE_LENGTH - 1;
        static int ct = 0;
        static OutputStream stream;

        static void checkFlush() throws IOException {
            if (ct >= lim) { stream.write(buf, 0, ct); ct = 0;}
        }
        
        static void close() throws IOException {
            stream.write(buf, 0, ct);ct = 0;
            stream.close();
        }
    }
    
    static final class RandomFasta {

        static final Freq[] lookup=new Freq[LOOKUP_SIZE];
        
        static void makeLookup(Freq[] a) {
            for (int i = 0, j = 0; i < LOOKUP_SIZE; i++) {
                while (a[j].p < i) j++;
                lookup[i] = a[j];
            }
        }

        static void addLine(int bytes) throws IOException{
            Out.checkFlush();
            int lct=Out.ct;
            while(lct<Out.ct+bytes){
                double r = Random.next();
                int ai = (int) r; while (lookup[ai].p < r) ai++;
                Out.buf[lct++] = lookup[ai].c;
            }
            Out.buf[lct++] = (byte)'\n';
            Out.ct=lct;
        }

        static void make(String desc, Freq[] a, int n) throws IOException {
            makeLookup(a);

            System.arraycopy(desc.getBytes(), 0, Out.buf, Out.ct, desc.length());
            Out.ct+=desc.length();
            
            while (n > 0) {
                int bytes = Math.min(LINE_LENGTH, n);
                addLine(bytes);
                n -= bytes;
            }
        }
    }

    static final class RepeatFasta {

        static void make(String desc, byte[] alu, int n) throws IOException {
            System.arraycopy(desc.getBytes(), 0, Out.buf, Out.ct, desc.length());
            Out.ct+=desc.length();

            byte buf[] = new byte[alu.length + LINE_LENGTH];
            for (int i = 0; i < buf.length; i += alu.length)
                System.arraycopy(alu, 0, buf, i, Math.min(alu.length, buf.length - i));

            int pos = 0;
            while (n > 0) {
                int bytes = Math.min(LINE_LENGTH, n);
                Out.checkFlush();
                System.arraycopy(buf, pos, Out.buf, Out.ct, bytes); Out.ct+=bytes;
                Out.buf[Out.ct++] = (byte)'\n';
                pos = (pos + bytes) % alu.length;
                n -= bytes;
            }
        }
    }


    public static void main(String[] args) throws IOException {
        int n = 2500000;
        if (args.length > 0) 
            n = Integer.parseInt(args[0]);

        sumAndScale(IUB);
        sumAndScale(HomoSapiens);

        Out.stream=System.out;
        RepeatFasta.make(">ONE Homo sapiens alu\n", ALU.getBytes(), n * 2);
        RandomFasta.make(">TWO IUB ambiguity codes\n", IUB, n * 3);
        RandomFasta.make(">THREE Homo sapiens frequency\n", HomoSapiens, n * 5);
        Out.close();
    }
}
// $Id: fibo.java,v 1.3 2005-04-25 19:01:38 igouy-guest Exp $
// http://www.bagley.org/~doug/shootout/

public class fibo {
    public static void main(String args[]) {
	int N = Integer.parseInt(args[0]);
	System.out.println(fib(N));
    }
    public static int fib(int n) {
	if (n < 2) return(1);
	return( fib(n-2) + fib(n-1) );
    }
}
/* The Great Computer Language Shootout
   http://shootout.alioth.debian.org/
 
   contributed by Paul Lofte
*/

import java.text.DecimalFormat;
import java.text.NumberFormat;

public class harmonic {

    static final NumberFormat formatter = new DecimalFormat("#.000000000");
    
    public static void main(String[] args) {
        int n = 10000000;
        if (args.length > 0) n = Integer.parseInt(args[0]);

        double partialSum = 0.0;
        for (int i=1; i<=n; i++) partialSum += 1.0/i;
        
        System.out.println(formatter.format(partialSum));
    }
}
// $Id: hash.java,v 1.1 2004-05-23 05:06:51 bfulgham Exp $
// http://www.bagley.org/~doug/shootout/

// this program is modified from:
//   http://cm.bell-labs.com/cm/cs/who/bwk/interps/pap.html
// Timing Trials, or, the Trials of Timing: Experiments with Scripting
// and User-Interface Languages</a> by Brian W. Kernighan and
// Christopher J. Van Wyk.

import java.io.*;
import java.util.*;

public class hash {

    public static void main(String args[]) throws IOException {
	int n = Integer.parseInt(args[0]);
	int i, c;
	String s = "";
	Integer ii;
	// the original program used:
	// Hashtable ht = new Hashtable();
	// John Olsson points out that Hashtable is for synchronized access
	// and we should use instead:
	HashMap ht = new HashMap();

	c = 0;
	for (i = 1; i <= n; i++)
	    ht.put(Integer.toString(i, 16), new Integer(i));
	for (i = 1; i <= n; i++)
	    // The original code converted to decimal string this way:
	    // if (ht.containsKey(i+""))
	    if (ht.containsKey(Integer.toString(i, 10)))
		c++;

	System.out.println(c);
    }
}

// $Id: hash2.java,v 1.1 2004-05-23 05:50:10 bfulgham Exp $
// http://www.bagley.org/~doug/shootout/

import java.util.*;

class Val {
    int val;
    Val(int init) { val = init; }
}

public class hash2 {
    public static void main(String args[]) {
	int n = Integer.parseInt(args[0]);
	HashMap hash1 = new HashMap(10000);
	HashMap hash2 = new HashMap(n);

	for(int i = 0; i < 10000; i++)
	    hash1.put("foo_" + Integer.toString(i, 10), new Val(i));
	for(int i = 0; i < n; i++) {
	    Iterator it = hash1.entrySet().iterator();
	    while(it.hasNext()) {
			Map.Entry h1 = (Map.Entry)it.next();
			String key = (String)h1.getKey();
			int v1 = ((Val)h1.getValue()).val;
			if (hash2.containsKey(key))
				((Val)hash2.get(key)).val += v1;
			else
				hash2.put(key, new Val(v1));
	    }
	}

	System.out.print(((Val)hash1.get("foo_1")).val    + " " +
	                 ((Val)hash1.get("foo_9999")).val + " " +
	                 ((Val)hash2.get("foo_1")).val    + " " +
	                 ((Val)hash2.get("foo_9999")).val + "\n");
    }
}
// $Id: heapsort.java,v 1.1 2004-05-23 06:37:41 bfulgham Exp $
// http://www.bagley.org/~doug/shootout/

import java.text.*;
import java.lang.reflect.Array;

public class heapsort {

    public static final long IM = 139968;
    public static final long IA =   3877;
    public static final long IC =  29573;

    public static void main(String args[]) {
	int N = Integer.parseInt(args[0]);
	NumberFormat nf = NumberFormat.getInstance();
	nf.setMaximumFractionDigits(10);
	nf.setMinimumFractionDigits(10);
	nf.setGroupingUsed(false);
	double []ary = (double[])Array.newInstance(double.class, N+1);
	for (int i=1; i<=N; i++) {
	    ary[i] = gen_random(1);
	}
	heapsort(N, ary);
	System.out.print(nf.format(ary[N]) + "\n");
    }

    public static long last = 42;
    public static double gen_random(double max) {
	return( max * (last = (last * IA + IC) % IM) / IM );
    }

    public static void heapsort(int n, double ra[]) {
	int l, j, ir, i;
	double rra;

	l = (n >> 1) + 1;
	ir = n;
	for (;;) {
	    if (l > 1) {
		rra = ra[--l];
	    } else {
		rra = ra[ir];
		ra[ir] = ra[1];
		if (--ir == 1) {
		    ra[1] = rra;
		    return;
		}
	    }
	    i = l;
	    j = l << 1;
	    while (j <= ir) {
		if (j < ir && ra[j] < ra[j+1]) { ++j; }
		if (rra < ra[j]) {
		    ra[i] = ra[j];
		    j += (i = j);
		} else {
		    j = ir + 1;
		}
	    }
	    ra[i] = rra;
	}
    }
}
// $Id: hello.java,v 1.3 2007-06-20 03:32:39 bfulgham Exp $
// http://shootout.alioth.debian.org/

public class hello {
    public static void main(String args[]) {
	System.out.print("hello world\n");
    }
}
/* The Computer Language Benchmarks Game
 http://shootout.alioth.debian.org/
 
 contributed by James McIlree
 ByteString code thanks to Matthieu Bentot and The Anh Tran
 */

import java.util.*;
import java.io.*;
import java.util.concurrent.*;

public class knucleotide {
    static ArrayList<Callable< Map<ByteString, ByteString> > > createFragmentTasks(final byte[] sequence, int[] fragmentLengths) {
	ArrayList<Callable<Map<ByteString, ByteString>>> tasks = new ArrayList<Callable<Map<ByteString, ByteString>>>();
	for (int fragmentLength : fragmentLengths) {
	    for (int index=0; index<fragmentLength; index++) {
		final int offset = index;
		final int finalFragmentLength = fragmentLength;
		tasks.add(new Callable<Map<ByteString, ByteString>>() {
		    public Map<ByteString, ByteString> call() {
			return createFragmentMap(sequence, offset, finalFragmentLength);
		    }
		});
	    }
	}
	return tasks;
    }
    	
    static Map<ByteString, ByteString> createFragmentMap(byte[] sequence, int offset, int fragmentLength) {
	HashMap<ByteString, ByteString> map = new HashMap<ByteString, ByteString>();
	int lastIndex = sequence.length - fragmentLength + 1;
	ByteString key = new ByteString(fragmentLength);	
	for (int index=offset; index<lastIndex; index+=fragmentLength) {
	    key.calculateHash(sequence, index);
	    ByteString fragment = map.get(key);
	    if (fragment != null) {
		fragment.count++;
	    } else {
		map.put(key, key);
		key = new ByteString(fragmentLength);
	    }
	}

	return map;
    }
        
    // Destructive!
    static Map<ByteString, ByteString> sumTwoMaps(Map<ByteString, ByteString> map1, Map<ByteString, ByteString> map2) {
	for (Map.Entry<ByteString, ByteString> entry : map2.entrySet()) {
	    ByteString sum = map1.get(entry.getKey());
	    if (sum != null)
		sum.count += entry.getValue().count;
	    else
		map1.put(entry.getKey(), entry.getValue());
	}
	return map1;
    }
    
    static String writeFrequencies(float totalCount, Map<ByteString, ByteString> frequencies) {
	SortedSet<ByteString> list = new TreeSet<ByteString>(frequencies.values());
	StringBuilder sb = new StringBuilder();
	for (ByteString k : list)
	    sb.append(String.format("%s %.3f\n", k.toString().toUpperCase(), (float)(k.count) * 100.0f / totalCount));
	
	return sb.append('\n').toString();
    }
    
    static String writeCount(List<Future<Map<ByteString, ByteString>>> futures, String nucleotideFragment) throws Exception {
	ByteString key = new ByteString(nucleotideFragment.length());
	key.calculateHash(nucleotideFragment.getBytes(), 0);
	
	int count = 0;
	for (Future<Map<ByteString, ByteString>> future : futures) {
	    ByteString temp = future.get().get(key);
	    if (temp != null) count += temp.count;
	}
	
	return count + "\t" + nucleotideFragment.toUpperCase() + '\n';
    }
    
    public static void main (String[] args) throws Exception {
	String line;
	BufferedReader in = new BufferedReader(new InputStreamReader(System.in));
	while ((line = in.readLine()) != null) {
	    if (line.startsWith(">THREE")) break;
	}
	    
	ByteArrayOutputStream baos = new ByteArrayOutputStream();
        byte bytes[] = new byte[100];
        while((line = in.readLine()) != null) {
	    if (line.length() > bytes.length)
		bytes = new byte[line.length()];
	    
	    int i;
	    for(i=0; i<line.length(); i++)
		bytes[i] = (byte)line.charAt(i);
	    baos.write(bytes, 0, i);
        }
	
	byte[] sequence = baos.toByteArray();
		
	ExecutorService pool = Executors.newFixedThreadPool(Runtime.getRuntime().availableProcessors());
	int[] fragmentLengths = { 1, 2, 3, 4, 6, 12, 18 };
	List<Future<Map<ByteString, ByteString>>> futures = pool.invokeAll(createFragmentTasks(sequence, fragmentLengths));
	pool.shutdown();
	
	StringBuilder sb = new StringBuilder();

	sb.append(writeFrequencies(sequence.length, futures.get(0).get()));
	sb.append(writeFrequencies(sequence.length - 1, sumTwoMaps(futures.get(1).get(), futures.get(2).get())));
	
	String[] nucleotideFragments = { "ggt", "ggta", "ggtatt", "ggtattttaatt", "ggtattttaatttatagt" };
	for (String nucleotideFragment : nucleotideFragments) {
	    sb.append(writeCount(futures, nucleotideFragment));
	}
	
	System.out.print(sb.toString());	
    }
    
    static final class ByteString implements Comparable<ByteString> {
        public int hash, count=1;
        public final byte bytes[];
	
        public ByteString(int size) {
            bytes = new byte[size];
        }
	
        public void calculateHash(byte k[], int offset) {
	    int temp = 0;
            for (int i=0; i<bytes.length; i++) {
		byte b = k[offset+i];
                bytes[i] = b;
                temp = temp * 31 + b;
            }
	    hash = temp;
        }
	
        public int hashCode() {
            return hash;
        }
	
        public boolean equals(Object obj) {
	    return Arrays.equals(bytes, ((ByteString)obj).bytes);
        }
	
        public int compareTo(ByteString other) {
            return other.count - count;
        }
	
	public String toString() {
	    return new String(bytes);
	}
    }
}
/* The Computer Language Benchmarks Game
 http://shootout.alioth.debian.org/

 contributed by James McIlree
 ByteString code thanks to Matthieu Bentot and The Anh Tran
 modified by Andy Fingerhut 
 */

import java.util.*;
import java.io.*;
import java.util.concurrent.*;

public class knucleotide {
    static ArrayList<Callable< Map<ByteString, ByteString> > > createFragmentTasks(final byte[] sequence, int[] fragmentLengths) {
	ArrayList<Callable<Map<ByteString, ByteString>>> tasks = new ArrayList<Callable<Map<ByteString, ByteString>>>();
	for (int fragmentLength : fragmentLengths) {
	    for (int index=0; index<fragmentLength; index++) {
		final int offset = index;
		final int finalFragmentLength = fragmentLength;
		tasks.add(new Callable<Map<ByteString, ByteString>>() {
		    public Map<ByteString, ByteString> call() {
			return createFragmentMap(sequence, offset, finalFragmentLength);
		    }
		});
	    }
	}
	return tasks;
    }

    static Map<ByteString, ByteString> createFragmentMap(byte[] sequence, int offset, int fragmentLength) {
	HashMap<ByteString, ByteString> map = new HashMap<ByteString, ByteString>();
	int lastIndex = sequence.length - fragmentLength + 1;
	ByteString key = new ByteString(fragmentLength);
	for (int index=offset; index<lastIndex; index+=fragmentLength) {
	    key.calculateHash(sequence, index);
	    ByteString fragment = map.get(key);
	    if (fragment != null) {
		fragment.count++;
	    } else {
		map.put(key, key);
		key = new ByteString(fragmentLength);
	    }
	}

	return map;
    }

    // Destructive!
    static Map<ByteString, ByteString> sumTwoMaps(Map<ByteString, ByteString> map1, Map<ByteString, ByteString> map2) {
	for (Map.Entry<ByteString, ByteString> entry : map2.entrySet()) {
	    ByteString sum = map1.get(entry.getKey());
	    if (sum != null)
		sum.count += entry.getValue().count;
	    else
		map1.put(entry.getKey(), entry.getValue());
	}
	return map1;
    }

    static String writeFrequencies(float totalCount, Map<ByteString, ByteString> frequencies) {
	SortedSet<ByteString> list = new TreeSet<ByteString>(frequencies.values());
	StringBuilder sb = new StringBuilder();
	for (ByteString k : list)
	    sb.append(String.format("%s %.3f\n", k.toString().toUpperCase(), (float)(k.count) * 100.0f / totalCount));

	return sb.append('\n').toString();
    }

    static String writeCount(List<Future<Map<ByteString, ByteString>>> futures, String nucleotideFragment) throws Exception {
	ByteString key = new ByteString(nucleotideFragment.length());
	key.calculateHash(nucleotideFragment.getBytes(), 0);

	int count = 0;
	for (Future<Map<ByteString, ByteString>> future : futures) {
	    ByteString temp = future.get().get(key);
	    if (temp != null) count += temp.count;
	}

	return count + "\t" + nucleotideFragment.toUpperCase() + '\n';
    }

    public static void main (String[] args) throws Exception {
	String line;
	BufferedReader in = new BufferedReader(new InputStreamReader(System.in));
	while ((line = in.readLine()) != null) {
	    if (line.startsWith(">THREE")) break;
	}

	ByteArrayOutputStream baos = new ByteArrayOutputStream();
        byte bytes[] = new byte[100];
        while((line = in.readLine()) != null) {
	    if (line.length() > bytes.length)
		bytes = new byte[line.length()];

	    int i;
	    for(i=0; i<line.length(); i++)
		bytes[i] = (byte)line.charAt(i);
	    baos.write(bytes, 0, i);
        }

	byte[] sequence = baos.toByteArray();

	ExecutorService pool = Executors.newFixedThreadPool(Runtime.getRuntime().availableProcessors());
	int[] fragmentLengths = { 1, 2, 3, 4, 6, 12, 18 };
	List<Future<Map<ByteString, ByteString>>> futures = pool.invokeAll(createFragmentTasks(sequence, fragmentLengths));
	pool.shutdown();

	StringBuilder sb = new StringBuilder();

	sb.append(writeFrequencies(sequence.length, futures.get(0).get()));
	sb.append(writeFrequencies(sequence.length - 1, sumTwoMaps(futures.get(1).get(), futures.get(2).get())));

	String[] nucleotideFragments = { "ggt", "ggta", "ggtatt", "ggtattttaatt", "ggtattttaatttatagt" };
	for (String nucleotideFragment : nucleotideFragments) {
	    sb.append(writeCount(futures, nucleotideFragment));
	}

	System.out.print(sb.toString());
    }

    static final class ByteString implements Comparable<ByteString> {
        public int hash, count=1;
        public final byte bytes[];

        public ByteString(int size) {
            bytes = new byte[size];
        }

        public void calculateHash(byte k[], int offset) {
	    int temp = 0;
            for (int i=0; i<bytes.length; i++) {
		byte b = k[offset+i];
                bytes[i] = b;
                temp = temp * 31 + b;
            }
	    hash = temp;
        }

        public int hashCode() {
            return hash;
        }

        public boolean equals(Object obj) {
	    return Arrays.equals(bytes, ((ByteString)obj).bytes);
        }

        public int compareTo(ByteString other) {
	    if (other.count != count) {
		return other.count - count;
	    } else {
		// Without this case, if there are two or more strings
		// with exactly the same count in a Map, then the
		// TreeSet constructor called in writeFrequencies will
		// only add the first one, and the rest will not
		// appear in the output.  Also this is required to
		// satisfy the rules of the k-nucleotide problem.
		return toString().compareTo(other.toString());
	    }
        }

	public String toString() {
	    return new String(bytes);
	}
    }
}
/* The Computer Language Benchmarks Game
   http://shootout.alioth.debian.org/
 
   contributed by Leonhard Holz
   thanks to James McIlree for the fragmentation idea
*/

import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.SortedSet;
import java.util.TreeSet;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

public class knucleotide
{
   private static final byte A = 0;
   private static final byte T = 1;
   private static final byte C = 2;
   private static final byte G = 3;
   private static final byte BITS_PER_CHAR = 2;
   private static final byte CHAR_BIT_MASK = 3;
   
   private static final int CHUNK_SIZE = 1024 * 1024 * 2;
   private static final int NUMBER_OF_CORES = Runtime.getRuntime().availableProcessors();
   
   private static void writeFrequencies(SortedSet<Fragment> set, StringBuilder sb)
   {
      int n = 0;
      Iterator<Fragment> i;
      
      i = set.iterator();
      while (i.hasNext()) {
         n += i.next().count;
      }
      
      i = set.iterator();
      while (i.hasNext()) {
         Fragment fragment = i.next();
         sb.append(fragment.toString()).append(String.format(" %.3f", fragment.count * 100.0f / n)).append("\n");
      }
   
      sb.append("\n");
   }

   public static void main(String[] args) throws IOException, InterruptedException, ExecutionException
   {
      boolean inGenom = false;
      List<Encoder> encoders = new ArrayList<Encoder>();
      ExecutorService service = Executors.newFixedThreadPool(NUMBER_OF_CORES);
      
      int read = CHUNK_SIZE, i;
      while (read == CHUNK_SIZE) {
         i = 0;
         byte[] buffer = new byte[CHUNK_SIZE];
         read = System.in.read(buffer);
         if (!inGenom) {
            for (; i < read; i++) if (buffer[i] == '\n' && i + 6 < buffer.length) {
               if (buffer[++i] == '>' && buffer[++i] == 'T' && buffer[++i] == 'H' && buffer[++i] == 'R' && buffer[++i] == 'E' && buffer[++i] == 'E') {
                  while (buffer[i++] != '\n');
                  inGenom = true;
                  break;
               }
            }
         }
         if (inGenom) {
            Encoder encoder = new Encoder(buffer, i, read);
            encoders.add(encoder);
            service.execute(encoder);
         }
      }

      Counter size1 = new Counter(encoders, 1);
      service.execute(size1);
      Counter size2 = new Counter(encoders, 2);
      service.execute(size2);

      String[] fragments = { "GGT", "GGTA", "GGTATT" };
      Counter[] counters = new Counter[fragments.length];
      for (i = 0; i < fragments.length; i++) {
         counters[i] = new Counter(encoders, fragments[i].length());
         service.execute(counters[i]);
      }
      
      String[] largeFragments = { "GGTATTTTAATT", "GGTATTTTAATTTATAGT" };
      List<OffsetCounter>[] counterMap = new ArrayList[largeFragments.length];
      for (i = 0; i < largeFragments.length; i++) {
         counterMap[i] = new ArrayList<OffsetCounter>();
         for (int j = 0; j < largeFragments[i].length(); j++) {
            OffsetCounter counter = new OffsetCounter(encoders, j, largeFragments[i].length());
            counterMap[i].add(counter); 
            service.execute(counter);
         }
      }

      service.shutdown();
      
      StringBuilder out = new StringBuilder(1024);
      writeFrequencies(new TreeSet<Fragment>(size1.get().values()), out);
      writeFrequencies(new TreeSet<Fragment>(size2.get().values()), out);

      for (i = 0; i < fragments.length; i++) {
         Fragment fragment = new Fragment(fragments[i]);
         out.append(counters[i].get().get(fragment).count).append("\t").append(fragments[i]).append("\n");
      }
      
      for (i = 0; i < largeFragments.length; i++) {
         int count = 0;
         Fragment fragment = new Fragment(largeFragments[i]);
         for (int j = 0; j < largeFragments[i].length(); j++) {
            Fragment counter = counterMap[i].get(j).get().get(fragment);
            if (counter != null) {
               count += counter.count;
            }
         }
         out.append(count).append("\t").append(largeFragments[i]).append("\n");
      }
   
      System.out.print(out.toString());
   }

   private static class Encoder implements Runnable
   {
      private byte[] src;
      private int start, end;
      private boolean done = false;
      private List<byte[]> result = new ArrayList<byte[]>();
      
      private static final int CHUNK_SIZE = 1024 * 250;
      
      private Encoder(byte[] src, int start, int end)
      {
         this.src = src;
         this.start = start;
         this.end = end;
      }

      public List<byte[]> get() throws InterruptedException, ExecutionException
      {
         while (!done) try {
            Thread.sleep(100);
         } catch (InterruptedException e) {
            // ignored
         }
         return result;
      }

      public void run()
      {
         int p = 0;
         byte[] encoded = new byte[CHUNK_SIZE];
         
         for (int i = start; i < end; i++) {
            byte c = src[i];
            if (c == 'a' || c == 'A') {
               encoded[p++] = A;
               if (p == encoded.length) {
                  result.add(encoded);
                  encoded = new byte[CHUNK_SIZE];
                  p = 0;
               }
            } else if (c == 't' || c == 'T') {
               encoded[p++] = T;
               if (p == encoded.length) {
                  result.add(encoded);
                  encoded = new byte[CHUNK_SIZE];
                  p = 0;
               }
            } else if (c == 'c' || c == 'C') {
               encoded[p++] = C;
               if (p == encoded.length) {
                  result.add(encoded);
                  encoded = new byte[CHUNK_SIZE];
                  p = 0;
               }
            } else if (c == 'g' || c == 'G') {
               encoded[p++] = G;
               if (p == encoded.length) {
                  result.add(encoded);
                  encoded = new byte[CHUNK_SIZE];
                  p = 0;
               }
            }
         }
      
         if (p != 0) {
            byte[] last = new byte[p];
            System.arraycopy(encoded, 0, last, 0, p);
            result.add(last);
         }
      
         done = true;
      }
   }
   
   private static class Fragment implements Comparable<Fragment>
   {
      private int count = 1;
      private int charsInValue;
      private long value;

      public Fragment(int size)
      {
         this.charsInValue = size;
      }
      
      public Fragment(String s)
      {
         for (int i = 0; i < s.length(); i++) {
            char c = s.charAt(i);
            if (c == 'A') {
               value = value << BITS_PER_CHAR | A;
               charsInValue++;
            } else if (c == 'T') {
               value = value << BITS_PER_CHAR | T;
               charsInValue++;
            } else if (c == 'G') {
               value = value << BITS_PER_CHAR | G;
               charsInValue++;
            } else if (c == 'C') {
               value = value << BITS_PER_CHAR | C;
               charsInValue++;
            }
         }
      }
      
      @Override
      public int hashCode()
        {
           return (int) value;
        }

      @Override
        public boolean equals(Object o)
        {
           Fragment f = (Fragment) o;
           return f.value == value;
        }
       
        public int compareTo(Fragment o)
        {
           return o.count - count;
        }
   
        public String toString()
        {
           long chars = value;
           StringBuilder s = new StringBuilder();
           for (int i = 0; i < charsInValue; i++) {
              int c = (int) (chars & CHAR_BIT_MASK);
              if (c == A) {
                 s.insert(0, 'A');
              } else if (c == T) {
                 s.insert(0, 'T');
              } else if (c == G) {
                 s.insert(0, 'G');
              } else if (c == C) {
                 s.insert(0, 'C');
              }
              chars >>= BITS_PER_CHAR;
           }
           return s.toString();
        }
   }

   private static class Counter implements Runnable
   {
      private int fragmentSize;
      private boolean done = false;
      private List<Encoder> nucleotides;
      private Map<Fragment, Fragment> fragments = new HashMap<Fragment, Fragment>();
      
      private Counter(List<Encoder> nucleotides, int fragmentSize)
      {
         this.nucleotides = nucleotides;
         this.fragmentSize = fragmentSize;
      }
      
      public Map<Fragment, Fragment> get() throws InterruptedException, ExecutionException
      {
         while (!done) try {
            Thread.sleep(100);
         } catch (InterruptedException e) {
            // ignored
         }
         return fragments;
      }

      public void run()
      {
         long dna = 0, bitmask = 0;

         for (int i = 0; i < fragmentSize; i++) {
            bitmask = bitmask << BITS_PER_CHAR | CHAR_BIT_MASK;
         }

         try {
         
            int j = 0;
            byte[] buffer = nucleotides.get(0).get().get(0);

            for (; j < fragmentSize - 1; j++) {
               dna = dna << BITS_PER_CHAR | buffer[j];
            }

            Fragment fragment = new Fragment(fragmentSize);
            Iterator<Encoder> fit = nucleotides.iterator();

            while (fit.hasNext()) {
               Encoder encoder = fit.next();
               Iterator<byte[]> bit = encoder.get().iterator();
            
               while (bit.hasNext()) {
                  buffer = bit.next();
               
                  for (; j < buffer.length; j++) {
                     dna = dna << BITS_PER_CHAR | buffer[j];
                     fragment.value = dna & bitmask;
                     Fragment counter = fragments.get(fragment);
                  
                     if (counter != null) {
                        counter.count++;
                     } else {
                        fragments.put(fragment, fragment);
                        fragment = new Fragment(fragmentSize);
                     }
                  }
                  j = 0;
               }
            }
         } catch (Exception e) {
            e.printStackTrace();
         }
         
         done = true;
      }
   }

   private static class OffsetCounter implements Runnable
   {
      private int offset;
      private int fragmentSize;
      private boolean done = false;
      private List<Encoder> nucleotides;
      private Map<Fragment, Fragment> fragments = new HashMap<Fragment, Fragment>();
      
      private OffsetCounter(List<Encoder> nucleotides, int offset, int fragmentSize)
      {
         this.offset = offset;
         this.nucleotides = nucleotides;
         this.fragmentSize = fragmentSize;
      }
      
      public Map<Fragment, Fragment> get() throws InterruptedException, ExecutionException
      {
         while (!done) try {
            Thread.sleep(100);
         } catch (InterruptedException e) {
            // ignored
         }
         return fragments;
      }

      public void run()
      {
         long bitmask = 0;

         for (int i = 0; i < fragmentSize; i++) {
            bitmask = bitmask << BITS_PER_CHAR | CHAR_BIT_MASK;
         }

         try {
         
            byte[] buffer;
            int j = offset;
            Fragment fragment = new Fragment(fragmentSize);
            Iterator<Encoder> fit = nucleotides.iterator();

            while (fit.hasNext()) {
               Encoder encoder = fit.next();
               Iterator<byte[]> bit = encoder.get().iterator();
            
               while (bit.hasNext()) {
                  buffer = bit.next();
               
                  for (; j < buffer.length - fragmentSize; j+= fragmentSize) {
                     long dna = 0;
                     for (int i = 0; i < fragmentSize; i++) {
                        dna = dna << BITS_PER_CHAR | buffer[j + i];
                     }
                     fragment.value = dna & bitmask;
                     Fragment counter = fragments.get(fragment);
                  
                     if (counter != null) {
                        counter.count++;
                     } else {
                        fragments.put(fragment, fragment);
                        fragment = new Fragment(fragmentSize);
                     }
                  }
                  j -= buffer.length - fragmentSize;
               }
            }
         } catch (Exception e) {
            e.printStackTrace();
         }
         
         done = true;
      }
   }
}
// $Id: lists.java,v 1.1 2004-05-23 07:12:55 bfulgham Exp $
// http://www.bagley.org/~doug/shootout/

import java.io.*;
import java.util.*;
import java.text.*;

public class lists {
    static int SIZE = 10000;

    public static void main(String args[]) {
	int n = Integer.parseInt(args[0]);
	int result = 0;
	for (int i = 0; i < n; i++) {
	    result = test_lists();
	}
	System.out.println(result);
    }
    public static int test_lists() {
	int result = 0;
	// create a list of integers (Li1) from 1 to SIZE
	LinkedList Li1 = new LinkedList();
	for (int i = 1; i < SIZE+1; i++) {
	    Li1.addLast(new Integer(i));
	}
	// copy the list to Li2 (not by individual items)
	LinkedList Li2 = new LinkedList(Li1);
	LinkedList Li3 = new LinkedList();
	// remove each individual item from left side of Li2 and
	// append to right side of Li3 (preserving order)
	while (! Li2.isEmpty()) {
	    Li3.addLast(Li2.removeFirst());
	}
	// Li2 must now be empty
	// remove each individual item from right side of Li3 and
	// append to right side of Li2 (reversing list)
	while (! Li3.isEmpty()) {
	    Li2.addLast(Li3.removeLast());
	}
	// Li3 must now be empty
	// reverse Li1
	LinkedList tmp = new LinkedList();
	while (! Li1.isEmpty()) {
	    tmp.addFirst(Li1.removeFirst());
	}
	Li1 = tmp;
	// check that first item is now SIZE
	if (((Integer)Li1.getFirst()).intValue() != SIZE) {
	    System.err.println("first item of Li1 != SIZE");
	    return(0);
	}
	// compare Li1 and Li2 for equality
	if (! Li1.equals(Li2)) {
	    System.err.println("Li1 and Li2 differ");
	    System.err.println("Li1:" + Li1);
	    System.err.println("Li2:" + Li2);
	    return(0);
	}
	// return the length of the list
	return(Li1.size());
    }
}
// $Id: lists.java-2.java,v 1.1 2004-11-10 06:43:53 bfulgham Exp $
// http://shootout.alioth.debian.org/
// from Stephen Darnell

//import java.io.*;		// XXX Not needed
//import java.util.*;	// XXX Replaced by private version
//import java.text.*;	// XXX Not needed

public class lists {

	// XXX Make SIZE a final static
    final static int SIZE = 10000;

    public static void main(String args[])
    {
		int n = 10;
		if (args.length == 1)
		{
			n = Integer.parseInt(args[0]);
		}

		long start = System.currentTimeMillis();

		int result = 0;
		for (int i = 0; i < n; i++) {
		    result = test_lists();
		}
		long stop = System.currentTimeMillis();

		System.out.println(result);
		// System.out.println("Took "+(stop-start)+" ms");
    }

    public static int test_lists() {
	int result = 0;
	// create a list of integers (Li1) from 1 to SIZE
	LinkedList Li1 = new LinkedList();
	for (int i = 1; i < SIZE+1; i++) {
	    Li1.addLast(new LLEntry(i));
	}

//	System.out.println("Li1 "+Li1.size());

	// copy the list to Li2 (not by individual items)
	LinkedList Li2 = new LinkedList(Li1);
	LinkedList Li3 = new LinkedList();

//	System.out.println("Li2 "+Li2.size()+" Li3 "+Li3.size());

	// remove each individual item from left side of Li2 and
	// append to right side of Li3 (preserving order)
	while (! Li2.isEmpty()) {
	    Li3.addLast(Li2.removeFirst());
	}

//	System.out.println("Li2 "+Li2.size()+" Li3 "+Li3.size());

	// Li2 must now be empty
	// remove each individual item from right side of Li3 and
	// append to right side of Li2 (reversing list)
	while (! Li3.isEmpty()) {
	    Li2.addLast(Li3.removeLast());
	}

//	System.out.println("Li2 "+Li2.size()+" Li3 "+Li3.size());

	// Li3 must now be empty
	// reverse Li1
	LinkedList tmp = new LinkedList();
	while (! Li1.isEmpty()) {
	    tmp.addFirst(Li1.removeFirst());
	}
	Li1 = tmp;
	// check that first item is now SIZE
	if (Li1.getFirst().val != SIZE) {
	    System.err.println("first item of Li1 != SIZE");
	    return(0);
	}
	// compare Li1 and Li2 for equality
	if (! Li1.equals(Li2)) {
	    System.err.println("Li1 and Li2 differ");
	    System.err.println("Li1:" + Li1);
	    System.err.println("Li2:" + Li2);
	    return(0);
	}
	// return the length of the list
	return(Li1.size());
    }
}

class LLEntry
{
	LLEntry prev, next;
	int val;

	LLEntry() { }

	LLEntry(int value) {
		val = value;
	}
}

class LinkedList extends LLEntry
{
	LinkedList()
	{
		next = prev = this;
	}

	LinkedList( LinkedList other )
	{
		this();

		LLEntry last = this;
		for( LLEntry curr = other.next ; curr != other ; curr = curr.next )
		{
			LLEntry entry = new LLEntry(curr.val);
			last.next = entry;
			entry.prev = last;
			last = entry;
		}
		last.next = this;
		this.prev = last;

		this.val = other.val;
	}

	boolean isEmpty()
	{
		return val == 0;
	}

	void addFirst( LLEntry entry )
	{
		entry.prev = this;
		entry.next = this.next;
		this.next.prev = entry;
		this.next = entry;
		this.val++;
	}

	void addLast( LLEntry entry )
	{
		entry.next = this;
		entry.prev = this.prev;
		this.prev.next = entry;
		this.prev = entry;
		this.val++;
	}

	LLEntry removeFirst()
	{
		LLEntry entry = this.next;
		if (entry == this)
			return null;

		this.val--;
		this.next = entry.next;
		entry.next.prev = this;
		return entry;
	}

	LLEntry removeLast()
	{
		LLEntry entry = this.prev;
		if (entry == this)
			return null;

		this.val--;
		this.prev = entry.prev;
		entry.prev.next = this;
		return entry;
	}

	LLEntry getFirst()
	{
		return this.next;
	}

	int size()
	{
// Simple sanity checking code:
//		int n = 0;
//		for( LLEntry curr = this.next; curr != this ; curr = curr.next)
//		{
//			n++;
//		}
//		if (n != this.val)
//			throw new Error("size mismatch");

		return this.val;
	}

	boolean equals(LinkedList other)
	{
		LLEntry myItem = this;
		LLEntry theirItem = other;
		do
		{
			if (myItem.val != theirItem.val)
				return false;
			theirItem = theirItem.next;
			myItem = myItem.next;
		}
		while(myItem != this);
		return true;
	}
}
/* The Computer Language Shootout
   http://shootout.alioth.debian.org/

   benchmark implementation (not optimized)
   JRE 1.5 only
   contributed by Josh Goldfoot */

import java.io.*;
import java.lang.*;
import java.util.*;

public class magicsquares {
    
    static int n;
    static int mn;
    
    public static class FfmResult {
        public int x;
        public int y;
        public int[] moves;
        public FfmResult(int ix, int iy, int[] imoves) {
            x = ix;
            y = iy;
            moves = (int[]) imoves.clone();
        }
    }
    
    public static class PQNode implements Comparable {
        public int [] grid;
        boolean priorityCalculated;
        private int priority;
        private FfmResult ffm;
        
        public PQNode() {
            grid = new int [n * n];
            int i;
            for (i = 0; i < n * n; i++)
                grid[i] = 0;
            priorityCalculated = false;
            ffm = null;
        }
        public PQNode(PQNode other) {
            grid = (int[]) other.grid.clone();
            priorityCalculated = false;
        }

        public int[] gridRow(int y) {
            int[] r = new int[n];
            int x;
            for (x = 0; x < n; x++)
                r[x] = grid[x + y * n];
            return r;
        }

        public int[] gridCol(int x) {
            int[] r = new int[n];
            int y;
            for (y = 0; y < n; y++)
                r[y] = grid[x + y * n];
            return r;
        }

        public int[] diagonal(int q) {
            int[] r = new int[n];
            int i;
            for (i = 0; i < n; i++) {
                if (q == 1)
                    r[i] = grid[i + i * n];
                else if (q == 2) {
                    r[i] = grid[i + (n - 1 - i) * n];
                }
            }
            return r;
        }

        public int[] possibleMoves(int x, int y) {
            int zerocount, sum, highest, j, i;
            
            if (grid[x + y * n] != 0)
                return ( new int [] { });
            ArrayList<int[]> cellGroups = new ArrayList<int[]>();
            cellGroups.add(gridCol(x));
            cellGroups.add(gridRow(y));
            if (x == y)
                cellGroups.add(diagonal(1));
            if (x + y == n - 1)
                cellGroups.add(diagonal(2));
            HashSet<Integer> usedNumbers = new HashSet<Integer>();
            for (i = 0; i < grid.length; i++)
                usedNumbers.add(new Integer(grid[i]));
            HashSet<Integer> onePossible = new HashSet<Integer>();
            highest = n * n;
            for (int[] group: cellGroups) {
                zerocount = 0;
                sum = 0;
                for (int num: group) {
                    if (num == 0)
                        zerocount++;
                    sum += num;
                }
                if (zerocount == 1)
                    onePossible.add(new Integer(mn - sum));
                if (mn - sum < highest)
                    highest = mn - sum;
            }
            if (onePossible.size() == 1) {
                Integer onlyPossibleMove = (Integer) onePossible.iterator().next();
                int opmint = onlyPossibleMove.intValue();
                if (opmint >= 1 && 
                        opmint <= n * n && 
                        ! usedNumbers.contains(onlyPossibleMove))
                    return (new int[] { opmint });
                else
                    return ( new int [] { });
            } else if (onePossible.size() > 1)
                return ( new int [] { });
            ArrayList<Integer> moves = new ArrayList<Integer>();
            for (i = 1; i <= highest; i++) {
                Integer ii = new Integer(i);
                if (! usedNumbers.contains(ii))
                    moves.add(ii);
            }
            int[] r = new int[moves.size()];
            i = 0;
            for (Integer move: moves) {
                r[i++] = move.intValue();
            }
            return r;
        }

        public FfmResult findFewestMoves() {
            if (ffm != null)
                return ffm;
            int x, y, mx, my, ind;
            int[] minmoves = (new int[] { });
            int[] moves;
            mx = my = -1;
            for (y = 0; y < n; y++)
                for (x = 0; x < n; x++) {
                    ind = x + y * n;
                    if (grid[ind] == 0) {
                        moves = possibleMoves(x,y);
                        if (mx == -1 || moves.length < minmoves.length) {
                            mx = x;
                            my = y;
                            minmoves = (int[]) moves.clone();
                        }
                    }
                }
            ffm = new FfmResult(mx, my, minmoves);
            return ffm;
        }
        
        public void calculatePriority()
        {
            int i, zerocount;
            zerocount = 0;
            for (int cell: grid)
                if (cell == 0)
                    zerocount++;
            priority = zerocount + findFewestMoves().moves.length;
            priorityCalculated = true;
        }
        
        public int getPriority()
        {
            if (priorityCalculated)
                return priority;
            else {
                calculatePriority();
                return priority;
            }
        }
        
        public int compareTo(Object anotherMSquare) throws ClassCastException {
            if (!(anotherMSquare instanceof PQNode))
                throw new ClassCastException();
            PQNode other = (PQNode) anotherMSquare;
            int c = getPriority() - other.getPriority();
            if (c == 0) {
                int i = 0;
                while (c == 0 && i < n * n) {
                    c = grid[i] - other.grid[i];
                    i++;
                }
            }
            return c;
        }

        public String toString() {
            StringBuilder sb = new StringBuilder();
            int y, x;
            for (y = 0; y < n; y++) {
                for (x = 0; x < n; x++) {
                    sb.append(grid[x + y * n]);
                    if (x < n-1)
                        sb.append(' ');
                }
                if (y < n-1)
                    sb.append('\n');
            }
            return sb.toString();
        }

        
    }
    
    public magicsquares() { }
    
    public static void main(String[] args) {
        n = 3;
        if (args.length > 0) 
            n = Integer.parseInt(args[0]);
        mn = n * (1 + n * n) / 2;
        PriorityQueue<magicsquares.PQNode> pq = new PriorityQueue<magicsquares.PQNode>(); 
        pq.offer(new magicsquares.PQNode() );
        while (! pq.isEmpty()) {
            PQNode topSquare = pq.poll();
            if (topSquare.getPriority() == 0) {
                System.out.println(topSquare);
                break;
            }
            magicsquares.FfmResult result = topSquare.findFewestMoves();

            int ind = result.x + result.y * n;
            for (int move: result.moves) {
                magicsquares.PQNode newSquare = new magicsquares.PQNode(topSquare);
                newSquare.grid[ind] = move;
                pq.offer(newSquare);
            }
        }
                
    }
}
/* The Computer Language Benchmarks Game
   http://shootout.alioth.debian.org/
   contributed by Stefan Krause
   slightly modified by Chad Whipkey
*/

import java.io.IOException;
import java.io.PrintStream;

class mandelbrot {

   public static void main(String[] args) throws Exception {
       new Mandelbrot(Integer.parseInt(args[0])).compute();
   }

   public static class Mandelbrot {
       private static final int BUFFER_SIZE = 8192;

       public Mandelbrot(int size) {
         this.size = size;
         fac = 2.0 / size;
         out = System.out;
         shift = size % 8 == 0 ? 0 : (8- size % 8);
      }
      final int size;
      final PrintStream out;
      final byte [] buf = new byte[BUFFER_SIZE];
      int bufLen = 0;
      final double fac;
      final int shift;

      public void compute() throws IOException
      {
         out.format("P4\n%d %d\n",size,size);
         for (int y = 0; y<size; y++)
            computeRow(y);
         out.write( buf, 0, bufLen);
         out.close();
      }

      private void computeRow(int y) throws IOException
      {
         int bits = 0;

         final double Ci = (y*fac - 1.0);
          final byte[] bufLocal = buf;
          for (int x = 0; x<size;x++) {
            double Zr = 0.0;
            double Zi = 0.0;
            double Cr = (x*fac - 1.5);
            int i = 50;
            double ZrN = 0;
            double ZiN = 0;
            do {
               Zi = 2.0 * Zr * Zi + Ci;
               Zr = ZrN - ZiN + Cr;
               ZiN = Zi * Zi;
               ZrN = Zr * Zr;
            } while (!(ZiN + ZrN > 4.0) && --i > 0);

            bits = bits << 1;
            if (i == 0) bits++;

            if (x%8 == 7) {
                bufLocal[bufLen++] = (byte) bits;
                if ( bufLen == BUFFER_SIZE) {
                    out.write(bufLocal, 0, BUFFER_SIZE);
                    bufLen = 0;
                }
               bits = 0;
            }
         }
         if (shift!=0) {
            bits = bits << shift;
            bufLocal[bufLen++] = (byte) bits;
            if ( bufLen == BUFFER_SIZE) {
                out.write(bufLocal, 0, BUFFER_SIZE);
                bufLen = 0;
            }
         }
      }
   }
}
/* The Computer Language Benchmarks Game
   http://shootout.alioth.debian.org/
 
   contributed by Stefan Krause
   slightly modified by Chad Whipkey
   parallelized by Colin D Bennett 2008-10-04
   reduce synchronization cost by The Anh Tran
  */

//package mandelbrot;

import java.io.*;
import java.io.IOException;
import java.util.concurrent.atomic.AtomicInteger;

public final class mandelbrot
{
    public static void main(String[] args) throws Exception
    {
        int size = 200;
        if (args.length >= 1)
            size = Integer.parseInt(args[0]);
        
        System.out.format("P4\n%d %d\n", size, size);
        
        int width_bytes = size /8 +1;
        byte[][] output_data = new byte[size][width_bytes];
        int[] bytes_per_line = new int[size];
        
        Compute(size, output_data, bytes_per_line);
        
        BufferedOutputStream ostream = new BufferedOutputStream(System.out);
        for (int i = 0; i < size; i++)
            ostream.write(output_data[i], 0, bytes_per_line[i]);
        ostream.close();
    }
    
    private static final void Compute(final int N, final byte[][] output, final int[] bytes_per_line)
    {
        final double inverse_N = 2.0 / N;
        final AtomicInteger current_line = new AtomicInteger(0);
        
        final Thread[] pool = new Thread[Runtime.getRuntime().availableProcessors()];
        for (int i = 0; i < pool.length; i++)
        {
            pool[i] = new Thread()
            {
                public void run()
                {
                    int y;
                    while ((y = current_line.getAndIncrement()) < N)
                    {
                        byte[] pdata = output[y];
                        
                        int bit_num = 0;
                        int byte_count = 0;
                        int byte_accumulate = 0;
                        
                        double Civ = (double)y * inverse_N - 1.0;
                        for (int x = 0; x < N; x++)
                        {
                            double Crv = (double)x * inverse_N - 1.5;
                            
                            double Zrv = Crv;
                            double Ziv = Civ;
                            
                            double Trv = Crv * Crv;
                            double Tiv = Civ * Civ;
                            
                            int i = 49;
                            do
                            {
                                Ziv = (Zrv * Ziv) + (Zrv * Ziv) + Civ;
                                Zrv = Trv - Tiv + Crv;
                                
                                Trv = Zrv * Zrv;
                                Tiv = Ziv * Ziv;
                            } while ( ((Trv + Tiv) <= 4.0) && (--i > 0));

                            byte_accumulate <<= 1;
                            if (i == 0)
                                byte_accumulate++;
                            
                            if (++bit_num == 8)
                            {
                                pdata[ byte_count++ ] = (byte)byte_accumulate;
                                bit_num = byte_accumulate = 0;
                            }
                        } // end foreach column
                        
                        if (bit_num != 0)
                        {
                            byte_accumulate <<= (8 - (N & 7));
                            pdata[ byte_count++ ] = (byte)byte_accumulate;
                        }
                        
                        bytes_per_line[y] = byte_count;
                    } // end while (y < N)
                } // end void run()
            }; // end inner class definition
            
            pool[i].start();
        }
        
        for (Thread t : pool)
        {
            try
            {
                t.join();
            }
            catch (InterruptedException e)
            {
                e.printStackTrace();
            }
        }
    }
}
/* The Computer Language Benchmarks Game
 * http://shootout.alioth.debian.org/
 * 
 * contributed by Stefan Krause
 * slightly modified by Chad Whipkey
 * parallelized by Colin D Bennett 2008-10-04
 * reduce synchronization cost by The Anh Tran
 * optimizations and refactoring by Enotus 2010-11-11
 */
 

import java.io.*;
import java.util.concurrent.atomic.AtomicInteger;

public final class mandelbrot {
    static byte[][] out;
    static AtomicInteger yCt;
    static double[] Crb;
    static double[] Cib;

    static int getByte(int x, int y){
        double Ci=Cib[y];
        int res=0;
        for(int i=0;i<8;i+=2){
            double Zr1=Crb[x+i];
            double Zi1=Cib[y];

            double Zr2=Crb[x+i+1];
            double Zi2=Cib[y];

            int b=0;
            int j=49;do{
                double nZr1=Zr1*Zr1-Zi1*Zi1+Crb[x+i];
                double nZi1=Zr1*Zi1+Zr1*Zi1+Cib[y];
                Zr1=nZr1;Zi1=nZi1;

                double nZr2=Zr2*Zr2-Zi2*Zi2+Crb[x+i+1];
                double nZi2=Zr2*Zi2+Zr2*Zi2+Cib[y];
                Zr2=nZr2;Zi2=nZi2;

                if(Zr1*Zr1+Zi1*Zi1>4) b|=2;
                if(Zr2*Zr2+Zi2*Zi2>4) b|=1;
                if(b==3) break;
            }while(--j>0);
            res=(res<<2)+b;
        }
        return res^-1;
    }
    
    static void putLine(int y, byte[] line){
        for (int xb=0; xb<line.length; xb++)
            line[xb]=(byte)getByte(xb*8,y);
    }
 
    public static void main(String[] args) throws Exception {
        int N=6000;
        if (args.length>=1) N=Integer.parseInt(args[0]);

        Crb=new double[N+7]; Cib=new double[N+7];
        double invN=2.0/N; for(int i=0;i<N;i++){ Cib[i]=i*invN-1.0; Crb[i]=i*invN-1.5; }
        yCt=new AtomicInteger();
        out=new byte[N][(N+7)/8];

        Thread[] pool=new Thread[2*Runtime.getRuntime().availableProcessors()];
        for (int i=0;i<pool.length;i++)
            pool[i]=new Thread(){
                public void run() {
                     int y; while((y=yCt.getAndIncrement())<out.length) putLine(y,out[y]);
                }
            };
        for (Thread t:pool) t.start();
        for (Thread t:pool) t.join();

        OutputStream stream = new BufferedOutputStream(System.out);
        stream.write(("P4\n"+N+" "+N+"\n").getBytes());
        for(int i=0;i<N;i++) stream.write(out[i]);
        stream.close();
    }
}

// $Id: matrix.java,v 1.1 2004-05-23 07:14:27 bfulgham Exp $
// http://www.bagley.org/~doug/shootout/
// modified to use a little less memory by Thomas Holenstein

import java.io.*;
import java.util.*;

public class matrix {
    static int SIZE = 30;

    public static void main(String args[]) {
	int n = Integer.parseInt(args[0]);
	int m1[][] = mkmatrix(SIZE, SIZE);
	int m2[][] = mkmatrix(SIZE, SIZE);
	int mm[][] = new int[SIZE][SIZE];
	for (int i=0; i<n; i++) {
	    mmult(SIZE, SIZE, m1, m2, mm);
	}
	System.out.print(mm[0][0]);
	System.out.print(" ");
	System.out.print(mm[2][3]);
	System.out.print(" ");
	System.out.print(mm[3][2]);
	System.out.print(" ");
	System.out.println(mm[4][4]);
    }

    public static int[][] mkmatrix (int rows, int cols) {
	int count = 1;
	int m[][] = new int[rows][cols];
	for (int i=0; i<rows; i++) {
	    for (int j=0; j<cols; j++) {
		m[i][j] = count++;
	    }
	}
	return(m);
    }

    public static void mmult (int rows, int cols, 
	                      int[][] m1, int[][] m2, int[][] m3) {
	for (int i=0; i<rows; i++) {
	    for (int j=0; j<cols; j++) {
		int val = 0;
		for (int k=0; k<cols; k++) {
		    val += m1[i][k] * m2[k][j];
		}
		m3[i][j] = val;
	    }
	}
    }
}
/* The Great Computer Language Shootout
   http://shootout.alioth.debian.org/
 
   contributed by James McIlree
*/


import java.util.*;

public class message {
   public static final int numberOfThreads = 3000;
   public static int numberOfMessagesToSend;

   public static void main(String args[]) {
     numberOfMessagesToSend = Integer.parseInt(args[0]);

     MessageThread chain = null;
     for (int i=0; i<numberOfThreads; i++){
       chain = new MessageThread(chain);
       new Thread(chain).start();
     }

     for (int i=0; i<numberOfMessagesToSend; i++) chain.enqueue(new Integer(0));
   }
}

class MessageThread implements Runnable {
   MessageThread nextThread;
   List list = new ArrayList(4);

   MessageThread(MessageThread nextThread){
     this.nextThread = nextThread;
   }

   public void run() {
     if (nextThread != null)
       while (true) nextThread.enqueue(dequeue());
     else {
       int sum = 0;
       int finalSum = message.numberOfThreads * message.numberOfMessagesToSend;
       while (sum < finalSum)
         sum += dequeue().intValue();

       System.out.println(sum);
       System.exit(0);
     }
   }

   public void enqueue(Integer message)
   {
     synchronized(list) {
       list.add(new Integer(message.intValue() + 1));
       if (list.size() == 1) {
         list.notify();
       }
     }
   }

   public Integer dequeue()
   {
     synchronized(list) {
       while(list.size() == 0) {
         try { list.wait(); } catch (Exception e) {}
       }
       return (Integer)list.remove(0);
     }
   }
}
/* The Computer Language Benchmarks Game
 http://shootout.alioth.debian.org/

 contributed by Mattias Bergander
 */

import java.util.LinkedList;
import java.util.List;

public class message {
    public static final int numberOfThreads = 500;

    public static int numberOfMessagesToSend;

    public static void main(String args[]) {
        numberOfMessagesToSend = Integer.parseInt(args[0]);

        MessageThread chain = null;
        for (int i = 0; i < numberOfThreads; i++) {
            chain = new MessageThread(chain);
            new Thread(chain).start();
        }

        for (int i = 0; i < numberOfMessagesToSend; i++) {
            chain.enqueue(new MutableInteger(0));
        }

    }
}

class MutableInteger {
    int value;

    public MutableInteger() {
        this(0);
    }

    public MutableInteger(int value) {
        this.value = value;
    }

    public MutableInteger increment() {
        value++;
        return this;
    }

    public int intValue() {
        return value;
    }
}

class MessageThread implements Runnable {
    MessageThread nextThread;

    List<MutableInteger> list = new LinkedList<MutableInteger>();

    MessageThread(MessageThread nextThread) {
        this.nextThread = nextThread;
    }

    public void run() {
        if (nextThread != null) {
            while (true) {
                nextThread.enqueue(dequeue());
            }
        } else {
            int sum = 0;
            int finalSum = message.numberOfThreads * message.numberOfMessagesToSend;
            while (sum < finalSum) {
                sum += dequeue().intValue();
            }
            System.out.println(sum);
            System.exit(0);
        }
    }

    /**
     * @param message
     */
    public void enqueue(MutableInteger message) {
        synchronized (list) {
            list.add(message);
            if (list.size() == 1) {
                list.notify();
            }
        }
    }

    public MutableInteger dequeue() {
        synchronized (list) {
            while (list.size() == 0) {
                try {
                    list.wait();
                } catch (InterruptedException e) {
                }
            }
            return list.remove(0).increment();
        }
    }
}
/* The Great Computer Language Shootout
   http://shootout.alioth.debian.org/ 
 
   contributed by James McIlree
*/


import java.util.*;

public class message {
   public static final int numberOfThreads = 500;
   public static int numberOfMessagesToSend;

   public static void main(String args[]) {
     numberOfMessagesToSend = Integer.parseInt(args[0]);

     MessageThread chain = null;
     for (int i=0; i<numberOfThreads; i++){
       chain = new MessageThread(chain);
       new Thread(chain).start();
     }

     for (int i=0; i<numberOfMessagesToSend; i++) chain.enqueue(new Integer(0));
   }
}

class MessageThread implements Runnable {
   MessageThread nextThread;
   List list = new ArrayList(4);

   MessageThread(MessageThread nextThread){
     this.nextThread = nextThread;
   }

   public void run() {
     if (nextThread != null)
       while (true) nextThread.enqueue(dequeue());
     else {
       int sum = 0;
       int finalSum = message.numberOfThreads * message.numberOfMessagesToSend;
       while (sum < finalSum)
         sum += dequeue().intValue();

       System.out.println(sum);
       System.exit(0);
     }
   }

   public void enqueue(Integer message)
   {
     synchronized(list) {
       list.add(new Integer(message.intValue() + 1));
       if (list.size() == 1) {
         list.notify();
       }
     }
   }

   public Integer dequeue()
   {
     synchronized(list) {
       while(list.size() == 0) {
         try { list.wait(); } catch (Exception e) {}
       }
       return (Integer)list.remove(0);
     }
   }
}
/* 
 The Computer Language Shootout
 http://shootout.alioth.debian.org/
 #303304
 contributed by tt@kyon.de
 */

public final class message extends Thread {

	private static final int THREADS = 500;
	private static int msgCount;
	private static int max;
	private final message nextThread;
	private int[] messages = new int[1024]; // reasonably sized buffer
	private int todo;

	public static void main(String args[]) {
		msgCount = Integer.parseInt(args[0]);
		max = msgCount * THREADS;
		message thread = null;
		for (int i = THREADS; --i >= 0;) {
			(thread = new message(thread)).start();
		}
		for (int i = msgCount; --i >= 0;) {
			thread.send(0);
		}
	}
	private message(message next) {
		nextThread = next;
	}
	public void run() {
		try {
			for (;;) {
				synchronized (this) {
					if (todo != 0) {
						break;
					}
				}
				yield();
			}
			if (nextThread != null) {
				pass();
			} else {
				add();
			}
		} catch (Exception e) {
			e.printStackTrace();
		}
	}
	private void pass() throws Exception {
		for (;;) {
			synchronized (this) {
				int done = todo;
				int[] m = messages;
				do {
					nextThread.send(m[--done] + 1);
				} while (done != 0);
				todo = 0;
			}
			while (todo == 0) {
				// no unsynchronized todos left
				yield();
			}
		}
	}
	private void add() throws Exception {
		int sum = 0;
		for (;;) {
			synchronized (this) {
				int done = todo;
				int[] m = messages;
				do {
					sum += m[--done] + 1;
				} while (done != 0);
				todo = 0;
			}
			while (todo == 0) {
				// no unsynchronized todos left
				if (sum == max) {
					System.out.println(sum);
					System.exit(0);
				}
				yield();
			}
		}
	}
	private synchronized void send(int message) {
		int[] m = messages;
		int l = m.length;
		if(todo == l) {
			int[] n = new int[l << 2];
			System.arraycopy(m, 0, n, 0, l);
			messages = m = n;
		}
		m[todo++] = message;
	}
}
/* The Computer Language Benchmarks Game
   http://shootout.alioth.debian.org/
   contributed by Graham Miller 
*/

public class message {
   public static final int numberOfThreads = 500;

   public static int numberOfMessagesToSend;

   public static void main(String args[]) {
      numberOfMessagesToSend = Integer.parseInt(args[0]);

      RingBufferThread chain = null;
      for (int i = 0; i < numberOfThreads; i++) {
         chain = new RingBufferThread(chain, numberOfMessagesToSend*(numberOfThreads));
         chain.start();
       }


      for (int i = 0; i < numberOfMessagesToSend; i++) {
         chain.enqueue(0);
      }
      chain.signalDoneSendingMessages();
    }

   public static class RingBufferThread extends Thread {

      private static final int RING_BUFFER_CAPACITY = 100;
         
      private volatile RingNode loadNode;
      private volatile RingNode consumeNode;

      RingBufferThread nextThread;
      private volatile boolean done = false;
      private final int finalSum;

      
      RingBufferThread(RingBufferThread nextThread, int finalSum) {
         RingNode node = new RingNode();
         RingNode tail = node;
         for (int i = 0; i < RING_BUFFER_CAPACITY-1; i ++){
            RingNode newNode = new RingNode();
            newNode.next = node;
            node = newNode;
         }
         // complete the ring
         tail.next = node;
         
         // both load and consume start at the same node
         loadNode = node;
         consumeNode = node;
         
         this.nextThread = nextThread;
         this.finalSum = finalSum;
      }

      public void run() {
         if (nextThread != null) {
            while (!done || !isEmpty()) {
               nextThread.enqueue(dequeue());
            }
            nextThread.signalDoneSendingMessages();
         } else {
            int sum = 0;
            while (sum < finalSum) {
               int message = dequeue();
               sum += message;
            }
            System.out.println(sum);
            System.exit(0);
         }
      }


      private boolean isEmpty() {
         return consumeNode == loadNode;
      }

      /**
       * @param message
       */
      public final void enqueue(int message) {
         // after this test becomes false, and the loop exits
         // the removal of an element by the "other" thread
         // cannot make it true again, so therefore it is invariant
         // for the rest of the execution of this method.
         // that is once we have some free space, we will always
         // have free space until the thread calling this method
         // adds an element.
         do { /* nothing */ } while (loadNode.next == consumeNode && trueYield());

         loadNode.message = message;
         loadNode = loadNode.next;
      }

      public final int dequeue() {
         // after this test becomes false, and the loop exits
         // the addition of an element by the "other" thread
         // cannot make it true again, therefore it is invariant
         // for the rest of the execution of this method
         // that is once we have at least one element, we will always
         // have at least one element until the thread calling this
         // method removes one.
         do { /* nothing */ } while (loadNode == consumeNode && trueYield());

         int message = 1 + consumeNode.message;
         consumeNode = consumeNode.next;
         
          return message;
      }

      public final void signalDoneSendingMessages() {
         // once done is true, I am the only
         // thread accessing any of my variables, so we have no
         // more threading issues
         done = true;
      }
      
      private final boolean trueYield() {
         Thread.yield();
         return true;
      }
   }
   
   static class RingNode {
      public volatile int message;
      public RingNode next;
   }
}
/**
 * The Computer Language Benchmarks Game
 * http://shootout.alioth.debian.org/
 * contributed by James Ahlborn
 */

import java.util.*;

public class message {

   public static final int numberOfThreads = 500;
   public static int numberOfMessagesToSend;

  public static void main(String args[]) {
    numberOfMessagesToSend = Integer.parseInt(args[0]);

    MessageThread chain = null;
    for (int i=0; i<numberOfThreads; i++){
      chain = new MessageThread(chain);
    }

    for (int i=0; i<numberOfMessagesToSend; i++)
      chain.enqueue(Integer.valueOf(0));
  }


  public static class MessageThread extends Thread {
    MessageThread nextThread;
    List<Integer> list = new ArrayList<Integer>(16);
    boolean started;
    
    MessageThread(MessageThread nextThread){
      this.nextThread = nextThread;
    }

    public void run() {
      if (nextThread != null)
        while (true) nextThread.enqueue(dequeue());
      else {
        int sum = 0;
        int finalSum = numberOfThreads * numberOfMessagesToSend;
        while (sum < finalSum)
          sum += dequeue().intValue();

        System.out.println(sum);
        System.exit(0);
      }
    }

    public void enqueue(Integer message)
    {
      if(!started) {
        start();
        started = true;
      }

      Integer newValue = Integer.valueOf(message.intValue() + 1);
      synchronized(list) {
        list.add(newValue);
        if (list.size() == 1) {
          list.notify();
        }
      }
    }

    public Integer dequeue()
    {
      synchronized(list) {
        while(list.size() == 0) {
          try { list.wait(); } catch (Exception e) {}
        }
        return list.remove(0);
      }
    }
  
  }
  
}
/**
 * The Computer Language Benchmarks Game
 * http://shootout.alioth.debian.org/
 * contributed by James Ahlborn.
 * Optimized by Klaus Friedel
 */

import java.util.LinkedList;
import java.util.List;

public class message {

  public static long startTime;
  public static final int numberOfThreads = 500;
  public static int numberOfMessagesToSend;

  public static void main(String args[]) {
    //startTime = System.currentTimeMillis();
    numberOfMessagesToSend = Integer.parseInt(args[0]);

    MessageThread chain = null;
    for (int i = 0; i < numberOfThreads; i++) {
      chain = new MessageThread(chain);
    }

    for (int i = 0; i < numberOfMessagesToSend; i++)
      chain.enqueue(0);

    try {
      MessageThread t = chain;
      while(t != null){
        t.join();
        t = t.nextThread;
      }
    } catch (InterruptedException e) {
      e.printStackTrace();
    }
  }


  public static class MessageThread extends Thread {
    MessageThread nextThread;
    List<Integer> list = new LinkedList<Integer>();
    boolean started;
    private static final int BUSY_RETRY_COUNT = 50;

    MessageThread(MessageThread nextThread) {
      this.nextThread = nextThread;
    }

    public void run() {
      if (nextThread != null)
        while (true) nextThread.enqueue(dequeue());
      else {
        int sum = 0;
        final int finalSum = numberOfThreads * numberOfMessagesToSend;
        while (sum < finalSum)
          sum += dequeue();

        System.out.println(sum);
        //long end = System.currentTimeMillis();
        //System.out.printf("Time: %.2fs\n", (end - startTime)/1000.0);
        System.exit(0);
      }
    }

    public void enqueue(Integer message) {
      if (!started) {
        start();
        started = true;
      }

      Integer newValue = message + 1;
      synchronized (list) {
        list.add(newValue);
        if (list.size() == 1) {
          list.notify();
        }
      }
    }

    public Integer dequeue() {
      synchronized (list) {
        try {
          while (list.size() == 0) {
            list.wait();
          }
        } catch (Exception e) {
        }
        return list.remove(0);
      }
    }

  }

}
/* The Computer Language Shootout
   http://shootout.alioth.debian.org/

   contributed by Tony Seebregts
   modified by 
*/

import java.util.ArrayList;
import java.util.Date;
import java.util.SortedSet;
import java.util.TreeSet;

/** First hack at a Java solver for the meteor puzzle - just the IBM 
  * developerWorks article algorithm optimized with precalculated shapes 
  * and bitmasks. Should be possible to optimize it some more to take 
  * advantage of reflections but its turning out to be less obvious 
  * than expected :-).
  * <p>
  * Notes:
  * <ul>
  * <li>Seems to run faster without the -server switch.
  * <li>Testing for islands seems to be slower than just fitting pieces.
  * </ul>
  * 
  * @author Tony Seebregts
  *
  */
  
public class meteor
   { // CONSTANTS
       
     private static final int[]    SHIFT = { 0,6,11,17,22,28,33,39,44,50 };
     private static final long[][] MASK  = { { 0x01L,      0x02L,      0x04L,      0x08L,      0x10L   },
                     { 0x01L << 6, 0x02L << 6, 0x04L << 6, 0x08L <<  6,0x10L << 6  },
                     { 0x01L << 11,0x02L << 11,0x04L << 11,0x08L << 11,0x10L << 11 },
                     { 0x01L << 17,0x02L << 17,0x04L << 17,0x08L << 17,0x10L << 17 },
                     { 0x01L << 22,0x02L << 22,0x04L << 22,0x08L << 22,0x10L << 22 },
                     { 0x01L << 28,0x02L << 28,0x04L << 28,0x08L << 28,0x10L << 28 },
                     { 0x01L << 33,0x02L << 33,0x04L << 33,0x08L << 33,0x10L << 33 },
                     { 0x01L << 39,0x02L << 39,0x04L << 39,0x08L << 39,0x10L << 39 },
                     { 0x01L << 44,0x02L << 44,0x04L << 44,0x08L << 44,0x10L << 44 },
                     { 0x01L << 50,0x02L << 50,0x04L << 50,0x08L << 50,0x10L << 50 }
                       };
     
     private static final boolean DEBUG = false;

     // CLASS VARIABLES
     
     // INSTANCE VARIABLES
     
     private SortedSet<String> solutions = new TreeSet<String>();
     private Entry[]       solution  = new Entry[10];
     private int       depth     = 0;
     private Piece[]       pieces    = { new Piece(PIECE0),
                     new Piece(PIECE1),
                     new Piece(PIECE2),
                     new Piece(PIECE3),
                     new Piece(PIECE4),
                     new Piece(PIECE5),
                     new Piece(PIECE6),
                     new Piece(PIECE7),
                     new Piece(PIECE8),
                     new Piece(PIECE9)
                       };
       
     // CLASS METHODS

     /** Application entry point.
       * 
       * @param args  Command line arguments:
       *      <ul>
       *      <li> solution limit
       *      </ul>
       */
     
     public static void main(String[] args)
        { int N = 2098;
        
          // ... parse command line arguments
        
          if (args.length > 0)
         if (args[0].matches("\\d+"))
            N = Integer.parseInt(args[0]);
            
          // ... solve puzzle
          
          meteor        puzzle = new meteor ();
          Date      start;
          Date      end;
          long      time;
          SortedSet<String> solutions;
          
          start     = new Date();
          solutions = puzzle.solve();
          end   = new Date();
          time      = end.getTime() - start.getTime();      
          
          // ... print result
            
          if (solutions.size() > N)
         System.out.println("ERROR");
         else if (solutions.size() < N)
         System.out.println("TIMEOUT");
         else
         { if (DEBUG)
              { System.out.println("START    : " + start);
            System.out.println("END      : " + end);
            System.out.println("TIME     : " + time);
            System.out.println("SOLUTIONS: " + solutions.size ());
            System.out.println("FIRST    : " + solutions.first());
            System.out.println("LAST     : " + solutions.last ());
            System.out.println();
              }
           
           System.out.print(solutions.size () + " solutions found\n\n");
           print(solutions.first());
           System.out.print("\n");
           print(solutions.last ());
           System.out.print("\n");
         }
        }

     /** Prints out the puzzle.
       * 
       * 
       */
    
     private static void print (String solution)
         { System.out.print(solution.replaceAll("(\\d{5})(\\d{5})","$1 $2")
                    .replaceAll("(\\d{5})","$1\n")
                    .replaceAll("(\\d)","$1 "));
         }

     // CONSTRUCTORS
     
     /** Initialises the puzzle.
       * 
       */

     public meteor ()
        { for (int i=0; i<10; i++)
          solution[i] = new Entry();
        }
     
     // INSTANCE METHODS
     
     /** Initialises the puzzle and solution set at [0,0]
       *
       * @return Sorted list of solution strings.
       */ 
     
     private SortedSet<String> solve()
         { solve(0x0002004008010020L,0,0);
         
           return solutions;
         }
     
     /** Recursively solves the puzzle by fitting pieces into the 
       * next available hexagon.
       * 
       * @param puzzle  Current puzzle bitmask.
       * @param row     Row of next available hexagon. 
       * @param col     Column next available hexagon. 
       * 
       */
      
     private void solve (long puzzle,int row,int col)
         { for (int ix=0; ix<pieces.length; ix++)
           { Piece   piece;
             Shape[] list;
 
             // ... find shapes that fit
             
             if ((piece = pieces[ix]) == null)
            continue;
            else
            list  = pieces[ix].shapes(row,col);
               
             for (Shape shape: list)
             { // ... fits badly ?
          
               if ((shape.bitmap & puzzle) != 0)
                  continue;
               
               // ... try piece in puzzle
 
               long clone = puzzle | shape.bitmap;
 
               // ... find next position
                
               int irow = row;
               int icol = col/2 + 1;
                
               next:
               while (irow < 10)
                 { while (icol < 5)
                     { if ((clone & MASK[irow][icol]) == 0)
                      break next;
                              
                       icol++;
                     }
                         
                   irow++;
                   icol = 0;
                 }
                 
               // ... solve next
               
               Entry entry;
                 
               pieces[ix]  = null;
               entry   = solution[depth++];
               entry.row   = row;
               entry.col   = col;
               entry.shape = shape;
 
               if (depth == 10)
                  solutions.add(serialize(solution));
                  else
                  solve (clone,irow,2*icol + (irow % 2));
                
               depth--;
               pieces[ix] = piece;
             }
           }
         }
      
     /** Serializes the current solution to a string.
       * 
       */
      
     private String serialize (Entry[] solution)
         { char[] puzzle = new char[50];
           Shape   shape;
           int     row;
           int     col;
           
           for (Entry entry: solution)
           { shape = entry.shape;
             row   = entry.row;
             col   = entry.col;
             
             for (int[] xy: shape.vector)
             puzzle[5 * (row + xy[0]) + (col + xy[1])/2] = shape.symbol;
           }
      
           return new String(puzzle);
         }
    
     // INNER CLASSES
     
     /** Container class for a solution set entry.
       * 
       */
     
     private static class Entry
         { public int   row;
           public int   col;
           public Shape shape; 
         }
     
     /** Container class for the shapes for a single puzzle piece.
       * 
       * 
       */
     
     private static class Piece
         { private Shape[][][] shapes = new Shape[10][10][];
         
           @SuppressWarnings("unchecked")
           private Piece (Shape[] list)
               { // ... initialise
               
             ArrayList[][] array = new ArrayList[10][10];
             
             for (int i=0; i<10; i++)
                 for (int j=0; j<10; j++)
                 array[i][j] = new ArrayList<Shape>();
             
             // ... generate list
             
             for (Shape mutant: list)
                 for (int row=0; row<=mutant.maxRow; row++)
                 for (int col=mutant.minCol; col<=mutant.maxCol; col++)
                     { if (!mutant.islet)
                      array[row][col].add(new Shape(mutant,row,col));
                      else if ((row != 0) || (col != 0))
                      array[row][col].add(new Shape(mutant,row,col));
                     }
             
             for (int row=0; row<10; row++)
                 for (int col=0; col<10; col++)
                 shapes[row][col] = (Shape[]) array[row][col].toArray(new Shape[0]);
               }
           
           @SuppressWarnings("unchecked")
           private Shape[] shapes(int row,int col)
               { return shapes[row][col];
               }
         
         }

     /** Container class for the shape vector and bitmap single puzzle piece mutation.
       * 
       * 
       */
     
     private static class Shape
        { private char    symbol;
          private int[][] vector;
          private long    bitmap;
          private int     shift;
          
          private boolean islet;
          private int     maxRow;
          private int     minCol;
          private int     maxCol;
          
          private Shape (char    symbol,
                 int[][] vector,
                 long    bitmap,
                 int     shift,
                 boolean islet,
                 int     maxRow,
                 int     minCol,
                 int     maxCol)
              { this.symbol  = symbol;
            this.vector  = vector;
            this.bitmap  = bitmap;
            this.shift   = shift;
            
            this.islet   = islet;
            this.maxRow  = maxRow;
            this.minCol  = minCol;
            this.maxCol  = maxCol;
              }
          
          private Shape (Shape shape,
                 int   row,
                 int   col)
              { this.symbol  = shape.symbol;
            this.vector  = shape.vector;
            this.bitmap  = shape.bitmap << ((SHIFT[row] + (col - (row % 2))/2) - shape.shift);
            
            this.islet   = shape.islet;
            this.maxRow  = shape.maxRow;
            this.minCol  = shape.minCol;
            this.maxCol  = shape.maxCol;
              }
        }
     
     // PIECES

     private static final Shape[] PIECE0 = { new Shape ('0',new int[][] {{3, 5},{2, 4},{1, 3},{0, 2},{0, 0}},0x0000000000082083L,0,false,6,0,4),
                     new Shape ('0',new int[][] {{4,-2},{3,-1},{2, 0},{1, 1},{0, 0}},0x0000000000421082L,1,false,5,2,8),
                     new Shape ('0',new int[][] {{1,-7},{1,-5},{1,-3},{1,-1},{0, 0}},0x00000000000003D0L,4,false,8,7,9),
                     new Shape ('0',new int[][] {{0, 0},{1, 1},{2, 2},{3, 3},{3, 5}},0x00000000000C1041L,0,false,6,0,4),
                     new Shape ('0',new int[][] {{0, 0},{1,-1},{2,-2},{3,-3},{4,-2}},0x0000000000821084L,2,false,5,3,9),
                     new Shape ('0',new int[][] {{0, 6},{0, 4},{0, 2},{0, 0},{1,-1}},0x000000000000005EL,1,false,8,1,3),
                     new Shape ('0',new int[][] {{0, 0},{1, 1},{2, 2},{3, 3},{4, 2}},0x0000000000841041L,0,false,5,0,6),
                     new Shape ('0',new int[][] {{0, 0},{1,-1},{2,-2},{3,-3},{3,-5}},0x0000000000062108L,3,false,6,5,9),
                     new Shape ('0',new int[][] {{1, 7},{1, 5},{1, 3},{1, 1},{0, 0}},0x00000000000003C1L,0,false,8,0,2),
                     new Shape ('0',new int[][] {{4, 2},{3, 1},{2, 0},{1,-1},{0, 0}},0x0000000001041042L,1,false,5,1,7),
                     new Shape ('0',new int[][] {{3,-3},{2,-2},{1,-1},{0, 0},{0, 2}},0x000000000002108CL,2,false,6,3,7),
                     new Shape ('0',new int[][] {{0, 0},{0, 2},{0, 4},{0, 6},{1, 7}},0x000000000000020FL,0,false,8,0,2)
                       };

     private static final Shape[] PIECE1 = { new Shape ('1',new int[][] {{0, 2},{0, 0},{1,-1},{2, 0},{3,-1}},0x0000000000021046L,1,false,6,1,7),
                     new Shape ('1',new int[][] {{1, 3},{0, 2},{0, 0},{1,-1},{1,-3}},0x00000000000002CCL,2,false,8,3,6),
                     new Shape ('1',new int[][] {{3, 3},{2, 4},{1, 3},{1, 1},{0, 0}},0x00000000000420C1L,0,false,6,0,5),
                     new Shape ('1',new int[][] {{3,-3},{3,-1},{2, 0},{1,-1},{0, 0}},0x0000000000062084L,2,false,6,3,9),
                     new Shape ('1',new int[][] {{0, 0},{1, 1},{1, 3},{0, 4},{0, 6}},0x00000000000000CDL,0,true, 8,0,3),
                     new Shape ('1',new int[][] {{0, 0},{1,-1},{2, 0},{2, 2},{3, 3}},0x0000000000083042L,1,false,6,1,6),
                     new Shape ('1',new int[][] {{0, 6},{1, 5},{1, 3},{0, 2},{0, 0}},0x000000000000018BL,0,true, 8,0,3),
                     new Shape ('1',new int[][] {{3, 3},{3, 1},{2, 0},{1, 1},{0, 0}},0x0000000000060841L,0,false,6,0,6),
                     new Shape ('1',new int[][] {{3,-3},{2,-4},{1,-3},{1,-1},{0, 0}},0x00000000000208C4L,2,false,6,4,9),
                     new Shape ('1',new int[][] {{1,-1},{0, 0},{0, 2},{1, 3},{1, 5}},0x0000000000000346L,1,false,8,1,4),
                     new Shape ('1',new int[][] {{0, 0},{0, 2},{1, 3},{2, 2},{3, 3}},0x0000000000041083L,0,false,6,0,6),
                     new Shape ('1',new int[][] {{0, 0},{1, 1},{2, 0},{2,-2},{3,-3}},0x0000000000023104L,2,false,6,3,8)
                       };

     private static final Shape[] PIECE2 = { new Shape ('2',new int[][] {{1, 1},{0, 0},{2, 0},{2,-2},{2,-4}},0x0000000000003904L,2,false,7,4,8),
                     new Shape ('2',new int[][] {{2, 4},{1, 5},{2, 2},{1, 1},{0, 0}},0x0000000000003141L,0,false,7,0,4),
                     new Shape ('2',new int[][] {{3,-1},{3, 1},{2,-2},{1,-1},{0, 0}},0x0000000000060842L,1,false,6,2,8),
                     new Shape ('2',new int[][] {{1,-1},{2, 0},{0, 0},{0, 2},{0, 4}},0x000000000000104EL,1,false,7,1,5),
                     new Shape ('2',new int[][] {{0, 0},{1,-1},{0, 2},{1, 3},{2, 4}},0x0000000000004146L,1,false,7,1,5),
                     new Shape ('2',new int[][] {{0, 2},{0, 0},{1, 3},{2, 2},{3, 1}},0x0000000000021083L,0,true, 6,0,6),
                     new Shape ('2',new int[][] {{0, 2},{1, 3},{0, 0},{1,-1},{2,-2}},0x0000000000000946L,1,false,7,2,6),
                     new Shape ('2',new int[][] {{1, 5},{2, 4},{0, 4},{0, 2},{0, 0}},0x0000000000002107L,0,false,7,0,4),
                     new Shape ('2',new int[][] {{3, 1},{3,-1},{2, 2},{1, 1},{0, 0}},0x0000000000062082L,1,false,6,1,7),
                     new Shape ('2',new int[][] {{2,-4},{1,-5},{2,-2},{1,-1},{0, 0}},0x0000000000003148L,3,false,7,5,9),
                     new Shape ('2',new int[][] {{1,-1},{0, 0},{2, 0},{2, 2},{2, 4}},0x0000000000007042L,1,false,7,1,5),
                     new Shape ('2',new int[][] {{0, 0},{0, 2},{1,-1},{2, 0},{3, 1}},0x0000000000041046L,1,false,6,1,7)
                       };

     private static final Shape[] PIECE3 = { new Shape ('3',new int[][] {{0, 0},{2, 0},{1,-1},{2,-2},{2,-4}},0x0000000000003884L,2,false,7,4,9),
                     new Shape ('3',new int[][] {{1, 5},{2, 2},{1, 3},{1, 1},{0, 0}},0x00000000000011C1L,0,false,7,0,4),
                     new Shape ('3',new int[][] {{3, 1},{2,-2},{2, 0},{1,-1},{0, 0}},0x0000000000041842L,1,false,6,2,8),
                     new Shape ('3',new int[][] {{2, 0},{0, 0},{1, 1},{0, 2},{0, 4}},0x0000000000000847L,0,false,7,0,5),
                     new Shape ('3',new int[][] {{1,-3},{0, 0},{1,-1},{1, 1},{2, 2}},0x00000000000041C4L,2,false,7,3,7),
                     new Shape ('3',new int[][] {{0, 0},{1, 3},{1, 1},{2, 2},{3, 1}},0x00000000000210C1L,0,true, 6,0,6),
                     new Shape ('3',new int[][] {{1, 3},{0, 0},{1, 1},{1,-1},{2,-2}},0x00000000000009C2L,1,false,7,2,6),
                     new Shape ('3',new int[][] {{2, 4},{0, 4},{1, 3},{0, 2},{0, 0}},0x0000000000002087L,0,false,7,0,5),
                     new Shape ('3',new int[][] {{3,-1},{2, 2},{2, 0},{1, 1},{0, 0}},0x0000000000023082L,1,false,6,1,7),
                     new Shape ('3',new int[][] {{1,-5},{2,-2},{1,-3},{1,-1},{0, 0}},0x00000000000021C8L,3,false,7,5,9),
                     new Shape ('3',new int[][] {{0, 0},{2, 0},{1, 1},{2, 2},{2, 4}},0x0000000000003841L,0,false,7,0,5),
                     new Shape ('3',new int[][] {{0, 0},{1,-3},{1,-1},{2,-2},{3,-1}},0x00000000000410C4L,2,false,6,3,9)
                       };

     private static final Shape[] PIECE4 = { new Shape ('4',new int[][] {{1, 5},{2, 2},{1, 3},{0, 2},{0, 0}},0x0000000000001183L,0,false,7,0,4),
                     new Shape ('4',new int[][] {{3, 1},{2,-2},{2, 0},{1, 1},{0, 0}},0x0000000000041882L,1,false,6,2,8),
                     new Shape ('4',new int[][] {{2, 0},{0, 0},{1, 1},{1, 3},{0, 4}},0x00000000000008C5L,0,true, 7,0,5),
                     new Shape ('4',new int[][] {{1,-3},{0, 0},{1,-1},{2, 0},{2, 2}},0x00000000000060C4L,2,false,7,3,7),
                     new Shape ('4',new int[][] {{0, 0},{1, 3},{1, 1},{2, 0},{3, 1}},0x00000000000208C1L,0,false,6,0,6),
                     new Shape ('4',new int[][] {{0, 0},{2, 0},{1,-1},{1,-3},{2,-4}},0x00000000000028C4L,2,false,7,4,9),
                     new Shape ('4',new int[][] {{0, 0},{1,-3},{1,-1},{2, 0},{3,-1}},0x00000000000420C4L,2,false,6,3,9),
                     new Shape ('4',new int[][] {{1, 3},{0, 0},{1, 1},{2, 0},{2,-2}},0x0000000000001982L,1,false,7,2,6),
                     new Shape ('4',new int[][] {{2, 4},{0, 4},{1, 3},{1, 1},{0, 0}},0x00000000000020C5L,0,true, 7,0,5),
                     new Shape ('4',new int[][] {{3,-1},{2, 2},{2, 0},{1,-1},{0, 0}},0x0000000000023042L,1,false,6,1,7),
                     new Shape ('4',new int[][] {{1,-3},{2, 0},{1,-1},{0, 0},{0, 2}},0x00000000000020CCL,2,false,7,3,7),
                     new Shape ('4',new int[][] {{0, 0},{2, 0},{1, 1},{1, 3},{2, 4}},0x00000000000028C1L,0,false,7,0,5)
                       };

     private static final Shape[] PIECE5 = { new Shape ('5',new int[][] {{0, 2},{1, 1},{0, 0},{1,-1},{2,-2}},0x00000000000008C6L,1,false,7,2,7),
                     new Shape ('5',new int[][] {{1, 5},{1, 3},{0, 4},{0, 2},{0, 0}},0x0000000000000187L,0,false,8,0,4),
                     new Shape ('5',new int[][] {{3, 1},{2, 0},{2, 2},{1, 1},{0, 0}},0x0000000000021841L,0,false,6,0,7),
                     new Shape ('5',new int[][] {{2,-4},{1,-3},{2,-2},{1,-1},{0, 0}},0x00000000000018C4L,2,false,7,4,9),
                     new Shape ('5',new int[][] {{0, 0},{0, 2},{1, 1},{1, 3},{1, 5}},0x00000000000001C3L,0,false,8,0,4),
                     new Shape ('5',new int[][] {{0, 0},{1, 1},{1,-1},{2, 0},{3, 1}},0x00000000000410C2L,1,false,6,1,8),
                     new Shape ('5',new int[][] {{0, 2},{0, 0},{1, 1},{1,-1},{1,-3}},0x00000000000001CCL,2,false,8,3,7),
                     new Shape ('5',new int[][] {{2, 4},{1, 3},{2, 2},{1, 1},{0, 0}},0x00000000000030C1L,0,false,7,0,5),
                     new Shape ('5',new int[][] {{3,-1},{2, 0},{2,-2},{1,-1},{0, 0}},0x0000000000021842L,1,false,6,2,9),
                     new Shape ('5',new int[][] {{1,-1},{1, 1},{0, 0},{0, 2},{0, 4}},0x00000000000000CEL,1,false,8,1,5),
                     new Shape ('5',new int[][] {{0, 0},{1, 1},{0, 2},{1, 3},{2, 4}},0x00000000000020C3L,0,false,7,0,5),
                     new Shape ('5',new int[][] {{0, 0},{1,-1},{1, 1},{2, 0},{3,-1}},0x00000000000210C2L,1,false,6,1,8)
                       };

     private static final Shape[] PIECE6 = { new Shape ('6',new int[][] {{1, 1},{0, 0},{1,-1},{1,-3},{2,-4}},0x00000000000009C4L,2,false,7,4,8),
                     new Shape ('6',new int[][] {{2, 4},{1, 5},{1, 3},{0, 2},{0, 0}},0x0000000000002183L,0,false,7,0,4),
                     new Shape ('6',new int[][] {{3,-1},{3, 1},{2, 0},{1, 1},{0, 0}},0x0000000000061082L,1,false,6,1,8),
                     new Shape ('6',new int[][] {{1,-5},{2,-4},{1,-3},{1,-1},{0, 0}},0x00000000000011C8L,3,false,7,5,9),
                     new Shape ('6',new int[][] {{0, 0},{1,-1},{1, 1},{2, 2},{2, 4}},0x00000000000060C2L,1,false,7,1,5),
                     new Shape ('6',new int[][] {{0, 2},{0, 0},{1, 1},{2, 0},{3, 1}},0x0000000000020843L,0,false,6,0,7),
                     new Shape ('6',new int[][] {{0, 0},{1, 1},{1,-1},{2,-2},{2,-4}},0x0000000000001984L,2,false,7,4,8),
                     new Shape ('6',new int[][] {{1, 5},{2, 4},{1, 3},{1, 1},{0, 0}},0x00000000000021C1L,0,false,7,0,4),
                     new Shape ('6',new int[][] {{3, 1},{3,-1},{2, 0},{1,-1},{0, 0}},0x0000000000061042L,1,false,6,1,8),
                     new Shape ('6',new int[][] {{2,-2},{1,-3},{1,-1},{0, 0},{0, 2}},0x00000000000010CCL,2,false,7,3,7),
                     new Shape ('6',new int[][] {{1,-1},{0, 0},{1, 1},{1, 3},{2, 4}},0x00000000000041C2L,1,false,7,1,5),
                     new Shape ('6',new int[][] {{0, 0},{0, 2},{1, 1},{2, 2},{3, 1}},0x0000000000021043L,0,false,6,0,7)
                       };

     private static final Shape[] PIECE7 = { new Shape ('7',new int[][] {{0, 2},{1, 1},{0, 0},{2, 0},{2,-2}},0x0000000000001886L,1,false,7,2,7),
                     new Shape ('7',new int[][] {{1, 5},{1, 3},{0, 4},{1, 1},{0, 0}},0x00000000000001C5L,0,true, 8,0,4),
                     new Shape ('7',new int[][] {{3, 1},{2, 0},{2, 2},{1,-1},{0, 0}},0x0000000000043042L,1,false,6,1,7),
                     new Shape ('7',new int[][] {{2,-2},{1,-1},{2, 0},{0, 0},{0, 2}},0x0000000000001846L,1,false,7,2,7),
                     new Shape ('7',new int[][] {{0, 0},{0, 2},{1, 1},{0, 4},{1, 5}},0x0000000000000147L,0,false,8,0,4),
                     new Shape ('7',new int[][] {{0, 0},{1, 1},{1,-1},{2, 2},{3, 1}},0x00000000000420C2L,1,false,6,1,7),
                     new Shape ('7',new int[][] {{0, 4},{0, 2},{1, 3},{0, 0},{1,-1}},0x000000000000014EL,1,false,8,1,5),
                     new Shape ('7',new int[][] {{2, 4},{1, 3},{2, 2},{0, 2},{0, 0}},0x0000000000003083L,0,false,7,0,5),
                     new Shape ('7',new int[][] {{3,-1},{2, 0},{2,-2},{1, 1},{0, 0}},0x0000000000021882L,1,false,6,2,8),
                     new Shape ('7',new int[][] {{1,-1},{1, 1},{0, 0},{1, 3},{0, 4}},0x00000000000001CAL,1,false,8,1,5),
                     new Shape ('7',new int[][] {{0, 0},{1, 1},{0, 2},{2, 2},{2, 4}},0x0000000000003043L,0,false,7,0,5),
                     new Shape ('7',new int[][] {{0, 0},{1,-1},{1, 1},{2,-2},{3,-1}},0x00000000000208C2L,1,false,6,2,8)
                       };

     private static final Shape[] PIECE8 = { new Shape ('8',new int[][] {{4, 2},{3, 1},{2, 0},{1, 1},{0, 0}},0x0000000000820841L,0,false,5,0,7),
                     new Shape ('8',new int[][] {{3,-5},{2,-4},{1,-3},{1,-1},{0, 0}},0x0000000000021188L,3,false,6,5,9),
                     new Shape ('8',new int[][] {{0, 0},{0, 2},{0, 4},{1, 5},{1, 7}},0x0000000000000307L,0,false,8,0,2),
                     new Shape ('8',new int[][] {{0, 0},{1, 1},{2, 2},{3, 1},{4, 2}},0x0000000000821041L,0,true, 5,0,7),
                     new Shape ('8',new int[][] {{0, 0},{1,-1},{2,-2},{2,-4},{3,-5}},0x0000000000023108L,3,false,6,5,9),
                     new Shape ('8',new int[][] {{1, 7},{1, 5},{1, 3},{0, 2},{0, 0}},0x0000000000000383L,0,false,8,0,2),
                     new Shape ('8',new int[][] {{0, 0},{1, 1},{2, 2},{2, 4},{3, 5}},0x0000000000083041L,0,false,6,0,4),
                     new Shape ('8',new int[][] {{0, 0},{1,-1},{2,-2},{3,-1},{4,-2}},0x0000000000420842L,1,false,5,2,9),
                     new Shape ('8',new int[][] {{0, 4},{0, 2},{0, 0},{1,-1},{1,-3}},0x00000000000000DCL,2,false,8,3,5),
                     new Shape ('8',new int[][] {{3, 5},{2, 4},{1, 3},{1, 1},{0, 0}},0x00000000000820C1L,0,false,6,0,4),
                     new Shape ('8',new int[][] {{4,-2},{3,-1},{2, 0},{1,-1},{0, 0}},0x0000000000421042L,1,false,5,2,9),
                     new Shape ('8',new int[][] {{1,-5},{1,-3},{1,-1},{0, 0},{0, 2}},0x00000000000001D8L,3,false,8,5,7)
                       };

     private static final Shape[] PIECE9 = { new Shape ('9',new int[][] {{3, 3},{2, 2},{1, 1},{0, 0},{0, 2}},0x0000000000041043L,0,false,6,0,6),
                     new Shape ('9',new int[][] {{3,-3},{2,-2},{1,-1},{0, 0},{1, 1}},0x0000000000021184L,2,false,6,3,8),
                     new Shape ('9',new int[][] {{0, 0},{0, 2},{0, 4},{0, 6},{1, 5}},0x000000000000010FL,0,false,8,0,3),
                     new Shape ('9',new int[][] {{0, 0},{1, 1},{2, 2},{3, 3},{3, 1}},0x0000000000061041L,0,true, 6,0,6),
                     new Shape ('9',new int[][] {{0, 0},{1,-1},{2,-2},{3,-3},{2,-4}},0x0000000000021884L,2,false,6,4,9),
                     new Shape ('9',new int[][] {{1, 5},{1, 3},{1, 1},{1,-1},{0, 0}},0x00000000000003C2L,1,false,8,1,4),
                     new Shape ('9',new int[][] {{0, 0},{1, 1},{2, 2},{3, 3},{2, 4}},0x0000000000043041L,0,false,6,0,5),
                     new Shape ('9',new int[][] {{0, 0},{1,-1},{2,-2},{3,-3},{3,-1}},0x0000000000061084L,2,false,6,3,9),
                     new Shape ('9',new int[][] {{0, 6},{0, 4},{0, 2},{0, 0},{1, 1}},0x000000000000004FL,0,false,8,0,3),
                     new Shape ('9',new int[][] {{3, 3},{2, 2},{1, 1},{0, 0},{1,-1}},0x00000000000820C2L,1,false,6,1,6),
                     new Shape ('9',new int[][] {{3,-1},{2, 0},{1, 1},{0, 2},{0, 0}},0x0000000000021086L,1,false,6,1,7),
                     new Shape ('9',new int[][] {{1,-5},{1,-3},{1,-1},{1, 1},{0, 0}},0x00000000000003C8L,3,false,8,5,8)
                       };
                       
    }
/* The Computer Language Benchmarks Game
   http://shootout.alioth.debian.org/
   transliterated from C++ (Ben St. John) and D (Michael Deardeuff) by Amir K aka Razii
*/


import java.util.*;

public final class meteor
{
    static final int X = 0;
    static final int Y = 1;
    static final int N_DIM = 2;

    static final int EVEN = 0;
    static final int ODD = 1;
    static final int N_PARITY = 2;

    static final int GOOD = 0;
    static final int BAD = 1;
    static final int ALWAYS_BAD = 2;

    static final int OPEN    = 0;
    static final int CLOSED  = 1;
    static final int N_FIXED = 2;

    static final int MAX_ISLAND_OFFSET = 1024;
    static final int N_COL = 5;
    static final int N_ROW = 10;
    static final int N_CELL = N_COL * N_ROW;
    static final int N_PIECE_TYPE = 10;
    static final int N_ORIENT = 12;


//-- Globals -------------------------
    static IslandInfo[] g_islandInfo = new IslandInfo [MAX_ISLAND_OFFSET];
    static int g_nIslandInfo = 0;
    static OkPieces[][] g_okPieces = new OkPieces [N_ROW][N_COL];

    static final int g_firstRegion[] = {
        0x00, 0x01, 0x02, 0x03,   0x04, 0x01, 0x06, 0x07,
        0x08, 0x01, 0x02, 0x03,   0x0c, 0x01, 0x0e, 0x0f,

        0x10, 0x01, 0x02, 0x03,   0x04, 0x01, 0x06, 0x07,
        0x18, 0x01, 0x02, 0x03,   0x1c, 0x01, 0x1e, 0x1f
    };

    static final int g_flip[] = {
        0x00, 0x10, 0x08, 0x18, 0x04, 0x14, 0x0c, 0x1c,
        0x02, 0x12, 0x0a, 0x1a, 0x06, 0x16, 0x0e, 0x1e,

        0x01, 0x11, 0x09, 0x19, 0x05, 0x15, 0x0d, 0x1d,
        0x03, 0x13, 0x0b, 0x1b, 0x07, 0x17, 0x0f, 0x1f,
    };

    static final int[] s_firstOne = {
        0, 0, 1, 0,   2, 0, 1, 0,
        3, 0, 1, 0,   2, 0, 1, 0,

        4, 0, 1, 0,   2, 0, 1, 0,
        3, 0, 1, 0,   2, 0, 1, 0,
    };

    static int getMask(int iPos) {
        return (1 << (iPos));
    }

    static int floor(int top, int bot) {
        int toZero = top / bot;
        // negative numbers should be rounded down, not towards zero;
        if ((toZero * bot != top) && ((top < 0) != (bot <= 0)))
            toZero--;

        return toZero;
    }

    static int getFirstOne(int v) {
        int startPos = 0;
        if (v == 0)
            return 0;

        int iPos = startPos;
        int mask = 0xff << startPos;
        while ((mask & v) == 0) {
            mask <<= 8;
            iPos += 8;
        }
        int result = (mask & v) >> iPos;
        int resultLow = result & 0x0f;
        if (resultLow != 0)
            iPos += s_firstOne[resultLow];
        else
            iPos += 4 + s_firstOne[result >> 4];

        return iPos;
    }

    static int countOnes(int v) {
        int n = 0;
        while (v != 0) {
            n++;
            v = v & (v - 1);
        }

        return n;
    }


    static int flipTwoRows(int bits) {
        int flipped = g_flip[bits >> N_COL] << N_COL;
        return (flipped | g_flip[bits & Board.TOP_ROW]);
    }

    static void markBad(IslandInfo info, int mask, int eo, boolean always) {
        info.hasBad[eo][OPEN] |= mask;
        info.hasBad[eo][CLOSED] |= mask;

        if (always)
            info.alwaysBad[eo] |= mask;
    }

    static void initGlobals() {
        for (int i = 0; i < MAX_ISLAND_OFFSET; i++)
        {
            g_islandInfo[i] = new IslandInfo();
        }

        for (int i = 0; i < N_ROW; i++)
        {
            for (int j = 0; j < N_COL; j++)
                g_okPieces[i][j] = new OkPieces();
        }
    }


//-- Classes -------------------------;

    static class OkPieces {
        byte[] nPieces = new byte[N_PIECE_TYPE];
        int[][] pieceVec = new int[N_PIECE_TYPE][N_ORIENT];
    }


    static class IslandInfo {
        int[][] hasBad  =  new int[N_FIXED][N_PARITY];
        int[][] isKnown =  new int[N_FIXED][N_PARITY];
        int[] alwaysBad =  new int[N_PARITY];
    }


    static class Soln {
        static final int NO_PIECE = -1;

        boolean isEmpty() {
            return (m_nPiece == 0);
        }
        void popPiece() {
            m_nPiece--;
            m_synched = false;
        }
        void pushPiece(int vec, int iPiece, int row) {
            SPiece p = m_pieces[m_nPiece++];
            p.vec = vec;
            p.iPiece = (short) iPiece;
            p.row = (short) row;
        }

        Soln() {
            m_synched = false;
            m_nPiece = 0;
            init();
        }

        class SPiece {
            int vec;
            short iPiece;
            short row;
            SPiece() {}
            SPiece(int avec, int apiece, int arow) {
                vec = avec;
                iPiece = (short)apiece;
                row = (short)arow;
            }
            SPiece(SPiece other) {
                vec = other.vec;
                iPiece = other.iPiece;
                row = other.row;
            }
        }

        SPiece[] m_pieces = new SPiece [N_PIECE_TYPE];
        int m_nPiece;
        byte[][] m_cells = new byte [N_ROW][N_COL];
        boolean m_synched;

        void init() {
            for (int i = 0; i < N_PIECE_TYPE; i++)
                m_pieces[i] = new SPiece();
        }
        Soln (int fillVal) {
            init();
            m_nPiece = 0;
            fill(fillVal);
        }
        public Soln clone2() {
            Soln s = new Soln();
            for (int i = 0; i < m_pieces.length; i++)
                s.m_pieces[i] = new SPiece(m_pieces[i]);

            s.m_nPiece = m_nPiece;
            //System.arraycopy(m_cells, 0, s.m_cells, 0, N_CELL);
            for (int i = 0; i < N_ROW; i++)
            {
                for (int j = 0; j < N_COL; j ++)
                {
                    s.m_cells[i][j] = m_cells[i][j];
                }
            }

            s.m_synched = m_synched;
            return s;
        }

        void fill(int val) {
            m_synched = false;
            for (int i = 0; i < N_ROW; i++)
            {
                for (int j = 0; j < N_COL; j++)
                    m_cells[i][j] = (byte) val;
            }
        }

        public String toString()  {
            StringBuffer result = new StringBuffer(N_CELL * 2);

            for (int y = 0; y < N_ROW; y++) {
                for (int x = 0; x < N_COL; x++) {
                    int val = m_cells[y][x];
                    //if (val == NO_PIECE) result.append('.');
                    {
                        result.append(val);
                    }
                    result.append(' ');
                }
                result.append('\n');

                // indent every second line
                if (y % 2 == 0)
                    result.append(" ");
            }
            return result.toString();
        }

        void setCells() {
            if (m_synched)
                return;

            for (int iPiece = 0; iPiece < m_nPiece; iPiece++) {
                SPiece p = m_pieces[iPiece];
                int vec = p.vec;
                byte pID = (byte) p.iPiece;
                int rowOffset = p.row;

                int nNewCells = 0;
                for (int y = rowOffset; y < N_ROW; y++) {
                    for (int x = 0; x < N_COL; x++) {
                        if ((vec & 1) != 0) {
                            m_cells[y][x] = pID;
                            nNewCells++;
                        }
                        vec >>= 1;
                    }
                    if (nNewCells == Piece.N_ELEM)
                        break;
                }
            }
            m_synched = true;
        }

        boolean lessThan(Soln r) {
            if (m_pieces[0].iPiece != r.m_pieces[0].iPiece) {
                return m_pieces[0].iPiece < r.m_pieces[0].iPiece;
            }

            setCells();
            r.setCells();

            for (int y = 0; y < N_ROW; y++) {
                for (int x = 0; x < N_COL; x++) {
                    int lval = m_cells[y][x];
                    int rval = r.m_cells[y][x];

                    if (lval != rval)
                        return (lval < rval);
                }
            }

            return false;
        }

        void spin(Soln spun) {
            setCells();

            for (int y = 0; y < N_ROW; y++) {
                for (int x = 0; x < N_COL; x++) {
                    byte flipped = m_cells[N_ROW - y - 1][N_COL - x - 1];
                    spun.m_cells[y][x] = flipped;
                }
            }


            spun.m_pieces[0].iPiece = m_pieces[N_PIECE_TYPE - 1].iPiece;
            spun.m_synched = true;
        }
    }


//-----------------------
    static class Board {
        static final int L_EDGE_MASK = 
                                       ((1 <<  0) | (1 <<  5) | (1 << 10) | (1 << 15) |
                                        (1 << 20) | (1 << 25) | (1 << 30));
        static final int R_EDGE_MASK = L_EDGE_MASK << 4;
        static final int TOP_ROW = (1 << N_COL) - 1;
        static final int ROW_0_MASK =
            TOP_ROW | (TOP_ROW << 10) | (TOP_ROW << 20) | (TOP_ROW << 30);
        static final int ROW_1_MASK = ROW_0_MASK << 5;
        static final int BOARD_MASK = (1 << 30) - 1;

        static int getIndex(int x, int y) {
            return y * N_COL + x;
        }

        Soln m_curSoln;
        Soln m_minSoln;
        Soln m_maxSoln;
        int m_nSoln;

        Board () {
            m_curSoln = new Soln(Soln.NO_PIECE);
            m_minSoln = new Soln(N_PIECE_TYPE);
            m_maxSoln = new Soln(Soln.NO_PIECE);
            m_nSoln = (0);
        }

        static boolean badRegion(int[] toFill, int rNew)
        {
            // grow empty region, until it doesn't change any more;
            int region;
            do {
                region = rNew;

                // simple grow up/down
                rNew |= (region >> N_COL);
                rNew |= (region << N_COL);

                // grow right/left
                rNew |= (region & ~L_EDGE_MASK) >> 1;
                rNew |= (region & ~R_EDGE_MASK) << 1;

                // tricky growth
                int evenRegion = region & (ROW_0_MASK & ~L_EDGE_MASK);
                rNew |= evenRegion >> (N_COL + 1);
                rNew |= evenRegion << (N_COL - 1);
                int oddRegion = region & (ROW_1_MASK & ~R_EDGE_MASK);
                rNew |= oddRegion >> (N_COL - 1);
                rNew |= oddRegion << (N_COL + 1);

                // clamp against existing pieces
                rNew &= toFill[0];
            }
            while ((rNew != toFill[0]) && (rNew != region));

            // subtract empty region from board
            toFill[0] ^= rNew;

            int nCells = countOnes(toFill[0]);
            return (nCells % Piece.N_ELEM != 0);
        }

        static int hasBadIslands(int boardVec, int row)
        {
            // skip over any filled rows
            while ((boardVec & TOP_ROW) == TOP_ROW) {
                boardVec >>= N_COL;
                row++;
            }

            int iInfo = boardVec & ((1 << 2 * N_COL) - 1);
            IslandInfo info = g_islandInfo[iInfo];

            int lastRow = (boardVec >> (2 * N_COL)) & TOP_ROW;
            int mask = getMask(lastRow);
            int isOdd = row & 1;

            if ((info.alwaysBad[isOdd] & mask) != 0)
                return BAD;

            if ((boardVec & (TOP_ROW << N_COL * 3)) != 0)
                return calcBadIslands(boardVec, row);

            int isClosed = (row > 6) ? 1 : 0;

            if ((info.isKnown[isOdd][isClosed] & mask) != 0)
                return (info.hasBad[isOdd][isClosed] & mask);

            if (boardVec == 0)
                return GOOD;

            int hasBad = calcBadIslands(boardVec, row);

            info.isKnown[isOdd][isClosed] |= mask;
            if (hasBad != 0)
                info.hasBad[isOdd][isClosed] |= mask;

            return hasBad;
        }
        static int calcBadIslands(int boardVec, int row)
        {
            int[] toFill = {~boardVec};
            if ((row & 1) != 0) {
                row--;
                toFill[0] <<= N_COL;
            }

            int boardMask = BOARD_MASK;
            if (row > 4) {
                int boardMaskShift = (row - 4) * N_COL;
                boardMask >>= boardMaskShift;
            }
            toFill[0] &= boardMask;

            // a little pre-work to speed things up
            int bottom = (TOP_ROW << (5 * N_COL));
            boolean filled = ((bottom & toFill[0]) == bottom);
            while ((bottom & toFill[0]) == bottom) {
                toFill[0] ^= bottom;
                bottom >>= N_COL;
            }

            int startRegion;
            if (filled || (row < 4))
                startRegion = bottom & toFill[0];
            else {
                startRegion = g_firstRegion[toFill[0] & TOP_ROW];
                if (startRegion == 0)  {
                    startRegion = (toFill[0] >> N_COL) & TOP_ROW;
                    startRegion = g_firstRegion[startRegion];
                    startRegion <<= N_COL;
                }
                startRegion |= (startRegion << N_COL) & toFill[0];
            }

            while (toFill[0] != 0)    {
                if (badRegion(toFill, startRegion))
                    return ((toFill[0]!=0) ? ALWAYS_BAD : BAD);
                int iPos = getFirstOne(toFill[0]);
                startRegion = getMask(iPos);
            }

            return GOOD;
        }
        static void calcAlwaysBad() {
            for (int iWord = 1; iWord < MAX_ISLAND_OFFSET; iWord++) {
                IslandInfo isleInfo = g_islandInfo[iWord];
                IslandInfo flipped = g_islandInfo[flipTwoRows(iWord)];

                for (int i = 0, mask = 1; i < 32; i++, mask <<= 1) {
                    int boardVec = (i << (2 * N_COL)) | iWord;
                    if ((isleInfo.isKnown[0][OPEN] & mask) != 0)
                        continue;

                    int hasBad = calcBadIslands(boardVec, 0);
                    if (hasBad != GOOD) {
                        boolean always = (hasBad==ALWAYS_BAD);
                        markBad(isleInfo, mask, EVEN, always);

                        int flipMask = getMask(g_flip[i]);
                        markBad(flipped, flipMask, ODD, always);
                    }
                }
                flipped.isKnown[1][OPEN] =  -1;
                isleInfo.isKnown[0][OPEN] = -1;
            }
        }

        static boolean hasBadIslandsSingle(int boardVec, int row)
        {
            int[] toFill = {~boardVec};
            boolean isOdd = ((row & 1) != 0);
            if (isOdd) {
                row--;
                toFill[0] <<= N_COL; // shift to even aligned
                toFill[0] |= TOP_ROW;
            }

            int startRegion = TOP_ROW;
            int lastRow = TOP_ROW << (5 * N_COL);
            int boardMask = BOARD_MASK; // all but the first two bits
            if (row >= 4)
                boardMask >>= ((row - 4) * N_COL);
            else if (isOdd || (row == 0))
                startRegion = lastRow;

            toFill[0] &= boardMask;
            startRegion &= toFill[0];

            while (toFill[0] != 0)    {
                if (badRegion(toFill, startRegion))
                    return true;
                int iPos = getFirstOne(toFill[0]);
                startRegion = getMask(iPos);
            }

            return false;
        }

        void genAllSolutions(int boardVec, int placedPieces, int row)
        {
            while ((boardVec & TOP_ROW) == TOP_ROW) {
                boardVec >>= N_COL;
                row++;
            }
            int iNextFill = s_firstOne[~boardVec & TOP_ROW];
            OkPieces allowed = g_okPieces[row][iNextFill];

            int iPiece = getFirstOne(~placedPieces);
            int pieceMask = getMask(iPiece);
            for (; iPiece < N_PIECE_TYPE; iPiece++, pieceMask <<= 1)
            {
                if ((pieceMask & placedPieces) != 0)
                    continue;

                placedPieces |= pieceMask;
                for (int iOrient = 0; iOrient < allowed.nPieces[iPiece]; iOrient++) {
                    int pieceVec = allowed.pieceVec[iPiece][iOrient];

                    if ((pieceVec & boardVec) != 0)
                        continue;

                    boardVec |= pieceVec;

                    if ((hasBadIslands(boardVec, row)) != 0) {
                        boardVec ^= pieceVec;
                        continue;
                    }

                    m_curSoln.pushPiece(pieceVec, iPiece, row);

                    // recur or record solution
                    if (placedPieces != Piece.ALL_PIECE_MASK)
                        genAllSolutions(boardVec, placedPieces, row);
                    else
                        recordSolution(m_curSoln);

                    boardVec ^= pieceVec;
                    m_curSoln.popPiece();
                }

                placedPieces ^= pieceMask;
            }
        }

        void recordSolution(Soln s) {
            m_nSoln += 2;

            if (m_minSoln.isEmpty()) {
                m_minSoln = m_maxSoln = s.clone2();
                return;
            }

            if (s.lessThan(m_minSoln))
                m_minSoln = s.clone2();
            else if (m_maxSoln.lessThan(s))
                m_maxSoln = s.clone2();

            Soln spun = new Soln();
            s.spin(spun);
            if (spun.lessThan(m_minSoln))
                m_minSoln = spun;
            else if (m_maxSoln.lessThan(spun))
                m_maxSoln = spun;
        }
    }

//----------------------
    static class Piece {
        class Instance {
            long m_allowed;
            int m_vec;
            int m_offset;
        }

        static final int N_ELEM = 5;
        static final int ALL_PIECE_MASK = (1 << N_PIECE_TYPE) - 1;
        static final int SKIP_PIECE = 5;

        static final int BaseVecs[] = {
            0x10f, 0x0cb, 0x1087, 0x427, 0x465,
            0x0c7, 0x8423, 0x0a7, 0x187, 0x08f
        };

        static Piece[][] s_basePiece = new Piece [N_PIECE_TYPE][N_ORIENT];

        Instance[] m_instance = new Instance [N_PARITY];

        void init() {
            for (int i = 0; i < N_PARITY; i++)
                m_instance[i] = new Instance();
        }
        Piece() {
            init();
        }

        static {
            for (int i = 0; i < N_PIECE_TYPE; i++) {
                for (int j = 0; j < N_ORIENT; j++)
                    s_basePiece[i][j] = new Piece();
            }
        }
        static void setCoordList(int vec, int[][] pts) {
            int iPt = 0;
            int mask = 1;
            for (int y = 0; y < N_ROW; y++) {
                for (int x = 0; x < N_COL; x++) {
                    if ((mask & vec) != 0) {
                        pts[iPt][X] = x;
                        pts[iPt][Y] = y;

                        iPt++;
                    }
                    mask <<= 1;
                }
            }
        }

        static int toBitVector(int[][] pts) {
            int y, x;
            int result = 0;
            for (int iPt = 0; iPt < N_ELEM; iPt++) {
                x = pts[iPt][X];
                y = pts[iPt][Y];

                int pos = Board.getIndex(x, y);
                result |= (1 << pos);
            }

            return result;
        }

        static void shiftUpLines(int[][] pts, int shift) {

            for (int iPt = 0; iPt < N_ELEM; iPt++) {
                if ((pts[iPt][Y] & shift & 0x1) != 0)
                    (pts[iPt][X])++;
                pts[iPt][Y] -= shift;
            }
        }

        static int shiftToX0(int[][] pts, Instance instance, int offsetRow)
        {
            int x, y, iPt;
            int xMin = pts[0][X];
            int xMax = xMin;
            for (iPt = 1; iPt < N_ELEM; iPt++) {
                x = pts[iPt][X];
                y = pts[iPt][Y];

                if (x < xMin)
                    xMin = x;
                else if (x > xMax)
                    xMax = x;
            }

            int offset = N_ELEM;
            for (iPt = 0; iPt < N_ELEM; iPt++) {

                pts[iPt][X] -= xMin;

                if ((pts[iPt][Y] == offsetRow) && (pts[iPt][X] < offset))
                    offset = pts[iPt][X];
            }

            instance.m_offset = offset;
            instance.m_vec = toBitVector(pts);
            return xMax - xMin;
        }

        void setOkPos(int isOdd, int w, int h) {
            Instance p = m_instance[isOdd];
            p.m_allowed = 0;
            long posMask = 1L << (isOdd * N_COL);

            for (int y = isOdd; y < N_ROW - h; y+=2, posMask <<= N_COL) {
                if ((p.m_offset) != 0)
                    posMask <<= p.m_offset;

                for (int xPos = 0; xPos < N_COL - p.m_offset; xPos++, posMask <<= 1) {

                    if (xPos >= N_COL - w)
                        continue;

                    int pieceVec = p.m_vec << xPos;

                    if (Board.hasBadIslandsSingle(pieceVec, y))
                        continue;

                    p.m_allowed |= posMask;
                }
            }
        }

        static void genOrientation(int vec, int iOrient, Piece target)
        {
            int[][] pts = new int[N_ELEM][N_DIM];
            setCoordList(vec, pts);

            int y, x, iPt;
            int rot = iOrient % 6;
            int flip = iOrient >= 6 ? 1 : 0;
            if (flip != 0) {
                for (iPt = 0; iPt < N_ELEM; iPt++)
                    pts[iPt][Y] = -pts[iPt][Y];
            }

            while ((rot--) != 0) {
                for (iPt = 0; iPt < N_ELEM; iPt++) {
                    x = pts[iPt][X];
                    y = pts[iPt][Y];

                    int xNew = floor((2 * x - 3 * y + 1), 4);
                    int yNew = floor((2 * x + y + 1), 2);
                    pts[iPt][X] = xNew;
                    pts[iPt][Y] = yNew;
                }
            }

            int yMin = pts[0][Y];
            int yMax = yMin;
            for (iPt = 1; iPt < N_ELEM; iPt++) {
                y = pts[iPt][Y];

                if (y < yMin)
                    yMin = y;
                else if (y > yMax)
                    yMax = y;
            }
            int h = yMax - yMin;
            Instance even = target.m_instance[EVEN];
            Instance odd = target.m_instance[ODD];

            shiftUpLines(pts, yMin);
            int w = shiftToX0(pts, even, 0);
            target.setOkPos(EVEN, w, h);
            even.m_vec >>= even.m_offset;

            shiftUpLines(pts, -1);
            w = shiftToX0(pts, odd, 1);
            odd.m_vec >>= N_COL;
            target.setOkPos(ODD, w, h);
            odd.m_vec >>= odd.m_offset;
        }

        static void genAllOrientations() {
            for (int iPiece = 0; iPiece < N_PIECE_TYPE; iPiece++) {
                int refPiece = BaseVecs[iPiece];
                for (int iOrient = 0; iOrient < N_ORIENT; iOrient++) {
                    Piece p = s_basePiece[iPiece][iOrient];
                    genOrientation(refPiece, iOrient, p);
                    if ((iPiece == SKIP_PIECE)  && (((iOrient / 3) & 1) != 0))
                        p.m_instance[0].m_allowed = p.m_instance[1].m_allowed = 0;
                }
            }
            for (int iPiece = 0; iPiece < N_PIECE_TYPE; iPiece++) {
                for (int iOrient = 0; iOrient < N_ORIENT; iOrient++) {
                    long mask = 1;
                    for (int iRow = 0; iRow < N_ROW; iRow++) {
                        Instance p = getPiece(iPiece, iOrient, (iRow & 1));
                        for (int iCol = 0; iCol < N_COL; iCol++) {
                            OkPieces allowed = g_okPieces[iRow][iCol];
                            if ((p.m_allowed & mask) != 0) {
                                allowed.pieceVec[iPiece][allowed.nPieces[iPiece]] = p.m_vec << iCol;
                                (allowed.nPieces[iPiece])++;
                            }

                            mask <<= 1;
                        }
                    }
                }
            }
        }

        static Instance getPiece(int iPiece, int iOrient, int iParity) {
            return s_basePiece[iPiece][iOrient].m_instance[iParity];
        }
    }


//-- Main ---------------------------
    public static void main(String[] args) {
        if (args.length > 2)
            System.exit(-1); // spec says this is an error;

        initGlobals();
        Board b = new Board();
        Piece.genAllOrientations();
        Board.calcAlwaysBad();
        b.genAllSolutions(0, 0, 0);

        System.out.println(b.m_nSoln + " solutions found\n");
        System.out.println(b.m_minSoln);
        System.out.println(b.m_maxSoln);
    }
}
// $Id: methcall.java,v 1.1 2004-05-23 07:14:27 bfulgham Exp $
// http://www.bagley.org/~doug/shootout/
// Collection class code is from my friend Phil Chu, Thanks Phil!

import java.io.*;
import java.util.*;
import java.text.*;

class Toggle {
    boolean state = true;
    public Toggle(boolean start_state) {
	this.state = start_state;
    }
    public boolean value() {
	return(this.state);
    }
    public Toggle activate() {
	this.state = !this.state;
	return(this);
    }
}

class NthToggle extends Toggle {
    int count_max = 0;
    int counter = 0;

    public NthToggle(boolean start_state, int max_counter) {
	super(start_state);
	this.count_max = max_counter;
	this.counter = 0;
    }
    public Toggle activate() {
	this.counter += 1;
	if (this.counter >= this.count_max) {
	    this.state = !this.state;
	    this.counter = 0;
	}
	return(this);
    }
}

public class methcall {
    public static void main(String args[]) throws IOException {
	int n = Integer.parseInt(args[0]);

	boolean val = true;
	Toggle toggle = new Toggle(val);
	for (int i=0; i<n; i++) {
	    val = toggle.activate().value();
	}
	System.out.println((val) ? "true" : "false");

	val = true;
	NthToggle ntoggle = new NthToggle(true, 3);
	for (int i=0; i<n; i++) {
	    val = ntoggle.activate().value();
	}
	System.out.println((val) ? "true" : "false");
    }
}
// $Id: moments.java,v 1.1 2004-11-23 08:08:44 bfulgham Exp $
// http://www.bagley.org/~doug/shootout/

import java.io.*;
import java.util.*;
import java.text.*;
import java.lang.Math;

public class moments {
    public static void main(String[] args) {
    String line;
    Vector nums = new Vector();
    double num, sum = 0.0;
    double mean = 0.0;
    double average_deviation = 0.0;
    double standard_deviation = 0.0;
    double variance = 0.0;
    double skew = 0.0;
    double kurtosis = 0.0;
    double median = 0.0;
    double deviation = 0.0;
    int i, n, mid = 0;

        try {
            BufferedReader in = new BufferedReader(new InputStreamReader(System.in));
            while ((line = in.readLine()) != null) {
        num = Double.parseDouble(line);
        sum += num;
        nums.add(new Double(num));
            }
        } catch (IOException e) {
            System.err.println(e);
            return;
        }

    n = nums.size();
    mean = sum/n;
    for (i=0; i<n; i++) {
        deviation = ((Double)nums.get(i)).doubleValue() - mean;
        average_deviation += Math.abs(deviation);
        variance += Math.pow(deviation,2);
        skew += Math.pow(deviation,3);
        kurtosis += Math.pow(deviation,4);
    }
    average_deviation /= n;
    variance /= (n - 1);
    standard_deviation = Math.sqrt(variance);
    if (variance != 0.0) {
        skew /= (n * variance * standard_deviation);
        kurtosis = kurtosis/(n * variance * variance) - 3.0;
    }
    
    Collections.sort(nums);

    mid = (n/2);
    median = (n % 2 != 0) ?
        ((Double)nums.get(mid)).doubleValue() :
        (((Double)nums.get(mid)).doubleValue() +
         ((Double)nums.get(mid-1)).doubleValue())/2;
    
    NumberFormat nf = NumberFormat.getInstance();
    nf.setMaximumFractionDigits(13);
    nf.setGroupingUsed(false);
    nf.setMaximumFractionDigits(6);
    nf.setMinimumFractionDigits(6);

    System.out.println("n:                  " + n);
    System.out.println("median:             " + nf.format(median));
    System.out.println("mean:               " + nf.format(mean));
    System.out.println("average_deviation:  " + nf.format(average_deviation));
    System.out.println("standard_deviation: " + nf.format(standard_deviation));
    System.out.println("variance:           " + nf.format(variance));
    System.out.println("skew:               " + nf.format(skew));
    System.out.println("kurtosis:           " + nf.format(kurtosis));
    }
}

/* The Computer Language Benchmarks Game
   http://shootout.alioth.debian.org/

   contributed by Mark C. Lewis
   modified slightly by Chad Whipkey
*/

public final class nbody {
    public static void main(String[] args) {
        int n = Integer.parseInt(args[0]);

        NBodySystem bodies = new NBodySystem();
        System.out.printf("%.9f\n", bodies.energy());
        for (int i=0; i<n; ++i)
           bodies.advance(0.01);
        System.out.printf("%.9f\n", bodies.energy());
    }
}

final class NBodySystem {
   private Body[] bodies;

   public NBodySystem(){
      bodies = new Body[]{
            Body.sun(),
            Body.jupiter(),
            Body.saturn(),
            Body.uranus(),
            Body.neptune()
         };

      double px = 0.0;
      double py = 0.0;
      double pz = 0.0;
      for(int i=0; i < bodies.length; ++i) {
         px += bodies[i].vx * bodies[i].mass;
         py += bodies[i].vy * bodies[i].mass;
         pz += bodies[i].vz * bodies[i].mass;
      }
      bodies[0].offsetMomentum(px,py,pz);
   }

   public void advance(double dt) {

      for(int i=0; i < bodies.length; ++i) {
            Body iBody = bodies[i];
         for(int j=i+1; j < bodies.length; ++j) {
                double dx = iBody.x - bodies[j].x;
            double dy = iBody.y - bodies[j].y;
            double dz = iBody.z - bodies[j].z;

                double dSquared = dx * dx + dy * dy + dz * dz;
                double distance = Math.sqrt(dSquared);
                double mag = dt / (dSquared * distance);

            iBody.vx -= dx * bodies[j].mass * mag;
            iBody.vy -= dy * bodies[j].mass * mag;
            iBody.vz -= dz * bodies[j].mass * mag;

            bodies[j].vx += dx * iBody.mass * mag;
            bodies[j].vy += dy * iBody.mass * mag;
            bodies[j].vz += dz * iBody.mass * mag;
         }
      }

        for ( Body body : bodies) {
         body.x += dt * body.vx;
         body.y += dt * body.vy;
         body.z += dt * body.vz;
      }
   }

   public double energy(){
      double dx, dy, dz, distance;
      double e = 0.0;

      for (int i=0; i < bodies.length; ++i) {
            Body iBody = bodies[i];
            e += 0.5 * iBody.mass *
                 ( iBody.vx * iBody.vx
                   + iBody.vy * iBody.vy
                   + iBody.vz * iBody.vz );

         for (int j=i+1; j < bodies.length; ++j) {
                Body jBody = bodies[j];
                dx = iBody.x - jBody.x;
            dy = iBody.y - jBody.y;
            dz = iBody.z - jBody.z;

            distance = Math.sqrt(dx*dx + dy*dy + dz*dz);
            e -= (iBody.mass * jBody.mass) / distance;
         }
      }
      return e;
   }
}


final class Body {
   static final double PI = 3.141592653589793;
   static final double SOLAR_MASS = 4 * PI * PI;
   static final double DAYS_PER_YEAR = 365.24;

   public double x, y, z, vx, vy, vz, mass;

   public Body(){}

   static Body jupiter(){
      Body p = new Body();
      p.x = 4.84143144246472090e+00;
      p.y = -1.16032004402742839e+00;
      p.z = -1.03622044471123109e-01;
      p.vx = 1.66007664274403694e-03 * DAYS_PER_YEAR;
      p.vy = 7.69901118419740425e-03 * DAYS_PER_YEAR;
      p.vz = -6.90460016972063023e-05 * DAYS_PER_YEAR;
      p.mass = 9.54791938424326609e-04 * SOLAR_MASS;
      return p;
   }

   static Body saturn(){
      Body p = new Body();
      p.x = 8.34336671824457987e+00;
      p.y = 4.12479856412430479e+00;
      p.z = -4.03523417114321381e-01;
      p.vx = -2.76742510726862411e-03 * DAYS_PER_YEAR;
      p.vy = 4.99852801234917238e-03 * DAYS_PER_YEAR;
      p.vz = 2.30417297573763929e-05 * DAYS_PER_YEAR;
      p.mass = 2.85885980666130812e-04 * SOLAR_MASS;
      return p;
   }

   static Body uranus(){
      Body p = new Body();
      p.x = 1.28943695621391310e+01;
      p.y = -1.51111514016986312e+01;
      p.z = -2.23307578892655734e-01;
      p.vx = 2.96460137564761618e-03 * DAYS_PER_YEAR;
      p.vy = 2.37847173959480950e-03 * DAYS_PER_YEAR;
      p.vz = -2.96589568540237556e-05 * DAYS_PER_YEAR;
      p.mass = 4.36624404335156298e-05 * SOLAR_MASS;
      return p;
   }

   static Body neptune(){
      Body p = new Body();
      p.x = 1.53796971148509165e+01;
      p.y = -2.59193146099879641e+01;
      p.z = 1.79258772950371181e-01;
      p.vx = 2.68067772490389322e-03 * DAYS_PER_YEAR;
      p.vy = 1.62824170038242295e-03 * DAYS_PER_YEAR;
      p.vz = -9.51592254519715870e-05 * DAYS_PER_YEAR;
      p.mass = 5.15138902046611451e-05 * SOLAR_MASS;
      return p;
   }

   static Body sun(){
      Body p = new Body();
      p.mass = SOLAR_MASS;
      return p;
   }

   Body offsetMomentum(double px, double py, double pz){
      vx = -px / SOLAR_MASS;
      vy = -py / SOLAR_MASS;
      vz = -pz / SOLAR_MASS;
      return this;
   }
}

// $Id: nestedloop.java,v 1.1 2004-05-23 07:14:28 bfulgham Exp $
// http://www.bagley.org/~doug/shootout/

import java.io.*;
import java.util.*;

public class nestedloop {
    public static void main(String args[]) throws IOException {
	int n = Integer.parseInt(args[0]);
	int x = 0;
	for (int a=0; a<n; a++)
	    for (int b=0; b<n; b++)
		for (int c=0; c<n; c++)
		    for (int d=0; d<n; d++)
			for (int e=0; e<n; e++)
			    for (int f=0; f<n; f++)
				x++;
	System.out.println(x);
    }
}
/* The Great Computer Language Shootout
   http://shootout.alioth.debian.org/

   contributed by Alexei Svitkine
*/

public class nsieve
{
   static int nsieve(int m, boolean[] isPrime)
   {
      for (int i=2; i <= m; i++) isPrime[i] = true;
      int count = 0;

      for (int i=2; i <= m; i++) {
         if (isPrime[i]) {
            for (int k=i+i; k <= m; k+=i) isPrime[k] = false;
            count++;
         }
      }
      return count;
   }

   public static String padNumber(int number, int fieldLen)
   {
      StringBuffer sb = new StringBuffer();
      String bareNumber = "" + number;
      int numSpaces = fieldLen - bareNumber.length();

      for (int i = 0; i < numSpaces; i++)
         sb.append(" ");

      sb.append(bareNumber);

      return sb.toString();
   }

   public static void main(String[] args)
   {
      int n = 2;
      if (args.length > 0) n = Integer.parseInt(args[0]);
      if (n < 2) n = 2;

      int m = (1<<n)*10000;
      boolean[] flags = new boolean[m+1];

      System.out.println("Primes up to " + padNumber(m, 8) + " " + padNumber(nsieve(m,flags), 8));
      m = (1<<n-1)*10000;
      System.out.println("Primes up to " + padNumber(m, 8) + " " + padNumber(nsieve(m,flags), 8));
      m = (1<<n-2)*10000;
      System.out.println("Primes up to " + padNumber(m, 8) + " " + padNumber(nsieve(m,flags), 8));
   }
}
/* The Computer Language Shootout
http://shootout.alioth.debian.org/
contributed by Alexei Svitkine
*/

public class nsieve
{
  static void nsieve(int m) {
    int count = 0, i, j;
    boolean[] flags = new boolean[m];
    
    for (i = 2; i < m; ++i)
      if (!flags[i]) {
        ++count;
        for (j = i << 1; j < m; j += i)
          flags[j] = true;
      }
        
    System.out.println(String.format("Primes up to %8d %8d", m, count));
  }

  public static void main(String[] args) {
    int m = 2;
    if (args.length > 0) m = Integer.parseInt(args[0]);
    for (int i = 0; i < 3; i++)
      nsieve(10000 << (m-i));
  }
}
/* The Computer Language Benchmarks Game
   http://shootout.alioth.debian.org/

   contributed by Alexei Svitkine
   modified by Razzi
*/

public final class nsieve
{
    static int nsieve(int m, byte[] isPrime)
    {
        int count = 0;
        for (int i=2; i <= m; i++) {
            if (isPrime[i] == 0) {
                for (int k=i+i; k <= m; k+=i) isPrime[k] = 1;
                count++;
            }
        }
        return count;
    }

    public static void main(String[] args)
    {
        int n = Integer.parseInt(args[0]);

        int m = (1<<n)*10000;
        byte [] flags = new byte[m+1];

        System.out.printf("Primes up to %8d %8d\n", m, nsieve(m,flags));
        m = (1<<n-1)*10000;
        System.out.printf("Primes up to %8d %8d\n", m, nsieve(m,flags));
        m = (1<<n-2)*10000;
        System.out.printf("Primes up to %8d %8d\n", m, nsieve(m,flags));
    }
}
/* The Computer Language Shootout
http://shootout.alioth.debian.org/

contributed by Alkis Evlogimenos
*/

import java.util.BitSet;

public class nsievebits
{
   private static int nsieve(int m, BitSet bits) {
      bits.set(0, m+1);

      int count = 0;
      for (int i = 2; i <= m; ++i) {
         if (bits.get(i)) {
         for (int j = i + i; j <=m; j += i)
            bits.clear(j);
            ++count;
         }
      }
      return count;
   }

   public static String padNumber(int number, int fieldLen)
   {
      StringBuffer sb = new StringBuffer();
      String bareNumber = "" + number;
      int numSpaces = fieldLen - bareNumber.length();

      for (int i = 0; i < numSpaces; i++)
         sb.append(" ");

      sb.append(bareNumber);

      return sb.toString();
   }

   public static void main(String[] args)
   {
      int n = 2;
      if (args.length > 0)
         n = Integer.parseInt(args[0]);
      if (n < 2)
         n = 2;

      int m = (1 << n) * 10000;
      BitSet bits = new BitSet(m+1);
      System.out.println("Primes up to " + padNumber(m, 8) + " " + padNumber(nsieve(m,bits), 8));

      m = (1 << n-1) * 10000;
      System.out.println("Primes up to " + padNumber(m, 8) + " " + padNumber(nsieve(m,bits), 8));

      m = (1 << n-2) * 10000;
      System.out.println("Primes up to " + padNumber(m, 8) + " " + padNumber(nsieve(m,bits), 8));
   }
}

/* The Computer Language Benchmarks Game
http://shootout.alioth.debian.org/

contributed by Alkis Evlogimenos
slightly modified by Pierre-Olivier Gaillard
*/



public class nsievebits
{
   private static class MyBitSet {
      private int[] bits;
      int length;
      private static final int mask = 31;
      private static final int shift = 5;
      public MyBitSet(int m) {
         bits = new int[m/8+1];
         length = m;
      }
      
      public void setRange() {
         for (int i =0; i < ((length >> shift)+1); i++ ){
               bits[i] = -1;
            }
      }
      public boolean get(int i){
         return  ((((int) bits[i >> shift]) >>>  (i & mask)) & 1) != 0; 
      }
      public void set(int i) {
         bits[i >> shift] |= (1 << (i & mask));
      }
      
      public void clear(int i) {
         bits[i >> shift] &= ~(1 << (i & mask));
      }
      
      public static void test() {
         MyBitSet bs = new MyBitSet(128);
         bs.setRange();
         bs.clear(5);
         System.out.println("Position 5 : " + bs.get(5));
         System.out.println("Position 6 : " + bs.get(6));
      }
   }
   private static int nsieve(int m, MyBitSet bits) {
   
      
      
     bits.setRange();
      int count = 0;
      for (int i = 2; i <= m; ++i) {
         if (bits.get(i)) {
         //System.err.println("Found prime : " + i);
         for (int j = i + i; j <=m; j += i)
            bits.clear(j);
            ++count;
         }
      }
      return count;
   }

   public static String padNumber(int number, int fieldLen)
   {
      StringBuffer sb = new StringBuffer();
      String bareNumber = "" + number;
      int numSpaces = fieldLen - bareNumber.length();

      for (int i = 0; i < numSpaces; i++)
         sb.append(" ");

      sb.append(bareNumber);

      return sb.toString();
   }

   public static void main(String[] args)
   {
     //MyBitSet.test();
      int n = 2;
      if (args.length > 0)
         n = Integer.parseInt(args[0]);
      if (n < 2)
         n = 2;

      int m = (1 << n) * 10000;
      MyBitSet bits = new MyBitSet(m+1);
      System.out.println("Primes up to " + padNumber(m, 8) + " " 
                            + padNumber(nsieve(m,bits), 8));

      m = (1 << n-1) * 10000;
      System.out.println("Primes up to " + padNumber(m, 8) + " " 
                            + padNumber(nsieve(m,bits), 8));

      m = (1 << n-2) * 10000;
      System.out.println("Primes up to " + padNumber(m, 8) + " " 
                            + padNumber(nsieve(m,bits), 8));
   }
}
/* The Computer Language Benchmarks Game
http://shootout.alioth.debian.org/

contributed by Alkis Evlogimenos
slightly modified by Pierre-Olivier Gaillard
slightly modified by Klaus Friedel
*/

import java.util.Arrays;

public class nsievebits {
  static class MyBitSet {
    private final int[] bits;
    private static final int mask = 31;
    private static final int shift = 5;

    public MyBitSet(int m) {
      bits = new int[(m >> shift) + 1];
    }

    public void setAll() {
      Arrays.fill(bits, -1);
    }

    public boolean get(int i) {
      return ((bits[i >> shift] >>> (i & mask)) & 1) != 0;
    }

    public void clear(int i) {
      bits[i >> shift] &= ~(1 << (i & mask));
    }
  }

  static int nsieve(int m, MyBitSet bits) {
    bits.setAll();
    int count = 0;
    for (int i = 2; i <= m; ++i) {
      if (bits.get(i)) {
        for (int j = i + i; j <= m; j += i) bits.clear(j);
        ++count;
      }
    }
    return count;
  }

  static void primes(int n, MyBitSet bits) {
    int m = (1 << n) * 10000;
    System.out.printf("Primes up to %8d %8d\n", m, nsieve(m, bits));
  }

  public static void main(String[] args) {
    int n = 2;
    if (args.length > 0)
      n = Integer.parseInt(args[0]);
    if (n < 2) n = 2;

    int m = (1 << n) * 10000;
    MyBitSet bits = new MyBitSet(m + 1);
    primes(n, bits);
    primes(n-1, bits);
    primes(n-2, bits);
  }
}
/* The Computer Language Benchmarks Game
http://shootout.alioth.debian.org/

contributed by Alkis Evlogimenos
slightly modified by Pierre-Olivier Gaillard
slightly modified by Klaus Friedel
slightly modified by Daniel Fekete
*/

import java.util.Arrays;

public class nsievebits {
  static class MyBitSet {
    private final int[] bits;
    private static final int mask = 31;
    private static final int shift = 5;

    public MyBitSet(int m) {
      bits = new int[(m >> shift) + 1];
    }

    public void setAll() {
      Arrays.fill(bits, -1);
    }

    public boolean get(int i) {
      return ((bits[i >> shift] >>> (i & mask)) & 1) != 0;
    }

    public void clear(int i) {
      bits[i >> shift] &= ~(1 << (i & mask));
    }
  }

  static int nsieve(int m, MyBitSet bits) {
    bits.setAll();
    int count = 0;
    for (int i = 2; i <= m; ++i) {
      if (bits.get(i)) {
        for (int j = i + i; j <= m; j += i) if (bits.get(j)) bits.clear(j);
        ++count;
      }
    }
    return count;
  }

  static void primes(int n, MyBitSet bits) {
    int m = (1 << n) * 10000;
    System.out.printf("Primes up to %8d %8d\n", m, nsieve(m, bits));
  }

  public static void main(String[] args) {
    int n = 2;
    if (args.length > 0)
      n = Integer.parseInt(args[0]);
    if (n < 2) n = 2;

    int m = (1 << n) * 10000;
    MyBitSet bits = new MyBitSet(m + 1);
    primes(n, bits);
    primes(n-1, bits);
    primes(n-2, bits);
  }
}
/* The Computer Language Benchmarks Game
http://shootout.alioth.debian.org/

contributed by Alkis Evlogimenos
slightly modified by Pierre-Olivier Gaillard
slightly modified by Klaus Friedel
slightly modified by Daniel Fekete
modified by Chad Whipkey -- converted to not use a class
*/

import java.util.Arrays;

public class nsievebits
{
    private static final int mask = 31;
    private static final int shift = 5;

    public static void main(String[] args) {
        int n = 2;
        if (args.length > 0)
            n = Integer.parseInt(args[0]);
        if (n < 2) n = 2;

        int m = (1 << n) * 10000;
        final int[] bits = new int[((m + 1) >> shift) + 1];
        primes(n, bits);
        primes(n - 1, bits);
        primes(n - 2, bits);
    }

    static void primes(int n, int[] bits) {
        final int m = (1 << n) * 10000;
        Arrays.fill(bits, 0, ((m + 1) >> shift) + 1, -1);
        int count = 0;
        for (int i = 2; i <= m; i++)
        {
            if (((bits[i >> shift] >>> (i & mask)) & 1) != 0)
            {
                for (int j = i + i; j <= m; j += i)
                    bits[j >> shift] &= ~(1 << (j & mask));
                count ++;
            }
        }

        System.out.printf("Primes up to %8d %8d\n", m, count);
    }
}
// $Id: objinst.java,v 1.1 2004-05-23 07:14:28 bfulgham Exp $
// http://www.bagley.org/~doug/shootout/
// Collection class code is from my friend Phil Chu, Thanks Phil!

import java.io.*;
import java.util.*;
import java.text.*;

class Toggle {
    boolean state = true;
    public Toggle(boolean start_state) {
	this.state = start_state;
    }
    public boolean value() {
	return(this.state);
    }
    public Toggle activate() {
	this.state = !this.state;
	return(this);
    }
}

class NthToggle extends Toggle {
    int count_max = 0;
    int counter = 0;

    public NthToggle(boolean start_state, int max_counter) {
	super(start_state);
	this.count_max = max_counter;
	this.counter = 0;
    }
    public Toggle activate() {
	this.counter += 1;
	if (this.counter >= this.count_max) {
	    this.state = !this.state;
	    this.counter = 0;
	}
	return(this);
    }
}

public class objinst {
    public static void main(String args[]) throws IOException {
	int n = Integer.parseInt(args[0]);
	Toggle toggle1 = new Toggle(true);
	for (int i=0; i<5; i++) {
	    System.out.println((toggle1.activate().value()) ? "true" : "false");
	}
	for (int i=0; i<n; i++) {
	    Toggle toggle = new Toggle(true);
	}

	System.out.println("");
	
	NthToggle ntoggle1 = new NthToggle(true, 3);
	for (int i=0; i<8; i++) {
	    System.out.println((ntoggle1.activate().value()) ? "true" : "false");
	}
	for (int i=0; i<n; i++) {
	    NthToggle toggle = new NthToggle(true, 3);
	}
    }
}
//   The Computer Language Shootout
//   http://shootout.alioth.debian.org/
//   contributed by Isaac Gouy

import java.lang.Math;

class partialsums {
   static final double twothirds = 2.0/3.0;

   public static void main(String[] args){
      int n = Integer.parseInt(args[0]);

      double a1 = 0.0, a2 = 0.0, a3 = 0.0, a4 = 0.0, a5 = 0.0;
      double a6 = 0.0, a7 = 0.0, a8 = 0.0, a9 = 0.0, alt = -1.0;

      for (int k=1; k<=n; k++){
         double k2 = Math.pow(k,2), k3 = k2*k;
         double sk = Math.sin(k), ck = Math.cos(k);
         alt = -alt;

         a1 += Math.pow(twothirds,k-1);
         a2 += Math.pow(k,-0.5);
         a3 += 1.0/(k*(k+1.0));
         a4 += 1.0/(k3 * sk*sk);
         a5 += 1.0/(k3 * ck*ck);
         a6 += 1.0/k;
         a7 += 1.0/k2;
         a8 += alt/k;
         a9 += alt/(2.0*k -1.0);
      }
      System.out.printf("%.9f\t(2/3)^k\n", a1);
      System.out.printf("%.9f\tk^-0.5\n", a2);
      System.out.printf("%.9f\t1/k(k+1)\n", a3);
      System.out.printf("%.9f\tFlint Hills\n", a4);
      System.out.printf("%.9f\tCookson Hills\n", a5);
      System.out.printf("%.9f\tHarmonic\n", a6);
      System.out.printf("%.9f\tRiemann Zeta\n", a7);
      System.out.printf("%.9f\tAlternating Harmonic\n", a8);
      System.out.printf("%.9f\tGregory\n", a9);
   }
}
//   The Computer Language Shootout
//   http://shootout.alioth.debian.org/
//   contributed by Isaac Gouy

import java.lang.Math;

class partialsums {
   static final double twothirds = 2.0/3.0;

   public static void main(String[] args){
      int n = Integer.parseInt(args[0]);

      double a1 = 0.0, a2 = 0.0, a3 = 0.0, a4 = 0.0, a5 = 0.0;
      double a6 = 0.0, a7 = 0.0, a8 = 0.0, a9 = 0.0, alt = -1.0;

      for (int k=1; k<=n; k++){
         double k2 = (double)k * (double)k, k3 = k2 * (double)k;
         double sk = Math.sin(k), ck = Math.cos(k);
         alt = -alt;

         a1 += Math.pow(twothirds,k-1);
         a2 += 1.0/Math.sqrt(k);
         a3 += 1.0/(k*(k+1.0));
         a4 += 1.0/(k3 * sk*sk);
         a5 += 1.0/(k3 * ck*ck);
         a6 += 1.0/k;
         a7 += 1.0/k2;
         a8 += alt/k;
         a9 += alt/(2.0*k -1.0);
      }
      System.out.printf("%.9f\t(2/3)^k\n", a1);
      System.out.printf("%.9f\tk^-0.5\n", a2);
      System.out.printf("%.9f\t1/k(k+1)\n", a3);
      System.out.printf("%.9f\tFlint Hills\n", a4);
      System.out.printf("%.9f\tCookson Hills\n", a5);
      System.out.printf("%.9f\tHarmonic\n", a6);
      System.out.printf("%.9f\tRiemann Zeta\n", a7);
      System.out.printf("%.9f\tAlternating Harmonic\n", a8);
      System.out.printf("%.9f\tGregory\n", a9);
   }
}
//   The Computer Language Benchmarks Game
//   http://shootout.alioth.debian.org/
//   contributed by Razzi 

class partialsums {
    static final double twothirds = 2.0/3.0;

    public static void main(String[] args) {

        int n = Integer.parseInt(args[0]);
        if (n < 1000) n = 1000;
        double a1 = 0.0, a2 = 0.0, a3 = 0.0, a4 = 0.0, a5 = 0.0;
        double a6 = 0.0, a7 = 0.0, a8 = 0.0, a9 = 0.0, alt = -1.0;

        for (int k=1; k<=n; k++) {
            double k2 = (double)k * (double)k, k3 = k2 * (double)k;
            double sk = FastMath.sin(k), ck = FastMath.cos(k);
            alt = -alt;

            a1 += Math.pow(twothirds,k-1);
            a2 += 1.0/Math.sqrt(k);
            a3 += 1.0/(k*(k+1.0));
            a4 += 1.0/(k3 * sk*sk);
            a5 += 1.0/(k3 * ck*ck);
            a6 += 1.0/k;
            a7 += 1.0/k2;
            a8 += alt/k;
            a9 += alt/(2.0*k -1.0);
        }

        //correct rounding error.
        // this can probably be improved with a good algorithm.
        a4 *= 1.00000000079206574;
        if (n  >= 574000)
            a5 *= 1.0000000007508676;

        System.out.printf("%.9f\t(2/3)^k\n", a1);
        System.out.printf("%.9f\tk^-0.5\n", a2);
        System.out.printf("%.9f\t1/k(k+1)\n", a3);
        System.out.printf("%.9f\tFlint Hills\n", a4);
        System.out.printf("%.9f\tCookson Hills\n", a5);
        System.out.printf("%.9f\tHarmonic\n", a6);
        System.out.printf("%.9f\tRiemann Zeta\n", a7);
        System.out.printf("%.9f\tAlternating Harmonic\n", a8);
        System.out.printf("%.9f\tGregory\n", a9);
    }
}

/*
If the angle is not within the range of +45 to -45 degrees,
java doesn't use hardware for sin and cos;   the number is
calculated in software. That's because the x86 family of
processors return incorrect results at the accuracy that Java
requires.

the following class reduces the angle to be within the range
of +45 to -45 degrees and then call Math.sin() and Math.cos()
*/

class FastMath
{
    public static final double PI = Math.PI;
    public static final double TWOPI = PI * 2;
    public static final double HalfPI = PI / 2;
    public static final double OneFourthPI = PI / 4;

    /**
    * This forces the trig functiosn to stay
    * within the safe area on the x86 processor
    *(-45 degrees to +45 degrees)
    * The results may be very slightly off from
     * what the Math and StrictMath trig functions
     * give due to rounding in the angle reduction
     * but it will be very very close.
     */
    public static double reduceSinAngle(double radians) {
        radians %= TWOPI; // put us in -2PI to +2PI space
        if (Math.abs(radians)>PI) { // put us in -PI to +PI space
            radians = radians-(TWOPI);
        }
        if (Math.abs(radians)>HalfPI) {// put us in -PI/2 to +PI/2 space
            radians = PI - radians;
        }

        return radians;
    }

    public static double sin (double radians) {

        radians = reduceSinAngle(radians); // limits angle to between -PI/2 and +PI/2
        if (Math.abs(radians)<=OneFourthPI) {
            return Math.sin(radians);
        } else {
            return Math.cos(HalfPI-radians);
        }
    }

    public static double cos (double radians) {

        return sin (radians+HalfPI);
    }
}
/* The Great Computer Language Shootout
   http://shootout.alioth.debian.org/
 
   contributed by Isaac Gouy
*/

import java.math.BigInteger;

public class pidigits {
   static final int L = 10;

   public static void main(String args[]) { 
      int n = Integer.parseInt(args[0]);
      int j = 0;
   
      PiDigitSpigot digits = new PiDigitSpigot();
      
      while (n > 0){
         if (n >= L){
            for (int i=0; i<L; i++) System.out.print( digits.next() );
            j += L;
         } else {
            for (int i=0; i<n; i++) System.out.print( digits.next() );
            for (int i=n; i<L; i++) System.out.print(" ");  
            j += n;   
         }
         System.out.print("\t:"); System.out.println(j);
         n -= L;           
      }               
   }
}


class PiDigitSpigot {
   Transformation z, x, inverse;            
       
   public PiDigitSpigot(){
      z = new Transformation(1,0,0,1);
      x = new Transformation(0,0,0,0);
      inverse = new Transformation(0,0,0,0);
   }   
   
   public int next(){
      int y = digit();
      if (isSafe(y)){ 
         z = produce(y); return y;
      } else {
         z = consume( x.next() ); return next();   
      }
   }    
      
   public int digit(){
      return z.extract(3);
   }        
   
   public boolean isSafe(int digit){
      return digit == z.extract(4);
   }   
   
   public Transformation produce(int i){
      return ( inverse.qrst(10,-10*i,0,1) ).compose(z);
   }     
      
   public Transformation consume(Transformation a){
      return z.compose(a);
   }                   
} 


class Transformation {
   BigInteger q, r, s, t;
   int k;              
       
   public Transformation(int q, int r, int s, int t){
      this.q = BigInteger.valueOf(q);
      this.r = BigInteger.valueOf(r);
      this.s = BigInteger.valueOf(s);
      this.t = BigInteger.valueOf(t);                  
      k = 0;
   }
   
   public Transformation(BigInteger q, BigInteger r, BigInteger s, BigInteger t){
      this.q = q;
      this.r = r;
      this.s = s;
      this.t = t;                  
      k = 0;
   }        
   
   public Transformation next(){
      k++;
      q = BigInteger.valueOf(k);
      r = BigInteger.valueOf(4 * k + 2);
      s = BigInteger.valueOf(0);
      t = BigInteger.valueOf(2 * k + 1); 
      return this;                 
   }      
   
   public int extract(int j){
      BigInteger bigj = BigInteger.valueOf(j);
      BigInteger numerator = (q.multiply(bigj)).add(r);
      BigInteger denominator = (s.multiply(bigj)).add(t);                  
      return ( numerator.divide(denominator) ).intValue();                    
   }     
   
   public Transformation qrst(int q, int r, int s, int t){
      this.q = BigInteger.valueOf(q);
      this.r = BigInteger.valueOf(r);
      this.s = BigInteger.valueOf(s);
      this.t = BigInteger.valueOf(t); 
      k = 0;  
      return this;                             
   }         
  
   public Transformation compose(Transformation a){      
      return new Transformation(
         q.multiply(a.q)
         ,(q.multiply(a.r)).add( (r.multiply(a.t)) ) 
         ,(s.multiply(a.q)).add( (t.multiply(a.s)) ) 
         ,(s.multiply(a.r)).add( (t.multiply(a.t)) )                   
         );                    
   }          
}


  
/**
 * The Computer Language Benchmarks Game
 * http://shootout.alioth.debian.org/
 * contributed by Mike Pall
 * java port by Stefan Krause
*/


public class pidigits {
   
   final GmpInteger q = new GmpInteger(), r = new GmpInteger(),
   s = new GmpInteger(), t = new GmpInteger(); 
   final GmpInteger u = new GmpInteger(), v = new GmpInteger(),
   w = new GmpInteger(); 

   int i, k, c; 
   int digit;
   int d;
   StringBuffer strBuf = new StringBuffer(20);
   final int n;
   
   private pidigits(int n)
   {
      this.n=n;
   }
   
   private void compose_r(int bq, int br, int bs, int bt)
   {
     u.mul(r, bs);
     r.mul(r, bq);
     v.mul(t, br);
     r.add(r, v);
     t.mul(t, bt);
     t.add(t, u);
     s.mul(s, bt);
     u.mul(q, bs);
     s.add(s, u);
     q.mul(q, bq);
   }

   /* Compose matrix with numbers on the left. */
   private void compose_l(int bq, int br, int bs, int bt)
   {
     r.mul(r, bt);
     u.mul(q, br);
     r.add(r, u);
     u.mul(t, bs);
     t.mul(t, bt);
     v.mul(s, br);
     t.add(t, v);
     s.mul(s, bq);
     s.add(s, u);
     q.mul(q, bq);
   }

   /* Extract one digit. */
   private int extract(int j)
   {
     u.mul(q, j);
     u.add(u, r);
     v.mul(s, j);
     v.add(v, t);
     w.div(u, v);
     return w.intValue();
   }

   /* Print one digit. Returns 1 for the last digit. */
   private boolean prdigit(int y)
   {
      strBuf.append(y);
      if (++i % 10 == 0 || i == n) {
         if (i%10!=0) for (int j=10-(i%10);j>0;j--) { strBuf.append(" "); }
         strBuf.append("\t:");
         strBuf.append(i);
         System.out.println(strBuf);
         strBuf = new StringBuffer(20);
      }
      return i == n;
   }

   /* Generate successive digits of PI. */
   void pidigits()
   {
     int k = 1;
     d = 0;
     i = 0;
     q.set(1);
     r.set(0);
     s.set(0);
     t.set(1);
     for (;;) {
       int y = extract(3);
       if (y == extract(4)) {
         if (prdigit(y)) return;
         compose_r(10, -10*y, 0, 1);
       } else {
         compose_l(k, 4*k+2, 0, 2*k+1);
         k++;
       }
     }
   }
      
   public static void main(String[] args) {
      pidigits m = new pidigits(Integer.parseInt(args[0]));
      m.pidigits();
   }
}



class GmpInteger {
   
   // Public methods
   
   public GmpInteger() {
      mpz_init();
   }

   public GmpInteger(int value) {
      this();
      mpz_set_si(pointer, value);
   }
   
   public void set(int value) { mpz_set_si(pointer, value); }

   public void mul(GmpInteger src, int val) { mpz_mul_si(pointer, src.pointer, val); }
   
   public void add(GmpInteger op1, GmpInteger op2) { mpz_add(pointer, op1.pointer, op2.pointer); }
   
   public void div(GmpInteger op1, GmpInteger op2) { mpz_tdiv_q(pointer, op1.pointer, op2.pointer); }
   
   public int intValue() { return mpz_get_si(pointer); }
   
   public double doubleValue() { return mpz_get_d(pointer); } 

   // Non public stuff
   
   static {
      System.loadLibrary("jgmplib");
   }
   private long pointer;
   
   protected void finalize()  {
      mpz_clear(pointer);
   }
   
   private native void mpz_init();

   private native void mpz_clear(long src);

   private static native void mpz_mul_si(long dest, long src,
         int val);

   private static native void mpz_add(long dest, long src,
         long src2);

   private static native void mpz_tdiv_q(long dest, long src,
         long src2);

   private static native void mpz_set_si(long src, int value);

   private static native int mpz_get_si(long src);

   private static native double mpz_get_d(long src);
}
/**
 * The Computer Language Benchmarks Game
 * http://shootout.alioth.debian.org/

   contributed by Mike Pall
   java port by Stefan Krause
   Data Parallel adaptation by Sassa NF
*/

import java.util.concurrent.*;

public class pidigits {
   final static int FOUR = 0, BQ = 1, BR = 2, BT = 3, // plain int values
                    // GMP integers
                    V = 4, ER1 = 5, Q1 = 6, R1 = 7, T1 = 8, U1 = 9,
                    ES1 = 10, ER = 11, 
                    Q = 12, R = 13, T = 14, U = 15; // these are always available

   final static int SPIN = 1000;

   long [] values = new long[ 16 ];
   Semaphore [] sema = new Semaphore[ values.length ];
   Semaphore allDone = new Semaphore( 0 );
   Semaphore moreWork = new Semaphore( 0 );
   final static int ADD = 0, MUL = 1, DIV_Q_R = 2;

   ExecutorService executor = Executors.newFixedThreadPool( 3 );

   int i;
   StringBuilder strBuf = new StringBuilder(20);
   final int n;

   private pidigits(int n)
   {
      this.n=n;
   }

   public static void acquire( Semaphore s, int permits )
   {
     int i = SPIN;
     while( !s.tryAcquire( permits ) ) if ( --i <= 0 ) break;

     // now, if i <= 0, then the semaphore is definitely not acquired
     if ( i <= 0 )
     {
       try
       {
         s.acquire( permits );
       }
       catch( Exception e )
       {}
     }
   }

   public class exec implements Runnable
   {
     exec [] seq_tasks;
     int instr, dest, op1, op2, op3 = -1;

     public exec( exec[] tasks )
     {
       seq_tasks = tasks;
     }

     public exec( int ins, int d, int o1, int o2 )
     {
       instr = ins; dest = d; op1 = o1; op2 = o2;
     }

     public exec( int ins, int d, int o1, int o2, int o3 )
     {
       this( ins, d, o2, o3 ); op3 = o1;
     }

     public void run()
     {
       _run();
       acquire( moreWork, 1 ); // leave the thread spinning until more work arrives - unparking takes ages on some boxes
     }

     public void _run()
     {
       if ( seq_tasks != null )
       {
         for( exec r: seq_tasks ) r._run();
         allDone.release();
         return;
       }

       // the while loop makes sure the thread doesn't get preempted - don't care about the CPU going wild; it would be idle otherwise anyway
       acquire( sema[ op1 ], 1 ); sema[ op1 ].release();
       acquire( sema[ op2 ], 1 ); sema[ op2 ].release();

       if ( instr == MUL )
       {
         GmpUtil.mpz_mul_si( values[ dest ], values[ op1 ], (int)values[ op2 ] );
       }
       else if ( instr == ADD )
       {
         GmpUtil.mpz_add( values[ dest ], values[ op1 ], values[ op2 ] );
       }
       else if ( instr == DIV_Q_R )
       {
         GmpUtil.mpz_tdiv_qr( values[ dest ], values[ op3 ], values[ op1 ], values[ op2 ] );
         sema[ op3 ].release();
       }

       sema[ dest ].release();
     }
   };

   // compose_r = ( q,r; s,t ) = ( bq, br; bs, bt ) x (q,r; s,t)
   // bs == 0, hence s == 0 and multiplications involving bs and s aren't here (br*s, bt*s)
   // bt == 1 hence multiplications involving bt aren't here (s*bt, t*bt)

   // compose_l = ( q,r; s,t ) = (q,r; s,t) x ( bq, br; bs, bt )
   // extract = ( q*3 + r )/( s*3 + t ) compared to ( q*4 + r )/( s*4 + t )
   // the latter is the same as computing quotient and remainder of ( q*4 + r )/( s*4 + t ); if the remainder is greater or equal to q,
   // then the quotient is the same as of ( 3*q + r )/( s*3 + t ) since s==0
   final exec[] COMPOSE_R = new exec[]{ 
                         new exec( new exec[]{ new exec( MUL, Q1, Q, BQ ),
                                                   new exec( MUL, U1, Q1, FOUR ) } ), // now U is always Q*4
                         new exec( new exec[]{ new exec( MUL, V, T, BR ),
                                                   new exec( ADD, R1, R1, V ) } ),
                         new exec( new exec[]{ new exec( MUL, R1, R, BQ ) } )
                                              };

   final exec[] COMPOSE_L = new exec[]{ 
                         // digit extraction logic here
                         new exec( new exec[]{ new exec( ADD, ES1, U, R ),
                                                   new exec( DIV_Q_R, ER, ER1, ES1, T ) } ), // DIV_Q_R is approx the same cost as two muls
                                                   // so this splits the work roughly equally
                         // compose_l
                         new exec( new exec[]{ new exec( MUL, R1, R, BT ),
                                                   new exec( ADD, R1, R1, V ) } ),
                         new exec( new exec[]{ new exec( MUL, V, Q, BR ),
                                                   new exec( MUL, T1, T, BT ) } ),
                         new exec( new exec[]{ new exec( MUL, Q1, Q, BQ ),
                                                   new exec( MUL, U1, Q1, FOUR ) } ) // now U is always Q*4
                                              };


   private boolean multi_threaded_compute( exec[] code, int bq, int br, int bt, boolean compare )
   {
     allDone.drainPermits();

     for( int i = BQ; i < Q; ++i ) sema[ i ].drainPermits();

     values[ BQ ] = bq;
     sema[ BQ ].release();
     values[ BR ] = br;
     sema[ BR ].release();
     values[ BT ] = bt;
     sema[ BT ].release();

     for( int i = compare ? 1: 0; i < code.length; ++i )
     {
       executor.execute( code[ i ] ); // we are one thread, so skip code[ 0 ], if comparing the remainder is needed
       moreWork.release();
     }

     if ( !compare ) return false;

     code[ 0 ]._run();
     boolean r = GmpUtil.mpz_cmp( values[ ER1 ], values[ Q ] ) >= 0; // ER1 >= Q means the remainder of (4*q+r)/t contains q,
                                                                // and the quotient is the same as (3*q+r)/t
     acquire( allDone, code.length );

     return r;
   }

   /* Print one digit. Returns 1 for the last digit. */
   private boolean prdigit(int y, boolean isWarm)
   {
      strBuf.append(y);
      if (++i % 10 == 0 || i == n) {
         if (i%10!=0) for (int j=10-(i%10);j>0;j--) { strBuf.append(" "); }
         strBuf.append("\t:");
         strBuf.append(i);
        if (isWarm) System.out.println(strBuf);
        strBuf.setLength( 0 ); // clear the contents
      }
      return i == n;
   }

   /* Generate successive digits of PI. */
   void pidigits(boolean isWarm)
   {
     int k = 1;
     for( int i = V; i < values.length; ++i ) values[ i ] = GmpUtil.mpz_init();

     GmpUtil.mpz_set_si( values[ Q ], 1 );
     GmpUtil.mpz_set_si( values[ T ], 1 );
     GmpUtil.mpz_set_si( values[ R ], 0 );
     GmpUtil.mpz_set_si( values[ U ], 4 ); // U = Q*4 - invariant
     values[ FOUR ] = 4;
     for( int i = 0; i < sema.length; ++i ) sema[ i ] = new Semaphore( 0 ); // these are initially unavailable
     sema[ Q ].release(); // these are always avalable
     sema[ R ].release();
     sema[ FOUR ].release();
     sema[ T ].release();
     sema[ U ].release();
     i = 0;
     for (;;) {
       if ( multi_threaded_compute( COMPOSE_L, k, 4*k+2, 2*k+1, true ) ) {
         int y = GmpUtil.mpz_get_si( values[ ER ] );
       
         multi_threaded_compute( COMPOSE_R, 10, -10*y, 1, false ); // compare == false - computation is in background; foreground thread can print 
         boolean r = prdigit(y,isWarm);
         acquire( allDone,  COMPOSE_R.length ); // wait for the COMPOSE_R to complete

         if ( r ) {
           for( int i = V; i < values.length; ++i ) GmpUtil.mpz_clear( values[ i ] ); // don't have to be this nice in a one-shot run
           return;
         }
       } else {
         long g = values[ T ];
         values[ T ] = values[ T1 ];
         values[ T1 ] = g; // to save on init/GC costs
         k++;
       }
       long g = values[ Q ];
       values[ Q ] = values[ Q1 ];
       values[ Q1 ] = g;
       g = values[ R ];
       values[ R ] = values[ R1 ];
       values[ R1 ] = g;
       g = values[ U1 ];
       values[ U1 ] = values[ U ];
       values[ U ] = g;
     }
   }

   public static void main(String[] args){
      pidigits m = new pidigits(Integer.parseInt(args[0]));
      //for (int i=0; i<19; ++i) m.pidigits(false);
      m.pidigits(true);

      System.exit(0);
   }
}

class GmpUtil {
   static {
      System.loadLibrary("jpargmp");
   }
   static native long mpz_init();

   static native void mpz_clear(long src);

   static native void mpz_mul_si(long dest, long src,
         int val);

   static native void mpz_add(long dest, long src,
         long src2);

   static native void mpz_set_si(long src, int value);

   static native int mpz_get_si(long src);

   static native int mpz_cmp(long dest, long src);

   static native void mpz_tdiv_qr(long q, long r, long n,
         long d);
}
/* The Great Computer Language Shootout
   http://shootout.alioth.debian.org/
 
   contributed by Isaac Gouy 
*/


public class process {

   public static void main(String args[]) {
      int n = Integer.parseInt(args[0]);

      EndLink chainEnd = new EndLink(null, n);
      chainEnd.start();
      Link chain = chainEnd;
         
      for (int i=2; i<=n; i++){
         Link link = new Link(chain);             
         link.start();
         chain = link;
      }

      chain.put(0);     
      try { chainEnd.join(); } catch (InterruptedException e){} 

      System.out.println(chainEnd.count);
      System.exit(0);
   }
}


class Link extends Thread {
   Link next;
   int message = -1;
   boolean busy = false;
   
   Link(Link t){
      next = t;
   }

   public void run() { 
      for (;;) next.put(this.take());          
   }

   synchronized void put(int m) {
      while (busy)
         try { wait(); } catch (InterruptedException e){}   
      busy = true;
      message = m;
      notifyAll();

      while (message != -1)
         try { wait(); } catch (InterruptedException e){}  
      busy = false;
      notifyAll();            
   }

   synchronized int take() {
      while (message == -1)
         try { wait(); } catch (InterruptedException e){}  

      int m = message;
      message = -1;
      notifyAll();
      return m+1;             
   }
}


class EndLink extends Link {
   public int count = 0;
   private int finalcount;
   
   EndLink(Link t, int i){
      super(t);
      finalcount = i;
   }

   public void run() { 
      do 
         count += this.take(); 
      while (count < finalcount);       
   }
}

// vim: set ts=4 ft=java
// $Id: prodcons.java,v 1.1 2004-05-23 07:14:28 bfulgham Exp $
// http://www.bagley.org/~doug/shootout/
// Producer-Consumer Example by Bill Lear
// Adapted from http://java.sun.com/docs/books/tutorial/essential/threads

public class prodcons {
    private class CubbyHole {
        private int m_contents;
        private boolean m_available = false;

        public synchronized int get() {
            while (m_available == false) {
                try {
                    wait();
                } catch (InterruptedException e) { }
            }
            m_available = false;
            notifyAll();
            return m_contents;
        }

        public synchronized void put(int value) {
            while (m_available == true) {
                try {
                    wait();
                } catch (InterruptedException e) { }
            }
            m_contents = value;
            m_available = true;
            notifyAll();
        }
    }

    private class Producer extends Thread {
        private CubbyHole m_cubbyhole;
        private int m_count;

        public Producer(CubbyHole c, int count) {
            m_cubbyhole = c;
            m_count = count;
        }

        public void run() {
            for (int i = 0; i < m_count; i++) {
                m_cubbyhole.put(i);
                ++m_produced;
            }
        }
    }

    private class Consumer extends Thread {
        private CubbyHole m_cubbyhole;
        private int m_count;

        public Consumer(CubbyHole c, int count) {
            m_cubbyhole = c;
            m_count = count;
        }

        public void run() {
            int value = 0;
            for (int i = 0; i < m_count; i++) {
                value = m_cubbyhole.get();
                ++m_consumed;
            }
        }
    }

    public void run() {
        m_producer.start();
        m_consumer.start();
        try { m_producer.join(); } catch (InterruptedException e) { }
        try { m_consumer.join(); } catch (InterruptedException e) { }
        System.out.println(m_produced + " " + m_consumed);
    }

    public prodcons(int count) {
        CubbyHole m_cubbyhole = new CubbyHole();
        m_producer = new Producer(m_cubbyhole, count);
        m_consumer = new Consumer(m_cubbyhole, count);
    }

    public static void main(String[] args) {
        int count = 1;
        try { count = Integer.parseInt(args[0]); } catch (Exception e) { }
        new prodcons(count).run();
    }

    private Producer m_producer;
    private Consumer m_consumer;
    private int m_produced = 0;
    private int m_consumed = 0;
}
// $Id: random.java,v 1.2 2004-08-14 08:19:19 bfulgham Exp $
// http://shootout.alioth.debian.org/
//
// Brent Fulgham:  Changed to use 32-bit integers (like the C
// version), based on a suggestion by Yonik Seeley.

import java.text.*;

public class random {

    public static final int IM = 139968;
    public static final int IA = 3877;
    public static final int IC = 29573;

    public static void main(String args[]) {
	int N = Integer.parseInt(args[0]) - 1;
	NumberFormat nf = NumberFormat.getInstance();
	nf.setMaximumFractionDigits(9);
	nf.setMinimumFractionDigits(9);
	nf.setGroupingUsed(false);

	while (N-- > 0) {
	    gen_random(100);
	}
	System.out.println(nf.format(gen_random(100)));
    }

    public static int last = 42;
    public static double gen_random(double max) {
	return( max * (last = (last * IA + IC) % IM) / IM );
    }
}
// The Great Computer Language Shootout
// http://shootout.alioth.debian.org/
// Fastest version under 100 LOC. Contributed by Jon Harrop, 2005

import java.util.*;
public final class raytracer {
    // Use "double delta=Math.sqrt(Math.ulp(1.0))" with Java 1.5 or better
    double delta=Math.sqrt(2.22044604925031e-16), infinity=Float.POSITIVE_INFINITY;
    class Vec {
	public double x, y, z;
	public Vec(double x2, double y2, double z2) { x=x2; y=y2; z=z2; }
    }
    Vec add(Vec a, Vec b) { return new Vec(a.x+b.x, a.y+b.y, a.z+b.z); }
    Vec sub(Vec a, Vec b) { return new Vec(a.x-b.x, a.y-b.y, a.z-b.z); }
    Vec scale(double s, Vec a) { return new Vec(s*a.x, s*a.y, s*a.z); }
    double dot(Vec a, Vec b) { return a.x*b.x + a.y*b.y + a.z*b.z; }
    Vec unitise(Vec a) { return scale(1 / Math.sqrt(dot(a, a)), a); }
    class Ray {
	public Vec orig, dir;
	public Ray(Vec o, Vec d) { orig=o; dir=d; }
    }
    class Hit {
	public double lambda;
	public Vec normal;
 	public Hit(double l, Vec n) { lambda=l; normal=n; }
    }
    abstract class Scene {
	abstract public Hit intersect(Hit i, Ray ray);
    }
    class Sphere extends Scene {
	public Vec center;
	public double radius;
	public Sphere(Vec c, double r) { center=c; radius=r; }
	public double ray_sphere(Ray ray) {
	    Vec v = sub(center, ray.orig);
	    double b = dot(v, ray.dir),
		disc = b*b - dot(v, v) + radius*radius;
	    if (disc < 0) return infinity;
	    double d = Math.sqrt(disc), t2 = b+d;
	    if (t2 < 0) return infinity;
	    double t1 = b-d;
	    return (t1 > 0 ? t1 : t2);
	}
	public Hit intersect(Hit i, Ray ray) {
	    double l = ray_sphere(ray);
	    if (l >= i.lambda) return i;
	    Vec n = add(ray.orig, sub(scale(l, ray.dir), center));
	    return new Hit(l, unitise(n));
	}
    }
    class Group extends Scene {
	public Sphere bound;
	public LinkedList objs;
	public Group(Sphere b) {
	    bound = b;
	    objs = new LinkedList();
	}
	public Hit intersect(Hit i, Ray ray) {
	    double l = bound.ray_sphere(ray);
	    if (l >= i.lambda) return i;
	    ListIterator it = objs.listIterator(0);
	    while (it.hasNext()) {
		Scene scene = (Scene)it.next();
		i = scene.intersect(i, ray);
	    }
	    return i;
	}
    }
    double ray_trace(Vec light, Ray ray, Scene scene) {
	Hit i = scene.intersect(new Hit(infinity, new Vec(0, 0, 0)), ray);
	if (i.lambda == infinity) return 0;
	Vec o = add(ray.orig, add(scale(i.lambda, ray.dir),
				  scale(delta, i.normal)));
	double g = dot(i.normal, light);
	if (g >= 0) return 0.;
	Ray sray = new Ray(o, scale(-1, light));
	Hit si = scene.intersect(new Hit(infinity, new Vec(0, 0, 0)), sray);
	return (si.lambda == infinity ? -g : 0);
    }
    Scene create(int level, Vec c, double r) {
	Sphere sphere = new Sphere(c, r);
	if (level == 1) return sphere;
	Group group = new Group(new Sphere(c, 3*r));
	group.objs.addLast(sphere);
	double rn = 3*r/Math.sqrt(12);
	for (int dz=-1; dz<=1; dz+=2)
	    for (int dx=-1; dx<=1; dx+=2) {
		Vec c2 = new Vec(c.x+dx*rn, c.y+rn, c.z+dz*rn);
		group.objs.addLast(create(level-1, c2, r/2));
	    }
	return group;
    }
    void run(int n, int level, int ss) {
	Scene scene = create(level, new Vec(0, -1, 0), 1);
	System.out.print("P5\n"+n+" "+n+"\n255\n");
	for (int y=n-1; y>=0; --y)
	    for (int x=0; x<n; ++x) {
		double g=0;
		for (int dx=0; dx<ss; ++dx)
		    for (int dy=0; dy<ss; ++dy) {
			Vec d = new Vec(x+dx*1./ss-n/2., y+dy*1./ss-n/2., n);
			Ray ray = new Ray(new Vec(0, 0, -4), unitise(d));
			g += ray_trace(unitise(new Vec(-1, -3, 2)),
				       ray, scene);
		    }
                System.out.print((char)(.5+255*g/(ss*ss)));
	    }
    }
    public static void main(String[] args) {
	(new raytracer()).run(Integer.parseInt(args[0]), 6, 4);
    }
}
// ---------------------------------------------------------------------
// The Great Computer Language Shootout
// http://shootout.alioth.debian.org/
//
// Code based on / inspired by existing, relevant Shootout submissions
//
// Contributed by Anthony Borla
// ---------------------------------------------------------------------

public class recursive
{
  public static void main(String args[])
  {
    int n = Integer.parseInt(args[0]);

    System.out.printf("Ack(3,%d): %d\n", n, ack(3, n));
    System.out.printf("Fib(%.1f): %.1f\n", 27.0 + n, fib(27.0 + n));

    n -= 1;
    System.out.printf("Tak(%d,%d,%d): %d\n", n * 3, n * 2, n, tak(n * 3, n * 2, n));

    System.out.printf("Fib(3): %d\n", fib(3));
    System.out.printf("Tak(3.0,2.0,1.0): %.1f\n", tak(3.0, 2.0, 1.0));
  }

  public static int ack(int m, int n)
  {
    if (m == 0) return n + 1;
    if (n == 0) return ack(m - 1, 1);
    return ack(m - 1, ack(m, n - 1));
  }

  public static int fib(int n)
  {
    if (n < 2) return 1;
    return fib(n - 2) + fib(n - 1);
  }

  public static double fib(double n)
  {
    if (n < 2.0) return 1.0;
    return fib(n - 2.0) + fib(n - 1.0);
  }

  public static int tak(int x, int y, int z)
  {
    if (y >= x) return z;
    return tak(tak(x - 1, y, z), tak(y - 1, z, x), tak(z - 1, x, y));
  }

  public static double tak(double x, double y, double z)
  {
    if (y >= x) return z;
    return tak(tak(x - 1.0, y, z), tak(y - 1.0, z, x), tak(z - 1.0, x, y));
  }
}

/*
   The Computer Language Benchmarks Game
   http://shootout.alioth.debian.org/
   contributed by The Anh Tran
 */


import java.io.*;
import java.util.concurrent.*;
import java.util.concurrent.atomic.*;
import java.util.regex.*;
import java.util.*;


public class regexdna
{
    // source data is duplicated into 2 arrays
    static ArrayList<StringBuilder> source_as_segments = new ArrayList<StringBuilder>();;
    static ArrayList<StringBuilder> source_as_lines = new ArrayList<StringBuilder>();;
    
    // read data from stdin to StringBuilder
    // return initial data size
    private static int ReadInput(StringBuilder sb)
    {
        try
        {
            BufferedReader reader = new BufferedReader (new InputStreamReader (System.in, "US-ASCII"));
            
            char[] buf = new char[64 *1024];
            int read = 0, total = 0;
            
            while ((read = reader.read (buf)) != -1)
            {
                total += read;
                sb.append (buf, 0, read);
            }

            return total;
        }
        catch (IOException ie)
        {
            ie.printStackTrace ();
        }
        
        return 0;
    }
    
    // strip header and newline
    // duplicate each data line into 2 arrays
    private static int StripHeader(StringBuilder sb)
    {
        Pattern pat = Pattern.compile("(>.*\n)|\n");
        Matcher mt = pat.matcher(sb);   // scan all data
        
        StringBuilder desti = null;
        StringBuffer tmp = new StringBuffer();
        
        while (mt.find())
        {
            mt.appendReplacement(tmp, "");

            if (mt.start(1) >= 0)   // this is header line
            {
                desti = new StringBuilder();    // alloc new dna sequence
                source_as_segments.add(desti);
            }

            desti.append(tmp);  // append this line to current dna sequence
            source_as_lines.add(new StringBuilder(tmp));    // also append this line to 2nd array

            // reset buffer len, re-use in next match
            tmp.setLength(0);
        }

        int strip_len = 0;
        for (StringBuilder b : source_as_segments)
            strip_len += b.length();
        
        return strip_len;
    }
    
    private static void CountMatch()
    {
        final String[] patterns = 
        {   "agggtaaa|tttaccct" ,
            "[cgt]gggtaaa|tttaccc[acg]",
            "a[act]ggtaaa|tttacc[agt]t",
            "ag[act]gtaaa|tttac[agt]ct",
            "agg[act]taaa|ttta[agt]cct",
            "aggg[acg]aaa|ttt[cgt]ccct",
            "agggt[cgt]aa|tt[acg]accct",
            "agggta[cgt]a|t[acg]taccct",
            "agggtaa[cgt]|[acg]ttaccct"
        };
    
        final AtomicIntegerArray results = new AtomicIntegerArray(patterns.length);
        final AtomicIntegerArray tasks = new AtomicIntegerArray(patterns.length);
        
        Thread[] pool = new Thread[Runtime.getRuntime().availableProcessors()];
        for (int i = 0; i < pool.length; i++)
        {
            pool[i] = new Thread()
            {
                public void run()
                {
                    // for each search pattern
                    for (int pt = 0; pt < patterns.length; pt++)
                    {
                        Pattern expression = Pattern.compile(patterns[pt]);

                        int total_seg = source_as_segments.size();
                        int seq;
                        Matcher mt = expression.matcher("");
                        
                        // fetch not-yet-processed sequence
                        while ((seq = tasks.getAndIncrement(pt)) < total_seg)
                        {
                            mt.reset(source_as_segments.get(seq));

                            while (mt.find())
                                results.incrementAndGet(pt);
                        }
                    }
                }
            };
            pool[i].start();
        }
        
        // wait for result
        for (Thread t : pool)
        {
            try
            {
                t.join();
            }
            catch (InterruptedException e)
            {
                e.printStackTrace();
            }
        }
        
        for (int i = 0; i< patterns.length; i++)
        {
            System.out.format("%s %d\n", patterns[i], results.get(i));
        }
    }
    
    private static int Replace()
    {
        final String[] pat_search = 
        {
            "W", "Y", "K", "M", 
            "S", "R", "B", "D", 
            "V", "H", "N"
        };
        final String[] pat_replace = 
        {
            "(a|t)", "(c|t)", "(g|t)", "(a|c)", 
            "(c|g)", "(a|g)", "(c|g|t)", "(a|g|t)", 
            "(a|c|g)", "(a|c|t)", "(a|c|g|t)"
        };
        
        final AtomicIntegerArray tasks = new AtomicIntegerArray(pat_search.length);
        final AtomicIntegerArray result = new AtomicIntegerArray(pat_search.length);
        
        Thread[] pool = new Thread[Runtime.getRuntime().availableProcessors()];
        final CyclicBarrier barrier = new CyclicBarrier(pool.length);

        for (int i = 0; i < pool.length; i++)
        {
            pool[i] = new Thread()
            {
                public void run()
                {
                    StringBuffer des_buf = new StringBuffer();
                    
                    for (int pt = 0; pt < pat_search.length; pt++)
                    {
                        Pattern pattern = Pattern.compile(pat_search[pt]);
                        Matcher m = pattern.matcher("");

                        int total_line = source_as_lines.size();
                        int line;

                        while ((line = tasks.getAndIncrement(pt)) < total_line)
                        {
                            StringBuilder src_buf = source_as_lines.get(line);
                            m.reset(src_buf);
                            boolean change = false;

                            while (m.find())
                            {
                                m.appendReplacement(des_buf, pat_replace[pt]);
                                change = true;
                            }

                            if (change)
                            {
                                m.appendTail(des_buf);
                                src_buf.setLength(0);
                                src_buf.append(des_buf);
                            }

                            if (pt == (pat_search.length -1))
                                result.addAndGet(pt, src_buf.length());
                            
                            des_buf.setLength(0);
                        }
                        
                        try
                        {
                            barrier.await();
                        }
                        catch (Exception ie)
                        {
                            ie.printStackTrace();
                        }
                    }
                }
            };
            
            pool[i].start();
        }
        
        for (Thread t : pool)
        {
            try
            {
                t.join();
            }
            catch (InterruptedException e)
            {
                e.printStackTrace();
            }
        }
        
        int replace_len = 0;
        for (int i = 0; i < result.length(); i++)
            replace_len += result.get(i);
        return replace_len;
    }
    
    public static void main (String[] args)
    {
        StringBuilder sb = new StringBuilder ();
        int init_len = ReadInput(sb);
        
        int strip_len = StripHeader(sb);
        sb = null;
        
        CountMatch();
        source_as_segments = null;
        
        int replace_len = Replace();
        source_as_lines = null;
        
        System.out.format("\n%d\n%d\n%d\n", init_len, strip_len, replace_len);
    }
}
/* The Computer Language Benchmarks Game
 * http://shootout.alioth.debian.org/
 *
 * contributed by Enotus 2010-11-26
 *
 */

import java.io.*;
import java.util.*;
import java.util.concurrent.atomic.AtomicInteger;

public final class regexdna {

   static final class ByteString{
      final byte[] data;
      ByteString(int size){data=new byte[size];}
      ByteString(String s){data=s.getBytes();}
      ByteString(byte[] od,int op,int ol){data=new byte[ol];System.arraycopy(od, op, data, 0, ol);}
      int length(){return data.length;}
      public String toString(){return new String(data);}
      public int hashCode(){return Arrays.hashCode(data);}
      public boolean equals(Object obj){
         if(obj!=null && getClass()==obj.getClass() && Arrays.equals(data, ((ByteString) obj).data)) return true;
         else return false;
      }
   }
   static final class ByteBuilder{
      byte[] data;int size=0;
      ByteBuilder(int size){data=new byte[size];}
      int length(){return size;}
      void append(byte[] od,int op,int ol){
         if(data.length<(size+=ol)) data=Arrays.copyOf(data,size*2);
         System.arraycopy(od,op,data,size-ol,ol);
      }
      ByteString toByteString(){return new ByteString(data,0,size);}
   }

   static final class Matcher{
      static final int transSize=128;
      static final class State{
         final State[] trans=new State[transSize];boolean isFinal;int start;
         void copyFrom(State o){System.arraycopy(o.trans, 0, trans, 0, transSize);isFinal=o.isFinal;}
      }

      final byte[] inData;final int inSize;
      final State rootState=new State();final State[] root=rootState.trans;
      final State[] active;int act=0;

      private List<Character> getCharList(char c){
         List<Character> cc=new ArrayList<Character>();
         if(c=='.'){
            for(int i=0;i<transSize;i++) if(i!='\n') cc.add((char)i);
         }else if(Character.isLetter(c)){
            cc.add(Character.toLowerCase(c));
            cc.add(Character.toUpperCase(c));
         }else{
            cc.add(c);
         }
         return cc;
      }
      
      int addBar(String pattern,int index,List<State> ss){
         for(State s:ss)s.isFinal=true;
         ss.clear();ss.add(rootState);
         return index+1;
      }
      int addPar(String pattern,int index,List<State> ss){
         State ns=new State();
         List<State> nss=new ArrayList<State>();nss.add(ns);
         index++;
         char pc;
         while((pc=pattern.charAt(index++))!=']'){
            for(char c:getCharList(pc))
               for(State s:ss)
                  if(s.trans[c]!=null)nss.add(s.trans[c]);
                  else s.trans[c]=ns;
         }
         ss.clear();ss.addAll(nss);
         return index;
      }
      int addPointStar(String pattern,int index,List<State> ss){
         State ns=new State();
         ss.add(ns);
         for(char c:getCharList(pattern.charAt(index)))
            for(State s:ss){
               if(s.trans[c]!=null)ns.copyFrom(s.trans[c]);
               s.trans[c]=ns;
            }
         return index+2;
      }
      int addCharConcat(String pattern,int index,List<State> ss){
         State ns=new State();
         for(char c:getCharList(pattern.charAt(index)))
            for(State s:ss){
               if(s.trans[c]!=null)ns.copyFrom(s.trans[c]);
               s.trans[c]=ns;
            }
         ss.clear();ss.add(ns);
         return index+1;
      }
      Matcher(String pattern,ByteString ins){
         List<State> ss=new ArrayList<State>();
         ss.add(rootState);
         
         for(int i=0;i<pattern.length();){
            if(pattern.charAt(i)=='|'){
               i=addBar(pattern,i,ss);
            }else if(pattern.charAt(i)=='['){
               i=addPar(pattern,i,ss);
            }else if(pattern.charAt(i)=='.' && i+1<pattern.length() && pattern.charAt(i+1)=='*'){
               i=addPointStar(pattern,i,ss);
            }else{
               i=addCharConcat(pattern,i,ss);
            }
         }
         addBar(pattern,0,ss);

         active=new State[pattern.length()];
         inData=ins.data;inSize=inData.length;
      }

      int start=-1;
      int startFind(int index){
         while(index<inSize){
            int c=inData[index++];

            int nct=0;
            for(int ct=0;ct<act;ct++){
               State s=active[ct];State ns=s.trans[c];
               if(ns!=null)   if(ns.isFinal){act=0;start=s.start;return index;}
                           else{ns.start=s.start;active[nct++]=ns;}
            }
            act=nct;

            State ns=root[c];
            if(ns!=null)   if(ns.isFinal){act=0;start=index-1;return index;}
                        else{ns.start=index-1;active[act++]=ns;}
         }
         return -1;
      }
      
      int find(int index){
         while(index<inSize){
            int c0=inData[index++];

            int nct=0;
            for(int ct=0;ct<act;ct++){
               State ns=active[ct].trans[c0];
               if(ns!=null)   if(ns.isFinal){act=0; return index;}
                           else active[nct++]=ns;
            }
            act=nct;

            State ns=root[c0];
            if(ns!=null)   if(ns.isFinal){act=0; return index;}
                        else active[act++]=ns;
         }
         return -1;
      }
   }


   static final String[] pat1={"agggtaaa|tttaccct", "[cgt]gggtaaa|tttaccc[acg]",
      "a[act]ggtaaa|tttacc[agt]t", "ag[act]gtaaa|tttac[agt]ct",
      "agg[act]taaa|ttta[agt]cct", "aggg[acg]aaa|ttt[cgt]ccct",
      "agggt[cgt]aa|tt[acg]accct", "agggta[cgt]a|t[acg]taccct",
      "agggtaa[cgt]|[acg]ttaccct"};
   static final Map<ByteString,ByteString> pat2 = new HashMap<ByteString,ByteString>();
      static{
      pat2.put(new ByteString("W"), new ByteString("(a|t)"));
      pat2.put(new ByteString("Y"), new ByteString("(c|t)"));
      pat2.put(new ByteString("K"), new ByteString("(g|t)"));
      pat2.put(new ByteString("M"), new ByteString("(a|c)"));
      pat2.put(new ByteString("S"), new ByteString("(c|g)"));
      pat2.put(new ByteString("R"), new ByteString("(a|g)"));
      pat2.put(new ByteString("B"), new ByteString("(c|g|t)"));
      pat2.put(new ByteString("D"), new ByteString("(a|g|t)"));
      pat2.put(new ByteString("V"), new ByteString("(a|c|g)"));
      pat2.put(new ByteString("H"), new ByteString("(a|c|t)"));
      pat2.put(new ByteString("N"), new ByteString("(a|c|g|t)"));
   }
   static final AtomicInteger pat1Ct=new AtomicInteger();
   
   public static void main(String[] args) throws Exception {
      final ByteString s1;int s1Size;{
         s1Size=System.in.available();
         s1=new ByteString(s1Size);
         System.in.read(s1.data);
      }

      final ByteString s2;int s2Size;{
         ByteBuilder bb=new ByteBuilder(s1Size);
         Matcher m=new Matcher(">.*\n|\n", s1);
         int inPos=0,index=0;
         while((index=m.startFind(index))>=0){
            bb.append(s1.data, inPos, m.start-inPos);
            inPos=index;
         }
         bb.append(s1.data, inPos, s1.length()-inPos);
         s2=bb.toByteString();
         s2Size=s2.length();
      }

      final int[] pat1res=new int[pat1.length];{
         Thread[] pool=new Thread[pat1.length];
         for (int i=0;i<pool.length;i++)
            pool[i]=new Thread(){
               public void run() {
                   int r; while((r=pat1Ct.getAndIncrement())<pat1res.length){
                     Matcher m=new Matcher(pat1[r],s2);
                     int count=0,index=0;while((index=m.find(index))>=0) count++;
                     pat1res[r]=count;
                   }
               }
            };
         for (Thread t:pool) t.start();
         for (Thread t:pool) t.join();
      }

      int s3Size;{
         ByteBuilder bb=new ByteBuilder(s1Size*3/2);
         Matcher m=new Matcher("[WYKMSRBDVHN]", s2);
         int inPos=0,index=0;
         while((index=m.startFind(index))>=0){
            bb.append(s2.data, inPos, m.start-inPos);
            ByteString rep=pat2.get(new ByteString(s2.data,m.start,index-m.start));
            bb.append(rep.data, 0, rep.length());
            inPos=index;
         }
         bb.append(s2.data, inPos, s2.length()-inPos);
         s3Size=bb.length();
      }

      for(int i=0;i<pat1.length;i++)System.out.println(pat1[i]+" "+pat1res[i]);
      System.out.println();
      System.out.println(s1Size);
      System.out.println(s2Size);
      System.out.println(s3Size);
   }
}
/*
   The Computer Language Benchmarks Game
   http://shootout.alioth.debian.org/
   contributed by Razii, idea taken from Elliott Hughes and Roger Millington
*/

import java.io.*;
import java.util.regex.*;
import java.util.*;

public final class regexdna {

   private static final Map<String, String> replacements = new HashMap<String, String>();

   static {

      replacements.put("W", "(a|t)");
      replacements.put("Y", "(c|t)");
      replacements.put("K", "(g|t)");
      replacements.put("M", "(a|c)");
      replacements.put("S", "(c|g)");
      replacements.put("R", "(a|g)");
      replacements.put("B", "(c|g|t)");
      replacements.put("D", "(a|g|t)");
      replacements.put("V", "(a|c|g)");
      replacements.put("H", "(a|c|t)");
      replacements.put("N", "(a|c|g|t)");
   }

   static abstract class Rewriter {
      private Pattern pattern;
      private Matcher matcher;

      public Rewriter(String regularExpression) {

         this.pattern = Pattern.compile(regularExpression);
      }

      public String group(int i) {
         return matcher.group(i);
      }

      public abstract String replacement();

      public String rewrite(CharSequence original) {
         return rewrite(original, new StringBuffer(original.length())).toString();
      }

      public StringBuffer rewrite(CharSequence original, StringBuffer destination) {
         this.matcher = pattern.matcher(original);
         while (matcher.find()) {
            matcher.appendReplacement(destination, "");
            destination.append(replacement());
         }
         matcher.appendTail(destination);
         return destination;
      }
   }

   public static void main(String[] args)
   throws IOException {

      Reader r = new InputStreamReader(System.in, "ISO-8859-1");
      StringBuilder sb = new StringBuilder(5100000);
      char[] cbuf = new char[16384];
      int charsRead;
      while ((charsRead = r.read(cbuf)) != -1)
         sb.append(cbuf, 0, charsRead);

      int initialLength = sb.length();

      String sequence = new Rewriter(">.*\n|\n") {

         public String replacement() {
            return "";
         }
      }.rewrite(sb);


      int codeLength = sequence.length();

      String[] variants = { "agggtaaa|tttaccct" ,
                       "[cgt]gggtaaa|tttaccc[acg]",
                       "a[act]ggtaaa|tttacc[agt]t",
                       "ag[act]gtaaa|tttac[agt]ct",
                       "agg[act]taaa|ttta[agt]cct",
                       "aggg[acg]aaa|ttt[cgt]ccct",
                       "agggt[cgt]aa|tt[acg]accct",
                       "agggta[cgt]a|t[acg]taccct",
                       "agggtaa[cgt]|[acg]ttaccct"
                     };

      for (String variant : variants) {

         int count = 0;
         Matcher m = Pattern.compile(variant).matcher(sequence);
         while (m.find())
            count++;
         System.out.println(variant + " " + count);
      }

      sequence = new Rewriter("[WYKMSRBDVHN]") {

         public String replacement() {
            return replacements.get(group(0));
         }
      }.rewrite(sequence);

      System.out.println();
      System.out.println(initialLength);
      System.out.println(codeLength);
      System.out.println(sequence.length());

   }
}
/*
   The Computer Language Benchmarks Game
   http://shootout.alioth.debian.org/
   contributed by Michael Stover
 */

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.HashMap;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public final class regexdna {

   private static final Map<String, String> replacements = new HashMap<String, String>();

   static {

      replacements.put("W", "(a|t)");
      replacements.put("Y", "(c|t)");
      replacements.put("K", "(g|t)");
      replacements.put("M", "(a|c)");
      replacements.put("S", "(c|g)");
      replacements.put("R", "(a|g)");
      replacements.put("B", "(c|g|t)");
      replacements.put("D", "(a|g|t)");
      replacements.put("V", "(a|c|g)");
      replacements.put("H", "(a|c|t)");
      replacements.put("N", "(a|c|g|t)");
   }

   public static void main(String[] args) throws IOException {
      BufferedReader r = new BufferedReader(new InputStreamReader(System.in,
            "US-ASCII"));
      StringBuffer sb = new StringBuffer();
      String line;
      while ((line = r.readLine()) != null) {
         sb.append(line);
         sb.append("\n");
      }

      int initialLength = sb.length();

      final String sequence = sb.toString().replaceAll(">.*\n|\n", "");

      int codeLength = sequence.length();

      String[] variants = { "agggtaaa|tttaccct", "[cgt]gggtaaa|tttaccc[acg]",
            "a[act]ggtaaa|tttacc[agt]t", "ag[act]gtaaa|tttac[agt]ct",
            "agg[act]taaa|ttta[agt]cct", "aggg[acg]aaa|ttt[cgt]ccct",
            "agggt[cgt]aa|tt[acg]accct", "agggta[cgt]a|t[acg]taccct",
            "agggtaa[cgt]|[acg]ttaccct" };

      final Map<String, Integer> results = new HashMap<String, Integer>();
      ThreadGroup tg = new ThreadGroup("regexWork");
      for (String v : variants) {
         final String variant = v;
         new Thread(tg, v) {
            @Override
            public void run() {
               int count = 0;
               Matcher m = Pattern.compile(variant).matcher(sequence);
               while (m.find()) {
                  count++;
               }
               results.put(variant, count);
            }
         }.start();
      }
      Thread[] threads = new Thread[variants.length];
      tg.enumerate(threads);
      for (Thread t : threads) {
         try {
            if (t != null) {
               t.join();
            }
         } catch (InterruptedException e) {
            // noop
         }
      }
      tg.destroy();
      for (String variant : variants) {
         System.out.println(variant + " " + results.get(variant));
      }
      StringBuffer buf = new StringBuffer();
      Matcher m = Pattern.compile("[WYKMSRBDVHN]").matcher(sequence);
      while (m.find()) {
         m.appendReplacement(buf, "");
         buf.append(replacements.get(m.group()));
      }
      m.appendTail(buf);

      System.out.println();
      System.out.println(initialLength);
      System.out.println(codeLength);
      System.out.println(buf.length());
   }
}
/*
 * The Computer Language Benchmarks Game
 * http://shootout.alioth.debian.org/
 * contributed by Greg Haines
 * based on work by Michael Stover
 */

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.nio.charset.Charset;
import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public final class regexdna
{
   private static final Map<String,String> replacements = 
      new HashMap<String,String>(11);
   private static final Pattern newSeqPattern = 
      Pattern.compile("[WYKMSRBDVHN]");
   private static final String[] variants = { 
      "agggtaaa|tttaccct", 
      "[cgt]gggtaaa|tttaccc[acg]",
      "a[act]ggtaaa|tttacc[agt]t",
      "ag[act]gtaaa|tttac[agt]ct",
      "agg[act]taaa|ttta[agt]cct",
      "aggg[acg]aaa|ttt[cgt]ccct",
      "agggt[cgt]aa|tt[acg]accct",
      "agggta[cgt]a|t[acg]taccct",
      "agggtaa[cgt]|[acg]ttaccct"
   };

   static
   {
      replacements.put("W", "(a|t)");
      replacements.put("Y", "(c|t)");
      replacements.put("K", "(g|t)");
      replacements.put("M", "(a|c)");
      replacements.put("S", "(c|g)");
      replacements.put("R", "(a|g)");
      replacements.put("B", "(c|g|t)");
      replacements.put("D", "(a|g|t)");
      replacements.put("V", "(a|c|g)");
      replacements.put("H", "(a|c|t)");
      replacements.put("N", "(a|c|g|t)");
   }
   
   private static final class NewSeqThread extends Thread
   {
      private final String sequence;
      private final AtomicInteger newSeqLength;
      private final AtomicInteger inputLength;

      private NewSeqThread(final ThreadGroup threadGroup, 
            final String sequence, final AtomicInteger newSeqLength, 
            final AtomicInteger inputLength)
      {
         super(threadGroup, "newSeq");
         this.sequence = sequence;
         this.newSeqLength = newSeqLength;
         this.inputLength = inputLength;
      }

      @Override
      public final void run()
      {
         final StringBuffer buf = new StringBuffer(
            (int)(this.inputLength.get() * 1.32));
         final Matcher m = newSeqPattern.matcher(this.sequence);
         while (m.find())
         {
            m.appendReplacement(buf, "");
            buf.append(replacements.get(m.group()));
         }
         m.appendTail(buf);
         this.newSeqLength.set(buf.length());
      }
   }

   private static final class VariantThread extends Thread
   {
      private final Map<String,Integer> results;
      private final String variant;
      private final String sequence;

      private VariantThread(final ThreadGroup threadGroup, 
            final String name, final Map<String,Integer> results, 
            final String variant, final String sequence)
      {
         super(threadGroup, name);
         this.results = results;
         this.variant = variant;
         this.sequence = sequence;
      }

      @Override
      public final void run()
      {
         int count = 0;
         final Matcher m = Pattern.compile(this.variant)
                        .matcher(this.sequence);
         while (m.find())
         {
            count++;
         }
         this.results.put(this.variant, count);
      }
   }
   
   private static String readInput(final AtomicInteger inputLength, 
         final AtomicInteger seqLength)
   throws IOException
   {
      final StringBuilder sb = new StringBuilder(10000000);
      final BufferedReader r = new BufferedReader(
         new InputStreamReader(System.in, Charset.defaultCharset()));
      int commentLength = 0;
      try
      {
         String line;
         while ((line = r.readLine()) != null)
         {
            if (line.charAt(0) == '>')
            {
               commentLength += line.length() + 1;
            }
            else
            {
               sb.append(line);
               commentLength += 1;
            }
         }
      }
      finally
      {
         r.close();
      }
      seqLength.set(sb.length());
      inputLength.set(seqLength.get() + commentLength);
      return sb.toString();
   }
   
   private static void awaitThreads(final ThreadGroup tg)
   {
      final Thread[] threads = new Thread[variants.length];
      tg.enumerate(threads);
      for (final Thread thread : threads)
      {
         if (thread != null)
         {
            while (thread.isAlive())
            {
               try { thread.join(); } catch (InterruptedException ie){}
            }
         }
      }
      tg.destroy();
   }

   public static void main(final String[] args)
   throws IOException
   {
      final AtomicInteger inputLength = new AtomicInteger(0);
      final AtomicInteger seqLength = new AtomicInteger(0);
      final AtomicInteger newSeqLength = new AtomicInteger(0);
      final Map<String,Integer> results = 
         new HashMap<String,Integer>(variants.length);
      {
         final ThreadGroup threadGroup = new ThreadGroup("regexWork");
         {
            final String sequence = readInput(inputLength, seqLength);
            new NewSeqThread(threadGroup, sequence, 
               newSeqLength, inputLength).start();
            for (final String variant : variants)
            {
               new VariantThread(threadGroup, variant, results, 
                  variant, sequence).start();
            }
         }
         awaitThreads(threadGroup);
      }
      for (final String variant : variants)
      {
         System.out.println(variant + " " + results.get(variant));
      }
      System.out.println();
      System.out.println(inputLength.get());
      System.out.println(seqLength.get());
      System.out.println(newSeqLength.get());
   }
}
/* The Computer Language Benchmarks Game
   http://shootout.alioth.debian.org/

   contributed by Jason Nordwick
*/

import java.io.FileDescriptor;
import java.io.FileInputStream;
import java.nio.ByteBuffer;
import java.nio.channels.FileChannel;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

final class ByteWrapper implements CharSequence {

   public byte[] backing;
   public int length;

   public ByteWrapper( byte[] backing ) {
      this( backing, backing.length );
   }
   
   public ByteWrapper( byte[] backing, int length ) {
      this.backing = backing;
      this.length = length;
   }

   public int length() {
      return length;
   }

   public char charAt(int index) {
      return (char) backing[index];
   }
   
   public CharSequence subSequence(int start, int end) {
      throw new UnsupportedOperationException();
   }
}


public final class regexdna {
   
   private static Pattern comments = Pattern.compile(">.*\n|\n");
   
   private static String[][] codes =
      {{"B", "(c|g|t)"},
      {"D", "(a|g|t)"},
      {"H", "(a|c|t)"},
      {"K", "(g|t)"},
      {"M", "(a|c)"},
      {"N", "(a|c|g|t)"},
      {"R", "(a|g)"},
      {"S", "(c|g)"},
      {"V", "(a|c|g)"},
      {"W", "(a|t)"},
      {"Y", "(c|t)"} };
   
   private static Pattern codesPat = Pattern.compile("[BDHKMNRSVWY]");
   
   private static final int longest;
   private static byte[] repl;
 
   private static String[] strs = {
      "agggtaaa|tttaccct",
      "[cgt]gggtaaa|tttaccc[acg]",
      "a[act]ggtaaa|tttacc[agt]t",
      "ag[act]gtaaa|tttac[agt]ct",
      "agg[act]taaa|ttta[agt]cct",
      "aggg[acg]aaa|ttt[cgt]ccct",
      "agggt[cgt]aa|tt[acg]accct",
      "agggta[cgt]a|t[acg]taccct",
      "agggtaa[cgt]|[acg]ttaccct"
   };

   private static Pattern[] pats = new Pattern[strs.length];
   
   static {
      for( int i = 0; i < pats.length; ++i )
         pats[i] = Pattern.compile( strs[i] );
      
      int l = 0;
      for( int i = 0; i < codes.length; ++i )
         l = Math.max( l, codes[i][1].length() );
      longest = l;
      
      repl = new byte[26 * longest + 1];
      for( int i = 0; i < codes.length; ++i ) {
         int off = longest * (codes[i][0].charAt( 0 ) - 'A');
         String code = codes[i][1];
         for( int j = 0; j < code.length(); ++j )
            repl[off + j] = (byte) code.charAt( j );
      }
   }
   
   private static void rmComments( ByteWrapper t ) {

      byte[] backing = t.backing;
      Matcher m = comments.matcher( t );
      
      if( !m.find() )
         return;
      
      int tail = m.start();
      int restart = m.end();
            
      while( m.find() ) {
         while( restart != m.start() )
            backing[tail++] = backing[restart++];
         restart = m.end();
      }
      
      while( restart < backing.length )
         backing[tail++] = backing[restart++];
      
      t.length = tail;
   }
   
   private static void countPatterns( ByteWrapper t ) {
            
      for( int i = 0; i < pats.length; ++i ) {
         int c = 0;
         Matcher m = pats[i].matcher( t );
         while( m.find() )
            ++c;
         System.out.println( strs[i] + ' ' + c );
      }
   }
   
   private static ByteWrapper replace( ByteWrapper t ) {
      
      byte[] backing = t.backing;
      byte[] buf = new byte[t.length * longest];
      int pos = 0;
      
      Matcher m = codesPat.matcher( t );
      int last = 0;
      
      while( m.find() ) {
         for( ; last < m.start(); ++last )
            buf[pos++] = backing[last];
         for( int i = longest * (backing[last] - 'A'); repl[i] != 0; ++i )
            buf[pos++] = repl[i];
         ++last;
      }
      
      for( ; last < t.length; ++last )
         buf[pos++] = backing[last];
      
      return new ByteWrapper( buf, pos );
   }

   public static void main( String[] args ) throws Exception {

      FileInputStream fis = new FileInputStream( FileDescriptor.in );
      FileChannel cin = fis.getChannel();
      ByteBuffer bb = ByteBuffer.allocate( (int) cin.size() );
      cin.read( bb );
      
      ByteWrapper t = new ByteWrapper( bb.array() );
      rmComments( t );

      countPatterns( t );
      
      ByteWrapper w = replace( t );
      
      System.out.println();
      System.out.println( t.backing.length );
      System.out.println( t.length() );
      System.out.println( w.length() );
   }

}

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class regexmatch {

    static final String regex = "(^|^\\D*[^\\(\\d])" // must be preceeded by non-digit
            + "((\\(\\d\\d\\d\\))|(\\d\\d\\d))" // match 2: Area Code inner match 3: area with perens,
            //                    inner match 4: without perens
            + "[ ]" // area code followed by one space
            + "(\\d\\d\\d)" //match 5: prefix of 3 digits
            + "[ -]" // prefix followed by space or dash
            + "(\\d\\d\\d\\d)" // match 6: last 4 digits
            + "(\\D.*|$)"; // followed by non numeric chars

    static final Pattern phonePattern = Pattern.compile(regex);

    public static void main(String args[]) {
        int n = (args.length > 0) ? Integer.parseInt(args[0]) : 1;
        ArrayList file = new ArrayList();
        String inLine;

        try {
            BufferedReader in = new BufferedReader(new InputStreamReader(System.in));
            while ((inLine = in.readLine()) != null) {
                file.add(inLine);
            }
            in.close();

            while (n > 0) {
                int count = 0;
                Iterator itr = file.listIterator(0);
                while (itr.hasNext()) {
                    Matcher match = phonePattern.matcher((String) itr.next());
                    if (match.matches()) {
                        if (n == 1) {
                            if (match.group(3) != null) {
                                System.out.println(++count + ": " + match.group(3) + " " + match.group(5) + "-"
                                        + match.group(6));
                            } else {
                                System.out.println(++count + ": (" + match.group(4) + ") " + match.group(5) + "-"
                                        + match.group(6));
                            }
                        }
                    }
                }
                n--;
            }
        } catch (IOException e) {
            System.err.println(e);
        }
        System.exit(0);
    }

}

/*
 * The Computer Language Benchmarks Game
 * http://shootout.alioth.debian.org/
 * contributed by Anthony Donnefort
 * redone by Enotus 2010-11-20
 */

import java.io.*;

public final class revcomp {

    static final byte[] map=new byte[128];
        static{
        String[] mm={"ACBDGHK\nMNSRUTWVYacbdghkmnsrutwvy","TGVHCDM\nKNSYAAWBRTGVHCDMKNSYAAWBR"};
        for(int i=0;i<mm[0].length();i++) map[mm[0].charAt(i)]=(byte)mm[1].charAt(i);
    } 

    static void reverse(byte[] buf,int begin,int end){
        while(true){
            byte bb=buf[begin];if(bb=='\n') bb=buf[++begin];
            byte be=buf[end];if(be=='\n') be=buf[--end];
            if(begin>end) break;
            buf[begin++]=map[be];
            buf[end--]=map[bb];
        }
    }
      
    public static void main(String[] args) throws IOException{
        final byte[] buf=new byte[System.in.available()];
        System.in.read(buf);

        for(int i=0;i<buf.length;){
            while(buf[i++]!='\n');
            int data=i; while(i<buf.length && buf[i++]!='>');
            reverse(buf,data,i-2);
        }

        System.out.write(buf);
    }
}
/* The Computer Language Benchmarks Game
   http://shootout.alioth.debian.org/
 
   contributed by Leonhard Holz
   thanks to Anthony Donnefort for the basic mapping idea
*/

import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.LinkedList;
import java.util.List;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

public class revcomp
{
   private static final byte[] map = new byte[256];      
   private static final int CHUNK_SIZE = 1024 * 1024 * 16;
   private static final int NUMBER_OF_CORES = Runtime.getRuntime().availableProcessors();
   private static final ExecutorService service = Executors.newFixedThreadPool(NUMBER_OF_CORES);
   private static final List<byte[]> list = Collections.synchronizedList(new ArrayList<byte[]>());

   static {
      for (int i = 0; i < map.length; i++) {
         map[i] = (byte) i;
      }
       map['t'] = map['T'] = 'A';
       map['a'] = map['A'] = 'T';
       map['g'] = map['G'] = 'C';
       map['c'] = map['C'] = 'G';
       map['v'] = map['V'] = 'B';
       map['h'] = map['H'] = 'D';
       map['r'] = map['R'] = 'Y';
       map['m'] = map['M'] = 'K';
       map['y'] = map['Y'] = 'R';
       map['k'] = map['K'] = 'M';
       map['b'] = map['B'] = 'V';
       map['d'] = map['D'] = 'H';
       map['u'] = map['U'] = 'A';
   }

   public static void main(String[] args) throws IOException
   {
      int read;
      byte[] buffer;
      Finder lastFinder = null; 
      
      do {
         buffer = new byte[CHUNK_SIZE];
         read = System.in.read(buffer);
         list.add(buffer);

         Finder finder = new Finder(buffer, read, lastFinder);
         service.execute(finder);
         lastFinder = finder;

      } while (read == CHUNK_SIZE);

      Status status = lastFinder.finish();
      Mapper mapper = new Mapper(status.lastFinding, status.count - 1, status.lastMapper);
      service.execute(mapper);

      service.shutdown();
   }

   private static final class Status
   {
      private int count = 0;
      private int lastFinding = 0;
      private Mapper lastMapper = null;
   }
   
   private static final class Finder implements Runnable
   {
      private int size;
      private byte[] a;
      private Status status;
      private Finder previous;
      private boolean done = false;
      
      public Finder(byte[] a, int size, Finder previous)
      {
         this.a = a;
         this.size = size;
         this.previous = previous;
      }
      
      public Status finish()
      {
         while (!done) try {
            Thread.sleep(1);
         } catch (InterruptedException e) {
            // ignored
         }
         return status;
      }

      public void run()
      {
         LinkedList<Integer> findings = new LinkedList<Integer>();

         for (int i = 0; i < size; i++) {
            if (a[i] == '>') {
               findings.add(i);
            }
         }
      
         if (previous == null) {
            status = new Status();
         } else {
            status = previous.finish();
            findings.add(0, status.lastFinding);
            for (int i = 1; i < findings.size(); i++) {
               findings.set(i, findings.get(i) + status.count);
            }
         }
      
         if (findings.size() > 1) for (int i = 0; i < findings.size() - 1; i++) {
            status.lastMapper = new Mapper(findings.get(i), findings.get(i + 1) - 1, status.lastMapper);
            service.execute(status.lastMapper);
         }
         
         status.lastFinding = findings.get(findings.size() - 1);
         status.count += size;
         done = true;
      }
   }
   
   private static final class Mapper implements Runnable
   {
      private int end;
      private int start;
      private Mapper previous;
      private boolean done = false;
      
      public Mapper(int start, int end, Mapper previous)
      {
         this.end = end;
         this.start = start;
         this.previous = previous;
      }
      
      public void finish()
      {
         while (!done) try {
            Thread.sleep(1);
         } catch (InterruptedException e) {
            // ignored
         }
      }

      public void run()
      {
         int[] positions = find(list, start, end);
         
         int lp1 = positions[0];
         byte[] tob = list.get(lp1);

         int lp2 = positions[2];
         byte[] bot = list.get(lp2);
         
         int p1 = positions[1];
         while (tob[p1] != '\n') p1++;

         int p2 = positions[3];
      
         while (lp1 < lp2 || p1 < p2) {
            if (tob[p1] == '\n') {
               p1++;
            } else if (bot[p2] == '\n') {
               p2--;
            } else {
               byte tmp = tob[p1];
               tob[p1] = map[bot[p2]];
               bot[p2] = map[tmp];
               p1++;
               p2--;
            }
            if (p1 == tob.length) {
               lp1++;
               tob = list.get(lp1);
               p1 = 0;
            }
            if (p2 == -1) {
               lp2--;
               bot = list.get(lp2);
               p2 = bot.length - 1;
            }
         }

         if (previous != null) {
            previous.finish();
         }

         write(list, positions[0], positions[1], positions[2], positions[3]);
         done = true;
      }
   }

   private static void write(List<byte[]> list, int lpStart, int start, int lpEnd, int end)
   {
      byte[] a = list.get(lpStart);
      while (lpStart < lpEnd) {
         System.out.write(a, start, a.length - start);
         lpStart++;
         a = list.get(lpStart);
         start = 0;
      }
      System.out.write(a, start, end - start + 1);
   }
   
   private static int[] find(List<byte[]> list, int start, int end)
   {
      int n = 0, lp = 0;
      int[] result = new int[4];
      boolean foundStart = false;

      for (byte[] bytes : list) {
         if (!foundStart && n + bytes.length > start) {
            result[0] = lp;
            result[1] = start - n;
            foundStart = true;
         }
         if (foundStart && n + bytes.length > end) {
            result[2] = lp;
            result[3] = end - n;
            break;
         }
         n += bytes.length;
         lp++;
      }
      return result;
   }   
}
/*
 * The Computer Language Benchmarks Game
 * http://shootout.alioth.debian.org/
 * contributed by Anthony Donnefort
 * slightly modified to read 82 bytes at a time by Razii
 */

import java.io.*;
public class revcomp {
   static final byte[] cmp = new byte[128];
   static {
      for (int i = 0; i < cmp.length; i++) cmp[i] = (byte) i;
      cmp['t'] = cmp['T'] = 'A';
      cmp['a'] = cmp['A'] = 'T';
      cmp['g'] = cmp['G'] = 'C';
      cmp['c'] = cmp['C'] = 'G';
      cmp['v'] = cmp['V'] = 'B';
      cmp['h'] = cmp['H'] = 'D';
      cmp['r'] = cmp['R'] = 'Y';
      cmp['m'] = cmp['M'] = 'K';
      cmp['y'] = cmp['Y'] = 'R';
      cmp['k'] = cmp['K'] = 'M';
      cmp['b'] = cmp['B'] = 'V';
      cmp['d'] = cmp['D'] = 'H';
      cmp['u'] = cmp['U'] = 'A';
   }

   static class ReversibleByteArray extends java.io.ByteArrayOutputStream {
      void reverse() throws Exception {
         if (count > 0) {
            int begin = 0, end = count - 1;
            while (buf[begin++] != '\n');
            while (begin <= end) {
               if (buf[begin] == '\n') begin++;
               if (buf[end] == '\n') end--;
               if (begin <= end) {
                  byte tmp = buf[begin];
                  buf[begin++] = cmp[buf[end]];
                  buf[end--] = cmp[tmp];
               }
            }
            System.out.write(buf, 0, count);
         }
      }
   }

   public static void main(String[] args) throws Exception {
      byte[] line = new byte[82];
      int read;
      ReversibleByteArray buf = new ReversibleByteArray();
      while ((read = System.in.read(line)) != -1) {
         int i = 0, last = 0;
         while (i < read) {
            if (line[i] == '>') {
               buf.write(line, last, i - last);
               buf.reverse();
               buf.reset();
               last = i;
            }
            i++;
         }
         buf.write(line, last, read - last);
      }
      buf.reverse();
   }
}

  
// $Id: reversefile.java,v 1.1 2004-05-23 07:14:28 bfulgham Exp $
// http://www.bagley.org/~doug/shootout/
// from Stephen Darnell

import java.io.*;

public class reversefile {
    final static int BUFF_SIZE = 4096;

    public static void main( String[] args ) {
	InputStream in = System.in;

	byte[] buff = new byte[BUFF_SIZE];
	byte[] obuff = new byte[BUFF_SIZE];

	try {
	    int pos = 0;
	    for (int n ; (n = in.read(buff, pos, BUFF_SIZE)) > 0 ;) {
		pos += n;
		if ((pos + BUFF_SIZE) > buff.length) {
		    byte[] new_buff = new byte[buff.length << 1];
		    System.arraycopy(buff, 0, new_buff, 0, buff.length);
		    buff = new_buff;
		}
	    }

	    int opos = 0;

	    for (int p = --pos ; ;) {
		--p;
		if (p < 0 || buff[p] == '\n') {
		    for( int bp = p ; ++bp <= pos ; ) {
			obuff[opos] = buff[bp];
			if (++opos >= BUFF_SIZE) {
			    System.out.write(obuff, 0, opos);
			    opos = 0;
			}
		    }
		    pos = p;

		    if (p < 0)
			break;
		}
	    }

	    System.out.write(obuff, 0, opos);
	}
	catch(IOException ex) {
	}
    }
}
// $Id: sieve.java,v 1.1 2004-05-23 07:14:28 bfulgham Exp $
// http://www.bagley.org/~doug/shootout/

public class sieve {
    public static void main(String args[]) {
	int NUM = Integer.parseInt(args[0]);
	boolean [] flags = new boolean[8192 + 1];
	int count = 0;
	while (NUM-- > 0) {
	    count = 0;
	    for (int i=2; i <= 8192; i++) {
		flags[i] = true;
	    }
	    for (int i=2; i <= 8192; i++) {
		if (flags[i]) {
		    // remove all multiples of prime: i
		    for (int k=i+i; k <= 8192; k+=i) {
			flags[k] = false;
		    }
		    count++;
		}
	    }
	}  
	System.out.print("Count: " + count + "\n");
    }
}

/* 
 The Great Computer Language Shootout
 http://shootout.alioth.debian.org/
 
 contributed by Java novice Jarkko Miettinen
 modified ~3 lines of the original C#-version 
 by Isaac Gouy
 */
 
import java.text.DecimalFormat;
import java.text.NumberFormat; 

public class spectralnorm
{
	
	private static final NumberFormat formatter = new DecimalFormat("#.000000000");
	
	public static void main(String[] args) {
		int n = 100;
		if (args.length > 0) n = Integer.parseInt(args[0]);
		
		System.out.println(formatter.format(new spectralnorm().Approximate(n)));
	}
	
	private final double Approximate(int n) {
		// create unit vector
		double[] u = new double[n];
		for (int i=0; i<n; i++) u[i] =  1;
		
		// 20 steps of the power method
		double[] v = new double[n];
		for (int i=0; i<n; i++) v[i] = 0;
		
		for (int i=0; i<10; i++) {
			MultiplyAtAv(n,u,v);
			MultiplyAtAv(n,v,u);
		}
		
		// B=AtA         A multiplied by A transposed
		// v.Bv /(v.v)   eigenvalue of v
		double vBv = 0, vv = 0;
		for (int i=0; i<n; i++) {
			vBv += u[i]*v[i];
			vv  += v[i]*v[i];
		}
		
		return Math.sqrt(vBv/vv);
	}
	
	
	/* return element i,j of infinite matrix A */
	private final double A(int i, int j){
		return 1.0/((i+j)*(i+j+1)/2 +i+1);
	}
	
	/* multiply vector v by matrix A */
	private final void MultiplyAv(int n, double[] v, double[] Av){
		for (int i=0; i<n; i++){
			Av[i] = 0;
			for (int j=0; j<n; j++) Av[i] += A(i,j)*v[j];
		}
	}
	
	/* multiply vector v by matrix A transposed */
	private final void MultiplyAtv(int n, double[] v, double[] Atv){
		for (int i=0;i<n;i++){
			Atv[i] = 0;
			for (int j=0; j<n; j++) Atv[i] += A(j,i)*v[j];
		}
	}
	
	/* multiply vector v by matrix A and then by matrix A transposed */
	private final void MultiplyAtAv(int n, double[] v, double[] AtAv){
		double[] u = new double[n];
		MultiplyAv(n,v,u);
		MultiplyAtv(n,u,AtAv);
	}
}
/*
The Computer Language Benchmarks Game
http://shootout.alioth.debian.org/
 
Based on C# entry by Isaac Gouy
contributed by Jarkko Miettinen
Parallel by The Anh Tran
 */

import java.text.DecimalFormat;
import java.text.NumberFormat;
import java.util.concurrent.CyclicBarrier;

public class spectralnorm
{
    private static final NumberFormat formatter = new DecimalFormat ("#.000000000");
    
    public static void main (String[] args)
    {
        int n = 1000;
        if (args.length > 0) n = Integer.parseInt (args[0]);
        
        System.out.println (formatter.format (spectralnormGame (n)) );
    }
    
    
    private static final double spectralnormGame (int n)
    {
        // create unit vector
        double[] u = new double[n];
        double[] v = new double[n];
        double[] tmp = new double[n];
        
        for (int i = 0; i < n; i++)
            u[i] = 1.0;
        
        // get available processor, then set up syn object
        int nthread = Runtime.getRuntime ().availableProcessors ();
        Approximate.barrier = new CyclicBarrier (nthread);
        
        int chunk = n / nthread;
        Approximate[] ap = new Approximate[nthread];
        
        for (int i = 0; i < nthread; i++)
        {
            int r1 = i * chunk;
            int r2 = (i < (nthread -1)) ? r1 + chunk : n;
            
            ap[i] = new Approximate (u, v, tmp, r1, r2);
        }
        
        
        double vBv = 0, vv = 0;
        for (int i = 0; i < nthread; i++)
        {
            try
            {
                ap[i].join ();
                
                vBv += ap[i].m_vBv;
                vv += ap[i].m_vv;
            }
            catch (Exception e)
            {
                e.printStackTrace ();
            }
        }
        
        return Math.sqrt (vBv/vv);
    }
    
    
    private static class Approximate extends Thread
    {
        private static CyclicBarrier barrier;
        
        private double[] _u;
        private double[] _v;
        private double[] _tmp;
        
        private int range_begin, range_end;
        private double m_vBv = 0, m_vv = 0;
        
        
        public Approximate (double[] u, double[] v, double[] tmp, int rbegin, int rend)
        {
            super ();
            
            _u = u;
            _v = v;
            _tmp = tmp;
            
            range_begin = rbegin;
            range_end = rend;
            
            start ();
        }
        
        public void run ()
        {
            // 20 steps of the power method
            for (int i = 0; i < 10; i++)
            {
                MultiplyAtAv (_u, _tmp, _v);
                MultiplyAtAv (_v, _tmp, _u);
            }
            
            for (int i = range_begin; i < range_end; i++)
            {
                m_vBv += _u[i] * _v[i];
                m_vv  += _v[i] * _v[i];
            }
        }
        
        /* return element i,j of infinite matrix A */
        private final static double eval_A (int i, int j)
        {
            int div = ( ((i+j) * (i+j+1) >>> 1) +i+1 );
            return 1.0 / div;
        }
        
        /* multiply vector v by matrix A, each thread evaluate its range only */
        private final void MultiplyAv (final double[] v, double[] Av)
        {
            for (int i = range_begin; i < range_end; i++)
            {
                double sum = 0;
                for (int j = 0; j < v.length; j++)
                    sum += eval_A (i, j) * v[j];
                
                Av[i] = sum;
            }
        }
        
        /* multiply vector v by matrix A transposed */
        private final void MultiplyAtv (final double[] v, double[] Atv)
        {
            for (int i = range_begin; i < range_end; i++)
            {
                double sum = 0;
                for (int j = 0; j < v.length; j++)
                    sum += eval_A (j, i) * v[j];
                
                Atv[i] = sum;
            }
        }
        
        /* multiply vector v by matrix A and then by matrix A transposed */
        private final void MultiplyAtAv (final double[] v, double[] tmp, double[] AtAv)
        {
            try
            {
                MultiplyAv (v, tmp);
                // all thread must syn at completion
                barrier.await ();
                MultiplyAtv (tmp, AtAv);
                // all thread must syn at completion
                barrier.await ();
            }
            catch (Exception e)
            {
                e.printStackTrace ();
            }
        }
    }
}
// $Id: spellcheck.java,v 1.1 2004-05-23 07:14:28 bfulgham Exp $
// http://www.bagley.org/~doug/shootout/

import java.io.*;
import java.util.*;

public class spellcheck {
    public static void main(String args[]) throws IOException {
	int n = Integer.parseInt(args[0]);
	HashMap dict = new HashMap();
	String word;

	try {
	    BufferedReader in = new BufferedReader(new FileReader("Usr.Dict.Words"));
	    while ((word = in.readLine()) != null) {
		dict.put(word, new Integer(1));
	    }
	    in.close();
        } catch (IOException e) {
            System.err.println(e);
            return;
        }
	
	try {
	    BufferedReader in = new BufferedReader(new InputStreamReader(System.in));
	    while ((word = in.readLine()) != null) {
		if (!dict.containsKey(word)) {
		    System.out.println(word);
		}
	    }
        } catch (IOException e) {
            System.err.println(e);
            return;
        }
    }
}
/*
 * The Great Computer Language Shootout 
 * http://shootout.alioth.debian.org/
 * 
 * modified by Mehmet D. AKIN
 *
 */

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.HashMap;

public class spellcheck {

	static HashMap dict = new HashMap(1024);
	static String word;

	public static void main(String args[]) throws IOException {
		try {
			BufferedReader in = new BufferedReader(new FileReader("Usr.Dict.Words"));
			while ((word = in.readLine()) != null) {
				dict.put(word, null);
			}
			in.close();
			in = new BufferedReader(new InputStreamReader(System.in));
			while ((word = in.readLine()) != null) {
				if (!dict.containsKey(word)) {
					System.out.println(word);
				}
			}
		} catch (IOException e) {
			System.err.println(e);
			return;
		}
	}
}
// $Id: strcat.java,v 1.1 2004-05-23 07:14:28 bfulgham Exp $
// http://www.bagley.org/~doug/shootout/
// Pelle Nilsson suggested we also illustrate StringBuffer
// since it is the preferred method for concatenating 
// strings in Java

import java.io.*;
import java.util.*;

public class strcat {
    public static void main(String args[]) throws IOException {
	int n = Integer.parseInt(args[0]);
	String hello = "hello\n";
	StringBuffer stringBuffer = new StringBuffer(32);

        for (int i=0; i<n; i++) {
            stringBuffer.append(hello);
	}

	System.out.println(stringBuffer.length());
    }
}
// $Id: sumcol.java,v 1.5 2007-06-20 03:32:39 bfulgham Exp $
// http://www.bagley.org/~doug/shootout/ 

import java.io.*;
import java.util.*;
import java.text.*;

public class sumcol {
   public static void main(String[] args) {
      int sum = 0;
      String line;
      try {
         BufferedReader in = new BufferedReader(new InputStreamReader(System.in));
         while ((line = in.readLine()) != null) {
            sum = sum + Integer.parseInt(line);
         }
      } catch (IOException e) {
         System.err.println(e);
         return;
      }
      System.out.println(Integer.toString(sum));
   }
}
/* The Computer Language Benchmarks Game
   http://shootout.alioth.debian.org/
   contributed by Amir K aka Razii
*/

import java.io.*;

public final class sumcol {

   static final byte[] buf = new byte [18432];
   final static InputStream in = System.in;

   public static void main(String[] args) throws Exception {

      System.out.println(sum());
   }

   private static int sum() throws Exception
   {
   	  int total = 0, num=0, j, neg = 1;
   	  while ((j = in.read(buf)) > 0)
   	  {
   	  	for (int i = 0; i < j; i++)
   	  	{
   	  		int c = buf[i];
            if (c >= '0' && c <= '9')
                num =  num * 10 + c - '0';
            else if (c == '-')
                neg = -1;
            else {
                total += (num * neg);
                num = 0;
                neg = 1;
            }	
   	  	}
   	  	
   	  }
      return total;
   }
}
/*
 * The Computer Language Benchmarks Game
 * http://shootout.alioth.debian.org/
 *
 * contributed by Klaus Friedel
 * modified by Jeffrey Stedfast
 *
 */

import java.io.StreamTokenizer;

public class sumcol {
   public static void main(String[] args) throws Exception{
      int sum = 0;
      StreamTokenizer lineTokenizer = new StreamTokenizer(System.in);
       while (lineTokenizer.nextToken() != StreamTokenizer.TT_EOF) {
         sum += (int) lineTokenizer.nval;
       }
       System.out.println(Integer.toString(sum));
   }
}
/* The Great Computer Language Shootout 
   http://shootout.alioth.debian.org/

   contributed by Isaac Gouy
*/


public class takfp {
    public static void main(String args[]) {
	int n = Integer.parseInt(args[0]);
	System.out.println( Tak(n*3.0f, n*2.0f, n*1.0f) );
    }

  public static float Tak (float x, float y, float z) {
    if (y >= x) return z;
    return Tak(Tak(x-1.0f,y,z), Tak(y-1.0f,z,x), Tak(z-1.0f,x,y)); 
  }
}
/**
 * The Computer Language Benchmarks Game
 * http://shootout.alioth.debian.org/
 * contributed by Klaus Friedel
 */

import java.util.*;

public class threadring {

  public static long startTime;
  public static final int THREAD_COUNT = 503;

  // The scheduler for cooperative Multithreading
  static class Scheduler extends Thread{
    private final List<CooperativeThread> threads = Collections.synchronizedList(new ArrayList<CooperativeThread>());
    private int rrIndex = -1;

    public void start(CooperativeThread t){
      threads.add(t);
    }

    public void run() {
      for(;;){ // Scheduler will run foerever
        CooperativeThread nextThread;
        synchronized (threads){
          rrIndex++;
          if(rrIndex >= threads.size()) rrIndex = 0;
          nextThread = threads.get(rrIndex);
        }
        nextThread.handleMessage();
      }
    }
  }

  static abstract class CooperativeThread{
    public abstract void handleMessage();
  }

  static class MessageThread extends CooperativeThread{
      MessageThread nextThread;
      String name;
      Integer msg;

      public MessageThread(MessageThread nextThread, int name) {
        this.name = "" + name;
        this.nextThread = nextThread;
      }

      public void handleMessage(){
        if(msg == null) return;
        if(msg == 0){
          System.out.println(getName());
          System.exit(0);
        }
        nextThread.put(msg - 1);
        msg = null;
      }

      void put(Integer message){
        msg = message;
      }

      String getName() {
        return name;
      }
    }


  public static void main(String args[]) throws Exception{
    int hopCount = Integer.parseInt(args[0]);

    MessageThread thread = null;
    MessageThread last = null;
    for (int i = THREAD_COUNT; i >= 1 ; i--) {
      thread = new MessageThread(thread, i);
      if(i == THREAD_COUNT) last = thread;
    }
    // close the ring:
    last.nextThread = thread;

    Scheduler scheduler = new Scheduler();
    // start all Threads
    MessageThread t = thread;
    do{
      scheduler.start(t);
      t = t.nextThread;
    }while(t != thread);
    scheduler.start();

    // inject message
    thread.put(hopCount);
  }
}
/**
 * The Computer Language Benchmarks Game
 * http://shootout.alioth.debian.org/
 * contributed by Klaus Friedel
 */

import java.util.concurrent.locks.LockSupport;

public class threadring {
  static final int THREAD_COUNT = 503;

  public static class MessageThread extends Thread {
    MessageThread nextThread;
    volatile Integer message;

    public MessageThread(MessageThread nextThread, int name) {
      super(""+name);
      this.nextThread = nextThread;
    }

    public void run() {
      while(true) nextThread.enqueue(dequeue());
    }

    public void enqueue(Integer hopsRemaining) {
      if(hopsRemaining == 0){
        System.out.println(getName());
        System.exit(0);
      }
      // as only one message populates the ring, it's impossible
      // that queue is not empty
      message = hopsRemaining - 1;
      LockSupport.unpark(this); // work waiting...
    }

    private Integer dequeue(){
      while(message == null){
        LockSupport.park();
      }
      Integer msg = message;
      message = null;
      return msg;
    }
  }

  public static void main(String args[]) throws Exception{
    int hopCount = Integer.parseInt(args[0]);

    MessageThread first = null;
    MessageThread last = null;
    for (int i = THREAD_COUNT; i >= 1 ; i--) {
      first = new MessageThread(first, i);
      if(i == THREAD_COUNT) last = first;
    }
    // close the ring:
    last.nextThread = first;

    // start all Threads
    MessageThread t = first;
    do{
      t.start();
      t = t.nextThread;
    }while(t != first);
    // inject message
    first.enqueue(hopCount);
    first.join(); // wait for System.exit
  }
}
/**
 * The Computer Language Benchmarks Game
 * http://shootout.alioth.debian.org/
 *
 * contributed by Fabien Le Floc'h
 *
 * Java implementation of thread-ring benchmark. Best performance is achieved with 
 * MAX_THREAD=1 as the thread-ring test is bested with only 1 os thread.
 * This implementation shows using a simple thread pool solves the thread context
 * switch issue.
 */

import java.util.concurrent.BlockingQueue;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.LinkedBlockingQueue;

public class threadring {
    private static final int MAX_NODES = 503;
    private static final int MAX_THREADS = 503;

    private ExecutorService executor;
    private final int N;

    public static void main(String[] args) {
        threadring ring = new threadring(Integer.parseInt(args[0]));
        Node node = ring.start(MAX_NODES);
        node.sendMessage(new TokenMessage(1,0));
    }
    
    public threadring(int n) {
        N = n;
    }

    public Node start(int n) {
        Node[] nodes = spawnNodes(n);
        connectNodes(n, nodes);
        return nodes[0];
    }

    private Node[] spawnNodes(int n) {
        executor = Executors.newFixedThreadPool(MAX_THREADS);
        Node[] nodes = new Node[n+1];
        for (int i = 0; i < n ; i++) {
            nodes[i] = new Node(i+1, null);
        }
        return nodes;
    }
    
    public void connectNodes(int n, Node[] nodes) {
        nodes[n] = nodes[0];
        for (int i=0; i<n; i++) {
            nodes[i].connect(nodes[i+1]);
        }
    }

    private static class TokenMessage {
        private int nodeId;
        private volatile int value;
        private boolean isStop;

        public TokenMessage(int nodeId, int value) {
            this.nodeId = nodeId;
            this.value = value;
        }

        public TokenMessage(int nodeId, int value, boolean isStop) {
            this.nodeId = nodeId;
            this.value = value;
            this.isStop = isStop;
        }
    }

    private class Node implements Runnable {
        private int nodeId;
        private Node nextNode;
        private BlockingQueue<TokenMessage> queue = new LinkedBlockingQueue<TokenMessage>();
        private boolean isActive = false;
        private int counter;

        public Node(int id, Node nextNode) {
            this.nodeId = id;
            this.nextNode = nextNode;
            this.counter = 0;
        }

        public void connect(Node node) {
            this.nextNode = node;
            isActive = true;
        }

        public void sendMessage(TokenMessage m) {
            queue.add(m);
            executor.execute(this);
        }


        public void run() {
            if (isActive) {
                try {
                    TokenMessage m = queue.take();
                    if (m.isStop) {
                        int nextValue = m.value+1;
                        if (nextValue == MAX_NODES) {
//                            System.out.println("last one");
                            executor.shutdown();                            
                        } else {
                            m.value = nextValue;
                            nextNode.sendMessage(m);
                        }
                        isActive = false;
//                        System.out.println("ending node "+nodeId);
                    } else {
                        if (m.value == N) {
                            System.out.println(nodeId);
                            nextNode.sendMessage(new TokenMessage(nodeId, 0, true));
                        } else {
                            m.value = m.value + 1;
                            nextNode.sendMessage(m);
                        }
                    }
                } catch (InterruptedException ie) {
                    ie.printStackTrace();
                }
            }
        }
    }
}
/**
 * The Computer Language Benchmarks Game
 * http://shootout.alioth.debian.org/
 * contributed by Birju Prajapati
 */

import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

public class threadring {

    private static final int TOTAL_NODES = 503;
    private static final ExecutorService EXECUTOR = Executors.newFixedThreadPool(TOTAL_NODES);
    
    private static Node firstNode;
    private static Node lastNode;

    public static void main(String[] args) {
        firstNode = new Node(1);
        lastNode.next = firstNode;
        firstNode.push(Integer.parseInt(args[0]));
    }

    private static class Node implements Runnable {

        private final int id;
        private Node next;
        private int token;

        public Node(int id) {
            this.id = id;
            if (id++ == TOTAL_NODES) {
                lastNode = this;
            } else {
                next = new Node(id);
            }
        }

        private void push(int token) {
            this.token = token;
            EXECUTOR.execute(this);
        }

        public void run() {
            if (token-- != 0) {
                next.push(token);
            } else {
                System.out.println(id);
                System.exit(0);
            }
            
        }
    }
}


/**
 * The Computer Language Benchmarks Game
 * http://shootout.alioth.debian.org/
 *
 * contributed by Fabien Le Floc'h
 *
 * This implementation cheats by adapting closely to the benchmark specifications. 
 * We use only 1 thread to process messages, we don't use a blocking queue but 
 * instead a linked list. The Nodes don't map directly to a thread, even though
 * they are processed in a different thread (the consumer). This is probably this kind
 * of scheme that more advanced languages like Haskell do behind the scenes.
 * 
 * I say it is a bit cheating because we don't use here a concurrent queue, because 
 * we know everything is processed in 1 thread: the consumer except the first message.
 */


import java.util.LinkedList;
import java.util.Queue;


public class threadring {
    public static void main(String[] args) {
        Node[] ring = new Node[503];
        for (int i=0; i<ring.length; i++) {
            ring[i] = new Node(i+1);
        }
        for (int i=0; i<ring.length; i++) {
            int nextIndex = (ring[i].label % ring.length);
            ring[i].next = ring[nextIndex];            
        }
        int nHops = Integer.parseInt(args[0]);
        new Thread(new Consumer()).start();
        ring[0].sendMessage(nHops);
    }

    private static Queue<Node> q = new LinkedList<Node>();

    static class Consumer implements Runnable {

        public void run() {
            while (true) {
                try {
                    Node node;
                    node = q.poll();
                    if (node == null) {
                        //ignore, wait for some element
                        Thread.sleep(100);
                    } else {
                        node.run();
                    } 
                } catch (InterruptedException e) {
                    e.printStackTrace();
                }
            }
        }
    }
    static class Node implements Runnable {
        private final int label;
        private Node next;
        private int message;

        public Node(int label) {
            this.label = label;
        }

        public void sendMessage(int message) {
            this.message=message;
            q.add(this);            
        }

        public void run() {
            //                System.out.println("after lock");
            if (message == 0) {
                System.out.println(label);
                System.exit(0);
            } else {
                next.sendMessage(message - 1);
            }
        }
    }
}
// $Id: wc.java,v 1.2 2004-06-20 08:39:47 bfulgham Exp $
// http://www.bagley.org/~doug/shootout/
// with help from Dirus@programmer.net

import java.io.*;
import java.util.*;
import java.text.*;

// this program modified from:
//   http://cm.bell-labs.com/cm/cs/who/bwk/interps/pap.html
// Timing Trials, or, the Trials of Timing: Experiments with Scripting
// and User-Interface Languages</a> by Brian W. Kernighan and
// Christopher J. Van Wyk.

public class wc {
    public static void main(String[] args) {
        int nl = 0, nw = 0, nc = 0;

        try {
            byte[] buff = new byte[4096];
            boolean inword = false;
            int length;
	    char c;

            while ((length = System.in.read(buff)) != -1) {
                nc += length;
                for(int i = 0; i < length; i++) {
                    c = (char)buff[i];
                    if (c == '\n')
                        ++nl;
                    if (Character.isWhitespace(c))
                        inword = false;
                    else if (inword == false) {
                        ++nw;
                        inword = true;
                    }
                }
            }
        } catch (IOException e) {
            System.err.println(e);
            return;
        }
        System.out.println(Integer.toString(nl) + " " +
                           Integer.toString(nw) + " " +
                           Integer.toString(nc));
    }
}
/* The Great Computer Language Shootout
   http://shootout.alioth.debian.org/

   contributed by bfulgham (with help from Dirus@programmer.net)
   modified by M. Hanauska
   
   this program modified from:
     http://cm.bell-labs.com/cm/cs/who/bwk/interps/pap.html
   Timing Trials, or, the Trials of Timing: Experiments with Scripting
   and User-Interface Languages</a> by Brian W. Kernighan and
   Christopher J. Van Wyk.
*/

import java.io.*;

public class wc {
    public static void main(String[] args) {
        int nl = 0, nw = 0, nc = 0;

        try {
            byte[] buff = new byte[4096];
            boolean inword = false;
            int length;
	   
            while ((length = System.in.read(buff)) != -1) {
                nc += length;
                for(int i = 0; i < length; i++) {
                    switch (buff[i]) {
                        /* Linebreak */
                        case '\n': nl++;
                                                
                        /* Whitespace */
                        case '\r':
                        case '\t':
                        case  ' ': inword = false; break;
                        
                        /* Within word */
                        default:
                            if (!inword) {
                                nw ++;
                                inword = true;
                            }
                    }
          
                }
            }
        } catch (IOException e) {
            System.err.println(e);
            return;
        }
        System.out.println(Integer.toString(nl) + " " +
                           Integer.toString(nw) + " " +
                           Integer.toString(nc));
    }
}
/* The Great Computer Language Shootout 
   http://shootout.alioth.debian.org/

   contributed by James McIlree
*/

import java.io.*;
import java.util.*;
import java.util.regex.*;

public class wordfreq {
  static class Counter {
    int count = 1;
  }

  public static void main(String[] args) 
    throws IOException
  {
    HashMap map = new HashMap();
    Pattern charsOnly = Pattern.compile("\\p{Lower}+");

    BufferedReader r = new BufferedReader(new InputStreamReader(System.in));
    String line;
    while ((line = r.readLine()) != null) {
      Matcher matcher = charsOnly.matcher(line.toLowerCase());
      while (matcher.find()) {
        String token = matcher.group();
        Counter c = (Counter)map.get(token);
        if (c != null)
          c.count++;
        else
          map.put(token, new Counter());
      }
    }
    
    ArrayList list = new ArrayList(map.entrySet());
    Collections.sort(list, new Comparator() {
        public int compare(Object o1, Object o2) {
          int c = ((Counter)((Map.Entry)o2).getValue()).count - ((Counter)((Map.Entry)o1).getValue()).count;
          if (c == 0) {
            c = ((String)((Map.Entry)o2).getKey()).compareTo((String)((Map.Entry)o1).getKey());
          }
          return c;
        }
      });
    
    String[] padding = { "error!", " ", "  ", "   ", "    ", "     ", "      ", "error!" };
    StringBuffer output = new StringBuffer();
    Iterator it = list.iterator();
    while(it.hasNext()) {
      Map.Entry entry = (Map.Entry)it.next();
      String word = (String)entry.getKey();
      String count = String.valueOf(((Counter)entry.getValue()).count);
      if (count.length() < 7)
        System.out.println(padding[7 - count.length()] + count + " " +word);
      else
        System.out.println(count + " " +word);
    }
  }
}

