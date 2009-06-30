/* 
 * Kodkod -- Copyright (c) 2005-2008, Emina Torlak
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 * THE SOFTWARE.
 */
package examples.sudoku;

import scala.collection.mutable.HashMap
import scala.collection.mutable.Map

import kodkod.instance.Tuple
import kodkod.instance.TupleFactory
import kodkod.instance.TupleSet
import kodkod.instance.Universe

/**
 * A utility for parsing various String specifications of Sudoku puzzles.
 * @author Emina Torlak
 */
final object SudokuParser {
  private def split(puzzle : String) : Array[String] =
  {
    val parsed = puzzle.split("\\s+")
	if (parsed.length>1)
	  parsed
    else
	  puzzle.replaceAll("(\\d)", "$1 ").split(" ")
  }
		
  /**
   * Returns the representation of the clues encoded in the given array as 
   * an NxNxN tuple set drawn from the given universe.  N is assumed
   * to be a perfect square, and the universe is assumed to consist of Integer objects in 
   * the range [1..N]. The  array is assumed to consist of N*N numbers, drawn 
   * from the set 0..N, inclusive, where a consecutive sequence of N
   * numbers represents one row of an NxN Sudoku grid.
   * Zeros stand for blank entries.  
   * @requires some r: int | puzzle.length = r * r * r * r
   * @requires universe.atoms[int] = {i: Integer | 1 <= i.intValue() <= puzzle.length}
   * @return a tupleset representation of the clues in the puzzle specified by the given array.
   */
  final def parse(puzzle : Array[String], univ : Universe) : TupleSet =
  { 
    val n = StrictMath.sqrt(puzzle.length).asInstanceOf[Int]
	val r = StrictMath.sqrt(n).asInstanceOf[Int]
	if (puzzle.length!=r*r*r*r)  throw new IllegalArgumentException("Not a valid puzzle spec: expected " + (r*r*r*r) + " numbers but found " + puzzle.length)
				
	val f = univ.factory()
		
	val givens = f.noneOf(3)
		
	for(i <- 0 until n)
    { 
	  for(j <- 0 until n)
      { 
	    try
        {
		  val digit = java.lang.Integer.parseInt(puzzle(i*n+j)).asInstanceOf[Int]
		  if (digit>0 && digit<=n)
          {
		    val t = f.tuple((i+1).asInstanceOf[AnyRef], (j+1).asInstanceOf[AnyRef], digit.asInstanceOf[AnyRef])
			givens.add(t)
		  }
          else if (digit>n)
          { 
		    throw new IllegalArgumentException("Not a valid puzzle spec: expected numbers from [0, " + n+"] but found "+digit)
	      } 
		}
        catch
        {
		  case nfe : NumberFormatException => throw new IllegalArgumentException("Not a valid puzzle spec: expected numbers from [0, " + n+"] but found "+puzzle(i*n+j))
	    }
	  }
	}
		
	givens
  }

  /**
   * Returns the representation of the clues encoded in the given array as 
   * an NxNxN tuple set drawn from a freshly constructed universe.  N is assumed
   * to be a perfect square, and the universe consist of Integer objects in 
   * the range [1..N]. The  array is assumed to consist of N*N numbers, drawn 
   * from the set 0..N, inclusive, where a consecutive sequence of N
   * numbers represents one row of an NxN Sudoku grid.
   * Zeros stand for blank entries.  
   * @requires some r: int | puzzle.length = r * r * r * r
   * @return a tupleset representation of the clues in the puzzle specified by the given array.
   */
  final def parse(puzzle : Array[String]) : TupleSet =
  { 
    val atoms = Array[java.lang.Integer](StrictMath.sqrt(puzzle.length).asInstanceOf[Int])
	for(i <- 0 until atoms.length) { atoms(i) = i+1 }
	parse(puzzle, new Universe(atoms : _*))
  }
	
  /**
   * Parses the given puzzle string and returns the representation of
   * the encoded clues as an NxNxN tuple set drawn from a freshly constructed universe.
   * N is assumed to be a perfect square, and the universe consists of Integer objects
   * in the range [1..N].  If N>9, this method assumes that the puzzle 
   * string consists of N*N space-separated numbers, drawn 
   * from the set 0..N, inclusive, where a consecutive sequence of N
   * space-separated numbers represents one row of an NxN Sudoku grid.
   * Zeros stand for blank entries.  If N<=9, then the spaces may be omitted.
   * @requires some r: [2..) | (puzzle.split("\\s+").length() = r * r * r * r) || (r<=3 && puzzle.length = r * r * r * r)
   * @return a tupleset representation of the clues in the puzzle specified by the given string.
   */
  final def parse(puzzle : String) : TupleSet =
  { 
    val parsed = split(puzzle)
    val atoms = new Array[java.lang.Integer](StrictMath.sqrt(parsed.length).asInstanceOf[Int])
    for(i <- 0 until atoms.length) { atoms(i) = i+1 }
    parse(parsed, new Universe(atoms : _*))
  }
	
  /**
   * Parses the given puzzle string and returns the representation of
   * the encoded clues as an NxNxN tuple set drawn from the given universe.  N is assumed
   * to be a perfect square, and the universe is assumed to consist of Integer objects in 
   * the range [1..N]. If N>9, the puzzle string is assumed to consist of N*N space-separated numbers, drawn 
   * from the set 0..N, inclusive, where a consecutive sequence of N
   * space-separated numbers represents one row of an NxN Sudoku grid.
   * Zeros stand for blank entries.  If N<=9, then the spaces may be omitted.
   * @requires some r: [2..) | (puzzle.split("\\s+").length() = r * r * r * r) || (r<=3 && puzzle.length = r * r * r * r)
   * @requires universe.atoms[int] = {i: Integer | 1 <= i.intValue() <= max(puzzle.split("\\s+").length(), puzzle.length())} 
   * @return a tupleset representation of the clues in the puzzle specified by the given string.
   */
  final def parse(puzzle : String, univ :  Universe) : TupleSet =
  { 
    parse(split(puzzle), univ)
  }
	
  /**
   * Returns a string representation of the given puzzle.
   * @requires some r: int | puzzle.universe.atoms[int] = { i: Integer | 1 <= i.intValue() <= r*r } 
   * @requires puzzle.arity = 3   
   * @return a string representation of the given puzzle
   */
  final def toString(puzzle : TupleSet) : String =
  { 
    val str = new StringBuilder()
	val n = puzzle.universe().size()
    val sep = if (n>9) " " else ""
	var itr = puzzle.iterator()
	if (!itr.hasNext())
    { 
      str.append(0)
	  for(i <- 1 until n*n)
      { 
        str.append(sep+0)
      }
	  str.toString()
	}
		
    var last = 0
    val tuple = itr.next()
	if (tuple.atom(0).asInstanceOf[Int]==1 && tuple.atom(1).asInstanceOf[Int]==1)
    { 
      str.append(tuple.atom(2))
	}
    else
    {
	  str.append(0)
	  itr = puzzle.iterator()
	}
		
	while(itr.hasNext())
    { 
      val tuple = itr.next()
	  val current = n*(tuple.atom(0).asInstanceOf[Int]-1) + (tuple.atom(1).asInstanceOf[Int]-1)
	  for(i <- last+1 until current)
      { 
	    str.append(sep+0)
	  }
	  str.append(sep+tuple.atom(2))
	  last = current
    }

    for(i <- last+1 until n*n)
    { 
      str.append(sep+0)
    }
    str.toString()
  }
	
  private def appendDivider(str : StringBuilder, r : Int) : Unit =
  { 
    val len = if (r<=3) "--" else "---"
	for(i <- 0 until r)
    { 
	  str.append("+-")
	  for(j <- 0 until r)
      { 
	    str.append(len)
	  }
	}
	str.append("+\n")
  }
	
  /**
   * Returns a pretty-printed string of the given sudoku solution.
   * @requires solution is a valid sudoku solution
   * @requires some r: int | solution.universe = { i: Integer | 1 <= i.intValue() <= r*r }
   * @return a pretty-printed string of the given sudoku solution
   */
  final def prettyPrint(solution : TupleSet) : String =
  { 
    val str = new StringBuilder
	val n = solution.universe().size()
	val r = Math.sqrt(n).asInstanceOf[Int]
	appendDivider(str, r)
	val psol = solution.iterator()
	for(i <- 1 until n+1)
    {
	  str.append("| ")
	  for(j <- 0 until r)
      {
	    for(k <- 0 until r)
        {
		  val atom = psol.next().atom(2).asInstanceOf[Int]
		  if (atom<10&&r>3) str.append(" ")
		  str.append(atom)
		  str.append(" ")
		}
		str.append("| ")
	  }
	  str.append("\n")
	  if (i%r==0) appendDivider(str, r)		
	}
	str.toString()
  }
	
  /**
   * Returns a map that maps each option in the given argument array to its value, 
   * or null if the option has no value.
   * Assumes that all options are of the form "-opt=val" or "-opt". 
   * @return a map that maps each option in the given argument array to its value, 
   * or null if the option has no value.
   */
  def options(args : Array[String]) : Map[String, String] =
  { 
    val opts = HashMap[String,String]()
    for(arg <- args)
    { 
      if (arg.startsWith("-"))
      { 
        val opt = arg.split("=")
        opt.length match
        { 
          case 1 => opts.put(opt(0), null)
		  case 2 => opts.put(opt(0), opt(1))
		  case _ => throw new IllegalArgumentException("Unrecognized option format: " + arg)
		}
	  }
    }
    opts
  }

}
