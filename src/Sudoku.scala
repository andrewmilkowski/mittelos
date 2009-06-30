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

import kodkod.ast.Formula.and
import kodkod.ast.Relation.ternary
import kodkod.ast.Relation.unary

import java.lang.management.ManagementFactory
import java.lang.management.ThreadMXBean

import java.util.ArrayList

import kodkod.ast.Decls
import kodkod.ast.Expression
import kodkod.ast.Formula
import kodkod.ast.Node
import kodkod.ast.Relation
import kodkod.ast.Variable
import kodkod.engine.Proof
import kodkod.engine.Solution
import kodkod.engine.Solver
import kodkod.engine.satlab.ReductionStrategy
import kodkod.engine.satlab.SATFactory
import kodkod.engine.ucore.AdaptiveRCEStrategy
import kodkod.engine.ucore.NCEStrategy
import kodkod.engine.ucore.SCEStrategy
import kodkod.instance.Bounds
import kodkod.instance.Tuple
import kodkod.instance.TupleFactory
import kodkod.instance.TupleSet

object SudokuMain
{
  /**
   * Returns the bounds for the default puzzle.
   * @return bounds for the default puzzle.
   */
  final def defaultPuzzle() : TupleSet =
  { 
    SudokuParser.parse("600200050018060020003000400000607800402050000000908000504090300020000014300005007")
  }
		
	
  private def usage() : Unit =
  { 
    System.out.println("Usage: java examples.sudoku.Sudoku [-core=<oce|rce|sce|nce>] [puzzle]")
    System.exit(1);
  }
		
  /**
   * Enumerates core extractors for use with sudoku.
   * @author Emina Torlak
   */
  sealed case class SudokuCoreExtractor()
  {
    def name() : String = { "SudokuCoreExtractor" }

	def extract(proof : Proof) : Array[Long] =
	{ 
	  val bean = ManagementFactory.getThreadMXBean()
	  bean.setThreadCpuTimeEnabled(true);
	  var strategy : ReductionStrategy = new AdaptiveRCEStrategy(proof.log())
	  this match
	  { 
	    case RCE => strategy = new AdaptiveRCEStrategy(proof.log())
		case SCE => strategy = new SCEStrategy(proof.log())
	    case NCE => strategy = new NCEStrategy(proof.log())
        case _ => throw new IllegalStateException("Unknown strategy: " + this)
	  }
	  val start = bean.getCurrentThreadUserTime()
	  proof.minimize(strategy)
	  val minCore = proof.highLevelCore().size().toLong
	  val end = bean.getCurrentThreadUserTime()
	  Array( minCore, toMillis(end-start) )
	}
		
	def toMillis(nanos : Long) : Long = { nanos / 1000000 }
  }
		
  case object OCE extends SudokuCoreExtractor
  {
    override def name() : String = { "OCE" }

    override def extract(proof : Proof) : Array[Long] =
	{ 
      val bean = ManagementFactory.getThreadMXBean()
	  bean.setThreadCpuTimeEnabled(true)
	  val start = bean.getCurrentThreadUserTime()
	  val initCore = proof.highLevelCore().size()
	  val end = bean.getCurrentThreadUserTime()
	  Array( initCore, toMillis(end-start) )
	}
  }

  case object RCE extends SudokuCoreExtractor
  {
    override def name() : String = { "RCE" }
  }
  case object SCE extends SudokuCoreExtractor
  {
    override def name() : String = { "SCE" }
  }
  case object NCE extends SudokuCoreExtractor
  {
    override def name() : String = { "NCE" }
  }
		

	
  /**
   * Usage: java examples.sudoku.Sudoku [-core=<oce|rce|sce|nce>] [puzzle]
   */
  def main(args : Array[String]) : Unit =
  {
	try
    {
	  val clues = if (args.length==0) defaultPuzzle() else SudokuParser.parse(args(args.length-1))
	  val opts = SudokuParser.options(args)
	  val extractor = opts.get("-core") match
      { 
        case Some("OCE") => OCE
        case Some("RCE") => RCE
        case Some("SCE") => SCE
        case Some("NCE") => NCE
        case None => RCE
        case _ => usage()
      }
	  (new Sudoku(Math.sqrt(clues.universe().size()).asInstanceOf[Int])).solve(clues, extractor.asInstanceOf[SudokuMain.SudokuCoreExtractor]);
	}
    catch
    {
      case iae : IllegalArgumentException => throw iae 
//			usage();
	}
  }
}

/**
 * A simple encoding of Sudoku, that includes several alternative
 * encodings of the rules.
 * @specfield n: int // the order of this sudoku puzzle
 * @author Emina Torlak
 */
final class Sudoku(r : Int)
{
  private val number = unary("number")
  private val grid  = ternary("grid")

  /**
   * Constructs a new instance of (r^2)x(r^2) Sudoku.
   * @requires r > 1
   * @effects this.n = r * r
   */
  if (r < 2) throw new IllegalArgumentException("r must be greater than 1:  r=" + r)
  private val region = new Array[Relation](r)
  for(i <- 0 until r)
  { 
    region(i) = unary("r"+(i+1))
  }
	
  /**
   * Returns grid[x][y].
   * @return grid[x][y].
   */
  private final def grid(x : Expression, y :  Expression) : Expression =
  { 
    y.join(x.join(grid))
  }
	
  /**
   * Returns the value of the complete predicate, given the specified rows and columns expressions.
   * @requires rows.arity = cols.arity = 1
   * @return complete(rows, cols)
   */
  private final def complete(rows : Expression, cols :  Expression) : Formula =
  {
    number.in(grid(rows,cols))
  }

  /**
   * Returns the rules of the game that are a bit slower than the other two formulations.
   * @return rules of the game
   */
  final def slowRules() : Formula =
  { 
    val rules = new ArrayList[Formula]()

    val x = Variable.unary("x")
    val y = Variable.unary("y")

    rules.add( grid(x,y).one().forAll(x.oneOf(number).and(y.oneOf(number))) )
	rules.add( complete(x, number).forAll(x.oneOf(number)) )
	rules.add( complete(number, y).forAll(y.oneOf(number)) )
	
    for(rx <- region)
    { 
	  for(ry <- region)
      { 
	    rules.add( complete(rx, ry) )
	  }
	}
	and(rules) 
  }
		
  /**
   * Returns the rules of the game that yield better performance than {@linkplain #slowRules()}.
   * @return rules of the game
   */
  final def rules() : Formula =
  { 
    val rules = new ArrayList[Formula]()

    val x = Variable.unary("x")
    val y = Variable.unary("y")

    val decls = x.oneOf(number).and(y.oneOf(number))

	rules.add( grid(x,y).some().forAll(decls) )
	rules.add( grid(x,y).intersection(grid(x, number.difference(y))).no().forAll(decls) )	
	rules.add( grid(x,y).intersection(grid(number.difference(x), y)).no().forAll(decls) )
	
	for(rx <- region)
    { 
	  for(ry <- region)
      { 
	    rules.add( grid(x, y).intersection(grid(rx.difference(x),ry.difference(y))).no().forAll(x.oneOf(rx).and(y.oneOf(ry))) )
	  }
	}
		
	and(rules)
  }
	
  /**
   * Returns a slightly different version of the rules that yields
   * better performance than {@linkplain #rules()}.
   * @return rules of the game
   */
  final def fastRules() :  Formula =
  { 
    val rules = new ArrayList[Formula]()

    val x = Variable.unary("x")
    val y = Variable.unary("y")

    val decls = x.oneOf(number).and(y.oneOf(number))

    rules.add( grid(x,y).one().forAll(decls) )
	rules.add( grid(x,y).intersection(grid(x, number.difference(y))).no().forAll(decls) )	
	rules.add( grid(x,y).intersection(grid(number.difference(x), y)).no().forAll(decls) )
	
	for(rx <- region)
    { 
	  for(ry <- region)
      { 
	    rules.add( complete(rx, ry) )
	  }
	}
	and(rules) 
  }
	
  /**
   * Constructs new bounds using the given set of puzzle clues.
   * @requires clues.universe = {i: Integer | 0 < i <= this.n}
   * @requires clues.arity = 3
   * @return new bounds using the given set of puzzle clues.
   */
  final def bounds(clues : TupleSet) : Bounds =
  { 
    val r = region.length
	val n = r*r
	if(clues.universe().size()!=n || clues.arity()!=3) throw new IllegalArgumentException()
		
	val bounds = new Bounds(clues.universe())
	val f = bounds.universe().factory()
		
	bounds.boundExactly(number, f.allOf(1))
	for(i <- 0 until r)
    { 
	  bounds.boundExactly(region(i), f.range(f.tuple((i*r+1).asInstanceOf[AnyRef]), f.tuple(((i+1)*r).asInstanceOf[AnyRef])))
	}
		
	val givens = clues.clone()
	val upper = f.allOf(3)

	for(t <- clues.toArray(new Array[Tuple](0)))
    { 
	  val x = t.atom(0)
	  val y = t.atom(1)
	  val v = t.atom(2)
	  for(i <- 1 until n+1)
      {
	    if (v!=i) upper.remove(f.tuple(x, y, i.asInstanceOf[AnyRef]))
	  }
	}
		
	bounds.bound(grid, givens, upper)
	bounds
  }
		
	
	
  /**
   * Solves the given puzzle using MiniSatProver and translation logging
   * if core is true; otherwise solves it using MiniSat.  Solution is
   * printed to standard output.
   */
  def solve(clues : TupleSet, extractor : SudokuMain.SudokuCoreExtractor) : Unit =
  {
    val solver = new Solver()
		
    solver.options().setSolver(SATFactory.MiniSatProver)
	solver.options().setLogTranslation(1)
		
				
    val sol = solver.solve(rules(), bounds(clues))
	if (sol.instance()!=null)
    { 
	  System.out.println(sol.stats());	
	  System.out.println(SudokuParser.prettyPrint(sol.instance().tuples(grid)));
	}
    else
    {
	  System.out.println(sol.stats());
	  val proof = sol.proof()
	  val coreData = extractor.extract(proof)
	  System.out.println("Core (strategy="+extractor.name().toLowerCase()+", size="+coreData(0)+", ms="+coreData(1)+"):")
	  for(n <- proof.highLevelCore().values().toArray)
      { 
	    System.out.println(n);
	  }
	}
  }
}

