
object M5b {



// for timing purposes
def time_needed[T](n: Int, code: => T) = {
  val start = System.nanoTime()
  for (i <- 0 until n) code
  val end = System.nanoTime()
  (end - start)/(n * 1.0e9)
}


type Mem = Map[Int, Int]

import io.Source
import scala.util._

def load_bff(name: String) : String = {
    Try(Source.fromFile(name).mkString).getOrElse("")
}


def sread(mem: Mem, mp: Int) : Int = {
    mem.getOrElse(mp, 0)
}

def write(mem: Mem, mp: Int, v: Int) : Mem = {
    mem + (mp -> v)
}

    
def jumpRight(prog: String, pc: Int, level: Int) : Int = {
    if (level == -1) pc
    else if (prog.size <= pc) pc
    else{
        prog(pc) match {
            case '[' => jumpRight(prog,pc+1, level + 1)
            case ']' => jumpRight(prog,pc+1, level - 1)
            case _ => jumpRight(prog, pc+1, level)  
        }
    }

}

def jumpLeft(prog: String, pc: Int, level: Int) : Int = {
    if (level == -1) pc + 2
    else if (pc < 0) pc
    else{
        prog(pc) match {
            case ']' => jumpLeft(prog,pc - 1, level + 1)
            case '[' => jumpLeft(prog, pc - 1, level - 1)
            case _ => jumpLeft(prog, pc - 1, level)
        }
    }

}

def jtable(pg: String) : Map[Int, Int] = {

  val map = (0 until pg.length).map(pc =>{
    pg(pc) match{
      case ']' => (pc,jumpLeft(pg,pc - 1, 0))
      case '[' => (pc, jumpRight(pg, pc + 1, 0))
      case _ => (-1,-1)
    }
  })

  map.filter(_._1 != -1).toMap
}




def optimise(s: String) : String = {
	val remove_comment_reg = """[^<>+\-.\[\]]"""
	val remove_mem_reg = """\[-\]"""
	s.replaceAll(remove_comment_reg, "").replaceAll(remove_mem_reg, "0")
}


def combine_recurse(s:String, prev: Char, pc: Int= 1, copy:Int = 1, out:String = "") : String ={
	if (pc == s.length) out
	else{
		val curr = s(pc)
		prev match {
			case ('>' | '<' | '+' | '-') =>{
				if (curr == prev && copy < 26) combine_recurse(s,prev,pc+1,copy+1,out)
				else if (copy >= 26) combine_recurse(s,curr,pc+1,1,out+prev+"Z")
				else combine_recurse(s,curr,pc+1,1,out+prev+((copy+64).toChar))
			}
			case _ => combine_recurse(s,curr,pc+1,1,out+prev)
		}
	}
}

def combine(s: String) : String = {
	combine_recurse(s+"£",s.head)
	
}



def compute(pg: String, tb: Map[Int, Int], pc: Int, mp: Int, mem: Mem) : Mem = {
	if ( pc < 0 || pc >= pg.length) mem 
    else{
        pg(pc) match{
            case '>' => compute4(pg, tb, pc + 2, mp + (pg(pc+1).toInt - 64), mem)
            case '<' => compute4(pg, tb, pc + 2, mp - (pg(pc+1).toInt - 64), mem)
            case '+' => compute4(pg, tb, pc + 2, mp, write(mem, mp, sread(mem, mp) + (pg(pc + 1).toInt - 64)))
            case '-' => compute4(pg, tb, pc + 2, mp, write(mem, mp, sread(mem, mp) - (pg(pc + 1).toInt - 64)))
            case '.' => {
                print(sread(mem,mp).toChar)
                compute4(pg,tb,pc+1, mp,mem)
            }
            case '[' => {
                if (sread(mem, mp) == 0){
                    compute4(pg,tb,tb(pc),mp,mem)
                }
                else{
                    compute4(pg,tb,pc+1,mp,mem)
                }
            }
            case ']' => {
                if (sread(mem, mp) != 0){
                    compute4(pg,tb,tb(pc),mp,mem)
                }
                else{
                    compute4(pg,tb,pc+1,mp,mem)
                }
            }
			case '0' => compute4(pg, tb, pc + 1, mp, write(mem, mp, 0))
            case _ => compute4(pg,tb, pc + 1, mp, mem )
        }
    }
}

def run(pg: String, m: Mem = Map()) = {
	compute(combine(optimise(pg)), jtable(combine(optimise(pg))),0,0,m)
}


// time_needed(1, run(load_bff("sierpinski.bf"))) 
// time_needed(1, run(load_bff("mandelbrot.bf")))

}
