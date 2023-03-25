

object M5a {

// representation of BF memory 

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



def compute(prog: String, pc: Int, mp: Int, mem: Mem) : Mem = {
    if ( pc < 0 || pc >= prog.length) mem 
    else{
        prog(pc) match{
            case '>' => compute(prog,pc + 1, mp + 1, mem )
            case '<' => compute(prog,pc + 1, mp - 1, mem )
            case '+' => compute(prog,pc+1, mp, write(mem,mp,sread(mem,mp) + 1))
            case '-' => compute(prog,pc+1, mp, write(mem,mp,sread(mem,mp) - 1))
            case '.' => {
                print(sread(mem,mp).toChar)
                compute(prog,pc+1, mp,mem)
            }
            case '[' => {
                if (sread(mem, mp) == 0){
                    compute(prog,jumpRight(prog,pc+1,0),mp,mem)
                }
                else{
                    compute(prog,pc+1,mp,mem)
                }
            }
            case ']' => {
                if (sread(mem, mp) != 0){
                    compute(prog,jumpLeft(prog,pc-1,0),mp,mem)
                }
                else{
                    compute(prog,pc+1,mp,mem)
                }
            }
            case _ => compute(prog, pc + 1, mp, mem )

        }
    }
}

def run(prog: String, m: Mem = Map()) = {
    compute(prog, 0, 0, m)
}



// calculates the Collatz series for numbers from 1 to 30
//
//run(load_bff("collatz.bf"))

}

