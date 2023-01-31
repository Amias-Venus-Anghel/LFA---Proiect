import scala.collection.mutable
import scala.util.control.Breaks._

class Dfa[A] (sink: A ,is: A, fs: Set[A], tr: Array[(A, A, String)]/* TODO : define the constructor params */){
  var transl: Array[(A, A, String)] = tr // array of transitions of the dfa
  var iState: A = is // initial state
  var fStates: Set[A] = fs // set of final states
  var sinkS = sink // sink state
  // The following methods are only the methods directly called by the test suite. You can (and should) define more.

  /* returns dfa in a string, for debugging */
  def printDfa(): String = {
    var rez: String = ""

    rez = "iState: " + iState + "\nfinal States: "
    for (el <- fStates) {
      rez += "\t" + el + "\n"
    }
    rez += "Transitios:\n"
    for (el <- transl) {
      rez += "\t" + el + "\n"
    }

    rez
  }

  def map[B](f: A => B) : Dfa[B] = {
    /* for each transition, f is applyed and a new resulting dfa is created */
    var transl2: Array[(B, B, String)] = new Array[(B, B, String)](0)
    var iss = f(iState)
    var fss: Set[B] = Set.empty

    for (el <- transl) {
      var x = f(el._1)
      var y = f(el._2)
      var z = el._3
      transl2 = transl2 :+ (x, y, z)
    }

    for (st <- fStates) {
      fss = fss + f(st)
    }

    new Dfa[B](f(sinkS) ,iss, fss, transl2)
  } // TODO implement map

  def next(state:A, c: Char): A = {
    /* find the transition from state, on character c
    * if there is no such transitions returns sink state */
    var nextS: A = sinkS
    for (el <- transl) {
      if (el._1 == state && el._3 == c.toString) {
        nextS = el._2
      }
    }
    nextS
  } // TODO implement next

  def accepts(str: String): Boolean = {

    def walkthro(remaininStr: String, state: A): Boolean = {
      if (state == sinkS) {
        /* if transitions end to sink state, string is not acceped */
        return false
      }
      if (isFinal(state) && remaininStr.isEmpty) {
        /* if state is final and the string has been consumend, is accepted */
        return true
      }

      var accept = false;
      var chr = '\u0000'
      if (remaininStr.nonEmpty) {
        chr = remaininStr.head
      }

      accept | walkthro(remaininStr.tail, next(state, chr))
    }

    walkthro(str, iState)
  } // TODO implement accepts

  def getStates : Set[A] = {
    var set: Set[A] = Set.empty
    set = set ++ fStates
    for (el <- transl) {
      set = set + el._1
    }
    set
  } // TODO implement getStates

  def isFinal(state: A): Boolean = {
    fStates.contains(state)
  }  // TODO implement isFinal
}

// This is a companion object to the Dfa class. This allows us to call the method fromPrenex without instantiating the Dfa class beforehand.
// You can think of the methods of this object like static methods of the Dfa class
object Dfa {
  def nfaToDfa(nfa: Nfa[Int]): Dfa[Int] = {
    var alphabet: Set[String] = nfa.getAlphabet // alphabet of nfa
    var stMap = mutable.Map[Int, Set[Int]]() /* map between new state names and
    a set Q of nfa states that merge into the new state*/
    var finalSt: Set[Int] = Set.empty // set of final states
    var transl: Array[(Int, Int, String)] = new Array[(Int, Int, String)](0) // transitions
    var acc: Int = 0 // accumulator, keeps track of current new state counters
    stMap(acc) = nfa.closeEps(nfa.iState) ++ Set(nfa.iState) // add initial state

    breakable { // check if initial state is final
      for (checkFinal <- stMap(acc)) {
        if (nfa.isFinal(checkFinal)) {
          finalSt = finalSt + acc
          break
        }
      }
    }

    var qToCheck: mutable.Queue[Int] = new mutable.Queue[Int]()
    qToCheck.enqueue(0)
    /* while there are remaining states to have transitions created for, fist element
    * is extracted and for each character in alphabet, a transition is created,
    * alongside the new necesarry states and their mapping and epsilon closures */
    while (qToCheck.nonEmpty) {
      var currState = qToCheck.dequeue()
      for (chr <- alphabet) {
        // pt fiecare chr, gaseste toate starile in care se poate ajunge prin el si creza o noua stare in dfa
        var nextSs: Set[Int] = Set.empty
        //get a set of all next states for chr
        for (ist <- stMap(currState)) {
          nextSs = nextSs ++ nfa.closureChr(ist, chr.head)
        }

        if (nextSs.nonEmpty) {// if there are any next states (not sink)
          // check if set has already been assigned to a new state
          var found: Boolean = false
          breakable {
            for (pair <- stMap) {
              if (pair._2 == nextSs) { // if set is the same add transition on chr
                transl = transl :+ (currState, pair._1, chr)
                found = true
                break
              }
            }
          }

          if (!found) {
            // if a new state has to be created
            acc += 1
            stMap(acc) = nextSs
            qToCheck.enqueue(acc)
            transl = transl :+ (currState, acc, chr)
            // check if new state is final
            breakable {
              for (checkFinal <- stMap(acc)) {
                if (nfa.isFinal(checkFinal)) {
                  finalSt = finalSt + acc
                  break
                }
              }
            }
          }
        }
        else {
          transl = transl :+ (currState, -1, chr)
        }
      }
    }

    new Dfa[Int](-1, 0, finalSt, transl)
  }

  def fromPrenex(str: String): Dfa[Int] = {
    /* get nfa from prenex and transforms it to dfa */
    var nfa = Nfa.fromPrenex(str)
    nfaToDfa(nfa)
  } // TODO implement Prenex -> Dfa transformation. hint: you should make use of Nfa.fromPrenex to build the Dfa
  // You can add more methods to this object
}
