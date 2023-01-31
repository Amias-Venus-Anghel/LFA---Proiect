import Nfa.star

import scala.collection.mutable

class Nfa[A](is: A, fs: A, tr: Array[(A, A, String)]) { /* TODO : define the constructor params */
  var transl: Array[(A, A, String)] = tr // array of transitions of the nfa
  var iState: A = is // initial state
  var fState: A = fs // final state

  /* returns nfa in a string, for debugging */
  def printNfa(): String = {
    var rez: String = ""

    rez = "iState: " + iState + "\nfState: " + fState + "\nTransitions:\n"
    for (el <- transl) {
      rez += "\t" + el + "\n"
    }

    rez
  }

  def map[B](f: A => B) : Nfa[B] = {
    /* for each transition, f is applyed and a new resulting nfa is created */
    var transl2: Array[(B, B, String)] = new Array[(B, B, String)](0)
    var iss = f(iState)
    var fss = f(fState)

    for (el <- transl) {
      var x = f(el._1)
      var y = f(el._2)
      var z = el._3
      transl2 = transl2 :+ (x, y, z)
    }

    new Nfa[B](iss, fss, transl2)
  } // TODO implement map

  def next(state:A, c: Char): Set[A] = {
    /* for each transition, if the src state is our given state and the
    * transition character is the given character, the dest state is added to the
    * return value
    * if the src state is the given state and there is a transition on epsilon,
    * transitions will be traversed until given character is found
    * if the character is null and the state is final, state is added to the return
    * set*/
    var set: Set[A] = Set.empty
    for (el <- transl) {
      if (el._1 == state && el._3 == c.toString) {
        set = set + el._2
      }
      else if (el._1 == state && el._3 == "eps") {
        set = set ++ next(el._2, c)
      }
      else if (c == '\u0000' && isFinal(state)) {
        set = set + state
      }
    }
    set
  }  // TODO implement next

  def closeEps(st: A): Set[A] = {
    /* epsilon closure for given state, includes only transitions on epsilon */
    var setesp: Set[A] = Set.empty
    for (el <- transl) {
      if (el._1 == st && el._3 == "eps") {
        setesp = setesp + el._2
        setesp = setesp ++ closeEps(el._2)
      }
    }
    setesp
  }

  def closureChr(state:A, c: Char): Set[A] = {
    /* returns the closure for a given character starting with a given state
    * after character is consumed, all epsilon closure states are added to the set */
    var set: Set[A] = Set.empty

    for (el <- transl) {
      if (el._1 == state && el._3 == c.toString) {
        set = set + el._2
        set = set ++ closeEps(el._2)
      }
    }
    set
  }

  def accepts(str: String): Boolean = {

    def walkthro(remaininStr: String, state: A): Boolean = {
      if (isFinal(state) && remaininStr.isEmpty) {
        /* if state is final and the string has been consumend, is accepted */
        return true
      }

      var accept = false;
      var chr = '\u0000'
      if (remaininStr.nonEmpty) {
        chr = remaininStr.head
      }
      if (chr == ' ') {
        return walkthro(remaininStr.tail, state)
      }
      var nextS = next(state, chr)
      for (st <- nextS) {
        /* if one of the checked paths accepts the word, then true is returned */
        accept = accept | walkthro(remaininStr.tail, st)
      }
      accept
    }

    walkthro(str, iState)
  } // TODO implement accepts

  def getStates : Set[A] = {
    var set: Set[A] = Set(iState, fState)
    for (el <- transl) {
      set = set + el._1
    }
    set
  } // TODO implement getStates

  def getAlphabet : Set[String] = {
    var set: Set[String] = Set.empty
    for (el <- transl) {
      if (el._3 != "eps") {
        set = set + el._3
      }
    }
    set
  }

  def isFinal(state: A): Boolean = {
    state == fState
  }  // TODO implement isFinal
}

// This is a companion object to the Nfa class. This allows us to call the method fromPrenex without instantiating the Nfa class beforehand.
// You can think of the methods of this object like static methods of the Nfa class
object Nfa {

  def atom(chr: String, last_state: Int): Nfa[Int] = {
    /* created a nfa for a atom */
    var is: Int = last_state + 1
    var fs: Int = last_state + 2
    var tr = (is, fs, chr)
    if (chr == "void") {
      tr = (is, is, chr)
    }

    var arr: Array[(Int, Int, String)] = new Array[(Int, Int, String)](0)
    arr = arr :+ tr

    new Nfa(is, fs, arr)
  }

  def concat(left: Nfa[Int], right: Nfa[Int]): Nfa[Int] = {
    /* creates nfa for concat */
    var is: Int = left.iState;
    var fs: Int = right.fState;
    val tr = (left.fState, right.iState, "eps")
    var arr: Array[(Int, Int, String)] = new Array[(Int, Int, String)](0)
    arr = arr :+ tr
    arr = arr.concat(left.transl)
    arr = arr.concat(right.transl)

    new Nfa(is, fs, arr)
  }

  def union(left: Nfa[Int], right: Nfa[Int], last_state: Int): Nfa[Int] = {
    /* creates nfa for union */
    var is: Int = last_state + 1
    var fs: Int = last_state + 2
    var arr: Array[(Int, Int, String)] = new Array[(Int, Int, String)](0)
    arr = arr :+ (is, right.iState, "eps") :+ (is, left.iState, "eps") :+ (right.fState, fs, "eps") :+ (left.fState, fs, "eps")
    arr = arr.concat(left.transl)
    arr = arr.concat(right.transl)

    new Nfa(is, fs, arr)
  }

  def star(part: Nfa[Int], last_state: Int): Nfa[Int] = {
    /* creates nfa for star */
    var is: Int = last_state + 1;
    var fs: Int = last_state + 2;
    var arr: Array[(Int, Int, String)] = new Array[(Int, Int, String)](0)
    arr = arr :+ (is, part.iState, "eps") :+ (is, fs, "eps") :+ (part.fState, fs, "eps") :+ (part.fState, part.iState, "eps")
    arr = arr.concat(part.transl)

    new Nfa(is, fs, arr)
  }

  def plus(part: Nfa[Int], last_state: Int): Nfa[Int] = {
    /* creates nfa for plus */
    def modifyState(state: Int): Int = {
      state + last_state + 1
    }

    var nfaToConcat = part.map(modifyState)
    var nfaStar = star(part, last_state * 2 + 1)
    concat(nfaToConcat, nfaStar)
  }

  def maybe(part: Nfa[Int], last_state: Int): Nfa[Int] = {
    /* creates nfa for maybe */
    var is: Int = last_state + 1
    var fs: Int = last_state + 2
    var arr: Array[(Int, Int, String)] = new Array[(Int, Int, String)](0)
    arr = arr :+ (is, part.iState, "eps"):+ (part.fState, fs, "eps") :+ (is, fs, "eps")
    arr = arr.concat(part.transl)

    new Nfa(is, fs, arr)
  }

  def fromPrenex(str: String): Nfa[Int] = {
    /* using two stacks the prenex is converted to nfa
    * one stack contains all separate words from prenex and the other the gradually
    * generated nfas
    * when a key word is found, nfas are extracted from the stack and used to create
    * a new nfa that is added back to the stack
    * at the end the stack contains only one nfa, the result one
    *
    * var last_state increments to keep trac of what states are already used by */
    var stack1 = mutable.Stack[String]()
    var stack2 = mutable.Stack[Nfa[Int]]()

    var arr: Array[String] = str.split(" ")

    for (el <- arr) {
      stack1.push(el)
    }

    var last_state = -1;

    while (stack1.nonEmpty) {
      var top: String = stack1.pop()

      top match {
        case "CONCAT" => {
          var param1: Nfa[Int] = stack2.pop()
          var param2: Nfa[Int] = stack2.pop()
          var nfa = concat(param1, param2)
          stack2.push(nfa)
        }
        case "UNION" => {
          var param1: Nfa[Int] = stack2.pop()
          var param2: Nfa[Int] = stack2.pop()
          var nfa = union(param1, param2, last_state)
          last_state = nfa.fState
          stack2.push(nfa)
        }
        case "MAYBE" => {
          var param: Nfa[Int] = stack2.pop()
          var nfa = maybe(param, last_state)
          last_state = nfa.fState
          stack2.push(nfa)
        }
        case "STAR" => {
          var param: Nfa[Int] = stack2.pop()
          var nfa = star(param, last_state)
          last_state = nfa.fState
          stack2.push(nfa)
        }
        case "PLUS" => {
          var param: Nfa[Int] = stack2.pop()
          var nfa = plus(param, last_state)
          last_state = nfa.fState
          stack2.push(nfa)
        }
        case _ => {
          var nfa = atom(top, last_state) // create the partial nfa
          last_state = nfa.fState // remember the last state
          stack2.push(nfa)
        }
      }
    }

    stack2.pop()
  }
}

// TODO implement Prenex -> Nfa transformation.

// You can add more methods to this object

