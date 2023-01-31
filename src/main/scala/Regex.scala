import scala.collection.mutable

object Regex {
  /*
    This function should:
    -> Classify input as either character(or string) or operator
    -> Convert special inputs like [0-9] to their correct form
    -> Convert escaped characters
  */
  def preprocess(s:List[Char]): List[Either[Char,Char]] = ???

  /* converts [0-9] , a+, a?, etc to the extended form
     [0-9] -> (0|1|2|3|4|5|6|7|8|9)
     a+ -> (aa*)
     a? -> (a|eps)
     add '-' to mark concat opperation */
  def convertForm(str:String): String = {
    var rez = "-"
    var q_initial = mutable.Queue[Char]()
    q_initial.enqueueAll(str)

    while (q_initial.nonEmpty) {
      /* where queue has elements to be procced, get element in queue and check
      if an extension is needed */

      var chr: Char = q_initial.dequeue()

      // add "-" to mark concat op in case it should be expected
      if (chr != '+' && chr != '*' && chr != '|' && chr != ')'
        && chr != '?' && rez.last != '|' && rez.last != '(') { // if next is not an opperator, add symbol for concat
        rez += "-"
      }

      if (chr == '[') { // [a-b] or similar
        var a = q_initial.dequeue() // first param
        q_initial.dequeue() // get rid of -
        var b = q_initial.dequeue() // second param
        q_initial.dequeue() // get rid of ]

        // recreate extended structure
        rez += "("
        if (Character.isDigit(a)) {
          /* we want to convert the string containing the digit into a digit
          if character is not converted to string first, result is the asci code instead */
          for (i <- a.toString.toInt until b.toString.toInt) {
            rez += i.toString + "|"
          }
        }
        else {
          // if the chrs are letter not need for conversions in for
          for (i <- a until b) {
            rez += i.toString + "|"
          }
        }

        rez += b + ")" // add last chr and ')'
      }

      else if (chr == '+') {
        var lastChr = rez.last

        var param = ""
        if (lastChr == ')') { // if last chr of rez is a ) get everything in ()
          rez = rez.dropRight(1) // remove ')'
          var last = rez.last // get the last chr in ()
          while (last != '(') { // while '(' hasn't been found, keep adding the the param
            param = last + param
            rez = rez.dropRight(1) // drop chr and check next
            last = rez.last
          }
          param = "(" + param + ")" // add () to param
        }
        else {
          param = lastChr.toString // if it's only one chr that becomes the param
        }
        rez = rez.dropRight(1) // remove '(' or the chr from rez
        rez += "(" + param + "-" + param + "*)" // add the new form
      }

      else if (chr == '?') {
        var lastChr = rez.last

        var param = ""
        if (lastChr == ')') { // if last chr of rez is a ) get everything in ()
          rez = rez.dropRight(1)
          var last = rez.last
          while (last != '(') {
            param = last + param
            rez = rez.dropRight(1)
            last = rez.last
          }
          param = "(" + param + ")"
        }
        else {
          param = lastChr.toString
        }
        rez = rez.dropRight(1)
        rez += "(" + param + "|eps)"
      }

      else { // if there is nothing to extend, just add chr to rezult
        if (chr == 'e' && q_initial.front == 'p' && q_initial.tail.front == 's') { // add eps
          // if eps is found, add it without "-" in between
          rez += chr
          chr = q_initial.dequeue()
          rez += chr
          chr = q_initial.dequeue()
          rez += chr
        }
        else
          rez += chr
      }
    }

    /* an addition '--' is added at the start, we remove it */
    rez.drop(2)
  }

  // This function should construct a prenex expression out of a normal one.
  def toPrenex(str: String): String = {
    /* using algorithm: https://www.youtube.com/watch?v=8QxlrRws9OI */

    // get priority of operator
    def priorityOf(chr:Char): Int = {
      if (chr == '*')
        return 3
      else if (chr == '-')
        return 2
      else if (chr == '|')
        return 1
      return 0
    }

    // translate operator in keyword reversed
    def getKeyWord(chr:Char): String = {
      if (chr == '-')
        return " TACNOC"
      else if (chr == '|')
        return " NOINU"
      else
        return " RATS"
    }

    //add all characters to stack
    var stack = mutable.Stack[Char]()
    stack.pushAll(convertForm(str))

    var operator = mutable.Stack[Char]()
    operator.push(')') // add place holder to avoid comparasion error

    var result = mutable.Stack[Char]()

    while (stack.nonEmpty) {
      var chr = stack.pop()
      // if chr is an operator check if we add it to stack or pop operators from stack
      if (chr == '-' || chr == '*' || chr == '|') {
          while (priorityOf(chr) < priorityOf(operator.head)) {
            result.pushAll(getKeyWord(operator.pop()))
          }
        operator.push(chr)
      }
      // if chr is ')' we add it to stack and expect a '(' to follow
      else if (chr == ')') {
        operator.push(chr)
      }
      // if '(' is found, pop chrs from operator stack until ')' is found
      else if (chr == '(') {
        while (operator.head != ')') {
          var op = operator.pop()
          result.pushAll(getKeyWord(op))
        }
        operator.pop()
      }
      else {
        // add to result normal characters
        result.push(' ')
        result.push(chr)
        // check if chr is eps and add it without spaces in between
        if (chr == 's' && stack.head == 'p' && stack.tail.head == 'e') {
          result.push(stack.pop())
          result.push(stack.pop())
        }
      }

    }

    while(operator.nonEmpty && operator.head != ')') {
      var op = operator.pop()
      result.pushAll(getKeyWord(op))
    }

    var rez = ""
    while(result.nonEmpty) {
      rez += result.pop()
    }

    rez
  }
}
