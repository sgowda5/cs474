import MySetTheoryDSL.classExp.{Constructor, Extends}
import MySetTheoryDSL.setExp.*

import scala.collection.mutable


object MySetTheoryDSL:
  type BasicType = Any

  class abstractClass(): //Contains all of the information for a class
    val method_map: collection.mutable.Map[String, Seq[setExp]] = collection.mutable.Map() //Methods for this class
    val field_map: collection.mutable.Map[String, assignRHS] = collection.mutable.Map() //Fields for this class
    val class_hierarchy: mutable.Stack[String] = new mutable.Stack[String]() //Stack of Strings that shows all of the classes this class inherits from


  private val macro_map: collection.mutable.Map[String, setExp] = collection.mutable.Map()
  private val scope_map: collection.mutable.Map[(String,Option[String]), Set[Any]] = collection.mutable.Map()
  private val current_scope: mutable.Stack[String] = new mutable.Stack[String]()


  private val vmt: collection.mutable.Map[String, abstractClass] = collection.mutable.Map() //Virtual method table

  private val object_binding: collection.mutable.Map[(String,Option[String]), abstractClass] = collection.mutable.Map() //Binds names to instances of objects




  def get_scope(name: String): Option[String] = //Walk up through the scope stack and find the first scope where our name is defined.
    current_scope.find(x => (scope_map get(name, Some(x))).isDefined)


  enum classBodyExp:
    case Field(name: String)
    case Method(name: String, body: setExp*)
    case ClassDef(name: String, parent: Extends, constructor: Constructor, args: classBodyExp*)

    def eval(): Any = {

      this match {
        case ClassDef(name,Extends(parent),Constructor(cBody*),args*) => {
          val myClass = new abstractClass()
          parent match {
            case Some(value) => myClass.class_hierarchy.addAll(vmt(value).class_hierarchy).push(name) //Add the new value to the inheritance stack, and add the whole thing to our class
            case None => myClass.class_hierarchy.push(name)
          }
          for (arg <- args) {
            arg.eval()
            arg match {
              case Field(f) => //println(f)
                myClass.field_map.update(f,assignRHS.Set(Insert()))
              case Method(n,e*) => myClass.method_map.update(n, e)
            }
          }
          vmt.update(name, myClass)
          cBody.foldLeft(Set())((v1,v2) => v1 | v2.eval())
        }
        case Field(str) =>
        case Method(str,body) =>
      }
    }


  enum fieldExp:
    case Object(name: String)
    case Set(set: setExp)

  enum classExp:
    case ClassDef(name: String, parent: Extends, constructor: Constructor, args: classBodyExp*)
    case Constructor(body: setExp*) //Only one
    case Extends(name: Option[String]) //Only one

    //def ClassDef(name: String, parent: Extends, constructor: Constructor, args: classExp*): None



    def eval(): Any = {
      this match {
        case ClassDef(name,Extends(parent),Constructor(cBody*),args*) => {
          val myClass = new abstractClass()
          parent match {
            case Some(value) => myClass.class_hierarchy.addAll(vmt(value).class_hierarchy).push(name) //Add the new value to the inheritance stack, and add the whole thing to our class
            case None => myClass.class_hierarchy.push(name)
          }
          for (arg <- args) {
            import MySetTheoryDSL.classBodyExp.*
            arg match {
              case Field(f) => //println(f)
                myClass.field_map.update(f,assignRHS.Set(Insert()))
              case Method(n,e*) => myClass.method_map.update(n, e)
            }
          }
          //println(myClass.class_hierarchy)
          vmt.update(name, myClass)
          cBody.foldLeft(Set())((v1,v2) => v1 | v2.eval())
        }
      }
    }

  enum assignRHS: //The value
    case Set(args: setExp)
    case CreateObject(name: String)


  enum setExp:
    //import MySetTheoryDSL.assignExp
    case AssignField(obj: String, fName: String, rhs: assignRHS)
    case Value(input: BasicType)
    case Variable(name: String)
    case Macro(name: String)
    case CreateMacro(name: String, op2: setExp)
    case Scope(name: String, op2:setExp)
    case Assign(name: String, op2: assignRHS)
    case Insert(op: setExp*)
    case NestedInsert(op: setExp*)
    case Delete(name: String)
    case Union(op1: setExp, op2: setExp)
    case Intersection(op1: setExp, op2: setExp)
    case Difference(op1: setExp, op2: setExp)
    case SymmetricDifference(op1: setExp, op2: setExp)
    case Product(op1: setExp, op2: setExp)
    case InvokeMethod(obj: String, mName: String)
    case GetField(obj: String, fName: String)




    def eval(): Set[Any] = { //Walks through the AST and returns a set. Set[Any]
      //import MySetTheoryDSL.assignExp.*
      this match {
        case Value(v) => Set(v)
        case Variable(name) => scope_map(name,get_scope(name)) //Lookup value
        case Macro(a) => macro_map(a).eval() //Lookup macro and execute
        case CreateMacro(a,b) =>
          macro_map.update(a,b)
          Set()
        case Scope(a,b) =>
          current_scope.push(a) //Push current scope onto stack
          val temp = b.eval() //Evaluate rhs
          current_scope.pop() //Current scope is over - go back to previous scope
          temp //Return the evaluated value
        //case Assign(Variable(name),Value(a)) => Set(a)
        case Assign(name, assignRHS.Set(set)) =>
          scope_map.update((name,current_scope.headOption),set.eval())
          Set()
        case Assign(name, assignRHS.CreateObject(oName)) =>
          object_binding.update((name,current_scope.headOption),vmt(oName))
          Set()
        case AssignField(obj, fName, v) =>
          vmt(obj).field_map(fName) = v
          Set()
        case GetField(obj, fName) => //Need to go up through stack
          for (parent <- object_binding(obj,current_scope.headOption).class_hierarchy) { //Check current class, then parent, then grandparent.. etc.
            vmt(parent).field_map get fName match {
              case Some(v) => return vmt(parent).field_map(fName) match { //The Field was found, evaluate it
                case assignRHS.Set(s) => s.eval()
                case assignRHS.CreateObject(name) => Set()
              }
              case None => //Not found in this class, go up one level and try again
            }
          }
          throw new RuntimeException("The field was not found")




        case Insert(to_insert*) => to_insert.foldLeft(Set())((v1,v2) => v1 | v2.eval())
        case NestedInsert(to_insert*) => to_insert.foldLeft(Set())((v1,v2) => v1 + v2.eval())
        case Delete(name) =>
          scope_map.remove(name,get_scope(name))
          Set()
        case Union(op1, op2) => op1.eval() | op2.eval()
        case Intersection(op1, op2) => op1.eval() & op2.eval()
        case Difference(op1, op2) => op1.eval() &~ op2.eval()
        case SymmetricDifference(op1, op2) =>
          val a = op1.eval()
          val b = op2.eval()
          (a &~ b).union(b &~ a)
        case Product(op1, op2) => //The two foldLeft()'s essentially act as a double for loop, so we can combine every element pairwise.
          op1.eval().foldLeft(Set())((left_op1, left_op2) => left_op1 | op2.eval().foldLeft(Set())((right_op1, right_op2) => right_op1 | Set(Set(left_op2) | Set(right_op2))))

        case InvokeMethod(obj,mName) => {
          for (parent <- object_binding(obj,current_scope.headOption).class_hierarchy) { //Check current class, then parent, then grandparent.. etc.
            vmt(parent).method_map get mName match {
              case Some(v) => return v.foldLeft(Set())((v1,v2) => v1 | v2.eval()) //The method was found, evaluate it
              case None => //Not found in this class, go up one level and try again
            }
          }
          throw new RuntimeException("The method was not found") //There wasn't a method with the name you tried to call
        }
      }
    }

  def Check(set_name: String, set_val: setExp, set_scope: Option[String] = None): Boolean = {  //the Scope can be optionally supplied, or global scope will be used if omitted.
    set_val.eval().subsetOf(scope_map(set_name,set_scope))
  }

  @main def runSetExp(): Unit =
    import setExp.*





