package.hw1

import hw 1.SetLangDSL.SetDeclarations
import hw 1.SetLangDSL.SetDeclarations.RetrieveField

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.compiletime.ops.int.-
import scala.language.postfixOps
import java.util.concurrent.atomic.AtomicBoolean

// The main object where all the definitions are stored for various methods of the language.
object SetLangDSL:

  type AnyMapType = scala.collection.mutable.Map[Any,Any]
  type  AnyListType = scala.collection.mutable.ListBuffer[String]
  type AnySetType = scala.collection.mutable.Set[Any]

  // A private map "bindingScope" map is created is using the scala mutable map.
  // This Hashmap will provide the necessary binding of the abstractions with memory for the DSL language.

  // bindingScope maps the variable name with its value in "key" -> "value" format
  // Example: a1 -> HashSet(3, 4)
  private val bindingScope: scala.collection.mutable.Map[Any, Any] = scala.collection.mutable.Map()


  // class binding scope to map the class definitions
  private val classBindingScope: scala.collection.mutable.Map[Any, Any] = scala.collection.mutable.Map()

  // Binding scope for the object created
  private val tempObjectBindingScope: scala.collection.mutable.Map[Any, Any] = scala.collection.mutable.Map()

  // Map to keep tract of the private, protected and public fields and methods of a class
  private val accessScope: scala.collection.mutable.Map[Any, Any] = scala.collection.mutable.Map()

  // Virtual Dispatch Table for maintaining methods for inherited classes.
  private val virtualDispatchTable: scala.collection.mutable.Map[Any, Any] = scala.collection.mutable.Map()

  // interface binding scope to map the interface definitions
  private val interfaceBindingScope: scala.collection.mutable.Map[Any, Any] = scala.collection.mutable.Map()


  // Exception CLass binding scope to map the interface definitions
  private val exceptionBindingScope: scala.collection.mutable.Map[Any, Any] = scala.collection.mutable.Map()


  // An atomic boolean is defined to keep track if the exception is thrown. Initial Value is set to false.
  private val isExceptionCaught: AtomicBoolean = new AtomicBoolean(false);





  // Function will be called to update the bindingScope map with additional bindings.
  // Parameters accepted: (name "key", op "value")
  private def addBindingScope(name: Any, op: Any, mapToUse: AnyMapType = bindingScope) = mapToUse += (name -> op)


  // SetDeclarations enum is collection of all the methods for different functionalities of the Set DSL language.
  // For example Variable, Assign. Check, Insert, Delete, in a set.
  // Union, Intersection, Difference, SymmetricDifference, CartesianProduct operations.
  // Type com.samihann.hw1.SetLangDSL.SetDeclarations defines these functions.
  enum SetDeclarations:
    // Encapsulate any value in the function.
    case Value(input: Any)

    // Get the value assigned to the variable with name as abstraction.
    case Variable(name: String)

    // TO check if the toCheckValue is present in the setName(Variable)
    case Check(setName: SetDeclarations, toCheckValue: SetDeclarations)

    // To assign the value (operation2) to the key stringName
    case Assign(stringName: Any, operation2: SetDeclarations*)

    // Inset values in a set
    case Insert(operation1: SetDeclarations, operation2: SetDeclarations)
    case InsertNew(setName: String, operation2: SetDeclarations)
    case Delete(operation1: SetDeclarations, operation2: SetDeclarations)

    // Macro Functions
    case Macro(macroString: String, operation: SetDeclarations)
    case ImpMacro(operation: SetDeclarations)

    // Set Operations
    case Union(operation1: SetDeclarations, operation2: SetDeclarations)
    case Intersection(set1: SetDeclarations, set2: SetDeclarations)
    case Difference(set1: SetDeclarations, set2: SetDeclarations)
    case SymmetricDifference(set1: SetDeclarations, set2: SetDeclarations)
    case CartesianProduct(set1: SetDeclarations, set2: SetDeclarations)

    // Scope Functions
    case Scope(name: String, operations: SetDeclarations*)
    case GetScopeVariable(scopeName: String, variableName: String)


    // Homework Two.
    // Class Functions to perform the operation of class creation, object creation, invoking method.
    case ClassDef(name: String, operations: SetDeclarations*)
    case Field(name: String)
    case Constructor(operation: SetDeclarations*)
    case Method(name: String, params: AnyListType , operations: SetDeclarations*)
    case Object(name: String)
    case NewObject(name: String, className: String)
    case Public(op: SetDeclarations)
    case Private(op: SetDeclarations)
    case Protected(op: SetDeclarations)
    case InvokeMethod(objName: String, methodName: String, paramToAssign: Any*)
    case RetrieveField(objectName: String, fieldName: String)


    // Homework Three
    // Abstract Function, Interface, Implements
    case AbstractClassDef(name: String, operations: SetDeclarations*)
    case AbstractMethod(name: String, params: AnyListType)
    case InterfaceDecl(name: String, operations: SetDeclarations*)


    // Homework Four
    // Conditional Statements
    case IfConditionalStatement(condition: SetDeclarations, thenClause: SetDeclarations, elseClause: SetDeclarations)
    case Then (operations: SetDeclarations*)
    case Else (operations: SetDeclarations*)
    case ExceptionClassDef(name: String, operations: SetDeclarations)
    case ThrowException(name: String, operation: SetDeclarations.Assign)
    case CatchException(name: String, operations: SetDeclarations*)
    case Catch(exception: SetDeclarations, operations: SetDeclarations*)
    case ConditionalCheck(setName: SetDeclarations, toCheckValue: SetDeclarations)



    // eval function contains all the definitions with the computations for the functions of DSL language.
    def eval(mapToUse: AnyMapType = bindingScope, abstractMainDef: Boolean = false): Any =

      // Matches the function name and executes the same.
      this match {
        // Returns the value provided
        case Value(i) => return i

        // Variable - Parameter Passed : Name of the key for the map.
        // Variable function will check if the key is present in bindingScope Map
        // If present will return the value for the key / else it will return a "!!!Error!!!" message
        case Variable(name) =>
          // CHeck if the key is present.
          if (mapToUse.contains(name)) {
            return mapToUse(name)
          }
          else {
            println("ERROR: The variable `" + name + "` does not exist. Please check the name.")
            val returnText = "!!!Error!!!"
            return returnText
          }

        // The function will create a binding with the macroString as key and the operation as value.
        // If the Macro is present it will print the appropriate message on the screen.
        case Macro(macroString, operation) =>
          // CHeck if the key is present.
          if (mapToUse.contains(macroString)) {
            println("ERROR: The variable `" + macroString + "` already exist. The Macro cannot be created.")
            val returnString = "!!!Error!!!"
            return returnString
          } else {
            addBindingScope(macroString, operation, mapToUse)
            return mapToUse(macroString)
          }


        // The function will compute the function, macro is pointing to.
        case ImpMacro(operation) =>
          // Checking if the operation input parameter is of the type SetDeclaration.
          if (operation.eval(mapToUse).isInstanceOf[SetDeclarations]) {
            val opTemp = operation.eval(mapToUse).asInstanceOf[SetDeclarations]
            val result = opTemp.eval(mapToUse)
            // Returning the evaluated value.
            return result
          } else {
            println("ERROR: The operation provided `" + operation + "` is not a macro.")
            val resultText = "The variable is not a macro"
            return resultText
          }

        // The function will create a binding with the name as key and the operation as value.
        // If a single operation is provided, it will bind the key to Int. If multiple values are provided.
        // it will create a set for the values and set it to the value.
        // Function will take variable number of parameters
        case Assign(name, operation2*) =>
          // Check if the name is a function or string
          if (name.isInstanceOf[SetDeclarations]) {
            val n = name.asInstanceOf[SetDeclarations].eval(mapToUse)
            val tempName = mapToUse.find(_._2 == n).map(_._1).getOrElse(0)
            if (operation2.length > 1) {
              val tempSet = scala.collection.mutable.Set[Any]()
              operation2.foreach(i => {
                val num = i.eval(mapToUse)
                tempSet.add(num)
              })
              mapToUse(tempName) = tempSet
            }
            else {
              operation2.foreach(i => {
                mapToUse(tempName) = i.eval(mapToUse)
              })
            }
            return name.asInstanceOf[SetDeclarations].eval(mapToUse)
          } else {
            // Check length of input parameters.
            if (operation2.length > 1) {
              val tempSet = scala.collection.mutable.Set[Any]()
              addBindingScope(name, tempSet, mapToUse)
              // Create and update the set
              operation2.foreach(i => {
                val num = i.eval(mapToUse)
                val temp = mapToUse(name).asInstanceOf[scala.collection.mutable.Set[Any]] ++ scala.collection.mutable.Set[Any](num)
                mapToUse += (name -> temp)
              })
            }
            else {
              // When only one parameter is present.
              operation2.foreach(i => {
                addBindingScope(name, i.eval(mapToUse), mapToUse)
              })
            }
            return mapToUse(name)
          }


        // Function will add the value to set.
        // It will accept the variable and value as parameter.
        case Insert(op1, op2) =>
          val o1 = op1.eval(mapToUse)
          val o2 = op2.eval(mapToUse)
          // check if the variable is present.
          if (op1.eval(mapToUse) != "!!!Error!!!") {
            // CHeck if it is a set.
            if (o1.isInstanceOf[scala.collection.mutable.Set[Any]]) {
              mapToUse.update(o1, o1.asInstanceOf[scala.collection.mutable.Set[Any]] += o2)
              return op1.eval(mapToUse)
            }
            else {
              println("ERROR: The variable `" + op1 + "` is not set.")
              val resultMsg = "Variable is not a set."
              return resultMsg
            }
          }
          else {
            println("ERROR: The variable `" + op1 + "` is not present.")
            val resultMsg = "Variable Not Present."
            return resultMsg
          }


        // Inset into the set using the key for the map binding.
        // Parameters accepted are string and SetDeclaration.
        case InsertNew(setName, operation2) =>
          if (mapToUse.contains(setName)) {
            val temp: scala.collection.mutable.Set[Any] = mapToUse(setName).asInstanceOf[scala.collection.mutable.Set[Any]] ++ scala.collection.mutable.Set[Any](operation2.eval(mapToUse))
            mapToUse += (setName -> temp)
          }
          else {
            val set: scala.collection.mutable.Set[Any] = scala.collection.mutable.Set[Any](operation2.eval(mapToUse))
            mapToUse += (setName -> set)
          }
          return mapToUse(setName)

        case Check(setValue, operation) =>
          if (setValue.eval(mapToUse) != "!!!Error!!!") {
            val checkResult = setValue.eval(mapToUse).asInstanceOf[scala.collection.mutable.Set[Any]].contains(operation.eval(mapToUse))
            return checkResult
          }
          else {
            println("ERROR: The variable is not present.")
            val resultMsg = "Variable Not Present."
            return resultMsg
          }

        // The function to delete an object from a set.
        // Parameters passed are Variable and Value to be deleted.
        case Delete(op1, op2) =>
          val o1 = op1.eval(mapToUse)
          val o2 = op2.eval(mapToUse)
          if (op1.eval(mapToUse) != "!!!Error!!!") {
            if (o1.isInstanceOf[scala.collection.mutable.Set[Any]]) {
              mapToUse.update(o1, o1.asInstanceOf[scala.collection.mutable.Set[Any]] -= o2)
              return op1.eval(mapToUse)
            }
            else {
              println("ERROR: The variable `" + op1 + "` is not set.")
              val resultMsg = "Variable is not a set."
              return resultMsg
            }
          }
          else {
            println("ERROR: The variable is not present.")
            val resultMsg = "Variable Not Present."
            return resultMsg
          }

        // THe function will perform the union operation of the sets passed as variables.
        // CheckSet() is used to verify if the passed parameters are set. It will return boolean.
        case Union(operation1, operation2) =>

          if (CheckSet(operation1, operation2)) {
            val union = operation1.eval(mapToUse).asInstanceOf[scala.collection.mutable.Set[Any]].union(operation2.eval(mapToUse).asInstanceOf[scala.collection.mutable.Set[Any]])
            return union
          } else {
            println("ERROR: The input parameters are not set")
            val resultMsg = "THe Inputs are not sets"
            return resultMsg
          }

        // THe function will perform the Intersection operation of the sets passed as variables.
        // CheckSet() is used to verify if the passed parameters are set. It will return boolean.
        case Intersection(set1, set2) =>
          if (CheckSet(set1, set2)) {
            val intersect = set1.eval(mapToUse).asInstanceOf[scala.collection.mutable.Set[Any]].intersect(set2.eval(mapToUse).asInstanceOf[scala.collection.mutable.Set[Any]])
            return intersect
          } else {
            println("ERROR: The input parameters are not set")
            val resultMsg = "THe Inputs are not sets"
            return resultMsg
          }


        // THe function will perform the Difference operation of the sets passed as variables.
        // CheckSet() is used to verify if the passed parameters are set. It will return boolean.
        case Difference(set1, set2) =>
          if (CheckSet(set1, set2)) {
            val diff = set1.eval(mapToUse).asInstanceOf[scala.collection.mutable.Set[Any]].diff(set2.eval(mapToUse).asInstanceOf[scala.collection.mutable.Set[Any]])
            return diff
          } else {
            println("ERROR: The input parameters are not set")
            val resultMsg = "THe Inputs are not sets"
            return resultMsg
          }

        // THe function will perform the SymmetricDifference operation of the sets passed as variables.
        // CheckSet() is used to verify if the passed parameters are set. It will return boolean.
        case SymmetricDifference(set1, set2) =>
          if (CheckSet(set1, set2)) {
            val differenceSet2 = set2.eval(mapToUse).asInstanceOf[scala.collection.mutable.Set[Any]]
            val sydiff = set1.eval(mapToUse).asInstanceOf[scala.collection.mutable.Set[Any]].diff(differenceSet2).union(differenceSet2.diff(set1.eval(mapToUse).asInstanceOf[scala.collection.mutable.Set[Any]]))
            return sydiff
          } else {
            println("ERROR: The input parameters are not set")
            val resultMsg = "The Inputs are not sets!!"
            return resultMsg
          }

        // THe function will perform the CartesianProduct operation of the sets passed as variables.
        // CheckSet() is used to verify if the passed parameters are set. It will return boolean.
        case CartesianProduct(s1, s2) =>
          if (CheckSet(s1, s2)) {
            val productSet1 = s1.eval(mapToUse).asInstanceOf[scala.collection.mutable.Set[Any]]
            val productSet2 = s2.eval(mapToUse).asInstanceOf[scala.collection.mutable.Set[Any]]
            val cartesianProduct = productSet1.flatMap(x => productSet2.map(y => (x, y)))
            return cartesianProduct
          } else {
            println("ERROR: The input parameters are not set")
            val resultMsg = "The Inputs are not sets!!"
            return resultMsg
          }

        // THe function will create Scope for passed parameters: Variables, Operations, Functions
        // Function will return the result of the last operation provide to the function.
        case Scope(name, operations*) =>
          // Looping though the operations passed to the Scope
          operations.foreach(i => {
            // operation.scopeEval(scopeName) create custom binding for the variables for the scope.
            val k = i.asInstanceOf[SetDeclarations].scopeEval(name)
            val z = k.asInstanceOf[SetDeclarations].eval(mapToUse)
            if (i == operations.last) {
              // Returning the result of the last operation passed to the function.
              return z
            }
          })

        // The function will return the value for the variable in the particular scope.
        case GetScopeVariable(scopeName, variableName) =>
          val sName = scopeName + "-SCOPEVARIABLE-" + variableName
          if (mapToUse.contains(sName)) {
            return mapToUse(sName)
          } else {
            println("ERROR: The variable is not present in the given scope")
            return "!!! ERROR !!!"
          }


        case ClassDef(name, operation*) =>
          if(classBindingScope.contains(name)){
            println("ERROR: The class with `" + name + "` already exists.")
            val returnText = "!!!Error!!!"
            return returnText
          } else {
            val tempClassMap = scala.collection.mutable.Map[Any,Any]("fields" -> null, "constructor" -> null, "method" -> null )
            val tempClassFieldMap = scala.collection.mutable.Map[Any,Any]()
            val tempMethodMap = scala.collection.mutable.Map[Any,Any]()
            val tempAbstractMethodMap = scala.collection.mutable.Map[Any,Any]()


            val allFields: scala.collection.mutable.Map[Any, Any] = scala.collection.mutable.Map()

            val privateMembers: scala.collection.mutable.Map[Any, Any] = scala.collection.mutable.Map()
            val privateField:scala.collection.mutable.Set[Any] = scala.collection.mutable.Set()
            val privateMethod:scala.collection.mutable.Set[Any] = scala.collection.mutable.Set()
            val privateAbstractMethod:scala.collection.mutable.Set[Any] = scala.collection.mutable.Set()
            privateMembers += ("fields" -> privateField)
            privateMembers += ("methods" -> privateMethod)
            privateMembers += ("abstractMethod" -> privateAbstractMethod)


            val publicMembers: scala.collection.mutable.Map[Any, Any] = scala.collection.mutable.Map()
            val publicField:scala.collection.mutable.Set[Any] = scala.collection.mutable.Set()
            val publicMethod:scala.collection.mutable.Set[Any] = scala.collection.mutable.Set()
            publicMembers += ("fields" -> publicField)
            publicMembers += ("methods" -> publicMethod)
            val publicAbstractMethod:scala.collection.mutable.Set[Any] = scala.collection.mutable.Set()
            publicMembers += ("abstractMethod" -> publicAbstractMethod)


            val protectedMembers: scala.collection.mutable.Map[Any, Any] = scala.collection.mutable.Map()
            val protectedField:scala.collection.mutable.Set[Any] = scala.collection.mutable.Set()
            val protectedMethod:scala.collection.mutable.Set[Any] = scala.collection.mutable.Set()
            protectedMembers += ("fields" -> protectedField)
            protectedMembers += ("methods" -> protectedMethod)
            val protectedAbstractMethod:scala.collection.mutable.Set[Any] = scala.collection.mutable.Set()
            protectedMembers += ("abstractMethod" -> protectedAbstractMethod)


            val classAccess: scala.collection.mutable.Map[Any, Any] = scala.collection.mutable.Map()
            classAccess += ("private" -> privateMembers)
            classAccess += ("public" -> publicMembers)
            classAccess += ("protected" -> protectedMembers)

            accessScope += (name -> classAccess)

            operation.foreach(i => {
              val op = i.asInstanceOf[SetDeclarations]
              if (op.isInstanceOf[SetDeclarations.Constructor]){
                tempClassMap("constructor") = op
              }else{
                val opReturn = op.classEval(name).asInstanceOf[SetDeclarations]
                opReturn match {
                  case _: SetDeclarations.Field =>
                    val name = opReturn.eval(mapToUse)
                    tempClassFieldMap += (name -> null)
                  case _: SetDeclarations.Method =>
                    val temp = opReturn.eval(mapToUse).asInstanceOf[mutable.Map[Any, Any]]
                    tempMethodMap.++=(temp)
                  case _ =>
                    println("Operation is not permitted to be sent through the ClassDef")
                    val returnText = "!!!Error!!!"
                    return returnText
                }
              }
            })
            tempClassMap("fields") = tempClassFieldMap
            tempClassMap("method") = tempMethodMap
            tempClassMap("inheritance") = false
            tempClassMap("abstractDef") = false
            classBindingScope += (name -> tempClassMap)
            return true
          }

        case Field(name) => return name

        case Method(name, listVar, operation*) =>

          if(abstractMainDef && operation.nonEmpty){
            println("An abstract method cannot have any implementation, only method signature declaration")
            val returnText = "!!!Error!!!"
            return returnText
          }

          val tempMethodMap = scala.collection.mutable.Map[Any,Any]("op" -> null, "variables" -> null)
          val tempClassMethodMap = scala.collection.mutable.Map[Any,Any]()
          val tempOpList = scala.collection.mutable.Set[Any]()
          operation.foreach(i => {
            tempOpList += i
          }
          )
          val tempVarMap = scala.collection.mutable.Map[Any,Any]()
          listVar.foreach(i => {
            tempVarMap += (i -> null)

          })
          tempMethodMap("op") = tempOpList
          tempMethodMap += ("variables" -> tempVarMap)
          tempClassMethodMap += (name -> tempMethodMap)
          return tempClassMethodMap

        case InvokeMethod(objName, name,paramToAssign*) =>
          if(!tempObjectBindingScope.contains(objName)){
            println(s"Object $objName does not exist.")
            val returnText = "!!!Error!!!"
            return returnText
          }

          if(!tempObjectBindingScope(objName).asInstanceOf[AnyMapType]("methods").asInstanceOf[AnyMapType].contains(name)){
            println(s"Method mentioned does not exits $objName does not exist.")
            val returnText = "!!!Error!!!"
            return returnText
          }

          paramToAssign.foreach(item => {
            val item1 = item.asInstanceOf[SetDeclarations]
            val objMap = tempObjectBindingScope(objName).
              asInstanceOf[AnyMapType]("methods").
              asInstanceOf[AnyMapType](name).asInstanceOf[AnyMapType]("variables").asInstanceOf[AnyMapType]
            item1.eval(objMap)
          })

          tempObjectBindingScope(objName).asInstanceOf[AnyMapType]("fields").asInstanceOf[AnyMapType].
            foreach{
              case (k, v) =>
                tempObjectBindingScope(objName).
                  asInstanceOf[AnyMapType]("methods").
                  asInstanceOf[AnyMapType](name).
                  asInstanceOf[AnyMapType]("variables")
                .asInstanceOf[AnyMapType] += (k -> v)

            }
          val classNameForObject = tempObjectBindingScope(objName).asInstanceOf[AnyMapType]("className")

          // CHeck if the method is present in the Virtual Dispatch Table.
          // Which signifies that the method is over written.
          if (virtualDispatchTable.contains(classNameForObject)){
            if(virtualDispatchTable(classNameForObject).asInstanceOf[AnyMapType].contains(name)){
              val tempMap = virtualDispatchTable(classNameForObject).asInstanceOf[AnyMapType](name).asInstanceOf[AnyMapType]
              val op = tempMap("op").asInstanceOf[scala.collection.mutable.Set[Any]]
              val variableMap = tempMap("variables").asInstanceOf[AnyMapType]

              op.foreach( z => {
                val res = z.asInstanceOf[SetDeclarations].eval(mapToUse = variableMap)
                if (z == op.last) {
                  // Returning the result of the last operation passed to the function.
                  return res
                }
              }
              )
            }
            else {
              val tempMap = tempObjectBindingScope(objName).asInstanceOf[AnyMapType]("methods").asInstanceOf[AnyMapType](name).asInstanceOf[AnyMapType]
              val op = tempMap("op").asInstanceOf[scala.collection.mutable.Set[Any]]
              val variableMap = tempMap("variables").asInstanceOf[AnyMapType]

              op.foreach( z => {
                val res = z.asInstanceOf[SetDeclarations].eval(mapToUse = variableMap)
                if (z == op.last) {
                  // Returning the result of the last operation passed to the function.
                  return res
                }
              }
              )
            }

          }
          else{
            val tempMap = tempObjectBindingScope(objName).asInstanceOf[AnyMapType]("methods").asInstanceOf[AnyMapType](name).asInstanceOf[AnyMapType]
            val op = tempMap("op").asInstanceOf[scala.collection.mutable.Set[Any]]
            val variableMap = tempMap("variables").asInstanceOf[AnyMapType]

            op.foreach( z => {
              val res = z.asInstanceOf[SetDeclarations].eval(mapToUse = variableMap)
              if (z == op.last) {
                // Returning the result of the last operation passed to the function.
                return res
              }
            }
            )
          }




        // Get field value of any field of any instance
        case RetrieveField(objectName, fieldName) =>
          if(!tempObjectBindingScope.contains(objectName)){
            // If object name does not exist, pop error and exit the program.
            println(s"Object name $objectName does not exist.")
            val returnText = "!!!Error!!!"
            return returnText
          }

          val temp = tempObjectBindingScope(objectName).asInstanceOf[scala.collection.mutable.Map[Any, Any]]("fields").asInstanceOf[scala.collection.mutable.Map[Any, Any]]
          if (!temp.contains(fieldName)){
            // If field name does not exist, pop error and exit the program.
            println(s"Field name $fieldName does not exist.")
            val returnText = "!!!Error!!!"
            return returnText
          }
          // Get class name
          val cname = tempObjectBindingScope(objectName).asInstanceOf[scala.collection.mutable.Map[Any, Any]]("className")

          // Get class private fields
          val privateFields = accessScope(cname).asInstanceOf[scala.collection.mutable.Map[Any, Any]]("private").asInstanceOf[scala.collection.mutable.Map[Any, Any]]("fields").asInstanceOf[scala.collection.mutable.HashSet[Any]]

          // Get class public fields
          val publicFields = accessScope(cname).asInstanceOf[scala.collection.mutable.Map[Any, Any]]("public").asInstanceOf[scala.collection.mutable.Map[Any, Any]]("fields").asInstanceOf[scala.collection.mutable.HashSet[Any]]

          // Get class protected fields
          val protectedFields = accessScope(cname).asInstanceOf[scala.collection.mutable.Map[Any, Any]]("protected").asInstanceOf[scala.collection.mutable.Map[Any, Any]]("fields").asInstanceOf[scala.collection.mutable.HashSet[Any]]

          // If field name is private or protected, it can not be accessed from Main
          if (privateFields.contains(fieldName)){
            println(s"Field name $fieldName can not be accessed.")
            val returnText = "!!!Error!!!"
            return returnText
          }
          else if (protectedFields.contains(fieldName)){
            println(s"Field name $fieldName can not be accessed.")
            val returnText = "!!!Error!!!"
            return returnText
          }
          // Else return the field name
          return temp(fieldName)


        // Object case
        case Object(name) => return name

        // Constructor
        case Constructor(instructions*) => return instructions

        // Create object
        case NewObject(name, className) =>

          if(interfaceBindingScope.contains(className)){
            println("ERROR: The object cannot be created from an interface")
            val returnText = "!!!Error!!!"
            return returnText
          }
          // Conditional statement to check if the object with this name already exists.
          if(tempObjectBindingScope.contains(name)){
            println("ERROR: The object with `" + name + "` already exists.")
            val returnText = "!!!Error!!!"
            return returnText
          }else{

            // Conditional statement to check if the class with this name exist.
            if(!classBindingScope.contains(className)){
              println("ERROR: The class with `" + className + "` does not exist.")
              val returnText = "!!!Error!!!"
              return returnText
            } else{
              val abstractDef: Boolean = classBindingScope(className).asInstanceOf[AnyMapType]("abstractDef").asInstanceOf[Boolean]
              if(abstractDef){
                println("ERROR: The abstract class cannot be used to create objects.")
                val returnText = "!!!Error!!!"
                return returnText
              }
              // Create a map for the object to store class name, fields and methods
              val tempNewObjectMap = scala.collection.mutable.Map[Any,Any]()
              tempNewObjectMap += ("className" -> className)

              // Extracting getting the class object from class binding Map
              val classObject = classBindingScope(className)

              // Extracting fields of the class
              val classFields = classObject.asInstanceOf[scala.collection.mutable.Map[Any,Any]]("fields")

              // Extracting methods of the class
              val classMethods = classObject.asInstanceOf[scala.collection.mutable.Map[Any,Any]]("method")

              // Create a copy of the fields and methods in the object
              tempNewObjectMap += ("fields" -> classFields)
              tempNewObjectMap += ("methods" -> classMethods)

              // Binding object name to newly created Map
              tempObjectBindingScope += (name -> tempNewObjectMap)
              // Second step pf object creation is to execute the operations present in the Constructor defined.
              // Retreiving the Constructor definition from the class binding scope map. And Evaluating the value to get the functions to execute.
              val constructorInstructions = classBindingScope(className).asInstanceOf[scala.collection.mutable.Map[Any, Any]]("constructor").asInstanceOf[SetDeclarations].eval(mapToUse)
              // Retrieving the fields map for the object created from the tempObjectMap created in the previous step. And the statements are executed.
              val bindigScopeFields = tempObjectBindingScope(name).asInstanceOf[scala.collection.mutable.Map[Any, Any]]("fields")
              constructorInstructions.asInstanceOf[scala.collection.immutable.ArraySeq[SetDeclarations]].foreach(instruction => {
                val a = instruction.eval(mapToUse = bindigScopeFields.asInstanceOf[scala.collection.mutable.Map[Any, Any]])
              })
            }
          }
          return true;



        // Abstract class definition.
        // It will accept a constructor, fields, methods and abstract methods
        // It should contain at lease one abstract method.
        case AbstractClassDef(name, operation*) =>
          if(classBindingScope.contains(name)){
            println("ERROR: The class with `" + name + "` already exists.")
            val returnText = "!!!Error!!!"
            return returnText
          } else {

            val flag = scala.collection.mutable.Set[Boolean]()

            val tempClassMap = scala.collection.mutable.Map[Any,Any]("fields" -> null, "constructor" -> null, "method" -> null, "abstractMethod" -> null )
            val tempClassFieldMap = scala.collection.mutable.Map[Any,Any]()
            val tempMethodMap = scala.collection.mutable.Map[Any,Any]()
            val tempAbstractMethodMap = scala.collection.mutable.Map[Any,Any]()

            val allFields: scala.collection.mutable.Map[Any, Any] = scala.collection.mutable.Map()

            val privateMembers: scala.collection.mutable.Map[Any, Any] = scala.collection.mutable.Map()
            val privateField:scala.collection.mutable.Set[Any] = scala.collection.mutable.Set()
            val privateAbstractMethod:scala.collection.mutable.Set[Any] = scala.collection.mutable.Set()
            val privateMethod:scala.collection.mutable.Set[Any] = scala.collection.mutable.Set()
            privateMembers += ("methods" -> privateMethod)
            privateMembers += ("fields" -> privateField)
            privateMembers += ("abstractMethod" -> privateAbstractMethod)

            val publicMembers: scala.collection.mutable.Map[Any, Any] = scala.collection.mutable.Map()
            val publicField:scala.collection.mutable.Set[Any] = scala.collection.mutable.Set()
            val publicAbstractMethod:scala.collection.mutable.Set[Any] = scala.collection.mutable.Set()
            val publicMethod:scala.collection.mutable.Set[Any] = scala.collection.mutable.Set()
            publicMembers += ("methods" -> publicMethod)
            publicMembers += ("fields" -> publicField)
            publicMembers += ("abstractMethod" -> publicAbstractMethod)

            val protectedMembers: scala.collection.mutable.Map[Any, Any] = scala.collection.mutable.Map()
            val protectedField:scala.collection.mutable.Set[Any] = scala.collection.mutable.Set()
            val protectedAbstractMethod:scala.collection.mutable.Set[Any] = scala.collection.mutable.Set()
            val protectedMethod:scala.collection.mutable.Set[Any] = scala.collection.mutable.Set()
            protectedMembers += ("methods" -> protectedMethod)
            protectedMembers += ("fields" -> protectedField)
            protectedMembers += ("abstractMethod" -> protectedAbstractMethod)

            val classAccess: scala.collection.mutable.Map[Any, Any] = scala.collection.mutable.Map()
            classAccess += ("private" -> privateMembers)
            classAccess += ("public" -> publicMembers)
            classAccess += ("protected" -> protectedMembers)

            accessScope += (name -> classAccess)

            operation.foreach(i => {
              val op = i.asInstanceOf[SetDeclarations]
              if (op.isInstanceOf[SetDeclarations.Constructor]){
                tempClassMap("constructor") = op
              }else{
                val opReturn = op.classEval(name).asInstanceOf[SetDeclarations]
                opReturn match {
                  case _: SetDeclarations.Field =>
                    val name = opReturn.eval(mapToUse)
                    tempClassFieldMap += (name -> null)
                  case _: SetDeclarations.AbstractMethod =>
                    val temp = opReturn.eval(mapToUse).asInstanceOf[mutable.Map[Any, Any]]
                    tempAbstractMethodMap.++=(temp)
                    flag += true
                  case _: SetDeclarations.Method =>
                    val temp = opReturn.eval(mapToUse).asInstanceOf[mutable.Map[Any, Any]]
                    tempMethodMap.++=(temp)
                  case _ =>
                    println("Operation is not permitted to be sent through the ClassDef")
                    val returnText = "!!!Error!!!"
                    accessScope -= name
                    return returnText
                }
              }
            })

            if(!flag.contains(true)){
              println("ERROR: The abstract class definition requires at least one Abstract method declaration.")
              val returnText = "!!!Error!!!"
              accessScope -= name
              return returnText
            }
            tempClassMap("fields") = tempClassFieldMap
            tempClassMap("abstarctMethod") = tempAbstractMethodMap
            tempClassMap("method") = tempMethodMap
            tempClassMap("abstractDef") = true
            classBindingScope += (name -> tempClassMap)
            return true
          }

        case AbstractMethod(name, listVar) =>

          val tempMethodMap = scala.collection.mutable.Map[Any,Any]("op" -> null, "variables" -> null)
          val tempClassMethodMap = scala.collection.mutable.Map[Any,Any]()
          val tempOpList = scala.collection.mutable.Set[Any]()
          val tempVarMap = scala.collection.mutable.Map[Any,Any]()
          listVar.foreach(i => {
            tempVarMap += (i -> null)

          })
          tempMethodMap("op") = tempOpList
          tempMethodMap += ("variables" -> tempVarMap)
          tempClassMethodMap += (name -> tempMethodMap)
          return tempClassMethodMap


        case InterfaceDecl(name, operation*) =>
          if(interfaceBindingScope.contains(name)){
            println("ERROR: The interface with `" + name + "` already exists.")
            val returnText = "!!!Error!!!"
            return returnText
          } else {

            val interfaceflag = scala.collection.mutable.Set[Boolean]()

            val tempInterfaceMap = scala.collection.mutable.Map[Any,Any]("fields" -> null, "constructor" -> null, "abstractMethod" -> null )
            val tempInterfaceFieldMap: AnyMapType = scala.collection.mutable.Map[Any,Any]()
            val tempInterfaceMethodMap: AnyMapType = scala.collection.mutable.Map[Any,Any]()
            val tempInterfaceAbstractMethodMap: AnyMapType = scala.collection.mutable.Map[Any,Any]()

            val allFields: AnyMapType = scala.collection.mutable.Map()


            val publicMembers: AnyMapType = scala.collection.mutable.Map()
            val protectedMembers: AnyMapType = scala.collection.mutable.Map()
            val privateMembers: AnyMapType = scala.collection.mutable.Map()

            val interfaceAccess: AnyMapType = scala.collection.mutable.Map()

            val publicField: AnySetType = scala.collection.mutable.Set()
            publicMembers += ("fields" -> publicField)

            val publicAbstractMethod:AnySetType = scala.collection.mutable.Set()
            publicMembers += ("abstractMethod" -> publicAbstractMethod)

            val publicMethod:AnySetType = scala.collection.mutable.Set()
            publicMembers += ("methods" -> publicMethod)

            val protectedField:AnySetType = scala.collection.mutable.Set()
            protectedMembers += ("fields" -> protectedField)

            val protectedAbstractMethod:AnySetType = scala.collection.mutable.Set()
            protectedMembers += ("abstractMethod" -> protectedAbstractMethod)

            val protectedMethod:AnySetType = scala.collection.mutable.Set()
            protectedMembers += ("methods" -> protectedMethod)

            val privateField:AnySetType = scala.collection.mutable.Set()
            val privateAbstractMethod: AnySetType = scala.collection.mutable.Set()
            val privateMethod:AnySetType = scala.collection.mutable.Set()
            privateMembers += ("methods" -> privateMethod)
            privateMembers += ("fields" -> privateField)
            privateMembers += ("abstractMethod" -> privateAbstractMethod)

            interfaceAccess += ("private" -> privateMembers)
            interfaceAccess += ("public" -> publicMembers)
            interfaceAccess += ("protected" -> protectedMembers)

            accessScope += (name -> interfaceAccess)

            operation.foreach(i => {
              val op = i.asInstanceOf[SetDeclarations]
              if (op.isInstanceOf[SetDeclarations.Constructor]){
                tempInterfaceMap("constructor") = op
              }else{
                val opReturn = op.classEval(name).asInstanceOf[SetDeclarations]
                opReturn match {
                  case _: SetDeclarations.Field =>
                    val name = opReturn.eval(mapToUse)
                    tempInterfaceFieldMap += (name -> null)
                  case _: SetDeclarations.AbstractMethod =>
                    val temp = opReturn.eval(mapToUse).asInstanceOf[mutable.Map[Any, Any]]
                    tempInterfaceAbstractMethodMap.++=(temp)
                    interfaceflag += true
                  case _: SetDeclarations.Method =>
                    println("Interface Just accepts Abstract Method Declaration")
                    val returnText = "!!!Error!!!"
                    accessScope -= name
                    return returnText
                  case _ =>
                    println("Operation is not permitted to be sent through the Interface")
                    val returnText = "!!!Error!!!"
                    accessScope -= name
                    return returnText
                }
              }
            })

            if(!interfaceflag.contains(true)){
              println("ERROR: The Interface definition requires at least one Abstract method declaration.")
              val returnText = "!!!Error!!!"
              accessScope -= name
              return returnText
            }
            tempInterfaceMap("fields") = tempInterfaceFieldMap
            tempInterfaceMap("abstarctMethod") = tempInterfaceAbstractMethodMap
            tempInterfaceMap("method") = tempInterfaceMethodMap
            interfaceBindingScope += (name -> tempInterfaceMap)
            return true
          }


        // Conditional statement definition.
        // It will accept a conditional statement and a then clause and a else clause
        // If condition is true it will run then clause else it will run the else clause.
        case IfConditionalStatement(condition, thenClause, elseClause) =>
          val conditionReturn = condition.asInstanceOf[SetDeclarations].eval()
          conditionReturn match {
            case true => thenClause.asInstanceOf[SetDeclarations].eval()
            case _ => elseClause.asInstanceOf[SetDeclarations].eval()
          }

        // Then clause definition.
        // To be run if the condition provided returns a true.
        // It will loop through all the operation and execute if the exception is not thrown.
        case Then(operations*) =>
          operations.foreach(i => {
            val op = i.asInstanceOf[SetDeclarations];
            // Checking if the op is of type ThrowException.
            // It will evaluate the operation then it will set the isExceptionCaught as true.
            if(op.isInstanceOf[SetDeclarations.ThrowException]){
              op.eval()
              isExceptionCaught.set(true)
            }
            // It will evaluate the expression if the exeption is not thrown in previous statments.
            else if (!isExceptionCaught.get()) {
              val z = op.eval();
              if (i == operations.last) {
                // Returning the result of the last operation passed to the function.
                return z
              }
            }
          })

        // Else clause definition.
        // To be run if the condition provided returns a false.
        // It will loop through all the operation and execute if the exception is not thrown.
        case Else(operations*) =>
          operations.foreach(i => {
            val op = i.asInstanceOf[SetDeclarations];
            // Checking if the op is of type ThrowException.
            // It will evaluate the operation then it will set the isExceptionCaught as true.
            if(op.isInstanceOf[SetDeclarations.ThrowException]){
              op.eval()
              isExceptionCaught.set(true)
            }
            // It will evaluate the expression if the exception is not thrown in previous statements.
            else if (!isExceptionCaught.get()) {
              val z = op.eval();
              if (i == operations.last) {
                // Returning the result of the last operation passed to the function.
                return z
              }
            }
          })

        //Defining the Exception CLass with taking fields as input.
        case ExceptionClassDef(name, operation) =>
          val tempMap = scala.collection.mutable.Map[Any,Any]()
          val op = operation.asInstanceOf[SetDeclarations]
          if(op.isInstanceOf[SetDeclarations.Field]){
            val fieldName = op.eval()
            tempMap += (fieldName -> null)
          }
          // It creates a temp map with assigning each field null value and adds it to exceptionBindingScope.
          exceptionBindingScope += (name -> tempMap)

        // ThrowException definition.
        // It updates the null value with the provided value of reason.
        case ThrowException(name, operation) =>
          val a = operation.eval(mapToUse = exceptionBindingScope(name).asInstanceOf[AnyMapType])
          isExceptionCaught.set(true)
          return a



        // CatchExpression definition
        // TRY block equivalent
        // It will evaluate expression one by one and look for any exception that are thrown.
        case CatchException(name, operations*) =>
          // Keeping track if exception is encountered in this block of code
          val trackIfExceptionFlag = new AtomicBoolean(false)
          // Looping though operations.
          operations.foreach(i => {
            val op = i.asInstanceOf[SetDeclarations]
            // Checking if op is of type ThrowException
            if (op.isInstanceOf[SetDeclarations.ThrowException]){
              op.eval()
              trackIfExceptionFlag.set(true)
            }
            // Check if the exception is not caught and op is not of type Catch & ThrowException
            else if(!isExceptionCaught.get()){
              if(!op.isInstanceOf[SetDeclarations.Catch] && !op.isInstanceOf[SetDeclarations.ThrowException]){
                val z = op.eval()
              }
            }
            // Checking if the exception is caught and Catch is found.
            else if (isExceptionCaught.get() && op.isInstanceOf[SetDeclarations.Catch]){
              Assign(name, Value(exceptionBindingScope(name).asInstanceOf[AnyMapType]("Reason"))).eval()
              val z = op.eval()
              trackIfExceptionFlag.set(true)
            }
          })
          // Return the exception reason if caught else return success message.
          if(trackIfExceptionFlag.get()){
            val returnValue = "EXCEPTION RAISED: " + exceptionBindingScope(name).asInstanceOf[AnyMapType]("Reason")
            return returnValue
          } else {
            return "Success"
          }

        //Catch definition
        // It will loop through all the expression and return custom message.
        case Catch(exception, operations*) =>
          val exceptionName = exception.eval()
          operations.foreach(i => {
            val op = i.asInstanceOf[SetDeclarations]
            val z = op.eval()
          })
          isExceptionCaught.set(false);
          val returnText = "EXCEPTION: " + exceptionName + "| Catch Block Successfull."
          return exceptionName

        // This will be used to perform a conditional check and return true or false
        case ConditionalCheck(setName, toCheckValue ) =>
          if (setName.eval(mapToUse) != "!!!Error!!!") {
            val checkResult = setName.eval(mapToUse).asInstanceOf[scala.collection.mutable.Set[Any]].contains(toCheckValue.eval(mapToUse))
            return checkResult
          }
          else {
            return false
          }

        case _ =>
          println("ERROR: Unknown Function Executed")
          return "!!ERROR!!"
      }

      // The function will check if the parameters are sets and return a boolean.
      def CheckSet(set1: SetDeclarations, set2: SetDeclarations): Boolean =
        if (set1.eval(mapToUse).isInstanceOf[scala.collection.mutable.Set[Any]] && set2.eval(mapToUse).isInstanceOf[scala.collection.mutable.Set[Any]]) {
          true
        } else {
          false
        }

    // scopeEval is used to create a custom, unique key for each variable inside a scope.
    // THis will shadow the scope variable from other scope from grobal variables.
    def scopeEval(scopeName: String, mapToUse: AnyMapType = bindingScope): Any =
      this match {
        case Value(v) =>
          val vl = Value(v)
          vl

        // Return the Variable with unique key
        case Variable(stringName) =>
          val sName = scopeName + "-SCOPEVARIABLE-" + stringName
          val a = Variable(sName)
          a

        // Return the Assign function with unique key
        case Assign(stringName, operation2*) =>
          val sName = scopeName + "-SCOPEVARIABLE-" + stringName
          val result = Assign(sName, operation2 *)
          if (mapToUse.contains(scopeName)) {
            val temp = mapToUse(scopeName).asInstanceOf[scala.collection.mutable.Set[Any]]
            temp.add(sName)
            mapToUse(scopeName) = temp
          }
          else {
            val temp = scala.collection.mutable.Set[Any](sName)
            addBindingScope(scopeName, temp, mapToUse = mapToUse )
          }
          result

        // Return the Insert function with unique parameters
        case Insert(op1, op2) =>
          val o1 = op1.scopeEval(scopeName).asInstanceOf[SetDeclarations]
          val o2 = op2.scopeEval(scopeName).asInstanceOf[SetDeclarations]
          val result = Insert(o1, o2)
          result

        // Return the Check function with unique parameters
        case Check(setValue, operation) =>
          val sV = setValue.scopeEval(scopeName).asInstanceOf[SetDeclarations]
          val op = operation.scopeEval(scopeName).asInstanceOf[SetDeclarations]
          val result = Check(sV, op)
          result

        // Return the Delete function with unique parameters
        case Delete(op1, op2) =>
          val o1 = op1.scopeEval(scopeName).asInstanceOf[SetDeclarations]
          val o2 = op2.scopeEval(scopeName).asInstanceOf[SetDeclarations]
          val result = Delete(o1, o2)
          result

        // Return the Union function with unique parameters
        case Union(operation1, operation2) =>
          val op1 = operation1.scopeEval(scopeName).asInstanceOf[SetDeclarations]
          val op2 = operation2.scopeEval(scopeName).asInstanceOf[SetDeclarations]
          val result = Union(op1, op2)
          result

        // Return the Intersection function with unique parameters
        case Intersection(operation1, operation2) =>
          val o1 = operation1.scopeEval(scopeName).asInstanceOf[SetDeclarations]
          val o2 = operation2.scopeEval(scopeName).asInstanceOf[SetDeclarations]
          val result = Intersection(o1, o2)
          result

        // Return the Difference function with unique parameters
        case Difference(operation1, operation2) =>
          val operation_1 = operation1.scopeEval(scopeName).asInstanceOf[SetDeclarations]
          val operation_2 = operation2.scopeEval(scopeName).asInstanceOf[SetDeclarations]
          val result = Difference(operation_1, operation_2)
          result

        // Return the SymmetricDifference function with unique parameters
        case SymmetricDifference(op1, op2) =>
          val o1 = op1.scopeEval(scopeName).asInstanceOf[SetDeclarations]
          val o2 = op2.scopeEval(scopeName).asInstanceOf[SetDeclarations]
          val result = SymmetricDifference(o1, o2)
          result

        // Return the CartesianProduct function with unique parameters
        case CartesianProduct(operation1, operation2) =>
          val set1 = operation1.scopeEval(scopeName).asInstanceOf[SetDeclarations]
          val set2 = operation2.scopeEval(scopeName).asInstanceOf[SetDeclarations]
          val productResult = CartesianProduct(set1, set2)
          productResult

        // Return the Macro function with unique parameters
        case Macro(string, operation) =>
          val sName = scopeName + "-SCOPEVARIABLE-" + string
          val op = operation.scopeEval(scopeName).asInstanceOf[SetDeclarations]
          val mac = Macro(sName, op)
          mac

        case CatchException(name, operations*) =>
          val c = CatchException(name, operations*)
          c

        // Return the ImpMacro function with unique parameters
        case ImpMacro(operation) =>
          val mainOp = operation.scopeEval(scopeName).asInstanceOf[SetDeclarations]
          mainOp

        case _ =>
          println("ERROR: Unknown Function Executed")
          "!!ERROR!!"
      }

    // Inheritance
    def Extends(parentClass: String) : Any =
      // First create the child class with fields, constructor and methods
      if(!classBindingScope.contains(parentClass)){
        println("ERROR: Parent CLass not found.  ")
        val returnText = "!!!Error!!!"
        return returnText
      }

      this.eval()

      // Check for multiple inheritance
      if (classBindingScope(this.asInstanceOf[ClassDef].name).asInstanceOf[AnyMapType]("inheritance").asInstanceOf[Boolean]){
        println("ERROR: Multiple Inheritance is not allowed. ")
        val returnText = "!!!Error!!!"
        return returnText
      }
      // Set flag for inheritance to True
      classBindingScope(this.asInstanceOf[ClassDef].name).asInstanceOf[AnyMapType]("inheritance") = true
      // Get all Public fields of Parent class and add them to Child class
      // Access Map helps extract public fields and methods. These names are searched in bindingScopeClass and added to child class
      accessScope(parentClass).asInstanceOf[AnyMapType]("public").
        asInstanceOf[AnyMapType]("fields").
        asInstanceOf[scala.collection.mutable.Set[Any]].foreach {
        item => {
          classBindingScope(this.asInstanceOf[ClassDef].name).
            asInstanceOf[AnyMapType]("fields").
            asInstanceOf[AnyMapType] += (item -> null)
        }
      }

      accessScope(parentClass).asInstanceOf[AnyMapType]("protected").
        asInstanceOf[AnyMapType]("fields").
        asInstanceOf[scala.collection.mutable.Set[Any]].foreach {
        item => {
          classBindingScope(this.asInstanceOf[ClassDef].name).
            asInstanceOf[AnyMapType]("fields").
            asInstanceOf[AnyMapType] += (item -> null)
        }
      }

      //Creating a method map to keep track of all overridden methods
      val methodMap: scala.collection.mutable.Map[Any, Any] = scala.collection.mutable.Map()
      virtualDispatchTable += (this.asInstanceOf[ClassDef].name -> methodMap)

      val isAbstract: Boolean = classBindingScope(parentClass).asInstanceOf[AnyMapType]("abstractDef").asInstanceOf[Boolean]
      val abstractClasses = accessScope(parentClass).asInstanceOf[AnyMapType]("public").asInstanceOf[AnyMapType]("abstractMethod").asInstanceOf[AnySetType]
      abstractClasses ++= accessScope(parentClass).asInstanceOf[AnyMapType]("protected").asInstanceOf[AnyMapType]("abstractMethod").asInstanceOf[AnySetType]

      abstractClasses.foreach{
        method => {
          if(classBindingScope(this.asInstanceOf[ClassDef].name).
            asInstanceOf[scala.collection.mutable.Map[Any, Any]]("method").
            asInstanceOf[scala.collection.mutable.Map[Any, Any]].contains(method)){

            virtualDispatchTable(this.asInstanceOf[ClassDef].name)
              .asInstanceOf[scala.collection.mutable.Map[Any, Any]]
              += (method -> classBindingScope(this.asInstanceOf[ClassDef].name)
              .asInstanceOf[scala.collection.mutable.Map[Any, Any]]("method")
              .asInstanceOf[scala.collection.mutable.Map[Any, Any]](method))
          } else {
            println("Please provide definitions for all the abstract class definitions")
            val returnText = "!!!Error!!!"
            classBindingScope -= this.asInstanceOf[ClassDef].name
            return returnText
          }
        }

      }

      accessScope(parentClass).asInstanceOf[AnyMapType]("public").
        asInstanceOf[AnyMapType]("methods").
        asInstanceOf[scala.collection.mutable.Set[Any]].foreach {
        method => {
          // Implementing Virtual Dispatch Table to keep track of overridden methods
          if(classBindingScope(this.asInstanceOf[ClassDef].name).
            asInstanceOf[scala.collection.mutable.Map[Any, Any]]("method").
            asInstanceOf[scala.collection.mutable.Map[Any, Any]].contains(method)){

            virtualDispatchTable(this.asInstanceOf[ClassDef].name)
              .asInstanceOf[scala.collection.mutable.Map[Any, Any]]
              += (method -> classBindingScope(this.asInstanceOf[ClassDef].name)
              .asInstanceOf[scala.collection.mutable.Map[Any, Any]]("method")
              .asInstanceOf[scala.collection.mutable.Map[Any, Any]](method))
          } else {
            val methodDefinition = classBindingScope(parentClass).asInstanceOf[AnyMapType]("method").asInstanceOf[AnyMapType](method)
            classBindingScope(this.asInstanceOf[ClassDef].name).
              asInstanceOf[AnyMapType]("method").
              asInstanceOf[AnyMapType] += (method -> methodDefinition)
          }
        }
      }

      accessScope(parentClass).asInstanceOf[AnyMapType]("protected").
        asInstanceOf[AnyMapType]("methods").
        asInstanceOf[scala.collection.mutable.Set[Any]].foreach {
        method => {
          // Implementing Virtual Dispatch Table to keep track of overridden methods
          if(classBindingScope(this.asInstanceOf[ClassDef].name).
            asInstanceOf[scala.collection.mutable.Map[Any, Any]]("method").
            asInstanceOf[scala.collection.mutable.Map[Any, Any]].contains(method)){

            virtualDispatchTable(this.asInstanceOf[ClassDef].name)
              .asInstanceOf[scala.collection.mutable.Map[Any, Any]]
              += (method -> classBindingScope(this.asInstanceOf[ClassDef].name)
              .asInstanceOf[scala.collection.mutable.Map[Any, Any]]("method")
              .asInstanceOf[scala.collection.mutable.Map[Any, Any]](method))

          } else {
            val methodDefinition = classBindingScope(parentClass).asInstanceOf[AnyMapType]("method").asInstanceOf[AnyMapType](method)
            classBindingScope(this.asInstanceOf[ClassDef].name).
              asInstanceOf[AnyMapType]("method").
              asInstanceOf[AnyMapType] += (method -> methodDefinition)
          }
        }
      }
      return true;


    // Implement from the interface to the new class created.
    def Implements (interface: String) : Any =
      println(interface)
      // First create the child class with fields, constructor and methods
      if(!interfaceBindingScope.contains(interface)){
        println("ERROR: Interface not found.  ")
        val returnText = "!!!Error!!!"
        return returnText
      }

      this.eval()
      val implementedClassName = this.asInstanceOf[ClassDef].name

      // Check for multiple inheritance
      if (classBindingScope(this.asInstanceOf[ClassDef].name).asInstanceOf[AnyMapType]("inheritance").asInstanceOf[Boolean]){
        println("ERROR: Multiple Inheritance is not allowed. ")
        val returnText = "!!!Error!!!"
        return returnText
      }
      // Set flag for inheritance to True
      classBindingScope(implementedClassName).asInstanceOf[AnyMapType]("inheritance") = true
      // Get all Public fields of Parent class and add them to Child class

      // Access Map helps extract protected fields and methods. These names are searched in bindingScopeClass and added to child class
      accessScope(interface).asInstanceOf[AnyMapType]("protected").
        asInstanceOf[AnyMapType]("fields").
        asInstanceOf[scala.collection.mutable.Set[Any]].foreach {
        item => {
          classBindingScope(implementedClassName).asInstanceOf[AnyMapType]("fields").asInstanceOf[AnyMapType] += (item -> null)
        }
      }

      // Access Map helps extract public fields and methods. These names are searched in bindingScopeClass and added to child class
      accessScope(interface).asInstanceOf[AnyMapType]("public").
        asInstanceOf[AnyMapType]("fields").
        asInstanceOf[scala.collection.mutable.Set[Any]].foreach {
        item => {
          classBindingScope(implementedClassName).asInstanceOf[AnyMapType]("fields").asInstanceOf[AnyMapType] += (item -> null)
        }
      }

      //Creating a method map to keep track of all overridden methods
      val implemetedMethodMap: AnyMapType = scala.collection.mutable.Map()
      virtualDispatchTable += (implementedClassName -> implemetedMethodMap)

      val abstractClasses = accessScope(interface)
        .asInstanceOf[AnyMapType]("public")
        .asInstanceOf[AnyMapType]("abstractMethod")
        .asInstanceOf[AnySetType]
      abstractClasses ++= accessScope(interface)
        .asInstanceOf[AnyMapType]("protected")
        .asInstanceOf[AnyMapType]("abstractMethod")
        .asInstanceOf[AnySetType]

      abstractClasses.foreach{
        method => {
          if(classBindingScope(this.asInstanceOf[ClassDef].name).
            asInstanceOf[scala.collection.mutable.Map[Any, Any]]("method").
            asInstanceOf[scala.collection.mutable.Map[Any, Any]].contains(method)){

            virtualDispatchTable(this.asInstanceOf[ClassDef].name)
              .asInstanceOf[scala.collection.mutable.Map[Any, Any]]
              += (method -> classBindingScope(this.asInstanceOf[ClassDef].name)
              .asInstanceOf[scala.collection.mutable.Map[Any, Any]]("method")
              .asInstanceOf[scala.collection.mutable.Map[Any, Any]](method))
          } else {
            println("Please provide definitions for all the abstract class definitions")
            val returnText = "!!!Error!!!"
            classBindingScope -= implementedClassName
            return returnText
          }
        }

      }
      return true;



    def classEval(name: String, mapToUse: scala.collection.mutable.Map[Any, Any] = bindingScope): Any =
      this match {
        case Public(operation) =>
          if(operation.isInstanceOf[Field]) {
            val fieldName = operation.classEval(name)
            accessScope(name).asInstanceOf[scala.collection.mutable.Map[Any, Any]]("public").asInstanceOf[scala.collection.mutable.Map[Any, Any]]("fields").
              asInstanceOf[scala.collection.mutable.Set[Any]] += fieldName
          }
          else if (operation.isInstanceOf[Method]){
            val MethodName = operation.classEval(name)
            accessScope(name).asInstanceOf[scala.collection.mutable.Map[Any, Any]]("public").asInstanceOf[scala.collection.mutable.Map[Any, Any]]("methods").
              asInstanceOf[scala.collection.mutable.Set[Any]] += MethodName
          }
          else {
            val abstractMethodName = operation.classEval(name)
            accessScope(name).asInstanceOf[scala.collection.mutable.Map[Any, Any]]("public").asInstanceOf[scala.collection.mutable.Map[Any, Any]]("abstractMethod").
              asInstanceOf[scala.collection.mutable.Set[Any]] += abstractMethodName
          }
          operation

        case Field(name) => name
        case Method(name, list, op*) => name
        case AbstractMethod(name, list) => name

        case Protected(operation) =>
          if(operation.isInstanceOf[Field]) {

            val fieldName = operation.classEval(name)
            accessScope(name).asInstanceOf[scala.collection.mutable.Map[Any, Any]]("protected").asInstanceOf[scala.collection.mutable.Map[Any, Any]]("fields").
              asInstanceOf[scala.collection.mutable.Set[Any]] += fieldName
          }
          else if (operation.isInstanceOf[Method]) {
            val methodName = operation.classEval(name)
            accessScope(name).asInstanceOf[scala.collection.mutable.Map[Any, Any]]("protected").asInstanceOf[scala.collection.mutable.Map[Any, Any]]("methods").
              asInstanceOf[scala.collection.mutable.Set[Any]] += methodName
          }
          else {
            val abstractMethodName = operation.classEval(name)
            accessScope(name).asInstanceOf[scala.collection.mutable.Map[Any, Any]]("protected").asInstanceOf[scala.collection.mutable.Map[Any, Any]]("abstractMethod").
              asInstanceOf[scala.collection.mutable.Set[Any]] += abstractMethodName
          }

          operation

        case Private(operation) =>
          if(operation.isInstanceOf[Field]) {
            val fieldName = operation.classEval(name)
            accessScope(name).asInstanceOf[scala.collection.mutable.Map[Any, Any]]("private").asInstanceOf[scala.collection.mutable.Map[Any, Any]]("fields").
              asInstanceOf[scala.collection.mutable.Set[Any]] += fieldName
          }
          else if (operation.isInstanceOf[Method]){
            val methodName = operation.classEval(name)
            accessScope(name).asInstanceOf[scala.collection.mutable.Map[Any, Any]]("private").asInstanceOf[scala.collection.mutable.Map[Any, Any]]("methods").
              asInstanceOf[scala.collection.mutable.Set[Any]] += methodName
          }
          else {
            val abstractMethodName = operation.classEval(name)
            accessScope(name).asInstanceOf[scala.collection.mutable.Map[Any, Any]]("private").asInstanceOf[scala.collection.mutable.Map[Any, Any]]("abstractMethod").
              asInstanceOf[scala.collection.mutable.Set[Any]] += abstractMethodName
          }
          operation

        case _ =>
          println("ERROR: Unknown Function Executed")
          "!!ERROR!!"
      }



  // Main Function to run the file.
  @main def runSetDSL(): Unit =
    import SetDeclarations.*
    println("\n**********************")
    println("SetLandDSL: Domain Specific Language in Scala to perform set operations.")
    println("**********************\n")