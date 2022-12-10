package.hw1

import.hw1.SetLangDSL.SetDeclarations.*

import scala.collection.mutable.ListBuffer


object Practice:

  @main def runPractice(): Unit = {


    /***
     * Homework 4
     *
     * Practice Code
     *
     */

    // Sample Code for conditional statement
    Assign("conditionSet1", Value(1), Value(2), Value(3)).eval()

    IfConditionalStatement(ConditionalCheck(Variable("conditionSet1"), Value(2)),
      Then(Insert(Variable("conditionSet1"), Value(6))),
      Else(Insert(Variable("conditionSet1"), Value(5)))
    ).eval()
    println("Conditional Statement: " +Variable("conditionSet1").eval())

    // Sample Code for exception handling

    ExceptionClassDef("FirstException", Field("Reason")).eval()

    val exceptionResult = Scope("hello",CatchException("FirstException",
      Assign("var1", Value(11), Value(200)),
      IfConditionalStatement(ConditionalCheck(Variable("var1"), Value(12)),
        Then(Insert(Variable("var1"), Value(2))),
        Else(ThrowException("FirstException",Assign("Reason",Value("This is a reason"))))
      ),
      Insert(Variable("var1"), Value(12)),
      Catch(Variable("FirstException"),Insert(Variable("var1"), Value(12222)))
    )).eval()

    println("Final Output for exception sample code.: " + exceptionResult)


    /***
     *
     * Homework 3
     *
     * Practice Code
     *
     */

    // Abstract Class definition
    val acDef = AbstractClassDef("class1", Public(Field("field1")),
      Constructor(Assign("field1", Value(1))),
      Public(Method("method1", ListBuffer("p1", "p2"), Union(Variable("p1"), Variable("p2")))),
      Public(AbstractMethod("method1", ListBuffer("p1", "p2")))).eval()
    println("Abstract Class Definition Returns: " + acDef)

    // Abstract Class definition error when no abstract class is declared.
    val acDef1 = AbstractClassDef("class2", Public(Field("field1")), Constructor(Assign("field1", Value(1))), Public(Method("method1", ListBuffer("p1", "p2"), Union(Variable("p1"), Variable("p2"))))).eval()
    println("Abstract Class Definition Returns Error with Abstract Method: " + acDef1)


    // Class Extends Abstract Class  returns error when all abstract methods are not overwritten.
    AbstractClassDef("abstractClass", Public(Field("field1")), Constructor(Assign("field1", Value(1))), Public(Method("method1", ListBuffer("p1", "p2"), Union(Variable("p1"), Variable("p2")))), Public(AbstractMethod("abstractMethod", ListBuffer("p1", "p2")))).eval()
    val extendsErr = ClassDef("extendedClass", Public(Field("field1")), Constructor(Assign("field1", Value(2))), Public(Method("newMethod",ListBuffer("a", "b"), Union(Variable("a"), Variable("b"))))) Extends "abstractClass"
    println("Class Inheritance from Abstract Class Returns Error when all AbMethods are not overwritten: "+ extendsErr)

    // New Object return true when created for inherited Class Def
    AbstractClassDef("abstractClass3", Public(Field("field1")), Constructor(Assign("field1", Value(1))), Public(Method("method1", ListBuffer("p1", "p2"), Union(Variable("p1"), Variable("p2")))), Public(AbstractMethod("abstractMethod", ListBuffer("p1", "p2")))).eval()
    ClassDef("extendedClass2", Public(Field("field1")), Constructor(Assign("field1", Value(2))), Public(Method("abstractMethod",ListBuffer("a", "b"), Union(Variable("a"), Variable("b"))))) Extends "abstractClass3"
    val newAbClassObj = NewObject("object2", "extendedClass2").eval()
    println("New Object return true when created for inherited Class Def:  " + newAbClassObj)


    // Abstract Class Definition, inheritance, object creation and method invocation of overwritten method
    // Invoked method on inherited class should refer to Virtual Dispatch Table for Lookup for Overwritten Function
    AbstractClassDef("abstractClass3",
      Public(Field("field1")),
      Constructor(Assign("field1", Value(1))),
      Public(Method("method1", ListBuffer("p1", "p2"), Union(Variable("p1"), Variable("p2")))),
      Public(AbstractMethod("abstractMethod", ListBuffer("p1", "p2")))).eval()

    ClassDef("extendedClass2", Public(Field("field1")),
      Constructor(Assign("field1", Value(2))),
      Public(Method("abstractMethod",ListBuffer("a", "b"), Union(Variable("a"), Variable("b"))))) Extends "abstractClass3"

    NewObject("object3", "extendedClass2").eval()

    val resultInkMethod = InvokeMethod("object3", "abstractMethod", Assign("a", Value(2), Value(3)), Assign("b", Value(4), Value(5))).eval()

    println("Invoked method on inherited class should refer to Virtual Dispatch Table for Lookup for Overwritten Function: "+ resultInkMethod )


    // New Object gives error when created for Abstract Class
    AbstractClassDef("abstractClass2", Public(Field("field1")), Constructor(Assign("field1", Value(1))), Public(Method("method1", ListBuffer("p1", "p2"), Union(Variable("p1"), Variable("p2")))), Public(AbstractMethod("abstractMethod", ListBuffer("p1", "p2")))).eval()
    val newObjErr = NewObject("object1", "abstractClass2").eval()
    println("New Object gives error when created for Abstract Class:  " + newObjErr )

    // Interface Declaration, inheritance, object creation and method invocation of overwritten method
    // Invoked method on inherited class should refer to Virtual Dispatch Table for Lookup for Overwritten Function
    InterfaceDecl("parentInterface3",
      Public(Field("field1")),
      Constructor(Assign("field1", Value(1))),
      Public(AbstractMethod("abstractMethod", ListBuffer("p1", "p2")))).eval()

    ClassDef("implementedClass3",
      Public(Field("field1")), Constructor(Assign("field1", Value(2))),
      Public(Method("abstractMethod",ListBuffer("a", "b"), Union(Variable("a"), Variable("b"))))) Implements "parentInterface3"

    NewObject("implementedObject3", "implementedClass3").eval()

    val interfaceResult = InvokeMethod("implementedObject3", "abstractMethod", Assign("a", Value(2), Value(3)), Assign("b", Value(4), Value(5))).eval()

    print("Result for Interface Declaration:  " + interfaceResult)



    /***
     * Homework -2
     */

    // Defining  a class creating a object for it.
    ClassDef("class1", Public(Field("field1")), Constructor(Assign("field1", Value(1))), Public(Method("method1", ListBuffer("p1", "p2"), Union(Variable("p1"), Variable("p2"))))).eval()
    println("Fetching the Field Value before object is created: " + RetrieveField("object1", "field1").eval() )

    // Once object is created the constructor is executed with a object map created.
    NewObject("object1", "class1").eval()
    println("Fetching the Field Value after object is created: " + RetrieveField("object1", "field1").eval() )


    // Creating a class, object and invoking a class for the value.
    ClassDef("class2", Public(Field("field2")), Constructor(Assign("field2", Value(1))), Public(Method("method2", ListBuffer("p1", "p2"), Union(Variable("p3"), Variable("p4"))))).eval()
    NewObject("object2", "class2").eval()
    // Parameters are passed by giving in the Assign functions.
    val result = InvokeMethod("object2", "method2", Assign("p3", Value(1), Value(2), Value(3)), Assign("p4", Value(1), Value(4))).eval()
    println("The Result of the invoke method: "+ result)


    //Creating a class by inheritance and initializing a object for the same.
    ClassDef("parentClass2", Protected(Field("parentField2")), Constructor(Assign("parentField2", Value(1)))).eval()
    ClassDef("childClass2", Public(Field("childField2")), Constructor(Assign("childField2", Value(2)))) Extends "parentClass2"
    NewObject("childObject2", "childClass2").eval()
    println(RetrieveField("childObject2", "parentField2").eval())




    /***
     * Basic Commands Check.
     */
    println("\n Basic Commands")
    // Value -> Returns 100
    val val1 = Value(100).eval()
    println("Value Output: " + val1)

    // Assign Variable to Int & Set | Sending multiple values will make a set.
    Assign("var1", Value(100)).eval()
    val varSet = Assign("varSet", Value(11), Value(102), Value(103)).eval()
    println("Assign Output: " + varSet)

    // Variable retrive from the map
    Variable("var1").eval()
    val retVarSet = Variable("varSet").eval()
    println("Variable Output: " + retVarSet)

    // Error returned when a non existing variable is accessed.
    Variable("varNotPresent").eval()

    // Insert a value to a set
    Assign("varSetTwo", Value(10), Value(11)).eval()
    Insert(Variable("varSetTwo"), Value(12)).eval()
    println("Insert Output: " + Variable("varSetTwo").eval())

    // Delete a value from a set
    println("Delete Output: " + Delete(Variable("varSetTwo"), Value(10)).eval())

    // Check if a value is in a set
    println("Check Output: " + Check(Variable("varSetTwo"), Value(11)).eval())

    //Set Operations
    println("Set1 and Set2: " + Variable("varSet").eval() + Variable("varSetTwo").eval())
    println("Union Output: " + Union(Variable("varSet"), Variable("varSetTwo")).eval())
    println("Intersection Output: " + Intersection(Variable("varSet"), Variable("varSetTwo")).eval())
    println("Difference Output: " + Difference(Variable("varSet"), Variable("varSetTwo")).eval())
    println("Symmetric Difference Output: " + SymmetricDifference(Variable("varSet"), Variable("varSetTwo")).eval())
    println("Cartesian Product Output: " + CartesianProduct(Variable("varSet"), Variable("varSetTwo")).eval())

    // Macro Operations
    Macro("macro", Union(Variable("varSet"), Variable("varSetTwo"))).eval()
    val macroResult = ImpMacro(Variable("macro")).eval()
    println("Macro Implementation Result: " + macroResult)

    // Scope
    val testScope = Scope("scope1", Assign("innerVariable2", Value(100), Value(3)), Assign("innerVariable1", Value(3), Value(4)), CartesianProduct(Variable("innerVariable1"), Variable("innerVariable2"))).eval()
    println("Scope Output: " + testScope)

    // Anonymous Scope
    val anScope = Scope("", Assign("innerVariable2", Value(100), Value(3)), Assign("innerVariable1", Value(3), Value(4)),
      CartesianProduct(Variable("innerVariable1"), Variable("innerVariable2"))).eval()
    println("Scope Output: " + anScope)


    // Get Scope Variable
    println("Get Scope Output: " + GetScopeVariable("scope1", "innerVariable1").eval())


  }
