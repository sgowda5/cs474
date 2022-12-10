Interface Definition
Assume a simple interface in Java

// defining an interface in java

interface someInterface {
  public int a = 10
  public abstract method1();
}
the above interface definition in myDSL would look like:

InterfaceDef("someInterface",
  Field("a", AccessType.PUBLIC, Value(10)),
  Method("method1", AccessType.PUBLIC, ImplementationType.ABSTRACT, List(), Value(1))
).eval()
InterfaceDef only accepts abstract implementation of methods

Does not accept private fields

Returns the definition of interface when no arguments are provided

Interface Implementation
assume a class that implements the above created interface

public class someClassName implements someInterface{
    public int x = 47;

    //constructor
    void someClassName(){
        x = 2;
    }

    @Override
    public void method1(){
        int z = 33;
    }     
}  
in myDSL that would look like

ClassDef("someClassName1",
    Field("x", AccessType.PUBLIC, Value(47)),
    Constructor(Assign("x", Value(2))),
    Method(
      "method1",
      AccessType.PUBLIC,
      ImplementationType.CONCRETE,
      List(),
      DeclareVar("z",Value(33))
    )
  ).eval()

ClassDef("someClassName1") Implements List(InterfaceDef("someInterface"))
where Implements accepts a List of InterfaceDef, that a class can implement

###Abstract Class assume an abstract class in java

  abstract class AbsClass1{
    public int y = 10
    abstract someMethod1();
  }
in myDSL would be declared in the following way

  AbstractClassDef("AbsClass1",
    Field("y", AccessType.PUBLIC, Value(10)),
    Method("someMeth1",
      AccessType.PUBLIC,
      ImplementationType.ABSTRACT,
      List()
    )
  ).eval()
AbstractClassDef requries atleast one abstract method as an argument and cannot be instantiated

Inheritance with abstract class work just like inheritance with classes

ClassDef("someClassName1") Extends AbstractClassDef("AbsClass1")
Limitations
myDSL uses Any as a data type to deal with varying data inputs (String, Int....). This might cause problems by undermining Scala's strongly typed system, as evident with usage of asInstanceOf methods and type matching to ensure the input is of a certain data type.

Questions to address
Can a class/interface inherit from itself?

No, a class/interface cannot inherit from itself. The Extends function checks for cyclic dependecy among two (static and dynamic) class/interface, by traversing a Map that defines inheritance between classes/interfaces. The function checks this cyclic dependency for both the class/interface involved, thereby taking care of a situation where a class/interface is made to inherit from itself
Can an interface inherit from an abstract class with all pure methods?

No in myDSL, interface can only inherit from other interfaces.
Can an interface implement another interface?

No, interfaces cannot implement other interface. myDSL dissallows Interface definitions to invoke the Implements method.
Can a class implement two or more different interfaces that declare methods with exactly the same signatures?

Yes, classes will be able to implement interfaces with that have methods with same signatures. myDSL checks for presence of method names in concrete class implementations, to ensure that abstract methods are overriden. Therefore existance of one concrete implementation of said method name should qualify and allow implementation of all those interfaces.
Can an abstract class inherit from another abstract class and implement interfaces where all interfaces and the abstract class have methods with the same signatures?

Yes, for rationale mentioned above abstract classes will be allowed to do so.
Can an abstract class implement interfaces?

Yes, abstract class can implement interfaces. Abstract class definition needs to be specifically defined, once that is done, the abstract class name and definitions are stored and treated similar to Class Definitions hence allowing Implements functionality
Can a class implement two or more interfaces that have methods whose signatures differ only in return types?

Since myDSL doesn't concretely define return types for methods and returns the last statement of the method, classes will be able to implements multiple interfaces with same methods but different return types.
Can an abstract class inherit from a concrete class?

Yes, abstract class much like a concrete class definition would be able to inherit from a concrete class provided they do not have circulaar dependency.
Can an abstract class/interface be instantiated as anonymous concrete classes?

Instantiation has been limited to concrete class definition, therefore you cannot instantiate abstract class/interface as anonymous concrete classes.