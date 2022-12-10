import org.scalatest.*
import org.scalatest.funspec.AnyFunSpec
import matchers.should.Matchers.*
import SetTheoryDSL.SetExpression.*

import scala.collection.mutable

class InterfaceAndAbstractClassDSLTest extends AnyFunSpec {

  describe("Testing Interfaces and Abstract Classes in DSL SetTheory") {

    describe("Interface declaration, implementation and inheritance") {

      it("should throw an Exception if one interface tries to implement another interface") {
        InterfaceDef(
          "I1",
          CreatePublicField("f1"),
          Method(
            "m1",
            PublicAccess(),
            ParamsExp(Param("a"), Param("b"))
          )
        ).eval
        val thrown = the [Exception] thrownBy InterfaceDef(
          "I2",
          Implements(InterfaceRef("I1")),
          CreatePublicField("f1"),
          Method(
            "m1",
            PublicAccess(),
            ParamsExp(Param("a"), Param("b"))
          )
        ).eval
        thrown.getMessage should equal ("An interface cannot implement another interface")
        GarbageCollector.eval
      }

      it("should throw an Exception if an attempt to declare an interface with the name of an existing interface in the same scope") {
        val interfaceName = "I3"
        InterfaceDef(
          interfaceName
        ).eval
        val thrown = the [Exception] thrownBy InterfaceDef(
          interfaceName
        ).eval
        thrown.getMessage should equal ("I3 interface already exists.")
        GarbageCollector.eval
      }

      it("should throw an Exception if one interface extends an another interface and tries to create a method with same name which is already declared in super interface") {
        val interfaceNameOne = "I4"
        val interfaceNameTwo = "I5"
        InterfaceDef(
          interfaceNameOne,
          CreatePublicField("f1"),
          Method(
            "m1",
            PublicAccess(),
            ParamsExp(Param("a"), Param("b"))
          )
        ).eval
        val thrown = the [Exception] thrownBy InterfaceDef(
          interfaceNameTwo,
          Extends(InterfaceRef(interfaceNameOne)),
          Method(
            "m1",
            PublicAccess(),
            ParamsExp(Param("c"), Param("d"))
          )
        ).eval
        thrown.getMessage should equal ("Method with same name (signature) already exists in the hierarchy")
        GarbageCollector.eval
      }

      it("should throw an Exception if 'Extends' expression is present more than once in interface body") {
        val interfaceNameOne = "I6"
        val interfaceNameTwo = "I7"
        InterfaceDef(
          interfaceNameOne
        ).eval
        val thrown = the [Exception] thrownBy InterfaceDef(
          interfaceNameTwo,
          Extends(InterfaceRef(interfaceNameOne)),
          Extends(InterfaceRef(interfaceNameOne))
        ).eval
        thrown.getMessage should equal ("An Interface can extend only a single Interface")
        GarbageCollector.eval
      }
    }

    describe("Classes, Abstract Classes, Interfaces and their relations including implementation and inheritance") {

      it("should throw an Exception if a non-abstract class tries to implement an interface but does not provide any implementation for interface methods") {
        val interfaceName = "I8"
        val className = "C1"
        InterfaceDef(
          interfaceName,
          Method(
            "m1",
            PublicAccess(),
            ParamsExp(Param("c"), Param("d"))
          )
        ).eval
        val thrown = the [Exception] thrownBy ClassDef(
          className,
          Implements(InterfaceRef(interfaceName))
        ).eval
        thrown.getMessage should equal ("Non-concrete classes (classes with abstract method) should be declared abstract")
        GarbageCollector.eval
      }

      it("should throw an Exception if an abstract class is declared without any abstract method") {
        val className = "C2"
        val thrown = the [Exception] thrownBy AbstractClassDef(
          className
        ).eval
        thrown.getMessage should equal ("Concrete classes cannot be declared abstract")
        GarbageCollector.eval
      }

      it("should throw an Exception if 'Implements' expression comes more than once in a class declaration") {
        val interfaceName = "I9"
        InterfaceDef(
          interfaceName,
          Method(
            "m1",
            PublicAccess(),
            ParamsExp(Param("c"), Param("d"))
          )
        ).eval
        val className = "C3"
        val thrown = the [Exception] thrownBy AbstractClassDef(
          className,
          Implements(InterfaceRef(interfaceName)),
          Implements(InterfaceRef(interfaceName))
        ).eval
        thrown.getMessage should equal ("Implement expression can only be used once")
        GarbageCollector.eval
      }

      it("should throw an Exception if same interface is implemented more than once in one expression") {
        val interfaceName = "I10"
        InterfaceDef(
          interfaceName,
          Method(
            "m1",
            PublicAccess(),
            ParamsExp(Param("c"), Param("d"))
          )
        ).eval
        val className = "C4"
        val thrown = the [Exception] thrownBy AbstractClassDef(
          className,
          Implements(InterfaceRef(interfaceName), InterfaceRef(interfaceName))
        ).eval
        thrown.getMessage should equal ("Same Interface cannot be implemented more than once in a single declaration")
        GarbageCollector.eval
      }

      it("should throw an Exception if non-abstract class declares an abstract method") {
        val className = "C5"
        val thrown = the [Exception] thrownBy ClassDef(
          className,
          Method(
            "m1",
            PublicAccess(),
            ParamsExp()
          )
        ).eval
        thrown.getMessage should equal ("A concrete class cannot have abstract methods")
        GarbageCollector.eval
      }

      it("should implement an interface and provide behavior for the abstract method") {
        val interfaceName = "I11"
        val className = "C6"
        InterfaceDef(
          interfaceName,
          CreatePublicField("f1"),
          Method(
            "m1",
            PublicAccess(),
            ParamsExp(Param("a"), Param("b"))
          )
        ).eval

        ClassDef(
          className,
          Implements(InterfaceRef(interfaceName)),
          Constructor(
            ParamsExp()
          ),
          Method(
            "m1",
            PublicAccess(),
            ParamsExp(Param("a"), Param("b")),
            SetIdentifier(Variable("a"), Variable("b"), Value(50))
          )
        ).eval

        Assign("object1", NewObject( ClassRef(className) ) ).eval
        Assign("set1", InvokeMethodOfObject("m1", Variable("object1"), Value(1), Value(5)) ).eval

        assert( Variable("set1").eval == Set(1, 5, 50) )
        GarbageCollector.eval
      }

      it("should invoke a default method of an interface") {
        val interfaceName = "I12"
        val className = "C7"
        InterfaceDef(
          interfaceName,
          CreatePublicField("f1"),
          Method(
            "m1",
            PublicAccess(),
            ParamsExp(Param("a"), Param("b")),
            Union( SetIdentifier( Variable("a") ), SetIdentifier( Variable("b") ) )
          )
        ).eval

        ClassDef(
          className,
          Implements(InterfaceRef(interfaceName)),
          Constructor(
            ParamsExp()
          )
        ).eval

        Assign("object2", NewObject( ClassRef(className) ) ).eval
        Assign("set2", InvokeMethodOfObject("m1", Variable("object2"), Value(1), Value(1)) ).eval

        assert( Variable("set2").eval == Set(1) )
        GarbageCollector.eval
      }

      it("should invoke a default method from an abstract method of an interface which is implemented by the class") {
        val interfaceOneName = "I13"
        val interfaceTwoName = "I14"
        val className = "C8"
        InterfaceDef(
          interfaceOneName,
          CreatePublicField("f1"),
          Method(
            "m1",
            PublicAccess(),
            ParamsExp(Param("a"), Param("b"))
          )
        ).eval

        InterfaceDef(
          interfaceTwoName,
          Extends(InterfaceRef(interfaceOneName)),
          Method(
            "m2",
            PublicAccess(),
            ParamsExp(),
            InvokeMethod("m1", Value("100"), Value(60))
          )
        ).eval

        ClassDef(
          className,
          Implements(InterfaceRef(interfaceTwoName), InterfaceRef(interfaceOneName)),
          Constructor(
            ParamsExp()
          ),
          Method(
            "m1",
            PublicAccess(),
            ParamsExp(Param("a"), Param("b")),
            SetIdentifier(Variable("a"), Variable("b"), Value(50))
          )
        ).eval

        Assign("object3", NewObject( ClassRef(className) ) ).eval
        Assign("set3", InvokeMethodOfObject("m2", Variable("object3") ) ).eval
        assert( Variable("set3").eval == Set("100", 60, 50) )
        GarbageCollector.eval
      }

      it("should undergo class and interface composition, where a field of interface are inherited by an abstract class and then by concrete class, also implmenting the super interfaces") {
        val interfaceOneName = "I15"
        val interfaceTwoName = "I16"
        val interfaceThreeName = "I17"
        val classOneName = "C9"
        val classTwoName = "C10"

        InterfaceDef(
          interfaceOneName,
          CreatePublicField("f1"),
          Method(
            "m1",
            PublicAccess(),
            ParamsExp(Param("a"), Param("b"))
          ),
          Method(
            "m2",
            PublicAccess(),
            ParamsExp(),
          )
        ).eval

        InterfaceDef(
          interfaceTwoName,
          Extends(InterfaceRef(interfaceOneName)),
          Method(
            "m3",
            PrivateAccess(),
            ParamsExp()
          )
        ).eval

        InterfaceDef(
          interfaceThreeName,
          Extends(InterfaceRef(interfaceOneName)),
          Method(
            "m5",
            ProtectedAccess(),
            ParamsExp(Param("x"))
          )
        ).eval

        AbstractClassDef(
          classOneName,
          Implements(InterfaceRef(interfaceTwoName)),
          Constructor(
            ParamsExp()
          )
        ).eval

        ClassDef(
          classTwoName,
          Extends(ClassRef(classOneName)),
          Implements(InterfaceRef(interfaceThreeName), InterfaceRef(interfaceOneName)),
          Constructor(
            ParamsExp(),
            SetField("f1", Value(20))
          ),
          Method(
            "m1",
            PublicAccess(),
            ParamsExp(Param("a"), Param("b")),
            SetIdentifier( Variable("a"), Variable("b") )
          ),
          Method(
            "m2",
            PublicAccess(),
            ParamsExp(),
            SetIdentifier( Field("f1") )
          ),
          Method(
            "m5",
            ProtectedAccess(),
            ParamsExp(Param("x")),
            Assign("y", Value("hello")),
            SetIdentifier( Variable("x"), Variable("y") )
          )

        ).eval

        Assign("object4", NewObject( ClassRef(classTwoName) ) ).eval
        Assign("set4", InvokeMethodOfObject("m2", Variable("object4")) ).eval

        assert( Variable("set4").eval == Set(20) )
        GarbageCollector.eval
      }

      it("should throw an Exception if any interface tries to extend itself") {
        val interfaceName = "I18"
        assertThrows[Exception] {
          InterfaceDef(
            interfaceName,
            Extends(InterfaceRef(interfaceName))
          ).eval
        }

        GarbageCollector.eval
      }

      it("should throw an Exception if any abstract class tries to extend itself") {
        val className = "C11"
        assertThrows[Exception] {
          AbstractClassDef(
            className,
            Extends(ClassRef(className))
          ).eval
        }

        GarbageCollector.eval
      }

      it("should throw an Exception if any concrete class tries to extend itself") {
        val className = "C12"
        assertThrows[Exception] {
          ClassDef(
            className,
            Extends(ClassRef(className))
          ).eval
        }

        GarbageCollector.eval
      }

      it("should not inherit any private access method of an interface") {
        val interfaceName = "I19"
        val className = "C13"
        InterfaceDef(
          interfaceName,
          CreatePublicField("f1"),
          CreatePrivateField("f2"),
          CreateField("f3"),
          Method(
            "m1",
            PrivateAccess(),
            ParamsExp()
          )
        ).eval

        ClassDef(
          className,
          Implements(InterfaceRef(interfaceName)),
          Constructor(
            ParamsExp(),
            SetField("f2", Value(10))
          )
        ).eval

        val thrown = the [Exception] thrownBy Assign("object5", NewObject(ClassRef(className))).eval
        thrown.getMessage should equal ("No such field Exist")
        GarbageCollector.eval
      }

      it("should not inherit any default access method of an interface") {
        val interfaceName = "I20"
        val className = "C14"
        InterfaceDef(
          interfaceName,
          CreatePublicField("f1"),
          CreatePrivateField("f2"),
          CreateField("f3"),
          Method(
            "m1",
            PrivateAccess(),
            ParamsExp()
          )
        ).eval

        ClassDef(
          className,
          Implements(InterfaceRef(interfaceName)),
          Constructor(
            ParamsExp(),
            SetField("f3", Value(20))
          )
        ).eval

        val thrown = the [Exception] thrownBy Assign("object6", NewObject(ClassRef(className))).eval
        thrown.getMessage should equal ("No such field Exist")
        GarbageCollector.eval
      }

    }

    describe("Member classes and Interface Access") {

      it("should only implement an inner interface of another interface") {
        val iOneName = "I21"
        val iTwoName = "I22"
        val cName = "C15"

        InterfaceDef(
          iOneName,
          InterfaceDef(
            iTwoName,
            Method(
              "m2",
              PublicAccess(),
              ParamsExp()
            )
          )
        ).eval

        ClassDef(
          cName,
          Implements(InterfaceRefFromInterface(iTwoName, InterfaceRef(iOneName))),
          Constructor(
            ParamsExp()
          ),
          Method(
            "m2",
            PublicAccess(),
            ParamsExp(),
            Value("hello")
          )
        ).eval

        Assign("object7", NewObject( ClassRef(cName) ) ).eval
        Assign("set7", InvokeMethodOfObject("m2", Variable("object7") ) ).eval
        assert( Variable("set7").eval == "hello" )
        GarbageCollector.eval
      }

      it("should only implement an inner interface of another class") {
        val iName = "I23"
        val cOneName = "C16"
        val cTwoName = "C17"

        ClassDef(
          cOneName,
          Constructor(
            ParamsExp()
          ),
          InterfaceDef(
            iName,
            Method(
              "m1",
              PublicAccess(),
              ParamsExp(Param("x"))
            )
          )
        ).eval

        ClassDef(
          cTwoName,
          Implements(InterfaceRefFromClass(iName, ClassRef(cOneName))),
          Constructor(
            ParamsExp()
          ),
          Method(
            "m1",
            PublicAccess(),
            ParamsExp(Param("x")),
            Variable("x")
          )
        ).eval

        Assign("object8", NewObject( ClassRef(cTwoName) ) ).eval
        Assign("set8", InvokeMethodOfObject("m1", Variable("object8"), Value("hello") ) ).eval
        assert( Variable("set8").eval == "hello" )
        GarbageCollector.eval
      }

      it("should only implement an inner interface of another object") {
        val iName = "I24"
        val cOneName = "C18"
        val cTwoName = "C19"

        ClassDef(
          cOneName,
          Constructor(
            ParamsExp()
          ),
          InterfaceDef(
            iName,
            Method(
              "m1",
              PublicAccess(),
              ParamsExp(Param("x"))
            )
          )
        ).eval

        Assign("object9", NewObject(ClassRef(cOneName))).eval

        ClassDef(
          cTwoName,
          Implements(InterfaceRefFromObject(iName, Variable("object9"))),
          Constructor(
            ParamsExp()
          ),
          Method(
            "m1",
            PublicAccess(),
            ParamsExp(Param("x")),
            Variable("x")
          )
        ).eval

        Assign("object10", NewObject( ClassRef(cTwoName) ) ).eval
        Assign("set10", InvokeMethodOfObject("m1", Variable("object10"), Value("29") ) ).eval
        assert( Variable("set10").eval == "29" )
        GarbageCollector.eval
      }
    }

  }

}
