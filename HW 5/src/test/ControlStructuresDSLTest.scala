import org.scalatest.*
import org.scalatest.funspec.AnyFunSpec
import matchers.should.Matchers.*
import SetTheoryDSL.SetExpression.*

import scala.collection.mutable

class ControlStructuresDSLTest extends AnyFunSpec {
  describe("Testing Control Structures") {

    describe("Testing If Control Structure") {
      it("should evaluate the then clause if condition is truthy") {
        Assign("var1", Value(20)).eval
        Assign("set1", SetIdentifier( Value(10) )).eval

        If( Check( Equals(Variable("var1"), Value(20)) ),
          Then(
            InsertInto( Variable("set1"), Value(50))
          )
        ).eval

        assert( Variable("set1").eval == Set(10, 50) )
        GarbageCollector.eval
      }

      it("should evaluate multiple expressions passed to the then clause if condition is truthy") {
        Assign("var2", Value(20)).eval
        Assign("set2", SetIdentifier( Value(10) )).eval

        If( Check( Equals(Variable("var2"), Value(20)) ),
          Then(
            InsertInto( Variable("set2"), Value(50), Value(true)),
            DeleteFrom( Variable("set2"), Value(true) )
          )
        ).eval

        assert( Variable("set2").eval == Set(10, 50) )
        GarbageCollector.eval
      }

      it("should not evaluate the then clause if condition is false") {
        Assign("var3", Value(20)).eval
        Assign("set3", SetIdentifier( Value(10) )).eval

        If( Check( Equals(Variable("var3"), Value("20")) ),
          Then(
            InsertInto( Variable("set3"), Value(50))
          )
        ).eval

        assert( Variable("set3").eval == Set(10) )
        GarbageCollector.eval
      }
    }

    describe("Testing IfElse Control Structure") {
      it("should evaluate then clause and ignore else clause of IfElse Expression if condition evaluated is truthy") {
        Assign("var4", Value(20)).eval
        Assign("set4", SetIdentifier( Value(10) )).eval

        IfElse( Check( Equals(Variable("var4"), Value(20)) ),
          Then(
            InsertInto( Variable("set4"), Value(50))
          ),
          Else(
            InsertInto( Variable("set4"), Value(20))
          )
        ).eval

        assert( Variable("set4").eval == Set(10, 50) )
        GarbageCollector.eval
      }

      it("should evaluate multiple expressions in the then clause of IfElse Expression if condition evaluated is truthy") {
        Assign("var5", Value(20)).eval
        Assign("set5", SetIdentifier( Value(10) )).eval

        IfElse( Check( Equals(Variable("var5"), Value(20)) ),
          Then(
            InsertInto( Variable("set5"), Value(50), Value(true) ),
            DeleteFrom( Variable("set5"), Value(50) )
          ),
          Else(
            InsertInto( Variable("set5"), Value(20))
          )
        ).eval

        assert( Variable("set5").eval == Set(10, true) )
        GarbageCollector.eval
      }

      it("should evaluate else clause and ignore then clause of IfElse Expression if condition evaluated is false") {
        Assign("var6", Value(20)).eval
        Assign("set6", SetIdentifier( Value(10) )).eval

        IfElse( Check( Equals(Variable("var6"), Value("20")) ),
          Then(
            InsertInto( Variable("set6"), Value(50))
          ),
          Else(
            InsertInto( Variable("set6"), Value(20))
          )
        ).eval

        assert( Variable("set6").eval == Set(10, 20) )
        GarbageCollector.eval
      }

      it("should evaluate multiple expressions in the else clause of IfElse Expression if condition evaluated is false") {
        Assign("var7", Value(20)).eval
        Assign("set7", SetIdentifier( Value(10) )).eval

        IfElse( Check( Equals(Variable("var7"), Value("20")) ),
          Then(
            InsertInto( Variable("set7"), Value(20))
          ),
          Else(
            InsertInto( Variable("set7"), Value(50), Value(true) ),
            DeleteFrom( Variable("set7"), Value(50) )
          )
        ).eval

        assert( Variable("set7").eval == Set(10, true) )
        GarbageCollector.eval
      }
    }

    describe("Testing exception throwing and handling capabilities of the DSL") {
      it("should create an exception class and throw an instance of that exception class in the global scope, which will not be handled") {
        val exceptionClassName = "Exception_1"
        val exceptionCause = "Custom Exception Cause"
        ExceptionClassDef(exceptionClassName,
          CreatePublicField("cause"),
          Constructor(
            ParamsExp(Param("passedCause")),
            SetField("cause", Variable("passedCause"))
          )
        ).eval

        val thrown = the [Exception] thrownBy ThrowNewException(ClassRef(exceptionClassName), Value(exceptionCause)).eval
        thrown.getMessage should equal ("Unhandled Exception")
        GarbageCollector.eval
      }

      it("should create an exception class and throw an instance of that exception class in a nested named scope, which will not be handled") {
        val exceptionClassName = "Exception_2"
        val exceptionCause = "Custom Exception Cause"
        ExceptionClassDef(exceptionClassName,
          CreatePublicField("cause"),
          Constructor(
            ParamsExp(Param("passedCause")),
            SetField("cause", Variable("passedCause"))
          )
        ).eval

        val thrown = the [Exception] thrownBy NamedScope("scope1",
          ThrowNewException(ClassRef(exceptionClassName), Value(exceptionCause))
        ).eval
        thrown.getMessage should equal ("Unhandled Exception")
        GarbageCollector.eval
      }

      it("should create an exception class and throw an instance of that exception class in a nested unnamed scope, which will not be handled") {
        val exceptionClassName = "Exception_3"
        val exceptionCause = "Custom Exception Cause"
        ExceptionClassDef(exceptionClassName,
          CreatePublicField("cause"),
          Constructor(
            ParamsExp(Param("passedCause")),
            SetField("cause", Variable("passedCause"))
          )
        ).eval

        val thrown = the [Exception] thrownBy UnnamedScope(
          ThrowNewException(ClassRef(exceptionClassName), Value(exceptionCause))
        ).eval
        thrown.getMessage should equal ("Unhandled Exception")
        GarbageCollector.eval
      }

      it("should catch an exception and handle it in the catch block of TryCatch expression when thrown inside the try block") {
        val exceptionClassName = "Exception_4"
        val exceptionCause = "Custom Exception Cause"
        ExceptionClassDef(exceptionClassName,
          CreatePublicField("cause"),
          Constructor(
            ParamsExp(Param("passedCause")),
            SetField("cause", Variable("passedCause"))
          )
        ).eval

        Assign("set8", SetIdentifier( Value(10) )).eval
        InsertInto(Variable("set8"), Value(20)).eval
        TryCatch(
          Try(
            InsertInto(Variable("set8"), Value(50)),
            ThrowNewException(ClassRef(exceptionClassName), Value(exceptionCause)),
            InsertInto(Variable("set8"), Value(100)),
          ),
          Catch("e1", ClassRef(exceptionClassName),
            InsertInto(Variable("set8"), FieldFromObject("cause", Variable("e1")))
          )
        ).eval

        assert(Variable("set8").eval == Set(10, 20, 50, exceptionCause))
        GarbageCollector.eval
      }

      it("should ignore all the expression after an exception is thrown in the try block and move on after handling the catch block expression") {
        val exceptionClassName = "Exception_5"
        val exceptionCause = "Custom Exception Cause"
        ExceptionClassDef(exceptionClassName,
          CreatePublicField("cause"),
          Constructor(
            ParamsExp(Param("passedCause")),
            SetField("cause", Variable("passedCause"))
          )
        ).eval

        Assign("set9", SetIdentifier( Value(10) )).eval
        InsertInto(Variable("set9"), Value(20)).eval
        TryCatch(
          Try(
            InsertInto(Variable("set9"), Value(50)),
            ThrowNewException(ClassRef(exceptionClassName), Value(exceptionCause)),
            InsertInto(Variable("set9"), Value(100)),
            DeleteFrom(Variable("set9"), Value(100), Value(50), Value(10))
          ),
          Catch("e1", ClassRef(exceptionClassName),
            InsertInto(Variable("set9"), FieldFromObject("cause", Variable("e1")))
          )
        ).eval
        InsertInto(Variable("set9"), Value("after try catch block")).eval

        assert(Variable("set9").eval == Set(10, 20, 50, exceptionCause, "after try catch block"))
        GarbageCollector.eval
      }

      it("should throw an exception deeply nested in a try block and propagate to the matching catch block") {
        val exceptionClassName = "Exception_6"
        val exceptionCause = "Custom Exception Cause"
        ExceptionClassDef(exceptionClassName,
          CreatePublicField("cause"),
          Constructor(
            ParamsExp(Param("passedCause")),
            SetField("cause", Variable("passedCause"))
          )
        ).eval

        Assign("set10", SetIdentifier( Value(10) )).eval
        InsertInto(Variable("set10"), Value(20)).eval
        TryCatch(
          Try(
            InsertInto(Variable("set10"), Value(50)),
            UnnamedScope(
              InsertInto(Variable("set10"), Value(60)),
              NamedScope("scope2",
                InsertInto(Variable("set10"), Value(70)),
                ThrowNewException(ClassRef(exceptionClassName), Value(exceptionCause))
              )
            ),
            InsertInto(Variable("set10"), Value(100)),
            DeleteFrom(Variable("set10"), Value(100), Value(50), Value(10))
          ),
          Catch("e1", ClassRef(exceptionClassName),
            InsertInto(Variable("set10"), FieldFromObject("cause", Variable("e1")))
          )
        ).eval

        assert(Variable("set10").eval == Set(10, 20, 50, 60, 70, exceptionCause))
        GarbageCollector.eval
      }

      it("should not handle an exception when the thrown exception is different from the one whose signature is mentioned in catch block") {
        val exceptionClassName1 = "Exception_7"
        val exceptionClassName2 = "Exception_8"
        val exceptionCause = "Custom Exception Cause"
        ExceptionClassDef(exceptionClassName1,
          CreatePublicField("cause"),
          Constructor(
            ParamsExp(Param("passedCause")),
            SetField("cause", Variable("passedCause"))
          )
        ).eval

        ExceptionClassDef(exceptionClassName2,
          CreatePublicField("cause"),
          Constructor(
            ParamsExp(Param("passedCause")),
            SetField("cause", Variable("passedCause"))
          )
        ).eval

        Assign("set11", SetIdentifier( Value(10) )).eval

        val thrown = the [Exception] thrownBy TryCatch(
          Try(
            InsertInto(Variable("set11"), Value(50)),
            ThrowNewException(ClassRef(exceptionClassName1), Value(exceptionCause)),
            InsertInto(Variable("set11"), Value(100)),
          ),
          Catch("e1", ClassRef(exceptionClassName2),
            InsertInto(Variable("set11"), FieldFromObject("cause", Variable("e1")))
          )
        ).eval

        thrown.getMessage should equal ("Unhandled Exception")
        GarbageCollector.eval
      }

      it("should take advantage of multiple catch blocks in TryCatch expression and handle exception later in the chain") {
        val exceptionClassName1 = "Exception_9"
        val exceptionClassName2 = "Exception_10"
        val exceptionCause = "Custom Exception Cause"
        ExceptionClassDef(exceptionClassName1,
          CreatePublicField("cause"),
          Constructor(
            ParamsExp(Param("passedCause")),
            SetField("cause", Variable("passedCause"))
          )
        ).eval

        ExceptionClassDef(exceptionClassName2,
          CreatePublicField("cause"),
          Constructor(
            ParamsExp(Param("passedCause")),
            SetField("cause", Variable("passedCause"))
          )
        ).eval

        Assign("set12", SetIdentifier( Value(10) )).eval

        TryCatch(
          Try(
            InsertInto(Variable("set12"), Value(50)),
            ThrowNewException(ClassRef(exceptionClassName1), Value(exceptionCause)),
            InsertInto(Variable("set12"), Value(100)),
          ),
          Catch("e1", ClassRef(exceptionClassName2),
            InsertInto(Variable("set12"), FieldFromObject("cause", Variable("e1")))
          ),
          Catch("e1", ClassRef(exceptionClassName1),
            InsertInto(Variable("set12"), Value(200))
          )
        ).eval

        assert(Variable("set12").eval == Set(10, 50, 200))
        GarbageCollector.eval
      }

      it("should take advantage of dynamic dispatch and inheritance and catch an exception of sub class type in catch block looking for exception with the parent type") {
        val exceptionClassName1 = "Exception_11"
        val exceptionClassName2 = "Exception_12"
        val exceptionCause = "Custom Exception Cause"
        ExceptionClassDef(exceptionClassName1,
          CreatePublicField("cause"),
          Constructor(
            ParamsExp(Param("passedCause")),
            SetField("cause", Variable("passedCause"))
          )
        ).eval

        ExceptionClassDef(exceptionClassName2,
          Extends(ClassRef(exceptionClassName1)),
          Constructor(
            ParamsExp(Param("passedCause")),
            SetField("cause", Variable("passedCause"))
          )
        ).eval

        Assign("set13", SetIdentifier( Value(10) )).eval

        TryCatch(
          Try(
            InsertInto(Variable("set13"), Value(50)),
            ThrowNewException(ClassRef(exceptionClassName2), Value(exceptionCause)),
            InsertInto(Variable("set13"), Value(100)),
          ),
          Catch("e1", ClassRef(exceptionClassName1),
            InsertInto(Variable("set13"), Value(200))
          )
        ).eval

        assert(Variable("set13").eval == Set(10, 50, 200))
        GarbageCollector.eval
      }

      it("should throw another exception in catch block which will be unhandled") {
        val exceptionClassName = "Exception_13"
        val exceptionCause = "Custom Exception Cause"
        ExceptionClassDef(exceptionClassName,
          CreatePublicField("cause"),
          Constructor(
            ParamsExp(Param("passedCause")),
            SetField("cause", Variable("passedCause"))
          )
        ).eval

        Assign("set14", SetIdentifier( Value(10) )).eval
        InsertInto(Variable("set14"), Value(20)).eval
        val thrown = the [Exception] thrownBy TryCatch(
          Try(
            InsertInto(Variable("set14"), Value(50)),
            ThrowNewException(ClassRef(exceptionClassName), Value(exceptionCause)),
            InsertInto(Variable("set14"), Value(100)),
          ),
          Catch("e1", ClassRef(exceptionClassName),
            InsertInto(Variable("set14"), FieldFromObject("cause", Variable("e1"))),
            ThrowNewException(ClassRef(exceptionClassName), Value(exceptionCause))
          )
        ).eval

        thrown.getMessage should equal ("Unhandled Exception")
        GarbageCollector.eval
      }

      it("should throw another exception in catch block which will be handled by an outer catch block") {
        val exceptionClassName = "Exception_14"
        val exceptionCause1 = "Custom Exception Cause"
        val exceptionCause2 = "Custom Outer Exception Cause"
        ExceptionClassDef(exceptionClassName,
          CreatePublicField("cause"),
          Constructor(
            ParamsExp(Param("passedCause")),
            SetField("cause", Variable("passedCause"))
          )
        ).eval

        Assign("set15", SetIdentifier( Value(10) )).eval
        InsertInto(Variable("set15"), Value(20)).eval
        TryCatch(
          Try(
            InsertInto(Variable("set15"), Value(50)),
            TryCatch(
              Try(
                ThrowNewException(ClassRef(exceptionClassName), Value(exceptionCause1)),
              ),
              Catch("e1", ClassRef(exceptionClassName),
                ThrowNewException(ClassRef(exceptionClassName), Value(exceptionCause2))
              )
            ),
            InsertInto(Variable("set15"), Value(100)),
          ),
          Catch("e1", ClassRef(exceptionClassName),
            InsertInto(Variable("set15"), FieldFromObject("cause", Variable("e1"))),
          )
        ).eval

        assert(Variable("set15").eval == Set(10, 20, 50, exceptionCause2))
        GarbageCollector.eval
      }

      it("should throw an exception in constructor while declaring a class") {
        val exceptionClassName = "Exception_15"
        val exceptionCause = "Custom Exception Cause"
        val className = "c1"

        ExceptionClassDef(exceptionClassName,
          CreatePublicField("cause"),
          Constructor(
            ParamsExp(Param("passedCause")),
            SetField("cause", Variable("passedCause"))
          )
        ).eval

        Assign("exceptionSet1", SetIdentifier()).eval

        ClassDef(className,
          CreatePublicField("f1"),
          CreatePublicField("f2"),
          Constructor(
            ParamsExp(Param("a"), Param("b")),
            ThrowNewException(ClassRef(exceptionClassName), Value(exceptionCause)),
            SetField("f1", Variable("a")),
            SetField("f2", Variable("b"))
          )
        ).eval

        TryCatch(
          Try(
            Assign("obj1", NewObject(ClassRef(className), Value(1), Value(2) ))
          ),
          Catch("e1", ClassRef(exceptionClassName),
            InsertInto(Variable("exceptionSet1"), FieldFromObject("cause", Variable("e1")))
          )
        ).eval

        assert( Variable("exceptionSet1").eval == Set(exceptionCause) )
        GarbageCollector.eval

      }

      it("should throw an exception in one of the methods of class and capture it in a catch block") {
        val exceptionClassName = "Exception_16"
        val exceptionCause = "Custom Exception Cause"
        val className = "c2"

        ExceptionClassDef(exceptionClassName,
          CreatePublicField("cause"),
          Constructor(
            ParamsExp(Param("passedCause")),
            SetField("cause", Variable("passedCause"))
          )
        ).eval

        Assign("exceptionSet2", SetIdentifier()).eval

        ClassDef(className,
          CreatePublicField("f1"),
          CreatePublicField("f2"),
          Constructor(
            ParamsExp(Param("a"), Param("b")),
            SetField("f1", Variable("a")),
            SetField("f2", Variable("b"))
          ),
          Method(
            "m1",
            PublicAccess(),
            ParamsExp(),
            SetField("f1", Value(30)),
            ThrowNewException(ClassRef(exceptionClassName), Value(exceptionCause)),
            SetField("f2", Value(40))
          )
        ).eval

        TryCatch(
          Try(
            Assign("obj1", NewObject(ClassRef(className), Value(1), Value(2) )),
            InvokeMethodOfObject("m1", Variable("obj1"))
          ),
          Catch("e1", ClassRef(exceptionClassName),
            InsertInto(Variable("exceptionSet2"), FieldFromObject("cause", Variable("e1")))
          )
        ).eval

        assert( Variable("exceptionSet2").eval == Set(exceptionCause) )
      }

      it("should throw a Java exception if the Exception Class does not have a 'cause' public field") {
        val exceptionClassName = "Exception_17"
        val exceptionCause = "Custom Exception Cause"
        ExceptionClassDef(exceptionClassName,
          Constructor(
            ParamsExp(Param("passedCause"))
          )
        ).eval

        val thrown = the [Exception] thrownBy ThrowNewException(ClassRef(exceptionClassName), Value(exceptionCause)).eval
        thrown.getMessage should equal ("No such field Exist")
      }
    }

  }
}
