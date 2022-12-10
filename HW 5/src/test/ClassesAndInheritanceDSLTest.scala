import org.scalatest.*
import org.scalatest.funspec.AnyFunSpec
import matchers.should.Matchers.*
import SetTheoryDSL.SetExpression.*

import scala.collection.mutable

class ClassesAndInheritanceDSLTest extends AnyFunSpec {

  describe("Testing Classes, Objects and Inheritance in DSL SetTheory") {

    describe("Class Definition, fields, and methods") {

      it("Should Define a class and create an object from it") {
        ClassDef(
          "ClassOne",
          CreatePublicField("f1"),
          CreateProtectedField("f2"),
          CreatePrivateField("f3"),
          CreateField("f4"),
          Constructor(
            ParamsExp(Param("a"), Param("b")),
            SetField("f1", Variable("a") ),
            SetField("f2", Variable("b") ),
            SetField("f3", Value(1)),
            SetField("f4", Value(2)),
          ),
          Method(
            "m1",
            PublicAccess(),
            ParamsExp(),
            InvokeMethod("m2")
          ),
          Method(
            "m2",
            PublicAccess(),
            ParamsExp(),
            Field("f2")
          ),
          Method(
            "m3",
            PublicAccess(),
            ParamsExp(),
            Field("f2")
          ),
          Method(
            "m5",
            ProtectedAccess(),
            ParamsExp(Param("x")),
            Variable("x")
          ),
          Method(
            "m6",
            PrivateAccess(),
            ParamsExp(),
            Value("private_value")
          ),
          Method(
            "m7",
            DefAccess(),
            ParamsExp(),
            Variable("default value")
          )
        ).eval
        Assign( "object1", NewObject( ClassRef("ClassOne"), Value(3), Value(4) ) ).eval

        assert( FieldFromObject("f1", Variable("object1")).eval == 3 )
      }

      it("Should Invoke one public method of an object") {
        assert(Assign("var2", InvokeMethodOfObject("m2", Variable("object1")) ).eval == 4)
      }

      it("Should change a public of an object") {
        SetFieldFromObject("f1", Variable("object1"), Value("abc") ).eval
        assert( FieldFromObject("f1", Variable("object1")).eval == "abc" )
      }

      it("Should invoke another one method from another one") {
        assert(Assign("var4", InvokeMethodOfObject("m1", Variable("object1")) ).eval == 4)
      }

      it("Should throw an error when trying to access a protected field") {
        assertThrows[Exception] {
          FieldFromObject("f2", Variable("object1")).eval
        }
      }

      it("Should throw an error when trying to access a private field") {
        assertThrows[Exception] {
          FieldFromObject("f3", Variable("object1")).eval
        }
      }

      it("Should throw an error when trying to access a default field") {
        assertThrows[Exception] {
          FieldFromObject("f4", Variable("object1")).eval
        }
      }

      it("Should create another class by extending other - inheriting its accessible fields") {
        ClassDef(
          "ClassTwo",
          Extends(ClassRef("ClassOne")),
          CreatePublicField("f5"),
          CreateProtectedField("f6"),
          CreatePrivateField("f7"),
          CreateField("f8"),
          Constructor(
            ParamsExp(Param("a"), Param("b")),
            SetField("f5", Field("f1") ),
            SetField("f6",  Field("f2") ),
            SetField("f7", Variable("a") ),
            SetField("f8", Variable("b") )
          ),
          Method(
            "m2",
            PublicAccess(),
            ParamsExp(),
            Value(100)
          ),
          Method(
            "m4",
            PublicAccess(),
            ParamsExp(),
            Field("f2")
          ),
          Method(
            "m9",
            PublicAccess(),
            ParamsExp(Param("x")),
            InvokeMethod("m5", Variable("x"))
          ),
          Method(
            "m10",
            PublicAccess(),
            ParamsExp(),
            InvokeMethod("m6")
          ),
          Method(
            "m11",
            PublicAccess(),
            ParamsExp(),
            InvokeMethod("m7")
          )
        ).eval

        Assign("object2", NewObject( ClassRef("ClassTwo"), Value(10), Value(11) ) ).eval
        assert( FieldFromObject("f5", Variable("object2") ).eval == 10  )
      }

      it("should prove that public fields are also inherited") {
        assert( FieldFromObject("f1", Variable("object2") ).eval == 10  )
      }

      it("should prove that protected fields are also inherited") {
        assert( InvokeMethodOfObject("m4", Variable("object2") ).eval == 11  )
      }

      it("should throw an error if superClass's private field is accessed - as private fields are not inherited") {
        assertThrows[Exception] {
          FieldFromObject("f3", Variable("object2") ).eval
        }
      }

      it("should throw an error if superClass's default field is accessed - as default fields are not inherited") {
        assertThrows[Exception] {
          FieldFromObject("f4", Variable("object2") ).eval
        }
      }

      it("should inherit public methods of its parent class and invoke those methods if not overridden in child class's body") {
        assert( InvokeMethodOfObject("m3", Variable("object2") ).eval == 11  )
      }

      it("should perform dynamic dispatch of method from super class's method") {
        assert( InvokeMethodOfObject("m1", Variable("object2") ).eval == 100  )
      }

      it("should also inherit protected methods of its parent class") {
        assert( InvokeMethodOfObject("m9", Variable("object2"), Value(55) ).eval == 55  )
      }

      it("should throw error if try to access private methods of its parent") {
        assertThrows[Exception] {
          InvokeMethodOfObject("m10", Variable("object2") ).eval
        }
      }

      it("should throw error if try to access default methods of its parent") {
        assertThrows[Exception] {
          InvokeMethodOfObject("m11", Variable("object2") ).eval
        }
      }

      it("should throw error if try to directly invoke its or its parent's protected methods") {
        assertThrows[Exception] {
          InvokeMethodOfObject("m5", Variable("object2"), Value(20) ).eval
        }
      }

      it("should create an inner class and access it using enclosing class to create object") {
        ClassDef(
          "OuterClass",
          CreatePublicField("f1"),
          Constructor(
            ParamsExp(),
            SetField("f1", Value("field_value"))
          ),
          ClassDef(
            "InnerClass",
            CreatePublicField("f2"),
            Constructor(
              ParamsExp(),
              SetField("f2", Value("inner_field_value"))
            )
          )
        ).eval

        Assign("object3", NewObject( ClassRefFromClass("InnerClass", ClassRef("OuterClass") ) ) ).eval

        assert( FieldFromObject("f2", Variable("object3") ).eval == "inner_field_value" )
      }

      it("should create an inner class and access it using enclosing class's instance") {
        Assign("object4", NewObject( ClassRef("OuterClass") ) ).eval

        Assign("object5", NewObject( ClassRefFromObject("InnerClass", Variable("object4") ) ) ).eval

        assert( FieldFromObject("f2", Variable("object5") ).eval == "inner_field_value" )

        GarbageCollector.eval
      }

    }

  }
}
