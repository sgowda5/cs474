import org.scalatest._
import flatspec.AnyFlatSpec
import matchers.should.Matchers.*
import SetTheoryDSL.SetExpression.*

import scala.collection.mutable

class SetTheoryDSLTest extends AnyFlatSpec {
  behavior of "Set Theory DSL - SetPlayground"

  it should "create a variable an insert a value into it" in {
    // Declaration of a variable named "var_t1" and assigning it a value 5
    Assign("var_t1", Value(5)).eval
    assert(Variable("var_t1").eval == 5)
    GarbageCollector.eval
  }

  it should "create a set and insert a string value in it" in {
    Assign("set_t2", SetIdentifier( Value("dummyText") )).eval
    assert(Variable("set_t2").eval == Set("dummyText"))
    GarbageCollector.eval
  }

  it should "create a set and insert multiple values in it" in {
    Assign("set_t3", SetIdentifier( Value("dummyText"), Value(10), Value(20.0f) )).eval
    assert(Variable("set_t3").eval == Set("dummyText", 10, 20.0f))
    GarbageCollector.eval
  }

  it should "create a macro and store it in a variable" in {
    Assign("macro_t4", Macro(Variable("var_t4"))).eval
    assert(Variable("macro_t4").eval == Variable("var_t4"))
    GarbageCollector.eval
  }

  it should "create a macro and evaluate that macro later to add value to a set" in {
    Assign("macro_t5", Macro(Variable("var_t5"))).eval
    Assign("var_t5", Value("macroExample")).eval
    Assign("set_t5", SetIdentifier( ComputeMacro( Variable("macro_t5") ) )).eval
    assert( Variable("set_t5").eval == Set("macroExample") )
    GarbageCollector.eval
  }

  it should "insert a value into an existing set" in {
    Assign("set_t6", SetIdentifier( Value(10) )).eval
    InsertInto( Variable("set_t6"), Value(20)).eval
    assert( Variable("set_t6").eval == Set(10, 20) )
    GarbageCollector.eval
  }

  it should "insert multiple values and/or variables into an existing set" in {
    Assign("set_t7", SetIdentifier( Value(10) )).eval
    Assign("var_t7", Value(50)).eval
    InsertInto( Variable("set_t7"), Value(20), Value(30), Variable("var_t7") ).eval
    assert( Variable("set_t7").eval == Set(10, 20, 30, 50) )
    GarbageCollector.eval
  }

  it should "delete a value from an existing set" in {
    Assign("set_t8", SetIdentifier( Value(10) )).eval
    DeleteFrom( Variable("set_t8"), Value(10)).eval
    assert( Variable("set_t8").eval == Set() )
    GarbageCollector.eval
  }

  it should "not delete a value from an existing set if that value is not there" in {
    Assign("set_t9", SetIdentifier( Value(10) )).eval
    DeleteFrom( Variable("set_t9"), Value(20)).eval
    assert( Variable("set_t9").eval == Set(10) )
    GarbageCollector.eval
  }

  it should "delete multiple values and/or variables from an existing set" in {
    Assign("var_t10", Value(50)).eval
    Assign("set_t10", SetIdentifier( Value(10), Value(20), Value(30), Variable("var_t10") )).eval
    DeleteFrom( Variable("set_t10"), Value(10), Value(20) ).eval
    assert( Variable("set_t10").eval == Set(30, 50) )
    GarbageCollector.eval
  }

  it should "create a named scope and insert a variable of outer scope to the set" in {
    Assign("var_t11", Value(50)).eval
    Assign("set_t11", SetIdentifier( Value(10) )).eval
    NamedScope("scope_11",
      InsertInto( Variable("set_t11"), Variable("var_t11"))
    ).eval
    assert( Variable("set_t11").eval == Set(10, 50) )
    GarbageCollector.eval
  }

  it should "create nested scopes and insert a variable of outer scope to the set" in {

    Assign("set_t13", SetIdentifier( Value(10) )).eval
    NamedScope("scope_13",
      Assign("var_t13", Value(50)),
      UnnamedScope(
        InsertInto( Variable("set_t13"), Variable("var_t13"))
      )
    ).eval
    assert( Variable("set_t13").eval == Set(10, 50) )
    GarbageCollector.eval
  }

  it should "create scopes whose bindings can be accessed again using the same names" in {

    Assign("set_t14", SetIdentifier( Value(10) )).eval
    NamedScope("scope_14",
      Assign("var_t14", Value(50))
    ).eval

    NamedScope("scope_14",
      InsertInto( Variable("set_t14"), Variable("var_t14"))
    ).eval
    assert( Variable("set_t14").eval == Set(10, 50) )
    GarbageCollector.eval
  }

  it should "evaluate the Union of two sets" in {
    Assign("set_15", Union( SetIdentifier( Value(1), Value(2) ), SetIdentifier(Value(3), Value(4) ) )).eval
    assert( Variable("set_15").eval == Set(1, 2, 3, 4) )
    GarbageCollector.eval
  }

  it should "evaluate the Intersection of two sets" in {
    Assign("set_16", Intersection( SetIdentifier( Value(1), Value(2) ), SetIdentifier(Value(2), Value(3) ) )).eval
    assert( Variable("set_16").eval == Set(2) )
    GarbageCollector.eval
  }

  it should "evaluate the Set Difference of two sets" in {
    Assign("set_17", SetDifference( SetIdentifier( Value(1), Value(2) ), SetIdentifier(Value(2), Value(3) ) )).eval
    assert( Variable("set_17").eval == Set(1) )
    GarbageCollector.eval
  }

  it should "evaluate the Symmetric Difference of two sets" in {
    Assign("set_18", SymDifference( SetIdentifier( Value(1), Value(2) ), SetIdentifier(Value(2), Value(3) ) )).eval
    assert( Variable("set_18").eval == Set(1, 3) )
    GarbageCollector.eval
  }

  it should "evaluate the Cartesian Product of two sets" in {
    Assign("set_19", CartesianProduct( SetIdentifier( Value(1), Value(2) ), SetIdentifier(Value(3), Value(4) ) )).eval
    assert( Variable("set_19").eval == Set((1,3), (1, 4), (2, 3), (2, 4)) )
    GarbageCollector.eval
  }


}