

import org.scalatest.*
import org.scalatest.funspec.AnyFunSpec
import matchers.should.Matchers.*
import SetTheoryDSL.SetExpression.*
import SetTheoryDSL.{SetExpression, defaultIfElseOptimizer, defaultIfOptimizer}

import scala.collection.mutable

class PartialEvaluationDSLTest extends AnyFunSpec {
  describe("Testing partial evaluation of SetTheoryDSL") {

    describe("Testing basic set expressions with partial evaluation") {
      it("should evaluate the result to a hashset if all the elements are values only") {
        assert(SetIdentifier(Value("1"), Value("2")).evaluate == Set("1", "2"))
      }

      it("should return a SetExpression if there are undefined variables") {
        assert(SetIdentifier(Variable("h5c1"), Value("2")).evaluate == SetIdentifier(Variable("h5c1"), Value("2")) )
      }
    }

    describe("Testing set operations with partial evaluation") {

      describe("testing partial evaluation and optimization for Union operation") {
        it("should evaluate a Union operation expression to a value if there are no unknown variable") {
          assert( Union(SetIdentifier(Value(2)), SetIdentifier(Value(1))).evaluate == Set(1, 2) )
        }

        it("should evaluate a Union operation expression to a value if there are known variable") {
          Assign("h5c2_1", Value(2)).evaluate
          assert( Union(SetIdentifier(Variable("h5c2_1")), SetIdentifier(Value(1))).evaluate == Set(1, 2) )
        }

        it("should partially evaluate a Union operation expression to another expression if there are unknown variable in the expression") {
          assert( Union(SetIdentifier(Variable("h5c2")), SetIdentifier(Value(1))).evaluate == Union(SetIdentifier(Variable("h5c2")), SetIdentifier(Value(1))) )
        }

        it("should perform the optimizing transformation and reduce the partially evaluated expression of Union if second set is empty") {
          assert( Union(SetIdentifier(Variable("h5c3")), SetIdentifier()).evaluate == SetIdentifier(Variable("h5c3")) )
        }

        it("should perform the optimizing transformation and reduce the partially evaluated expression of Union if first set is empty") {
          assert( Union(SetIdentifier(), SetIdentifier(Variable("h5c3"))).evaluate == SetIdentifier(Variable("h5c3")) )
        }
      }

      describe("testing partial evaluation and optimization for intersection operation") {
        it("should evaluate a intersection operation expression to a value if there are no unknown variable") {
          assert( Intersection(SetIdentifier(Value(2)), SetIdentifier(Value(2))).evaluate == Set(2) )
        }

        it("should evaluate a intersection operation expression to a value if there are known variable") {
          Assign("h5c2_2", Value(2)).evaluate
          assert( Intersection(SetIdentifier(Variable("h5c2_2")), SetIdentifier(Value(1))).evaluate == Set() )
        }

        it("should partially evaluate a intersection operation expression to another Expression if there are unknown variable in the expression") {
          assert( Intersection(SetIdentifier(Variable("h5c4")), SetIdentifier(Value(1))).evaluate == Intersection(SetIdentifier(Variable("h5c4")), SetIdentifier(Value(1))) )
        }

        it("should perform the optimizing transformation and reduce the partially evaluated expression of intersection if second set is empty") {
          assert( Intersection(SetIdentifier(Variable("h5c5")), SetIdentifier()).evaluate == Set() )
        }

        it("should perform the optimizing transformation and reduce the partially evaluated expression of intersection if first set is empty") {
          assert( Intersection(SetIdentifier(), SetIdentifier(Variable("h5c5"))).evaluate == Set() )
        }
      }

      describe("testing partial evaluation and optimization for set difference operation") {
        it("should evaluate a set difference operation expression to a value if there are no unknown variable") {
          assert( SetDifference(SetIdentifier(Value(2)), SetIdentifier(Value(2))).evaluate == Set() )
        }

        it("should evaluate a set difference operation expression to a value if there are known variable") {
          Assign("h5c2_3", Value(2)).evaluate
          assert( SetDifference(SetIdentifier(Variable("h5c2_3")), SetIdentifier(Value(1))).evaluate == Set(2) )
        }

        it("should partially evaluate a set difference operation expression to another Expression if there are unknown variable in the expression") {
          assert( SetDifference(SetIdentifier(Variable("h5c4")), SetIdentifier(Value(1))).evaluate == SetDifference(SetIdentifier(Variable("h5c4")), SetIdentifier(Value(1))) )
        }

        it("should perform the optimizing transformation and reduce the partially evaluated expression of set difference if second set is empty") {
          assert( SetDifference(SetIdentifier(Variable("h5c5")), SetIdentifier()).evaluate == SetIdentifier(Variable("h5c5")) )
        }

        it("should perform the optimizing transformation and reduce the partially evaluated expression of set difference if first set is empty") {
          assert( SetDifference(SetIdentifier(), SetIdentifier(Variable("h5c5"))).evaluate == Set() )
        }
      }

      describe("testing partial evaluation and optimization for symmetric set difference operation") {
        it("should evaluate a symmetric set difference operation expression to a value if there are no unknown variable") {
          assert( SymDifference(SetIdentifier(Value(2)), SetIdentifier(Value(1))).evaluate == Set(1, 2) )
        }

        it("should evaluate a symmetric set difference operation expression to a value if there are known variable") {
          Assign("h5c2_4", Value(2)).evaluate
          assert( SymDifference(SetIdentifier(Variable("h5c2_4")), SetIdentifier(Value(1))).evaluate == Set(1, 2) )
        }

        it("should partially evaluate a symmetric set difference operation expression to another Expression if there are unknown variable in the expression") {
          assert( SymDifference(SetIdentifier(Variable("h5c4")), SetIdentifier(Value(1))).evaluate == SymDifference(SetIdentifier(Variable("h5c4")), SetIdentifier(Value(1))) )
        }

        it("should perform the optimizing transformation and reduce the partially evaluated expression of symmetric set difference if second set is empty") {
          assert( SymDifference(SetIdentifier(Variable("h5c5")), SetIdentifier()).evaluate == SetIdentifier(Variable("h5c5")) )
        }

        it("should perform the optimizing transformation and reduce the partially evaluated expression of symmetric set difference if first set is empty") {
          assert( SymDifference(SetIdentifier(), SetIdentifier(Variable("h5c5"))).evaluate == SetIdentifier(Variable("h5c5")) )
        }
      }

      describe("testing partial evaluation and optimization for cartesian product operation") {
        it("should evaluate a cartesian product operation expression to a value if there are no unknown variable") {
          assert( CartesianProduct(SetIdentifier(Value(2)), SetIdentifier(Value(1))).evaluate == Set((2, 1)) )
        }

        it("should evaluate a cartesian product operation expression to a value if there are known variable") {
          Assign("h5c2_5", Value(2)).evaluate
          assert( CartesianProduct(SetIdentifier(Variable("h5c2_5")), SetIdentifier(Value(1))).evaluate == Set((2, 1)) )
        }

        it("should partially evaluate a cartesian product operation expression to another Expression if there are unknown variable in the expression") {
          assert( CartesianProduct(SetIdentifier(Variable("h5c4")), SetIdentifier(Value(1))).evaluate == CartesianProduct(SetIdentifier(Variable("h5c4")), SetIdentifier(Value(1))) )
        }

        it("should perform the optimizing transformation and reduce the partially evaluated expression of cartesian product if second set is empty") {
          assert( CartesianProduct(SetIdentifier(Variable("h5c5")), SetIdentifier()).evaluate == Set() )
        }

        it("should perform the optimizing transformation and reduce the partially evaluated expression of cartesian product if first set is empty") {
          assert( CartesianProduct(SetIdentifier(), SetIdentifier(Variable("h5c5"))).evaluate == Set() )
        }
      }

    }

    describe("Testing partial evaluation of add and remove operation expressions of SetTheoryDSL") {

      it("evaluation of InsertInto expression with unknown variable expression inside") {
        assert( InsertInto(SetIdentifier(), Variable("h5c6")).evaluate == InsertInto(SetIdentifier(), Variable("h5c6")) )
      }

      it("evaluation of InsertInto expression with no variable expression inside") {
        assert( InsertInto(SetIdentifier(), Value("10")).evaluate == Set("10") )
      }

      it("evaluation of InsertInto expression with known variable expression inside") {
        Assign("h5c6", Value("20")).evaluate
        assert( InsertInto(SetIdentifier(), Variable("h5c6")).evaluate == Set("20") )
      }

      it("evaluation of DeleteFrom expression with unknown variable expression inside") {
        assert( DeleteFrom(SetIdentifier(), Variable("h5c7")).evaluate == DeleteFrom(SetIdentifier(), Variable("h5c7")) )
      }

      it("evaluation of DeleteFrom expression with no variable expression inside") {
        assert( DeleteFrom(SetIdentifier(Value("10")), Value("10")).evaluate == Set() )
      }

      it("evaluation of DeleteFrom expression with known variable expression inside") {
        Assign("h5c7", Value("20")).evaluate
        assert( DeleteFrom(SetIdentifier(Value("20")), Variable("h5c7")).evaluate == Set() )
      }
    }

    describe("Testing partial evaluation of If expression") {
      it("should partially evaluates the If expression to another expression if it contains unknown variables") {
        assert(
          If(Contains(SetIdentifier(Value(10)), Variable("h5_c8") ), Then(
            Union(SetIdentifier(Value(10)), SetIdentifier(Value(20)))
          ) ).evaluate == If(Contains(SetIdentifier(Value(10)), Variable("h5_c8") ), Then(
            Union(SetIdentifier(Value(10)), SetIdentifier(Value(20)))
          ) )
        )
      }

      it("should evaluates the If expression to a value if it contains no unknown variables and just constants") {
        assert(
          If(Contains(SetIdentifier(Value(10)), Value(10) ), Then(
            Union(SetIdentifier(Value(10)), SetIdentifier(Value(20)))
          ) ).evaluate == Set(10, 20)
        )
      }

      it("should evaluates the If expression to a value if it contains known variables and just constants") {
        Assign("h5c8", Value(10)).evaluate
        assert(
          If(Contains(SetIdentifier(Value(10)), Variable("h5c8") ), Then(
            Union(SetIdentifier(Value(10)), SetIdentifier(Value(20)))
          ) ).evaluate == Set(10, 20)
        )
      }

      it("should perform the optimization transformation and reduces the expression using partial evaluates if it contains unknown variables") {
        assert(
          If(Contains(SetIdentifier(Value(10)), Value(10) ), Then(
            Union(SetIdentifier(Value(10)), SetIdentifier(Variable("h5c9")))
          ) ).evaluate == UnnamedScope(
            Union(SetIdentifier(Value(10)), SetIdentifier(Variable("h5c9")))
          )
        )
      }

      it("should reduce the expression to a Unit value if condition is complete and evaluates to false") {
        assert(
          If(Contains(SetIdentifier(Value(10)), Value(20) ), Then(
            Union(SetIdentifier(Value(10)), SetIdentifier(Variable("h5c9")))
          ) ).evaluate == ()
        )
      }
    }

    describe("Testing partial evaluation of IfElse expression") {
      it("should partially evaluates the IfElse expression to another expression if it contains unknown variables") {
        assert(
          IfElse(Contains(SetIdentifier(Value(10)), Variable("h5_c8") ), Then(
            Union(SetIdentifier(Value(10)), SetIdentifier(Value(20)))
          ), Else(
            SetDifference(SetIdentifier(Value(10)), SetIdentifier(Value(20)))
          ) ).evaluate == IfElse(Contains(SetIdentifier(Value(10)), Variable("h5_c8") ), Then(
            Union(SetIdentifier(Value(10)), SetIdentifier(Value(20)))
          ), Else(
            SetDifference(SetIdentifier(Value(10)), SetIdentifier(Value(20)))
          ) )
        )
      }

      it("should evaluates the IfElse expression to a value of then block if it contains no unknown variables and just constants") {
        assert(
          IfElse(Contains(SetIdentifier(Value(10)), Value(10) ), Then(
            Union(SetIdentifier(Value(10)), SetIdentifier(Value(20)))
          ),  Else(
            SetDifference(SetIdentifier(Value(10)), SetIdentifier(Value(20)))
          ) ).evaluate == Set(10, 20)
        )
      }

      it("should evaluates the IfElse expression to a value of else block if it contains no unknown variables and just constants") {
        assert(
          IfElse(Contains(SetIdentifier(Value(10)), Value(20) ), Then(
            Union(SetIdentifier(Value(10)), SetIdentifier(Value(20)))
          ),  Else(
            SetDifference(SetIdentifier(Value(10)), SetIdentifier(Value(20)))
          ) ).evaluate == Set(10)
        )
      }

      it("should evaluates the IfElse expression to a value if it contains known variables and just constants") {
        Assign("h5c9", Value(10)).evaluate
        assert(
          IfElse(Contains(SetIdentifier(Value(10)), Variable("h5c9") ), Then(
            Union(SetIdentifier(Value(10)), SetIdentifier(Value(20)))
          ),  Else(
            SetDifference(SetIdentifier(Value(10)), SetIdentifier(Value(20)))
          )  ).evaluate == Set(10, 20)
        )
      }

      it("should perform the optimization transformation and reduces the IfElse expression using partial evaluates if it contains unknown variables") {
        assert(
          IfElse(Contains(SetIdentifier(Value(10)), Value(10) ), Then(
            Union(SetIdentifier(Value(10)), SetIdentifier(Variable("h5c10")))
          ), Else(
            SetDifference(SetIdentifier(Value(10)), SetIdentifier(Value(20)))
          ) ).evaluate == UnnamedScope(
            Union(SetIdentifier(Value(10)), SetIdentifier(Variable("h5c10")))
          )
        )

        assert(
          IfElse(Contains(SetIdentifier(Value(10)), Value(20) ), Then(
            Union(SetIdentifier(Value(10)), SetIdentifier(Variable("h5c10")))
          ), Else(
            SetDifference(SetIdentifier(Value(10)), SetIdentifier(Variable("h5c10")))
          ) ).evaluate == UnnamedScope(
            SetDifference(SetIdentifier(Value(10)), SetIdentifier(Variable("h5c10")))
          )
        )
      }

    }

    describe("Testing scoping constructs with partial evaluation") {

      it("should evaluate the named scopes when there are no variables in the expression") {
        assert(
          NamedScope("h5s1",
            Union(SetIdentifier(Value(1)), SetIdentifier(Value(2)))
          ).evaluate == Set(1, 2)
        )
      }

      it("should evaluate the unnamed scopes when there are no variables in the expression") {
        assert(
          UnnamedScope(
            Union(SetIdentifier(Value(1)), SetIdentifier(Value(2)))
          ).evaluate == Set(1, 2)
        )
      }

      it("should partially evaluate the named scopes to an expression when there are unknown variables in the expression") {
        assert(
          NamedScope("h5s2",
            Union(SetIdentifier(Value(1)), SetIdentifier(Variable("h5c10")))
          ).evaluate == NamedScope("h5s2",
            Union(SetIdentifier(Value(1)), SetIdentifier(Variable("h5c10")))
          )
        )
      }

      it("should partially evaluate the anonymous scopes to an expression when there are unknown variables in the expression") {
        assert(
          UnnamedScope(
            Union(SetIdentifier(Value(1)), SetIdentifier(Variable("h5c10")))
          ).evaluate == UnnamedScope(
            Union(SetIdentifier(Value(1)), SetIdentifier(Variable("h5c10")))
          )
        )
      }

      it("should optimize the partial evaluation of children expression of named scope expression") {
        assert(
          NamedScope("h5s3",
            Union(SetIdentifier(), SetIdentifier(Variable("h5c10")))
          ).evaluate == NamedScope("h5s3",
            SetIdentifier(Variable("h5c10"))
          )
        )
      }

      it("should optimize the partial evaluation of children expression of anonymous scope expression") {
        assert(
          UnnamedScope(
            Union(SetIdentifier(), SetIdentifier(Variable("h5c10")))
          ).evaluate == UnnamedScope(
            SetIdentifier(Variable("h5c10"))
          )
        )
      }
    }

    describe("Testing the map method for different expressions") {
      it("should map the If expression and apply a default transformer function to it") {
        assert(
          If(Contains(SetIdentifier(Value(10)), Value(10) ), Then(
            Union(SetIdentifier(Value(10)), SetIdentifier(Value(20)))
          ) ).map(defaultIfOptimizer) == UnnamedScope(
            Union(SetIdentifier(Value(10)), SetIdentifier(Value(20)))
          )
        )
      }

      it("should map the If expression and apply a default transformer function to it and then evaluate it") {
        assert(
          If(Contains(SetIdentifier(Value(10)), Value(10) ), Then(
            Union(SetIdentifier(Value(10)), SetIdentifier(Value(20)))
          ) ).map(defaultIfOptimizer).evaluate == Set(10, 20)
        )
      }

      it("should map the IfElse expression and apply a default transformer function to it") {
        assert(
          IfElse(Contains(SetIdentifier(Value(10)), Value(20) ), Then(
            Union(SetIdentifier(Value(10)), SetIdentifier(Value(20)))
          ), Else(
            CartesianProduct(SetIdentifier(Value(10)), SetIdentifier(Value(20)))
          ) ).map(defaultIfElseOptimizer) == UnnamedScope(
            CartesianProduct(SetIdentifier(Value(10)), SetIdentifier(Value(20)))
          )
        )
      }

      it("should map the IfElse expression and apply a default transformer function to it and then evaluate it") {
        assert(
          IfElse(Contains(SetIdentifier(Value(10)), Value(10) ), Then(
            Union(SetIdentifier(Value(10)), SetIdentifier(Value(20)))
          ), Else(
            CartesianProduct(SetIdentifier(Value(10)), SetIdentifier(Value(20)))
          ) ).map(defaultIfElseOptimizer).evaluate == Set(10, 20)
        )
      }

      it("should map one expression to another expression depending upon the transformer function specified by the user of DSL language") {
        // a SetExpression transformer that changes every Union expression to an empty set expression i.e. SetIdentifier()
        def unionToSetTransformer(exp: SetExpression): SetExpression = exp match {
          case Union(s1, s2) => SetIdentifier()
          case _ => exp
        }

        assert(
          UnnamedScope(
            Union(SetIdentifier(Value(1)), SetIdentifier(Variable("h5c12"))),
            Union(SetIdentifier(Value(2)), SetIdentifier(Variable("h5c13"))),
          ).map(
            unionToSetTransformer
          ) == UnnamedScope(
            SetIdentifier(),
            SetIdentifier()
          )
        )
      }

      it("should map one expression to another expression and then evaluate it depending upon the transformer function specified by the user of DSL language") {
        // a SetExpression transformer that changes every Union expression to an empty set expression i.e. SetIdentifier()
        def unionToSetTransformer(exp: SetExpression): SetExpression = exp match {
          case Union(s1, s2) => SetIdentifier()
          case _ => exp
        }

        assert(
          UnnamedScope(
            Union(SetIdentifier(Value(1)), SetIdentifier(Variable("h5c12"))),
            Union(SetIdentifier(Value(2)), SetIdentifier(Variable("h5c13"))),
          ).map(
            unionToSetTransformer
          ).evaluate == Set()
        )
      }

    }
  }
}
