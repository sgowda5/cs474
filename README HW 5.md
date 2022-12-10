Partial Evalutation
myDSL has been modified to allow Partial Evaluation. myDSL not returns Either[Any,Any]. Left[+A] returned denotes partially evaluated expression
Right[+A] returned denotes partially evaluated expression

Assume an If construct

If(
    Check(Var("a"), Var("b")),
    AnonScope(Var("x"), Var("y"), Value(3)),
    AnonScope(Insert(Var("a"), Value(4))),
  )
when evaluated in myDSL would return

  Left(
    If(
      Check(Var(a),Var(b)),
      AnonScope(ArraySeq(Var(x), Var(y), Value(3))),
      AnonScope(ArraySeq(Insert(Var(a),ArraySeq(Value(4)))))
    )
  )
Left denoting a partially evaluated If condition with

Assume values for Var("a") and Var("y")are provided

  DeclareVar("a", CreateSet(Value(1), Value(2), Value(3))).eval()
  DeclareVar("y", Value(2)).eval()
  Left(
        If(Check(Value(Set(1, 2, 3)),Var("b")),
          AnonScope(ArraySeq(Var("x"), Value(2), Value(3))),
          AnonScope(ArraySeq(Value(Set(1, 2, 3, 4)))))
      )
Monads
Monads can be used by invoking myDSLExpMonadand passing an Exp
Further the map function can be used to perform operations on the Exp passed

Take the following applicaation as an example:

def CartProductComputation(exp: Exp):Exp = {
    exp.eval() match{

      case Right(immutableSet: Set[Any])=>
        Value(Product(Value(immutableSet), Value(immutableSet)).eval().merge)
      case Left(value: Exp)=>
        Product(value,value)
    }
  }

  myDSLExpMonad(CreateSet(Value(1), Value(2), Value(3)))
    .map(CartProductComputation) // Value(Set((2,2), (2,1), (1,2), (1,1), (3,2), (3,1), (3,3), (2,3), (1,3)