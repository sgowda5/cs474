import MySetTheoryDSL.*
import MySetTheoryDSL.setExp.*
import MySetTheoryDSL.classExp.*
import MySetTheoryDSL.classBodyExp.*
import MySetTheoryDSL.assignRHS.*
import org.scalatest.funsuite.AnyFunSuite

class ClassesTest extends AnyFunSuite {
  test("Basic Classes Test") {
    // Assign(a,NewObject("mycoolclass"))
    ClassDef("animal",Extends(None),
      Constructor(
        //Assign(Field("length"),Value(7),
        //Assign(Field("width"),Value(5)))
      ),
        Field("length"),
        Field("width")).eval()
    ClassDef("dog",Extends(Some("animal")),Constructor(AssignField("dog","tail_length",Set(Value(5)))),Field("tail_length"),Method("eat",Insert(Value("food")))).eval()
    ClassDef("beagle",Extends(Some("dog")),Constructor(),Field("4")).eval()
    ClassDef("daisy",Extends(Some("beagle")),Constructor(),Field("4"),Method("eat",Insert(Value("bacon")))).eval()

    Assign("dog1",CreateObject("daisy")).eval()

    Assign("myAnimal",CreateObject("dog")).eval()
    println(InvokeMethod("dog1","eat").eval())
    //println(InvokeMethod("myAnimal","eat").eval())
    println(GetField("dog1","tail_length").eval())
    println(GetField("myAnimal","tail_length").eval())

  }


}
