Libraries
Parsing Combinator Library was used to perform all the heavy-lifting of parsing the input string
No joke, thats it!
How do I get set up?
To run the application, navigate to the project directory:

sbt run
# OR 
sbt test
From there, you should see the following (assuming there were no exceptions)

λ -- Starting the Lambda calculus Interpreter -- λ

To use this tool type an expression such as the following:

      (lambda x.(lambda y.(x y)) 3) square
            (λx.(λy.(x y)) 3) square
            (\x.(\y.(x y)) 3) square

Available commands include:

     q | quit => Quit the application
     h | help => Display this help message again
     v | verb => Toggle each step in the evaluation.
     d | debug => Toggle LOTS of information about the processing steps. Do this at your own risk!
     def => Display user defined variables

λ ---------------------------------------------- λ

This will be followed immediately by a prompt

>:
Type your equation there! Note, It will continue to prompt you until you submit q | quit

For example

>: (λc.λd.λe.c d e) (λx.λy.x) a b
Sample output, assuming you do NOT set verbose mode would look like the following:

>: (λc.λd.λe.c d e) (λx.λy.x) a b
 ~  a  ~
Noting that a is the result of the expression

#####Verbose Mode

>: v
Verbose mode set to: true
>: (λc.λd.λe.c d e) (λx.λy.x) a b
=> (λc.λd.λe.c d e) (λx.λy.x) a b
=> (λd.λe.(λx.λy.x) d e) a b
=> (λe.(λx.λy.x) a e) b
=> (λx.λy.x) a b
=> (λy.a) b
=> a

 ~  a  ~
Defined Variables are:
Free Variables are: b, a
Bound Variables are: c, y, d, x, e 
#####Debug Mode

>: d
Debug mode set to: true
>: (λc.λd.λe.c d e) (λx.λy.x) a b
=>  λx.λy.x for c
=> *-β reduction-*
=>  a for d
=> *-β reduction-*
=>  b for e
=> *-β reduction-*
=>  a for x
=> *-β reduction-*
=> *-β reduction-*
~  a  ~
>:
#####Debug Mode AND Verbose Mode

>: (λc.λd.λe.c d e) (λx.λy.x) a b
=> (λc.λd.λe.c d e) (λx.λy.x) a b
=>  λx.λy.x for c
=> *-β reduction-*
=> (λd.λe.(λx.λy.x) d e) a b
=>  a for d
=> *-β reduction-*
=> (λe.(λx.λy.x) a e) b
=>  b for e
=> *-β reduction-*
=> (λx.λy.x) a b
=>  a for x
=> *-β reduction-*
=> (λy.a) b
=> *-β reduction-*
=> a
η(a)

~  a  ~
Defined Variables are:
Free Variables are: b, a
Bound Variables are: c, y, d, x, e
####Setting up from IntelliJ ####

If no project is currently open in IntelliJ IDEA, click Import Project on the Welcome screen. Otherwise, select File | New | Project from Existing Sources.

In the dialog that opens, select the directory that contains the project to be imported, or a file that contains an appropriate project description. Click OK.

On the first page of the Import Project wizard, select SBT, and click Next. (This page is not shown if IntelliJ IDEA has guessed what you are importing.)

On the next page of the wizard, specify SBT project settings and global SBT settings, click Finish.

Discussion
Unfortunately I had some trouble getting the parse to recognize symbols outside of Alpha-Numeric, so try not to input anything fancy like ````@ # $ % ^ & * + _ ``` I had some semblance of assignments and definitions but it broke the rest of the parser, so it had to be sacrificed! It operates on Normal Order reduction, and I would REALLY appreciate it if you tried to give it "proper input" See above if you arent sure what I mean by that. The parser will parse of course, but might not do as expected.. Feel free to check out some of the unit tests for fancy operations.

Unit Testing
For Unit-testing I utilized ScalaTest using the FunSuite. Its fun and was surprisingly simple to get it up and running. There are some half decent tests for once!

Go figure.

To run the tests, in Intellij simply type in the command line sbt test youll be up and running! Que sera