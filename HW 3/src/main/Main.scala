package com.hw3

// My stuff
import java.io.File

object Main extends App {

    implicit val system = ActorSystem("Hw3System")
    implicit val materializer = ActorMaterializer()
    import system.dispatcher

    val masterActor = system.actorOf(Props[MasterActor], name = "master")

    val bar = "\n**----------------------------------------------------------------------------**\n"
    val greeting:String = bar.concat("\nHello! From the following list:\n".concat(languages.foldRight("")(_+", "+_)))

    var lang = ""
    /*
    */
    while(!isLegalLang(lang)) {
        try {
            println(greeting)
            lang = StdIn.readLine("Please enter Language: > ")
            if (!isLegalLang(lang)) println(s"Im sorry, but $lang is not a valid entry! Try again...")
        } catch {
            case e: NullPointerException => {
                println("Thanks for playing!")
            }
        }
    }

    println(s"Great! Looking for Repositories written in $lang now...")
    masterActor ! Language(lang)

    //     Sleep for a moment
    Thread.sleep(300000)
    println("Thanks for playing!")
    // Shut the system down
    Http().shutdownAllConnectionPools().onComplete(_ => system.terminate())


}


