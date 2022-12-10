
package object Utils {

    /**
      * Some "global values that will surely come in handy later
      */
    val token = "902ce489af3ba9806e067f1eea20b8f454940614" // This is my GitHub Personal OAuth token. Use it wisely
    val App_Start_Range = 52001  // not really sure what this is yet
    val languages = List("ada", "assembly", "c", "c++", "c#", "fortran", "java", "jovial", "delphi", "pascal", "pl", "m", "vhdl", "cobol", "php", "html", "css", "javascript", "python")
    val resourceDir = s"$pwd/src/main/resources/repositories"
    /**
      * Useful things to know for the future
      */
    /*
     * To
     *
     *
     * Tree: A tree file just represents the directory tree
     *
     * Blobs: A blob is literally a FILE, retrieving it in raw format retrieves the raw text data
     * In order to retrieve the blob in readable format use the following pattern
     * curl -i -u KrbAlmryde:VaZgxHQk2N --header "Accept: application/vnd.github-blob.raw" https://api.github.com/repos/ltg-uic/wallcology/git/blobs/14e0dcf717778ac59fc1060740df7e6ca13297aa
     *
     */

    /**
      * isLegalLang
      */
    def isLegalLang(lang: String): Boolean = languages.contains( lang.toLowerCase )


    /**
      * Enumeration alias to make identifying the Call and dependency graph easy
      */
    object GraphTypes extends Enumeration {
        val Call = Value(1)
        val Deps = Value(0)
    }


    def parseArguments(arg: String) = {
        arg.toLowerCase match {
            case _ =>
        }
    }


    /**
      * Convenience function which gives me the current working directory
      * @return
      */
    def pwd:String = System.getProperty("user.dir")



    /**
      * Name:
      *     ParseFilesInDir
      *
      * Description:
      *     Recursively parses Files in the local project Resources/ directory producing
      *     an array of Strings containing file paths to each of the source files
      *     found.
      *
      * Source:
      *     This function was adapted from the accepted answer of this StackOverflow question
      *     http://stackoverflow.com/questions/2637643/how-do-i-list-all-files-in-a-subdirectory-in-scala
      *
      * @param dir: a Java File object containing the source to the directory
      * @return Array[String]
      */
    def parseFilesInDir(dir: File): Array[File] = {
        val files = dir.listFiles
        val allFiles = files ++ files.filter(_.isDirectory).flatMap(parseFilesInDir)
        allFiles.filter( f => """.*\.java$""".r.findFirstIn(f.getName).isDefined)
    }



}
