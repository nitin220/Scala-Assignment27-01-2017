import scala.io.Source
import java.io._

trait ProvideSource
{
	def getsourceFromSql{}
	def getSourceFromFile():Array[File]{}
	def capitalContent(){}
	def wordCount(){}
}
class Capitalize extends ProvideSource
{
	def getSourceFromFile():Array[File]=
	{
		val fileHere=(new File(".")).listFiles
		fileHere
	}
	override def capitalContent()
	{
		val filesHere=getSourceFromFile
		try{
			for(file<-filesHere)
			{
				if (file.getName.endsWith(".scala"))
				{
					val writer = new PrintWriter(new File(raw"F:\output2\$file"))
					for (line <- Source.fromFile(file)) {
						writer.write(line.toUpper)
					}
					writer.close()
				}	
			}
		}
		catch
		{
			case ex:FileNotFoundException=>println("File not found")
			case ex:IOException=>println("Input-Output error found")
		}
	}
}
class CountWords extends ProvideSource
{
	def getSourceFromFile():Array[File]=
	{
		val fileHere=(new File(".")).listFiles
		fileHere
	}
	override def wordCount()
	{
		val filesHere=getSourceFromFile
		try{
			for(file<-filesHere)
			{
				if (file.getName.endsWith(".scala"))
				{
					val res=getContent(file)
					val wordsCount=getCount(res)
					val (words,count)=wordsCount.unzip
					val stringToWrite=generateString("",words,count)
					val writer = new PrintWriter(new File(raw"F:\output\$file"))
					writer.write(stringToWrite)
					writer.close()
				}		
			}
		}
		catch{
			case ex:FileNotFoundException=>println("File not found")
			case ex:IOException=>println("Input-Output error found")
		}
		def getContent(file:File)=
		{
			//@throws(classOf[Exception])
			
			for {
				line <- Source.fromFile(file).getLines.toList
			}yield line.trim	
		}
		def getCount(list:List[String])=
		{
			val spitList=list.flatMap(x=>x.split("[\\W]+"))
			val setList=spitList.toSet
			val listSet=setList.toList
			val count=for{
					check<-listSet
				}yield spitList.count(_==check)
			val resultCount=listSet.zip(count)
			resultCount	
		}
		def generateString(result:String,list_String:List[String],list_Int:List[Int]):String=
		{
			((list_String,list_Int): @unchecked) match{
				case (Nil,Nil)=> result
				case (x::tail_1,y::tail_2)=>generateString(result+x+"="+y+" ",tail_1,tail_2)
			}
		}
	}

}

object ELTProcess extends App
{
	val obj_1=new CountWords()
	obj_1.wordCount()
	val obj_2=new Capitalize
	obj_2. capitalContent()
}