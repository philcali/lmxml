package lmxml
package cache

/*
source changed?
     |
     |
  -------
  |     |
 No    Yes---------------
  |                     |
  |                     |
  |                     -----parse, serialize, md5, save, convert
Load from
cache, deserialize,
and convert
*/
import java.io.{
  File, 
  FileInputStream
}
import java.security.{
  MessageDigest,
  DigestInputStream
}

object BasicFileStorage extends FileHashes {
}

trait FileHashes extends HashLogic {
  val location: File

  require(location.isDirectory)

  def retrieve(hash: String) = {
    val file = new File(location, hash)

    if (!file.exists || file.isFile)
      None
    else
      file.listFiles.find(_.isFile).map(_.getName)
  }

  def store(file: File) {
    // Serialize
    
  }

  def data(file: File) {
  }
}

trait HashLogic extends FileLoading {
  def retrieve(hash: String): Option[String]

  def clear: Boolean

  override def fromFile[A](file: File)(implicit converter: Seq[ParsedNode] => A) = {
    if (changed(file)) {
    }

    import scala.io.Source.{fromFile => open}

    val text = open(file).getLines.mkString("\n")

    convert(text)(converter)
  }

  def changed(file: File) = { 
    retrieve(hashFilename(file)).map(_ != (hashContents(file)).getOrElse(true)
  }

  def hashContents(file: File) = {
    hash { md =>
      val ins = new FileInputStream(file)

      val di = new DigestInputStream(ins, md)

      try {
        while (di.read() > 0) {
          di.read()
        }
      } finally {
        di.close()
      }
    }
  }

  def hashFilename(file: File) = {
    hash { md =>
      val byteNames = file.getAbsolutePath.getBytes

      md.update(byteNames, 0, byteNames.length
    }
  }

  def hash(fun: MessageDigest => Unit) = {
    val md = MessageDigest.getInstance("MD5")

    fun(md)

    val digest = md.digest()

    digest.foldLeft("") { (complete, byte) =>
      complete + Integer.toString((byte & 0xff) + 0x100, 16).substring(1)
    }
  }
}
