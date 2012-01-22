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
  InputStream,
  FileInputStream,
  ObjectInputStream,
  OutputStream,
  FileOutputStream,
  ObjectOutputStream
}
import java.security.{
  MessageDigest,
  DigestInputStream
}

trait HomeFileStorage extends FileHashes {
  val location = {
    val home = System.getProperty("user.home")

    val base = new File(home, directoryName)

    if (!base.exists) {
      base.mkdir
    }

    base
  }

  def directoryName = ".lmxml"
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

  def outStream(file: File) = new FileOutputStream(hashedFile(file))

  def inStream(file: File) = new FileInputStream(hashedFile(file))

  def clear = {
    def recurse(file: File): Unit = {
      if (file.isDirectory())
        file.listFiles.foreach(recurse)

      file.delete
    }

    recurse(location)
    !location.exists
  }

  private def hashedFile(input: File) = {
    val folder = new File(location, hashFilename(input))

    if (!folder.exists) {
      folder.mkdirs
    }

    new File(folder, hashContents(input))
  }
}

trait HashLogic extends FileLoading {
  def retrieve(hash: String): Option[String]

  def clear: Boolean

  def inStream(file: File): InputStream

  def outStream(file: File): OutputStream

  def writeNodes(file: File, nodes: Seq[ParsedNode]) {
    val os = new ObjectOutputStream(outStream(file))

    nodes.foreach(os.writeObject)
    os.close()
  }

  def readNodes(file: File) = {
    val buffer = new scala.collection.mutable.ListBuffer[ParsedNode]

    val rs = new ObjectInputStream(inStream(file))
      
    try {
      while(rs.available > 0) {
        buffer += rs.readObject.asInstanceOf[ParsedNode]
      }
    } finally {
      rs.close()
    }

    buffer.toList
  }

  override def fromFile[A](file: File)(implicit converter: Seq[ParsedNode] => A) = {
    if (changed(file)) {
      import scala.io.Source.{fromFile => open}

      val text = open(file).getLines.mkString("\n")

      val parser = apply(text)

      val nodes = parser.parseNodes(text)

      writeNodes(file, nodes)

      converter(nodes)
    } else {
      converter(readNodes(file))
    }
  }

  def changed(file: File) = { 
    retrieve(hashFilename(file)).map(_ != hashContents(file)).getOrElse(true)
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

      md.update(byteNames, 0, byteNames.length)
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
