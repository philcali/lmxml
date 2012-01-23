package lmxml
package cache

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

trait FileHashes extends HashLogic[File] with FileLoading {
  val location: File

  def changed(file: File) = { 
    retrieve(hashFilename(file)).map(_ != hashFileContents(file)).getOrElse(true)
  }

  def retrieve(hash: String) = {
    val file = new File(location, hash)

    if (!file.exists || file.isFile)
      None
    else
      file.listFiles.find(_.isFile).map(_.getName)
  }

  def outStream(file: File) = new FileOutputStream(hashedFile(file, true))

  def inStream(file: File) = new FileInputStream(hashedFile(file))

  def clear = {
    def recurse(file: File): Unit = {
      if (file.isDirectory())
        file.listFiles.foreach(recurse)

      file.delete
    }

    recurse(location)
  }

  private def hashedFile(input: File, removeOld: Boolean = false) = {
    val folder = new File(location, hashFilename(input))

    if (!folder.exists) {
      folder.mkdirs
    } else if(removeOld) {
      folder.listFiles.filter(_.isFile).foreach(_.delete)
    }

    new File(folder, hashFileContents(input))
  }

  def hashFileContents(file: File) = {
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
    hashString(file.getAbsolutePath)
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
}

trait HashLogic[A] {
  def clear: Unit 

  def inStream(source: A): InputStream

  def outStream(source: A): OutputStream

  def changed(source: A): Boolean

  def writeNodes(source: A, nodes: Seq[ParsedNode]): OutputStream = {
    val original = outStream(source)

    val os = new ObjectOutputStream(original)

    nodes.foreach(os.writeObject)
    os.close()

    original
  }

  def readNodes(source: A) = {
    val buffer = new scala.collection.mutable.ListBuffer[ParsedNode]

    val rs = new ObjectInputStream(inStream(source))
      
    try {
      while(rs.available > 0) {
        buffer += rs.readObject.asInstanceOf[ParsedNode]
      }
    } finally {
      rs.close()
    }

    buffer.toList
  }

  def hashString(contents: String) = {
    hash { md =>
      val bytes = contents.getBytes

      md.update(bytes, 0, bytes.length)
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
