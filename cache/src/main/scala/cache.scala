package lmxml
package cache

import java.io.{
  File,
  InputStream,
  FileInputStream,
  ObjectInputStream,
  BufferedInputStream,
  OutputStream,
  FileOutputStream,
  ObjectOutputStream
}

import java.security.{
  MessageDigest,
  DigestInputStream
}

trait FileHashes extends FileLoading {
  val storage: FileHashLogic

  override def fromFile[A](file: File)(implicit converter: Seq[ParsedNode] => A) = {
    if (storage.changed(file)) {
      import scala.io.Source.{fromFile => open}

      val text = open(file).getLines.mkString("\n")

      val parser = apply(text)

      val nodes = parser.parseNodes(text)

      storage.writeNodes(file, nodes)

      converter(nodes)
    } else {
      converter(storage.readNodes(file))
    }
  }
}

class FileStorage(val location: File) extends FileHashLogic {
  def check(hash: String) = {
    val file = new File(location, hash)

    if (!file.exists || file.isFile)
      None
    else
      file.listFiles.find(_.isFile).map(_.getName)
  }

  def changed(file: File) = { 
    check(hashFilename(file)).map(_ != hashContents(file)).getOrElse(true)
  }

  def clear() = {
    def recurse(file: File): Unit = {
      if (file.isDirectory())
        file.listFiles.foreach(recurse)

      file.delete
    }

    recurse(location)
  }

  def remove(file: File) {
    location.listFiles.find(_ == hashFilename(file)).map { f =>
      f.listFiles.foreach(_.delete)
      f.delete
    }
  }

  def store(file: File) = new FileOutputStream(hashedFile(file, true))

  def retrieve(file: File) = new FileInputStream(hashedFile(file))

  private def hashedFile(input: File, removeOld: Boolean = false) = {
    val folder = new File(location, hashFilename(input))

    if (!folder.exists) {
      folder.mkdirs
    } else if(removeOld) {
      folder.listFiles.filter(_.isFile).foreach(_.delete)
    }

    new File(folder, hashContents(input))
  }
}

trait FileHashLogic extends HashLogic[File] {
  def hashContents(file: File) = hash { md =>
    val fis = new FileInputStream(file)

    val buffer = new BufferedInputStream(fis) 

    val di = new DigestInputStream(buffer, md)

    try {
      val bytes = new Array[Byte](1024)
      while(di.read(bytes) > 0) {
        di.read(bytes)
      }
    } finally {
      di.close()
    }
  }

  def hashFilename(file: File) = hashString(file.getName)
}

trait HashLogic[A] {
  def store(source: A): OutputStream
  def retrieve(source: A): InputStream
  def changed(source: A): Boolean
  def remove(source: A): Unit
  def clear(): Unit

  def writeNodes(source: A, nodes: Seq[ParsedNode]): OutputStream = {
    val original = store(source)

    val os = new ObjectOutputStream(original)

    try {
      os.writeObject(nodes)
    } finally {
      os.close()
    }

    original
  }

  def readNodes(source: A) = {
    val buffer = new BufferedInputStream(retrieve(source))

    val rs = new ObjectInputStream(buffer) {
      override def resolveClass(desc: java.io.ObjectStreamClass): Class[_] = {
        try {
          Class.forName(desc.getName, false, getClass.getClassLoader)
        } catch {
          case ex: java.lang.ClassNotFoundException =>
            super.resolveClass(desc)
        }
      }
    }
  
    try {
      rs.readObject.asInstanceOf[List[ParsedNode]]
    } finally {
      rs.close()
    }
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
