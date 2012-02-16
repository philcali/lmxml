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
  val storage: FileHashStorage

  override def fromFile[A](file: File)(implicit converter: Seq[ParsedNode] => A) = {
    if (storage.changed(file)) {
      import scala.io.Source.{fromFile => open}

      val text = open(file).getLines.mkString("\n")

      val parser = apply(text)

      val nodes = parser.parseNodes(text)

      storage.store(file, nodes)

      converter(nodes)
    } else {
      converter(storage.retrieve(file))
    }
  }
}

trait MemoryLoading[A] extends SerialStreams[A] {
  val memory = collection.mutable.HashMap[String, Seq[ParsedNode]]()

  val timer = new java.util.Timer()

  def key(source: A): String

  protected def expiresEvery(interval: Long) {
    timer.scheduleAtFixedRate(new java.util.TimerTask{
      def run() = memory.clear()
    }, 0, interval)
  }

  override def store(source: A, nodes: Seq[ParsedNode]) {
    memory(key(source)) = nodes

    super.store(source, nodes)
  }

  override def retrieve(source: A) = {
    val k = key(source)
    memory.get(k).getOrElse {
      val nodes = super.retrieve(source)
      memory(k) = nodes
      nodes
    }
  }
}

class FileStorage(val location: File) extends FileHashStorage with SerialStreams[File] {
  val packer = DefaultPacker

  def check(hash: String) = {
    val file = new File(location, hash)

    if (!file.exists || file.isFile)
      None
    else
      file.listFiles.find(_.isFile).map(_.getName)
  }

  def contains(file: File) = new File(location, hashFilename(file)).exists

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

  def outStream(file: File) = new FileOutputStream(hashedFile(file, true))

  def inStream(file: File) = new FileInputStream(hashedFile(file))

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

trait FileHashStorage extends HashStorage[File] {
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

  def hashFilename(file: File) = hashString(file.getAbsolutePath)
}

trait SerialStreams[A] extends HashStorage[A] {
  val packer: DualPacker

  def outStream(source: A): OutputStream
  def inStream(source: A): InputStream

  def store(source: A, nodes: Seq[ParsedNode]) = {
    packer.serialize(nodes, outStream(source))
  }

  def retrieve(source: A) = packer.unserialize(inStream(source))
}

trait Packer {
  def serialize(nodes: Seq[ParsedNode], out: OutputStream)
}

trait Unpacker {
  def unserialize(in: InputStream): Seq[ParsedNode]
}

trait DualPacker extends Packer with Unpacker

object DefaultPacker extends DualPacker {
  def serialize(nodes: Seq[ParsedNode], out: OutputStream) {
    val os = new ObjectOutputStream(out)

    try {
      os.writeObject(nodes)
    } finally {
      os.close()
    }
  }

  def unserialize(in: InputStream) = {
    val buffer = new BufferedInputStream(in)

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
      rs.readObject.asInstanceOf[Seq[ParsedNode]]
    } finally {
      rs.close()
    }
  }
}

trait HashStorage[A] {
  def store(source: A, nodes: Seq[ParsedNode]): Unit
  def retrieve(source: A): Seq[ParsedNode]

  def changed(source: A): Boolean
  def contains(source: A): Boolean

  def remove(source: A): Unit
  def clear(): Unit

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
