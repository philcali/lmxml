package lmxml
package cache

import java.io.{
  File,
  InputStream,
  FileInputStream,
  BufferedInputStream,
  OutputStream,
  FileOutputStream
}

import java.security.{
  MessageDigest,
  DigestInputStream
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

trait SerialStreams[A] extends LmxmlStorage[A] {
  val packer: DualPacker

  def outStream(source: A): OutputStream
  def inStream(source: A): InputStream

  def store(source: A, nodes: Seq[ParsedNode]) = {
    packer.serialize(nodes, outStream(source))
  }

  def retrieve(source: A) = packer.unserialize(inStream(source))
}

trait HashStorage[A] extends LmxmlStorage[A] {
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

trait LmxmlStorage[A] {
  def store(source: A, nodes: Seq[ParsedNode]): Unit
  def retrieve(source: A): Seq[ParsedNode]

  def changed(source: A): Boolean
  def contains(source: A): Boolean

  def remove(source: A): Unit
  def clear(): Unit
}
