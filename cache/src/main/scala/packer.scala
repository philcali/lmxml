package lmxml
package cache

import java.io.{
  InputStream,
  ObjectInputStream,
  BufferedInputStream,
  OutputStream,
  ObjectOutputStream
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
