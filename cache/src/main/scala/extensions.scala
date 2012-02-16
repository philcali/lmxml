package lmxml
package cache

import java.io.File

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
