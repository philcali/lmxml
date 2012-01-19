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

object HashLogic {
  def hash(file: File) = {
    val ins = new FileInputStream(file)

    val md = MessageDigest.getInstance("MD5")

    val di = new DigestInputStream(ins, md)

    try {
      while (di.read() > 0) {
        di.read()
      }
    } finally {
      di.close()
    }

    val digest = md.digest()

    digest.foldLeft("") { (complete, byte) =>
      complete + Integer.toString((byte & 0xff) + 0x100, 16).substring(1)
    }
  }
}
