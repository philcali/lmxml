package lmxml
package markdown

import com.tristanhunt.knockoff.Discounter

import transforms.{
  Transform,
  Processor
}

trait MarkdownProcessor extends Processor with Discounter {
  def apply(transform: Transform, node: ParsedNode) = node match {
    case TextNode(s, _, c) =>
      TextNode(toXHTML(knockoff(s)).toString, true, transform(c))
    // Pass
    case _ => node
  }
}

object DefaultMarkdownProcessor extends MarkdownProcessor

object MarkdownConvert
  extends Transform("[textNode]" -> DefaultMarkdownProcessor)
