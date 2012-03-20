package lmxml
package markdown

import com.tristanhunt.knockoff.Discounter

import transforms.{
  Transform,
  Processor
}

trait MarkdownProcessor extends Processor with Discounter {
  def apply(transform: Transform, node: ParsedNode) =
    node.children.headOption.map {
      case TextNode(s, _, c) =>
        TextNode(toXHTML(knockoff(s)).toString, true, transform(c))
      case head => head
    } getOrElse {
      TextNode("", children = transform(node.children))
    }
}

object DefaultMarkdownProcessor extends MarkdownProcessor

object MarkdownConvert extends Transform("md" -> DefaultMarkdownProcessor)
