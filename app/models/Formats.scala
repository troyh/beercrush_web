package BeerCrush

import play.api.libs.json._
import scala.xml.{Node, NodeSeq, Elem, Text, Null}

trait JsonFormat {
	def toJSON: JsObject
}

trait XmlFormat {
	def toXML: xml.NodeSeq
	/**
	  * Applies values in this object to an existing XML, merging the two 
	  * versions. This is useful for saving edited documents -- any XML elements in 
	  * the original XML doc are left intact and only values from this object are 
	  * added/changed.
	  *
	  * Note: Empty elements are removed from the resulting XML.
      *
      * @param nodes The XML to be transformed with this object's data
      * @return The new XML document
	  */
	def transform(nodes: NodeSeq): NodeSeq 

	/**
	 * Used for subclasses to implement transform() easily
	 */
	class SuperNode(val node: Node) {
		def withMissingChildElements(labels: Seq[String]): Node = {
			val existingNodeLabels = node.child.filter(_.isInstanceOf[Elem]).map(_.label)
			node match {
				case Elem(prefix,label,attribs,scope,kids @ _*) => 
					Elem(prefix,label,attribs,scope,kids ++ labels.filterNot(existingNodeLabels.contains(_)).map(addlabel => Elem(null,addlabel,Null,xml.TopScope)) : _*)
					case other => other // Silently fail to add any missing elements
			}
		}
	}

	object SuperNode {
		implicit def node2SuperNode(node: Node): SuperNode = new SuperNode(node)
		implicit def SuperElem2Node(se: SuperNode) = se.node
	}
	
	import SuperNode._
	protected def replaceChildTextElem(elem: Elem, s: String) = elem.copy(child=
		for (e <- elem.withMissingChildElements(Seq("text")).child) yield e match {
			case t @ <text>{_*}</text> => t.asInstanceOf[Elem].copy(child=Text(s))
			case other => other
		}
	)

}

