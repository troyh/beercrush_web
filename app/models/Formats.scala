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
	protected def applyValuesToXML(nodes: NodeSeq, elements: Map[String, (Elem) => NodeSeq]): NodeSeq = {
		(for (node <- nodes ++ elements.keySet.filter(e => !nodes.exists(n => e==n.label) ).map { e =>
			/* 
			 *	Add empty elements for all the ones in the list above that don't already exist
			 */
			Elem(null,e,Null,xml.TopScope)
		}) yield node match { 
			/*
			 * Iterate nodes, applying the supplied function if one is defined
			 */
			case elem @ Elem(prefix,label,attribs,scope,children @ _*) => {
				if (elements.get(label).isDefined)
					(elements.get(label).get)(elem.asInstanceOf[Elem])
				else
					node
			}
			case other => other
		}).flatten.filter(e => e.attributes.length > 0 || e.child.length > 0) // Strip out any empty elements
	}

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

