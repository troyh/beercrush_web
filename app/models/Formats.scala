package BeerCrush

import play.api.libs.json._
import scala.xml.{Node, NodeSeq, Elem, Null}

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
	protected def applyValuesToXML(nodes: NodeSeq, elements: Map[String, (Node) => NodeSeq]): NodeSeq = {
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
					(elements.get(label).get)(elem)
				else
					node
			}
			case other => other
		}).flatten.filter(e => e.attributes.length > 0 || e.child.length > 0) // Strip out any empty elements
	}
}

