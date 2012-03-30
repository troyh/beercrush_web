package BeerCrush

import play.api.libs.json._
import scala.xml.{Node, NodeSeq, Elem, Text, Null}
import scala.annotation._
import scala.collection.mutable.ArrayBuffer

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

	
	import SuperNode._
	protected def replaceChildTextElem(elem: Elem, s: String) = elem.copy(child=
		for (e <- elem.withMissingChildElements(Seq("text")).child) yield e match {
			case t @ <text>{_*}</text> => t.asInstanceOf[Elem].copy(child=Text(s))
			case other => other
		}
	)

}

object Utility {
	def formatDateISO8601(date: Option[java.util.Date]): String = 
		new java.text.SimpleDateFormat(BeerCrush.ISO8601DateFormat).format(date.getOrElse(new java.util.Date()))
}

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

	var stopit=false
	private def _recurseWhile(ns:  NodeSeq, f: (Elem,Int) =>  Unit, depth: Int)(test: Node => Boolean): Unit = {
		for (n <- ns) {
			if (stopit==false) {
				n match {
					case e: Elem => f(e,depth)
					case _ => // Ignore non-Elements
				}

				if (test(n)==false) { // Stop recursing
					stopit=true
					return
				}

				_recurseWhile(n.child,f,depth+1)(test)
			}
		}
	}
		
	def recurseWhile(f: (Elem,Int) => Unit)(test: Node => Boolean): Unit = _recurseWhile(this, f, 0)(test)

	import SuperNode._
	def getAncestors(root: Node): Seq[Node] = {
		var path=ArrayBuffer[Node]()

		// Recurse from root until we get to this node
		root.recurseWhile { (node,depth) =>
			path.reduceToSize(depth)
			path += node
		} (this.attribute("id") != _.attribute("id")) // FIXME: This is not generic
		
		path.toSeq
	}
}

object SuperNode {
	implicit def node2SuperNode(node: Node): SuperNode = new SuperNode(node)
	implicit def SuperNode2Node(se: SuperNode): Node = se.node
}
