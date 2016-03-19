package sjc.delta.std

import sjc.delta.Delta
import sjc.delta.util.DeltaListOps

import scala.annotation.tailrec
import scala.xml._


object xml extends xml("left", "right") {
  object beforeAfter    extends xml("before", "after")
  object actualExpected extends xml("actual", "expected")
}

case class xml(lhsName: String, rhsName: String) {
  implicit val nodeDelta: Delta.Aux[Node, NodePatch] = new NodeDelta
  implicit val elemDelta: Delta.Aux[Elem, NodePatch] = nodeDelta.contramap[Elem](e ⇒ e)

  class NodeDelta extends Delta[Node] {
    type Out = NodePatch

    def apply(leftN: Node, rightN: Node): Out = NodePatch(recurse(Context(Nil), (leftN, rightN)).toList).reduce

    private def recurse(context: Context, leftRight: (Node, Node)): Stream[SingleNodePatch] = leftRight match {
      case (leftEE: Elem, rightE: Elem) if leftEE.label != rightE.label ⇒ context.diff(leftEE, rightE)

      case (leftE: Elem, rightE: Elem) ⇒ {
        lazy val (leftA, rightA) = leftE.attributeMap disjoint rightE.attributeMap

        if (leftA != rightA) context.diff(leftE.withAttributes(leftA), rightE.withAttributes(rightA)) else {
          childElems(leftE).zipExact(childElems(rightE)) match {
            case (zipped, unzipped) ⇒ {
              val res = zipped.toStream.flatMap(recurse(context + leftE, _))

              unzipped match {
                case None ⇒ res
                case Some(Left(missing)) ⇒ res ++ (context + leftE).missing(missing)
                case Some(Right(extra)) ⇒ res ++ (context + leftE).extra(extra)
              }
            }
          }
        }
      }
    }
  }

  case class NodePatch(diffs: List[SingleNodePatch]) {
    def asXml: Node = <diffs>{diffs.map(_.asXml)}</diffs>

    def reduce: NodePatch = NodePatch(diffs.foldLeft(Stream.empty[SingleNodePatch]) {
      case (previous #:: rest, diff) ⇒ diff.reduce.lift(previous).fold(Stream(diff, previous))(Stream(_)) #::: rest
      case (rest, diff)              ⇒ diff #:: rest
    }.reverse.toList)
  }

  trait SingleNodePatch {
    def asXml: Node

    val reduce: PartialFunction[SingleNodePatch, SingleNodePatch]
  }

  case class Changed(path: String, left: Node, right: Node) extends SingleNodePatch {
    def asXml: Node = <diff context={path}>{leftElem(left)}{rightElem(right)}</diff>

    val reduce: PartialFunction[SingleNodePatch, SingleNodePatch] = {
      case Changed(`path`, left2, right2) if right2.xml_sameElements(left) ⇒ copy(left = left2)
    }
  }

  object Extra {
    def create(path: String, extra: Node*): Extra = new Extra(path, extra)
  }

  case class Extra(path: String, extra: NodeSeq) extends SingleNodePatch {
    def asXml: Node = <diff context={path}>{leftElem()}{rightElem(extra: _*)}</diff>

    val reduce: PartialFunction[SingleNodePatch, SingleNodePatch] = {
      case Changed(`path`, left, right) if left.xml_sameElements(extra) ⇒ copy(extra = right)
    }
  }

  object Missing {
    def create(path: String, missing: Node*): Missing = new Missing(path, missing)
  }

  case class Missing(path: String, missing: NodeSeq) extends SingleNodePatch {
    def asXml: Node = <diff context={path}>{leftElem(missing: _*)}{rightElem()}</diff>

    val reduce: PartialFunction[SingleNodePatch, SingleNodePatch] = {
      case Changed(`path`, left, right) if right.xml_sameElements(missing) ⇒ copy(missing = left)
    }
  }


  private case class Context(elements: List[String]) {
    def +(elem: Elem): Context = copy(elem.label :: elements)

    def diff(left: Node, right: Node): Stream[SingleNodePatch] = Stream(Changed(path, left, right))
    def missing(missing: NodeSeq): Stream[SingleNodePatch] = Stream(Missing(path, missing))
    def extra(extra: NodeSeq): Stream[SingleNodePatch] = Stream(Extra(path, extra))

    private def path = "/" + elements.reverse.mkString("/")
  }

  private implicit class ElemOps(elem: Elem) {
    def attributeMap: Attributes = new Attributes(
      elem.attributes.foldAttributes[Map[String, String]](Map())(key ⇒ value ⇒ map ⇒ map + ((key, value)))
    )

    def withAttributes(attributes: Attributes): Elem = elem.copy(attributes = attributes.metaData)
  }

  private implicit class MetaDataOps(metaData: MetaData) {
    def foldAttributes[A](zero: A)(f: String ⇒ String ⇒ A ⇒ A): A = {
      @tailrec def recurse(acc: A, current: MetaData): A = current match {
        //              case PrefixedAttribute(_, key, Text(value), next) ⇒ recurse(f(key)(value)(acc), next)
        case UnprefixedAttribute(key, Text(value), next)  ⇒ recurse(f(key)(value)(acc), next)
        case Null ⇒ acc
      }

      recurse(zero, metaData)
    }
  }

  private case class Attributes(values: Map[String, String]) {
    def disjoint(other: Attributes): (Attributes, Attributes) = (this -- other, other -- this)
    def --(other: Attributes): Attributes = new Attributes(values.filterNot(other.contains))
    def contains(kv: (String, String)): Boolean = values.get(kv._1).contains(kv._2)

    def metaData: MetaData = values.foldLeft(Null: MetaData) {
      case (acc, (key, value)) ⇒ new UnprefixedAttribute(key, Text(value), acc)
    }
  }

  private def childElems(elem: Elem): List[Node] = elem.child.toList.filter {
    case e: Elem ⇒ true
  }

  private def leftElem(children: Node*): Elem = elem(lhsName, children: _*)
  private def rightElem(children: Node*): Elem = elem(rhsName, children: _*)

  private def elem(label: String, children: Node*): Elem =
    Elem(null, label, Null, TopScope, children.isEmpty, children: _*)
}
