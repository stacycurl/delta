package sjc.delta.std

import sjc.delta.Delta

import scala.annotation.tailrec
import scala.xml._


object xml {
  implicit val nodeDelta: Delta.Aux[Node, NodePatch] = new NodeDelta
  implicit val elemDelta: Delta.Aux[Elem, NodePatch] = nodeDelta.contramap[Elem](e ⇒ e)

  class NodeDelta extends Delta[Node] {
    type Out = NodePatch

    def apply(beforeN: Node, afterN: Node): Out = NodePatch(recurse(Context(Nil), (beforeN, afterN)).toList).reduce

    private def recurse(context: Context, beforeAfter: (Node, Node)): Stream[SingleNodePatch] = beforeAfter match {
      case (beforeE: Elem, afterE: Elem) if beforeE.label != afterE.label ⇒ context.diff(beforeE, afterE)

      case (beforeE: Elem, afterE: Elem) ⇒ {
        lazy val (beforeA, afterA) = beforeE.attributeMap disjoint afterE.attributeMap

        if (beforeA != afterA) context.diff(beforeE.withAttributes(beforeA), afterE.withAttributes(afterA)) else {
          childElems(beforeE).zipExact(childElems(afterE)) match {
            case (zipped, unzipped) ⇒ {
              val res = zipped.toStream.flatMap(recurse(context + beforeE, _))

              unzipped match {
                case None ⇒ res
                case Some(Left(missing)) ⇒ res ++ (context + beforeE).missing(missing)
                case Some(Right(extra)) ⇒ res ++ (context + beforeE).extra(extra)
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

  case class BeforeAfter(path: String, before: Node, after: Node) extends SingleNodePatch {
    def asXml: Node = <diff context={path}><before>{before}</before><after>{after}</after></diff>

    val reduce: PartialFunction[SingleNodePatch, SingleNodePatch] = {
      case BeforeAfter(`path`, before2, after2) if after2.xml_sameElements(before) ⇒ copy(before = before2)
    }
  }

  object Extra {
    def create(path: String, extra: Node*): Extra = new Extra(path, extra)
  }

  case class Extra(path: String, extra: NodeSeq) extends SingleNodePatch {
    def asXml: Node = <diff context={path}><before></before><after>{extra}</after></diff>

    val reduce: PartialFunction[SingleNodePatch, SingleNodePatch] = {
      case BeforeAfter(`path`, before, after) if before.xml_sameElements(extra) ⇒ copy(extra = after)
    }
  }

  object Missing {
    def create(path: String, missing: Node*): Missing = new Missing(path, missing)
  }

  case class Missing(path: String, missing: NodeSeq) extends SingleNodePatch {
    def asXml: Node = <diff context={path}><before>{missing}</before><after></after></diff>

    val reduce: PartialFunction[SingleNodePatch, SingleNodePatch] = {
      case BeforeAfter(`path`, before, after) if after.xml_sameElements(missing) ⇒ copy(missing = before)
    }
  }


  private case class Context(elements: List[String]) {
    def +(elem: Elem): Context = copy(elem.label :: elements)

    def diff(before: Node, after: Node): Stream[SingleNodePatch] = Stream(BeforeAfter(path, before, after))
    def missing(missing: NodeSeq): Stream[SingleNodePatch] = Stream(Missing(path, missing))
    def extra(extra: NodeSeq): Stream[SingleNodePatch] = Stream(Extra(path, extra))

    private def path = "/" + elements.reverse.mkString("/")
  }

  private implicit class DeltaListOps[A](list: List[A]) {
    import scala.collection.immutable.{::, List, Nil}

    // delta is simpler if it has no dependencies, including the brilliant pimpathon (ahem)
    def zipExact[B](other: List[B]): (List[(A, B)], Option[Either[List[A], List[B]]]) = {
      @tailrec
      def recurse(as: List[A], bs: List[B], abs: List[(A,B)]): (List[(A,B)], Option[Either[List[A], List[B]]]) = {
        (as, bs) match {
          case (l :: left, r :: right) ⇒ recurse(left, right, (l, r) :: abs)
          case (Nil, Nil)              ⇒ (abs.reverse, None)
          case (left, Nil)             ⇒ (abs.reverse, Some(Left(left)))
          case (Nil, right)            ⇒ (abs.reverse, Some(Right(right)))
        }
      }

      recurse(list, other, Nil)
    }
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
    def contains(kv: (String, String)): Boolean = values.get(kv._1) == Some(kv._2)

    def metaData: MetaData = values.foldLeft(Null: MetaData) {
      case (acc, (key, value)) ⇒ new UnprefixedAttribute(key, Text(value), acc)
    }
  }

  private def childElems(elem: Elem): List[Node] = elem.child.toList.filter {
    case e: Elem ⇒ true
  }
}
