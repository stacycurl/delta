package sjc.delta.std

import org.scalatest.{Matchers, FreeSpec}
import scala.xml.{Utility, Node}

import sjc.delta.Delta.DeltaOps
import sjc.delta.std.xml.{nodeDelta, Changed, Missing, Extra, NodePatch, SingleNodePatch}


class NodeTest extends FreeSpec with Matchers {
  "node" in {
    test(
      <abc/>,
      <abc/>,
      Nil: _*
    )

    test(
      <abc/>,
      <def/>,
      Changed("/", <abc/>, <def/>)
    )

    test(
      <abc name="foo"/>,
      <abc name="bar"/>,
      Changed("/", <abc name="foo"/>, <abc name="bar"/>)
    )

    test(
      <abc name="foo" def="def"/>,
      <abc def="def" name="bar"/>,
      Changed("/", <abc name="foo"/>, <abc name="bar"/>)
    )

    test(
      <parent><abc/></parent>,
      <parent><def/></parent>,
      Changed("/parent", <abc/>, <def/>)
    )

    test(
      <parent><child><abc/></child></parent>,
      <parent><child><def/></child></parent>,
      Changed("/parent/child", <abc/>, <def/>)
    )

    test(
      <parent><abc/><def/></parent>,
      <parent><abc/><ghi/></parent>,
      Changed("/parent", <def/>, <ghi/>)
    )

    test(
      <parent><def/><abc/></parent>,
      <parent><ghi/><abc/></parent>,
      Changed("/parent", <def/>, <ghi/>)
    )


    test(
      <parent><abc/></parent>,
      <parent></parent>,
      Missing("/parent", <abc/>)
    )

    test(
      <parent><abc/><def/></parent>,
      <parent></parent>,
      Missing.create("/parent", <abc/>, <def/>)
    )

    test(
      <parent><abc/><def/><ghi/></parent>,
      <parent><abc/><ghi/></parent>,
      Missing("/parent", <def/>)
    )


    test(
      <parent></parent>,
      <parent><def/></parent>,
      Extra("/parent", <def/>)
    )

    test(
      <parent></parent>,
      <parent><abc/><def/></parent>,
      Extra.create("/parent", <abc/>, <def/>)
    )

    test(
      <parent><abc/><ghi/></parent>,
      <parent><abc/><def/><ghi/></parent>,
      Extra("/parent", <def/>)
    )


    test(
      <parent><first><abc/></first><second><def/></second></parent>,
      <parent><first><ghi/></first><second><jkl/></second></parent>,
      Changed("/parent/first",  <abc/>, <ghi/>),
      Changed("/parent/second", <def/>, <jkl/>)
    )

    test(
      <parent><first><abc/></first><second></second></parent>,
      <parent><first><ghi/></first><second><jkl/></second></parent>,
      Changed("/parent/first",  <abc/>, <ghi/>),
      Extra("/parent/second", <jkl/>)
    )

    test(
      <parent><first><abc/></first><second><def/></second></parent>,
      <parent><first><ghi/></first><second></second></parent>,
      Changed("/parent/first",  <abc/>, <ghi/>),
      Missing("/parent/second", <def/>)
    )
  }

  "xmlPatchReduce" in {
    NodePatch(List(Changed("path", <abc/>, <def/>), Changed("path", <def/>, <ghi/>))).reduce shouldBe
      NodePatch(List(Changed("path", <abc/>, <ghi/>)))

    NodePatch(List(Changed("path", <def/>, <ghi/>), Missing("path", <ghi/>))).reduce shouldBe
      NodePatch(List(Missing("path", <def/>)))

    NodePatch(List(Changed("path", <ghi/>, <def/>), Extra("path", <ghi/>))).reduce shouldBe
      NodePatch(List(Extra("path", <def/>)))
  }

  private def test(left: Node, right: Node, expected: SingleNodePatch*): Unit =
    Utility.trim((left delta right).asXml) shouldBe Utility.trim(NodePatch(expected.toList).asXml)
}
