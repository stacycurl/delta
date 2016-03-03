package sjc.delta.std

import org.junit.Test
import sjc.delta.TestUtil
import scala.xml.{Utility, Node}

import sjc.delta.Delta.DeltaOps
import sjc.delta.std.xml.{nodeDelta, BeforeAfter, Missing, Extra, NodePatch, SingleNodePatch}


class NodeTest extends TestUtil {
  @Test def nodeDeltaTest(): Unit = {
    test(
      <abc/>,
      <abc/>,
      Nil: _*
    )

    test(
      <abc/>,
      <def/>,
      BeforeAfter("/", <abc/>, <def/>)
    )

    test(
      <abc name="foo"/>,
      <abc name="bar"/>,
      BeforeAfter("/", <abc name="foo"/>, <abc name="bar"/>)
    )

    test(
      <abc name="foo" def="def"/>,
      <abc def="def" name="bar"/>,
      BeforeAfter("/", <abc name="foo"/>, <abc name="bar"/>)
    )

    test(
      <parent><abc/></parent>,
      <parent><def/></parent>,
      BeforeAfter("/parent", <abc/>, <def/>)
    )

    test(
      <parent><child><abc/></child></parent>,
      <parent><child><def/></child></parent>,
      BeforeAfter("/parent/child", <abc/>, <def/>)
    )

    test(
      <parent><abc/><def/></parent>,
      <parent><abc/><ghi/></parent>,
      BeforeAfter("/parent", <def/>, <ghi/>)
    )

    test(
      <parent><def/><abc/></parent>,
      <parent><ghi/><abc/></parent>,
      BeforeAfter("/parent", <def/>, <ghi/>)
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
      BeforeAfter("/parent/first",  <abc/>, <ghi/>),
      BeforeAfter("/parent/second", <def/>, <jkl/>)
    )

    test(
      <parent><first><abc/></first><second></second></parent>,
      <parent><first><ghi/></first><second><jkl/></second></parent>,
      BeforeAfter("/parent/first",  <abc/>, <ghi/>),
      Extra("/parent/second", <jkl/>)
    )

    test(
      <parent><first><abc/></first><second><def/></second></parent>,
      <parent><first><ghi/></first><second></second></parent>,
      BeforeAfter("/parent/first",  <abc/>, <ghi/>),
      Missing("/parent/second", <def/>)
    )
  }

  @Test def xmlPatchReduce(): Unit = {
    NodePatch(List(BeforeAfter("path", <abc/>, <def/>), BeforeAfter("path", <def/>, <ghi/>))).reduce shouldEqual
      NodePatch(List(BeforeAfter("path", <abc/>, <ghi/>)))

    NodePatch(List(BeforeAfter("path", <def/>, <ghi/>), Missing("path", <ghi/>))).reduce shouldEqual
      NodePatch(List(Missing("path", <def/>)))

    NodePatch(List(BeforeAfter("path", <ghi/>, <def/>), Extra("path", <ghi/>))).reduce shouldEqual
      NodePatch(List(Extra("path", <def/>)))
  }


  private def test(before: Node, after: Node, expected: SingleNodePatch*): Unit =
    (before delta after).asXml.shouldEqualBy(Utility.trim, NodePatch(expected.toList).asXml)
}
