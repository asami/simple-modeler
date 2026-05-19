package org.simplemodeling.SimpleModeler.transformer

import org.goldenport.RAISE
import org.goldenport.realm.Realm
import org.goldenport.values.PathName
import org.goldenport.tree.TreeNode
import org.goldenport.util.StringUtils
import org.simplemodeling.model._
import org.simplemodeling.SimpleModeler.transformer.maker._

/*
 * Derived from SimpleModel2JavaRealmTransformerBase.java (Feb. 3, 2011)
 * Derived from SimpleModel2ProgramRealmTransformerBase.scala (Apr.  7, 2012 - Jan. 29, 2013)
 * 
 * @since   Dec.  8, 2019
 *  version Dec. 15, 2019
 *  version Mar.  8, 2020
 *  version May.  4, 2020
 *  version May. 18, 2025
 *  version Sep. 21, 2025
 *  version Feb. 16, 2026
 *  version Mar. 25, 2026
 * @version May. 20, 2026
 * @author  ASAMI, Tomoharu
 */
trait ProgramRealmTransformerBase {
  def context: PContext
  def fileSuffix: String

  /*
   * Legacy
   */
  def transform(model: PModel): TransformResult = {
    val src = model.root.elements.foldLeft(Realm.Builder())(_build(model)).build
    val realm = Realm.create()
    val r = realm.merge(source_main_pathname, src)
    TransformResult(r)
  }

  private def _build(model: PModel)(b: Realm.Builder, p: PElement): Realm.Builder =
    p match {
      case m: PPackage => _build_package(b, model, m)
      case m: PEntity => _build_entity(b, model, m)
      case m: PAssociation => _build_association(b, model, m)
    }

  private def _build_package(b: Realm.Builder, model: PModel, p: PPackage) = {
    ???
  }

  private def _build_entity(b: Realm.Builder, model: PModel, p: PEntity) = {
    val pathname = code_pathname(p)
    b.set(pathname, make_entity(model, p))
  }

  protected final def make_entity(model: PModel, p: PEntity): String = make_entity_legacy(model, p)

  protected def make_entity_legacy(model: PModel, p: PEntity): String

  private def _build_association(b: Realm.Builder, model: PModel, p: PAssociation) = {
    ???
  }

  protected final def code_pathname(p: PObject): PathName = {
    val pkgpath = package_file_pathname(p)
    val sourcename = code_filename(p)
    pkgpath :+ sourcename
  }

  protected final def code_filename(p: PObject): String = s"${p.classNameBase}.${fileSuffix}"

  protected final def package_file_pathname(p: PObject): PathName = package_file_pathname_legacy(p)

  protected def package_file_pathname_legacy(p: PObject): PathName

  def transform(model: SimpleModel): TransformResult = {
    val realm = Realm.create()

    val whole = model.elements.foldLeft(Realm.Builder())(_build).build
    val (generated, impl) = _distill_realms(whole)
    val r1 = realm.merge(source_managed_main_pathname, generated)
    val r2 = r1.merge(source_main_pathname, impl)
    val r3 = _build_makefile(r2)
    val r = r3
    TransformResult(r)
  }

  private def _build(b: Realm.Builder, p: MElement): Realm.Builder = {
    p match {
      case m: MAssociation => _build_association(b, m)
      case m: MReference => b

      case m: MDataType => b
      case m: MAttributeType => b

//      case m: MAttribute => b
//      case m: MOperation => b

//      case m: MClassRef => b
//      case m: MCollaboration => b
//      case m: MConstraint => b

      case m: MEntity => _build_entity(b, m)
      case m: MValue => _build_value(b, m)
      case m: MPowertype => _build_powertype(b, m)
      case m: MStateMachine => _build_state_machine(b, m)
      case m: MComponent => _build_component(b, m)
      case m: MObject => b

//      case m: MMultiplicity => b
//      case m: MObjectRef => b
//      case m: MPackageRef => b
//      case m: MPowertypeKind => b
//      case m: MPowertypeRef => b
//      case m: MStateMachineRef => b
//      case m: MStereotype => b
//      case m: MTraitRef => b

      case m: MElement => b
    }
  }

  private def _build_association(b: Realm.Builder, p: MAssociation): Realm.Builder = {
    b
  }

  private def _build_entity(b: Realm.Builder, p: MEntity): Realm.Builder = {
    build_entity(b, p)
    // val pathname = object_to_pathname(p)
    // b.set(pathname, make_entity(p))
  }

  protected def build_entity(b: Realm.Builder, p: MEntity): Realm.Builder

  private def _build_value(b: Realm.Builder, p: MValue): Realm.Builder =
    build_value(b, p)

  protected def build_value(b: Realm.Builder, p: MValue): Realm.Builder = b

  private def _build_powertype(b: Realm.Builder, p: MPowertype): Realm.Builder =
    build_powertype(b, p)

  protected def build_powertype(b: Realm.Builder, p: MPowertype): Realm.Builder = b

  private def _build_state_machine(b: Realm.Builder, p: MStateMachine): Realm.Builder =
    build_state_machine(b, p)

  protected def build_state_machine(b: Realm.Builder, p: MStateMachine): Realm.Builder = b

  // protected final def make_entity(p: MEntity): String = make_entity_legacy(p)

  // protected def make_entity_legacy(p: MEntity): String

  protected def source_main_pathname: String

  protected def source_managed_main_pathname: String = source_main_pathname

  protected final def package_to_pathname(p: MObject): String = package_pathname(p)

  protected def package_pathname(p: MObject): String

  protected final def object_to_pathname(p: MObject): String = object_pathname(p)

  protected def object_pathname(p: MObject): String

  private def _build_makefile(b: Realm): Realm =
    build_makefile(b)

  protected def build_makefile(b: Realm): Realm = RAISE.notImplementedYetDefect

  private def _build_component(b: Realm.Builder, p: MComponent): Realm.Builder =
    build_component(b, p)

  protected def build_component(b: Realm.Builder, p: MComponent): Realm.Builder = RAISE.notImplementedYetDefect

  private def _distill_realms(p: Realm): (Realm, Realm) = {
    val splitter = new ProgramRealmTransformerBase.Splitter()
    p.traverse(splitter)
    splitter.result
  }
}

object ProgramRealmTransformerBase {
  class Splitter() extends Realm.Visitor {
    private val _generated = Realm.Builder()
    private val _impl = Realm.Builder()

    def result: (Realm, Realm) = (_generated.build(), _impl.build())

    override def enter(p: TreeNode[Realm.Data]): Unit =
      p.getContent foreach { x =>
        if (_is_impl(p))
          _impl.set(p.pathnameValue, x)
        else
          _generated.set(p.pathnameValue, x)
      }

    private def _is_impl(p: TreeNode[Realm.Data]): Boolean = {
      val pn = p.pathnameValue
      p.getParent.fold(false)(_.pathnameValue.lastComponent == "impl")
    }
  }
}
