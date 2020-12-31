package org.simplemodeling.SimpleModeler.transformer

import org.goldenport.realm.Realm
import org.goldenport.values.PathName
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
 * @version May.  4, 2020
 * @author  ASAMI, Tomoharu
 */
trait ProgramRealmTransformerBase {
  def context: PContext
  def fileSuffix: String

  def transform(model: PModel): TransformResult = {
    val realm = model.root.elements./:(Realm.Builder())(_build(model)).build
    TransformResult(realm)
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

  protected final def make_entity(model: PModel, p: PEntity): String = make_Entity(model, p)

  protected def make_Entity(model: PModel, p: PEntity): String

  private def _build_association(b: Realm.Builder, model: PModel, p: PAssociation) = {
    ???
  }

  protected final def code_pathname(p: PObject): PathName = {
    val pkgpath = package_file_pathname(p)
    val sourcename = code_filename(p)
    pkgpath :+ sourcename
  }

  protected final def code_filename(p: PObject): String = s"${p.classNameBase}.${fileSuffix}"

  protected final def package_file_pathname(p: PObject): PathName = package_File_Pathname(p)

  protected def package_File_Pathname(p: PObject): PathName

  /*
   * Legacy
   */
  def transform(model: SimpleModel): TransformResult = {
    // val realm = model.elements./:(Realm.Builder())(_build).build
    // TransformResult(realm)
    ???
  }

  private def _build(b: Realm.Builder, p: MElement): Realm.Builder = {
    p match {
      case m: MAssociation => _build_association(b, m)
      case m: MReference => b

      case m: MDatatype => b
      case m: MAttributeType => b

//      case m: MAttribute => b
//      case m: MOperation => b

//      case m: MClassRef => b
//      case m: MCollaboration => b
//      case m: MConstraint => b

      case m: MEntity => _build_entity(b, m)
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
    val pathname = package_to_pathname(p)
    b.set(pathname, make_entity(p))
  }

  protected final def make_entity(p: MEntity): String = make_Entity(p)

  protected def make_Entity(p: MEntity): String

  protected final def package_to_pathname(p: MObject): String = package_To_Pathname(p)

  protected def package_To_Pathname(p: MObject): String
}
