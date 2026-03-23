package org.simplemodeling.SimpleModeler.transformer.scala

import org.goldenport.RAISE
import org.goldenport.context.Consequence
import org.goldenport.util.StringUtils
import org.simplemodeling.model._
import org.simplemodeling.SimpleModeler.generator.scala.model._
import org.simplemodeling.SimpleModeler.transformer.scala.ScalaModelTransformer.Purpose
import org.simplemodeling.SimpleModeler.transformers.scala._

/*
 * @since   Sep. 19, 2025
 *  version Sep. 29, 2025
 *  version Nov. 11, 2025
 *  version Feb. 27, 2026
 * @version Mar. 24, 2026
 * @author  ASAMI, Tomoharu
 */
abstract class ScalaModelTransformer() extends PartialFunction[(MObject, ScalaModelTransformer.Purpose), Consequence[Vector[SClassBase]]] {
  protected def is_Accept_Object(p: MObject): Boolean
  protected def accept_Purposes: Vector[Purpose]
  protected def sub_Package_Name: Option[String] = None

  protected final def is_accept_purpose(p: Purpose): Boolean = 
    accept_Purposes.contains(p)

  def isDefinedAt(p: (MObject, Purpose)): Boolean =
    is_Accept_Object(p._1) && is_accept_purpose(p._2)

  protected final def to_scala_core_with_subpackage(p: MObject): ClassCore =
    to_scala_core(p, sub_Package_Name)

  protected final def to_scala_core(p: MObject, subpkg: Option[String]): ClassCore =
    subpkg.fold(to_scala_core(p))(to_scala_core_subpackage(p, _))

  protected final def to_scala_core_subpackage(
    p: MObject,
    subpkg: String
  ): ClassCore = to_scala_core(p).moveToSubPackage(subpkg)

  protected final def to_scala_core(p: MObject): ClassCore = {
    val packagename = PackageName(p.packageName)
    val declaration = to_scala_core_declaration(p)
    val classname = ClassName(p.name)
    val parentclass = to_scala_core_parent(p)
    val traits = to_scala_core_traits(p)
    val attrs = effective_attributes(p)
    val parameters = to_parameters(attrs)
    val fields = to_fields(attrs)
    val methods = to_methods(p.operations)
    val receptions = ReceptionCompartment.empty // TODO
    val directive = Directive.default.withCanonicalSchemaOwner(
      TypeName.Plain(packagename, p.name)
    )
    ClassCore(
      packagename,
      declaration,
      classname,
      parentclass,
      traits,
      parameters,
      fields,
      methods,
      receptions,
      directive
    )
  }

  protected def effective_attributes(p: MObject): List[MAttribute] =
    _collect_effective_attributes(p, Set.empty)

  private def _collect_effective_attributes(
    p: MObject,
    visited: Set[String]
  ): List[MAttribute] = {
    val key = p.qualifiedName
    if (visited.contains(key))
      p.attributes
    else {
      val baseattrs = p.base.flatMap(ScalaModelTransformer.resolveObject(_, p)).
        map(_collect_effective_attributes(_, visited + key)).
        getOrElse(Nil)
      _merge_attributes(baseattrs, p.attributes)
    }
  }

  private def _merge_attributes(
    base: List[MAttribute],
    own: List[MAttribute]
  ): List[MAttribute] = {
    val m = scala.collection.mutable.LinkedHashMap[String, MAttribute]()
    base.foreach(x => m.update(x.name, x))
    own.foreach(x => m.update(x.name, x))
    m.values.toList
  }

  protected def to_scala_core_declaration(p: MObject) = p match {
    case m: MComponent => ClassDeclaration.Control
    case _ => ClassDeclaration.CaseClass
  }

  protected def to_scala_core_parent(p: MObject) = p match {
    case m: MComponent =>
      if (p.base.isEmpty)
        Some(TypeName(PackageName("org.goldenport.cncf.component"), "Component"))
      else
        to_scala_core_parent_default(p)
    case _ => to_scala_core_parent_default(p)
  }

  protected final def to_scala_core_parent_default(p: MObject) =
    p.base.flatMap(_to_parent_type(p, _))

  protected def to_scala_core_traits(p: MObject) = 
    p.traits.map(_to_type)

  private def _to_parent_type(
    scope: MObject,
    p: MObjectRef
  ): Option[TypeName] =
    if (_is_simple_entity(p.targetName)) {
      Some(TypeName(PackageName("org.simplemodeling.model"), "SimpleEntity"))
    } else {
      Some(TypeName(PackageName(p.targetPackageName), p.targetName))
    }

  private def _is_simple_entity(p: String): Boolean =
    p.equalsIgnoreCase("SimpleEntity") || p.equalsIgnoreCase("simple_entity")

  private def _to_type(p: MTraitRef): TypeName =
    TypeName(PackageName(p.packageRef.packageName), p.name)

  protected def to_parameters(ps: List[MAttribute]): ParameterSequence =
    ParameterSequence(ps.toVector.flatMap(to_parameter_required))

  protected def to_parameter_required(p: MAttribute): Option[Parameter] =
    if (p.isRequired)
      Some(to_parameter(p))
    else
      None

  protected def to_parameter(p: MAttribute): Parameter = {
    val typename = to_typename(p)
    val dbcolumnname = p.column.flatMap(x => Option(x.sql.name).map(_.trim).filterNot(_.isEmpty))
    val dbcolumntype = p.column.flatMap(_.sql.datatype.map(_.fullName))
    val externalname = p.column.flatMap(_.aliases.headOption).map(_.trim).filterNot(_.isEmpty)
    Parameter(
      ParameterName(p.name),
      typename,
      isAttribute = true,
      isDefault = false,
      dbColumnName = dbcolumnname,
      dbColumnType = dbcolumntype,
      externalName = externalname
    )
  }

  protected def to_fields(ps: List[MAttribute]): FieldCompartment =
    FieldCompartment(ps.toVector.flatMap(x => Vector(to_field(x))))

  protected def to_field(p: MAttribute): Field = {
    val typename = to_typename(p)
    Field(FieldName(p.name), typename)
  }

  protected def to_attributes(ps: List[MAttribute]): AttributeSequence =
    AttributeSequence(ps.toVector.map(to_attribute))

  protected def to_attribute(p: MAttribute): Attribute = {
    val typename = to_typename(p)
    val dbcolumnname = p.column.flatMap(x => Option(x.sql.name).map(_.trim).filterNot(_.isEmpty))
    val dbcolumntype = p.column.flatMap(_.sql.datatype.map(_.fullName))
    val externalname = p.column.flatMap(_.aliases.headOption).map(_.trim).filterNot(_.isEmpty)
    Attribute(AttributeName(p.name), typename, dbcolumnname, dbcolumntype, externalname)
  }

  def to_typename(p: MAttribute): TypeName = p.multiplicity match {
    case MOne => _typename_one(p.attributeType)
    case MZeroOne => _typename_zeroone(p.attributeType)
    case MOneMore => _typename_zeromore(p.attributeType)
    case MZeroMore => _typename_onemore(p.attributeType)
    case m: MRange => _typename_range(p.attributeType)
    case m: MRanges => _typename_ranges(p.attributeType)
  }

  private def _typename_one(p: MAttributeType): TypeName =
    _to_typename(p)

  private def _typename_zeroone(p: MAttributeType): TypeName =
    TypeName.Container(
      TypeName.Plain(PackageName("scala"), "Option"),
      _to_typename(p)
    )

  protected def container_type_zeromore: TypeName =
    TypeName.Plain(PackageName("scala.collection.immutable"), "Vector")

  protected def container_type_onemore: TypeName =
    TypeName.Plain(PackageName("cats.data"), "NonEmptyVector")

  private def _typename_zeromore(p: MAttributeType): TypeName =
    TypeName.Container(container_type_zeromore, _to_typename(p))

  private def _typename_onemore(p: MAttributeType): TypeName =
    TypeName.Container(container_type_onemore, _to_typename(p))

  private def _typename_range(p: MAttributeType): TypeName =
    _typename_zeromore(p) // TODO

  private def _typename_ranges(p: MAttributeType): TypeName =
    _typename_zeromore(p) // TODO

  private def _to_typename(p: MAttributeType): TypeName =
    p match {
      case m: MDataType => to_typename(m)
      case m: MObject => to_typename(m)
      case m: MObjectRef => to_typename(m)
    }

  final protected def to_methods(ps: List[MOperation]): MethodCompartment = {
    val xs = ps.toVector.map(to_method)
    MethodCompartment(xs)
  }

  final protected def to_method(p: MOperation): SMethod = {
    val params = ParameterSequence(p.parameters.map(to_parameter).toVector)
    val result = to_result(p.result)
    val descriptor = to_descriptor(p.descriptor)
    SMethod(MethodName(p.name), descriptor, params, result, p.body)
  }

  final protected def to_parameter(p: MParameter): Parameter = {
    import MParameter._
    val t0 = p.parameterType match {
      case MDataTypeParameterType(dt) => to_typename(dt)
      case MObjectParameterType(o) => to_typename(o)
      case MObjectRefParameterType(ref) => to_typename(ref)
    }
    val tname = p.multiplicity match {
      case MZeroOne => TypeName.option(t0)
      case _ => t0
    }
    val value = _get_value(p.parameterType)
    Parameter(ParameterName(p.name), tname, value = value)
  }

  private def _get_value(p: MParameter.MParameterType): Option[SClassBase] =
    p match {
      case MParameter.MObjectParameterType(o) => _from_object(o)
      case _ => None
    }

  private def _from_object(p: MObject): Option[SClassBase] = p match {
    case m: MEntityValue => Some(_from_entity_value(m))
    case m: MTypedObject => m.typeParameters.head match {
      case MTypedObject.Slot.ObjectBody(o) => _from_object(o)
      case _ => None
    }
    case m: MValue => None
    case m: MEntity => None
    case _ => None
  }

  private def _from_entity_value(p: MEntityValue): SClassBase = 
    p.kind match {
      case MEntityValue.Kind.Create =>
        val tx = new EntityValueCreateScalaModelTransformer()
        _to_class(tx(p.entity))
      case MEntityValue.Kind.Save =>
        val tx = new EntityValueCreateScalaModelTransformer() // CHECK
        _to_class(tx(p.entity))
      case MEntityValue.Kind.Update =>
        val tx = new EntityValueUpdateScalaModelTransformer()
        _to_class(tx(p.entity))
      case MEntityValue.Kind.Query =>
        val tx = new EntityValueQueryScalaModelTransformer()
        _to_class(tx(p.entity))
      case MEntityValue.Kind.Whole =>
        val tx = new EntityValueReadScalaModelTransformer()
        _to_class(tx(p.entity))
      case MEntityValue.Kind.Aggregate =>
        val tx = new EntityValueAggregateScalaModelTransformer()
        _to_class(tx(p.entity))
      case MEntityValue.Kind.View =>
        val tx = new EntityValueViewScalaModelTransformer()
        _to_class(tx(p.entity))
      case MEntityValue.Kind.Summary =>
        val tx = new EntityValueReadScalaModelTransformer() // CHECK
        _to_class(tx(p.entity))
    }

  private def _to_class(p: Consequence[Vector[SClassBase]]): SClassBase =
    p.take.head

  final protected def to_result(p: MResult): TypeName = {
    import MResult._
    p.resultType match {
      case MUnitResultType() => TypeName.Unit()
      case MDataTypeResultType(dt) => to_typename(dt)
      case MObjectResultType(o) => to_typename(o)
      case MObjectRefResultType(ref) => to_typename(ref)
    }
  }

  protected def to_typename(p: MDataType): TypeName =
    TypeName.create(p.datatype)

  final protected def to_typename(o: MObject): TypeName = o match {
    case m: MTypedObject => to_typename(m)
    case _ => TypeName.create(o.packageName, o.name)
  }

  final protected def to_typename(o: MTypedObject): TypeName = {
    val contenee = to_typename(o.typeParameters.head)
    val container = TypeName.create(o.packageName, o.name)
    TypeName.Container(container, contenee)
  }

  final protected def to_typename(o: MTypedObject.Slot): TypeName = o match {
    case MTypedObject.Slot.ObjectRef(ref) => to_typename(ref)
    case MTypedObject.Slot.ObjectBody(o) => to_typename(o)
  }

  final protected def to_typename(o: MObjectRef): TypeName =
    TypeName.create(o.packageName, o.objectName)

  final protected def to_descriptor(p: MOperation.Descriptor): SMethod.Descriptor = {
    SMethod.Descriptor(
      p.kind match {
        case MOperation.Kind.Query => SMethod.Kind.Query
        case MOperation.Kind.Command => SMethod.Kind.Command
      }
    )
  }

  final protected def make_title_name(
    p: String,
    ps: String*
  ): String = {
    val raw = (p +: ps).map(StringUtils.makeTitle).mkString
    if (raw.length <= 32)
      raw
    else
      raw.take(32)
  }

  protected def project_dir = "scala.d"

  protected def src_main = s"${project_dir}/src/main"

  protected def src_main_scala = s"$src_main/scala"

  protected def package_To_Pathname(p: MObject): String = {
    s"${src_main_scala}/${p.packageName.replace('.', '/')}"
  }

  protected def object_To_Pathname(p: MObject): String = {
    s"${package_To_Pathname(p)}/${p.name}.scala"
  }
}

object ScalaModelTransformer {
  private val _object_registry =
    scala.collection.concurrent.TrieMap.empty[String, MObject]
  private val _object_registry_by_name =
    scala.collection.concurrent.TrieMap.empty[String, Vector[MObject]]

  def clearObjectRegistry(): Unit = synchronized {
    _object_registry.clear()
    _object_registry_by_name.clear()
  }

  def registerObject(p: MObject): Unit = synchronized {
    _object_registry.update(p.qualifiedName, p)
    val xs = _object_registry_by_name.getOrElse(p.name, Vector.empty)
    val ys = (xs.filterNot(_.qualifiedName == p.qualifiedName) :+ p)
    _object_registry_by_name.update(p.name, ys)
  }

  def resolveObject(ref: MObjectRef, scope: MObject): Option[MObject] = {
    val qnamecandidates = _qualified_name_candidates(ref, scope)
    qnamecandidates.toStream.flatMap(_object_registry.get).headOption.orElse {
      _resolve_by_name(ref.objectName, ref.packageName, scope.packageName)
    }
  }

  private def _qualified_name_candidates(ref: MObjectRef, scope: MObject): Vector[String] = {
    val local = _qualified_name(scope.packageName, ref.objectName)
    val target = _qualified_name(ref.packageName, ref.objectName)
    Vector(local, target, ref.objectName).distinct
  }

  private def _qualified_name(pkg: String, name: String): String =
    if (pkg == null || pkg.isEmpty)
      name
    else
      s"$pkg.$name"

  private def _resolve_by_name(
    name: String,
    targetPackage: String,
    scopePackage: String
  ): Option[MObject] =
    _object_registry_by_name.get(name).flatMap {
      case Vector(single) =>
        Some(single)
      case xs =>
        xs.find(_.packageName == scopePackage).
          orElse(xs.find(_.packageName == targetPackage)).
          orElse(xs.headOption)
    }

  sealed trait Purpose
  object Purpose {
    val elements = Vector(
      Plain,
      Create,
      Read,
      Update,
      Delete,
      Operation,
      View,
      Aggregate,
      Query
    )

    case object Plain extends Purpose
    case object Create extends Purpose
    case object Read extends Purpose
    case object Update extends Purpose
    case object Delete extends Purpose
    case object Operation extends Purpose
    case object View extends Purpose
    case object Aggregate extends Purpose
    case object Query extends Purpose
  }
}
