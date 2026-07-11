package org.simplemodeling.SimpleModeler.generators.scala

import org.simplemodeling.SimpleModeler.generator.SourceArtifacts
import org.simplemodeling.SimpleModeler.generator.scala.model.{SComponent, SMethod}

/*
 * Generates the public component API contract separately from the provider
 * implementation component. Consumers depend on the api package and CNCF SPI
 * runtime, not on handwritten provider implementation classes.
 *
 * @since   Jul. 11, 2026
 * @version Jul. 11, 2026
 * @author  ASAMI, Tomoharu
 */
object ComponentApiSourceGenerator {
  def generate(component: SComponent): SourceArtifacts =
    _providers(component).foldLeft(SourceArtifacts.empty) { (artifacts, definition) =>
      val apiname = _api_name(definition)
      val packagename = s"${component.packageName.name}.api"
      val path = s"${packagename.replace('.', '/')}/$apiname.scala"
      artifacts + SourceArtifacts.create(path, _source(component, definition, packagename, apiname))
    }

  private def _source(
    component: SComponent,
    definition: SComponent.ComponentServiceDefinition,
    packagename: String,
    apiname: String
  ): String = {
    val operations = component.services.find(x => _normalize(x.serviceName) == _normalize(definition.name))
      .map(_.methods).getOrElse(Vector.empty)
    val methods = operations.map(_method(component, _)).mkString("\n")
    val proxies = operations.map(_proxy(component, definition.name, _)).mkString("\n")
    val selectors = operations.map { operation =>
      s"""      org.goldenport.cncf.spi.SpiOperationSelector("${operation.name.name}", Some("${definition.name}"))"""
    }.mkString(",\n")
    val contractname = s"${component.componentName}.${definition.name}"
    s"""package $packagename
       |
       |import org.goldenport.Consequence
       |import org.goldenport.cncf.context.ExecutionContext
       |import org.goldenport.cncf.spi.*
       |
       |trait $apiname {
       |$methods
       |}
       |
       |object $apiname {
       |  val contract: SpiContract[$apiname] = SpiContract("$contractname", classOf[$apiname])
       |
       |  final case class Proxy(binding: ResolvedSpiBinding) extends $apiname {
       |$proxies
       |  }
       |
       |  object Provider extends SpiBoundProvider[$apiname] with SpiOperationProvider {
       |    def supports(contract: SpiContract[$apiname], selection: SpiSelection)(using ExecutionContext): Boolean =
       |      contract.name == $apiname.contract.name && contract.runtimeClass == classOf[$apiname]
       |
       |    def provideBound(binding: ResolvedSpiBinding)(using ExecutionContext): Consequence[$apiname] =
       |      Consequence.success(Proxy(binding))
       |
       |    def spiOperations(contract: String): Vector[SpiOperationSelector] =
       |      if (contract == $apiname.contract.name) Vector(
       |$selectors
       |      ) else Vector.empty
       |  }
       |
       |  final class Socket(
       |    override val spiSocketName: String = "default",
       |    override val spiRequired: Boolean = true
       |  ) extends SpiSocket[$apiname] {
       |    private var _service: Option[$apiname] = None
       |
       |    def spiContract: SpiContract[$apiname] = contract
       |    override def isSpiInstalled: Boolean = _service.nonEmpty
       |    def installSpi(service: $apiname): Unit = _service = Some(service)
       |    def serviceOption: Option[$apiname] = _service
       |    def service: $apiname =
       |      _service.getOrElse(throw new IllegalStateException(s"SPI socket is not installed: $$spiSocketName"))
       |  }
       |
       |  final class SocketSet(
       |    override val spiSocketName: String = "default",
       |    override val spiRequired: Boolean = false
       |  ) extends SpiSocketSet[$apiname] {
       |    private var _members: Vector[ResolvedSpiMember[$apiname]] = Vector.empty
       |
       |    def spiContract: SpiContract[$apiname] = contract
       |    def spiMembers: Vector[ResolvedSpiMember[$apiname]] = _members
       |    def installSpiMembers(members: Vector[ResolvedSpiMember[$apiname]]): Unit = _members = members
       |  }
       |}
       |""".stripMargin
  }

  private def _method(
    component: SComponent,
    operation: SMethod
  ): String = {
    val input = _input_type(component, operation)
    val output = _output_type(component, operation)
    s"  def ${operation.name.name}(request: $input)(using ExecutionContext): Consequence[$output]"
  }

  private def _proxy(
    component: SComponent,
    servicename: String,
    operation: SMethod
  ): String = {
    val input = _input_type(component, operation)
    val output = _output_type(component, operation)
    val request = if (_is_record(input)) "request" else "request.toRecord()"
    val response = if (_is_record(output)) "Consequence.success(record)" else s"$output.createC(record)"
    s"""    def ${operation.name.name}(request: $input)(using ExecutionContext): Consequence[$output] =
       |      binding.invoke(SpiOperationSelector("${operation.name.name}", Some("$servicename")), $request).flatMap { record =>
       |        $response
       |      }""".stripMargin
  }

  private def _input_type(component: SComponent, operation: SMethod): String =
    _operation(component, operation).map(_.inputType).filter(_.trim.nonEmpty)
      .map(_value_type(component, _)).getOrElse("org.goldenport.record.Record")

  private def _output_type(component: SComponent, operation: SMethod): String =
    _operation(component, operation).map(_.outputType).filter(_.trim.nonEmpty)
      .map(_value_type(component, _)).getOrElse("org.goldenport.record.Record")

  private def _operation(
    component: SComponent,
    operation: SMethod
  ): Option[SComponent.OperationDefinition] =
    component.operationDefinitions.find(x => _normalize(x.name) == _normalize(operation.name.name))

  private def _value_type(component: SComponent, name: String): String = {
    val value = name.trim
    if (_is_record(value)) "org.goldenport.record.Record"
    else if (value.contains(".")) value
    else s"${component.packageName.name}.value.$value"
  }

  private def _is_record(name: String): Boolean =
    name == "Record" || name == "org.goldenport.record.Record"

  private def _providers(component: SComponent): Vector[SComponent.ComponentServiceDefinition] =
    component.componentDefinitions.flatMap(_.services).filter { definition =>
      definition.spiDirection.equalsIgnoreCase("provides") && definition.spiSocket
    }

  private def _api_name(definition: SComponent.ComponentServiceDefinition): String = {
    val stem = definition.spiApiName.map(_.trim).filter(_.nonEmpty).getOrElse(_title(definition.name))
    if (stem.endsWith("Api")) stem else s"${stem}Api"
  }

  private def _title(name: String): String =
    Option(name).getOrElse("").split("[^A-Za-z0-9]+").toVector.filter(_.nonEmpty).map { token =>
      token.head.toUpper + token.tail
    }.mkString

  private def _normalize(name: String): String =
    Option(name).getOrElse("").toLowerCase(java.util.Locale.ROOT).replaceAll("[^a-z0-9]", "")
}
