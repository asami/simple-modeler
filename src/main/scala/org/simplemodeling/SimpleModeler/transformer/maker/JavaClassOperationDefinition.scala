package org.simplemodeling.SimpleModeler.transformer.maker

import scalaz._, Scalaz._
import com.asamioffice.goldenport.text.UString.notNull
import org.simplemodeling.model._
import java.text.SimpleDateFormat
import java.util.TimeZone

/*
 * Java Class Operation Definition
 * 
 * @since   Nov. 12, 2012
 *  version Dec.  1, 2012
 *  version Mar.  2, 2020
 * @version May.  1, 2020
 * @author  ASAMI, Tomoharu
 */
class JavaClassOperationDefinition(
  pContext: PContext,
  model: PModel,
  aspects: Seq[JavaAspect],
  op: POperation,
  owner: JavaClassDefinition,
  jmaker: JavaMaker
) extends GenericClassOperationDefinition(pContext, model, aspects, op, owner) with JavaMakerHolder {
  jm_open(jmaker, aspects)

  private def _dt2docname(t: PVoucherType): String = {
    Option(t.voucher).fold("UnknownVoucher-" + t.name)(_.name)
  }

  val methodname = op.name
  val resultType = op.out.map(_dt2docname)
  val paramType = op.in.map(_dt2docname)
  val effectiveResultTypeName = resultType | "void"
  val effectiveParamLists = paramType.map(x => {
    x + " in"
  }) | ""

  def method {
    jm_public_method("%s %s(%s)", effectiveResultTypeName, methodname,
                     effectiveParamLists) {
      jm_UnsupportedOperationException
    }
  }
}
