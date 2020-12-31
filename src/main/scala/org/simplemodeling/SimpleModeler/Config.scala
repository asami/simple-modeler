package org.simplemodeling.SimpleModeler

import org.goldenport.RAISE
import org.goldenport.i18n.I18NString
import org.goldenport.cli.{Config => CliConfig, Environment}
import org.goldenport.value._
// import org.goldenport.record.v3.{Table => _, _}
import org.goldenport.record.unitofwork.interpreter._
import org.goldenport.record.unitofwork.container.{Config => UConfig, ConfigBase}

/*
 * @since   Feb. 11, 2020
 * @version Feb. 11, 2020
 * @author  ASAMI, Tomoharu
 */
case class Config(
  cliConfig: CliConfig,
  serviceLogic: UnitOfWorkLogic,
  storeLogic: StoreOperationLogic,
  isLocation: Boolean = true
) extends ConfigBase {
}

object Config {
  def create(env: Environment): Config = {
    Config(
      env.config,
      UConfig.defaultServiceLogic,
      UConfig.defaultStoreLogic
    )
  }
}
