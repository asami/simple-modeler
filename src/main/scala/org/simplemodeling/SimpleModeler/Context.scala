package org.simplemodeling.SimpleModeler

import org.goldenport.Strings
import org.goldenport.recorder.{ForwardRecorder, Recorder}
import org.goldenport.cli.Environment
import org.goldenport.cli.ShellCommand
import org.goldenport.io.StringInputSource
import org.goldenport.bag.ChunkBag

/*
 * @since   May.  5, 2020
 * @version May. 19, 2020
 * @author  ASAMI, Tomoharu
 */
class Context(
  val environment: Environment,
  val config: Config
) extends Environment.AppEnvironment with ForwardRecorder {
  protected def forward_Recorder: Recorder = recorder

  def recorder = environment.recorder
  def isPlatformWindows: Boolean = environment.isPlatformWindows

  def shellRunAsString(pcmd: String, pin: String): String =
    shellExecute(pcmd, pin).toText

  def shellRunAsChunkBag(pcmd: String, pin: String): ChunkBag =
    shellExecute(pcmd, pin).toChunkBag

  def shellRunAsChunkBag(pcmd: String, pin: String, name: Option[String]): ChunkBag =
    shellExecute(pcmd, pin).toChunkBag(name)

  def shellExecute(pcmd: String, pin: String): ShellCommand.Result = {
    val cmd = Strings.totokens(pcmd, " ")
    val env = Map.empty[String, String]
    val dir = config.workDirectory
    val in = StringInputSource(pin)
    val timeout = None
    val sh = new ShellCommand(cmd, env, dir, Some(in), timeout)
    sh.execute
  }
}
