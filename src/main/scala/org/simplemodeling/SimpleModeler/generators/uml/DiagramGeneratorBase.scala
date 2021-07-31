package org.simplemodeling.SimpleModeler.generators.uml

import java.io.{InputStream, OutputStream, IOException}
// import scala.collection.mutable.{ArrayBuffer, HashMap}
// import org.goldenport.entity.content._
// import org.goldenport.entity.GEntityContext
// import org.goldenport.entities.graphviz._
// import org.simplemodeling.SimpleModeler.entity._
// import org.simplemodeling.SimpleModeler.entity.flow._
import org.goldenport.recorder.Recordable
// import org.goldenport.util.MimeType
import org.goldenport.bag.ChunkBag
import org.simplemodeling.model._
import org.simplemodeling.SimpleModeler.Context

/*
 * @since   Mar. 21, 2011
 *  version Mar. 26, 2011
 *  version Sep. 18, 2012
 *  version Oct.  5, 2012
 *  version May. 19, 2020
 * @version Jul. 12, 2021
 * @author  ASAMI, Tomoharu
 */
abstract class DiagramGeneratorBase(
  val context: Context,
  val simpleModel: SimpleModel
) extends Recordable {
  set_Recorder(context)

//   protected final def make_diagram_png(text: String, name: Option[String] = None): ChunkBag = {
//     val layout = "dot"
//     var in: InputStream = null
//     var out: OutputStream = null
//     try {
//       val dot: Process = context.executeCommand("dot -Tpng -K%s -q".format(layout))
//       in = dot.getInputStream()
//       out = dot.getOutputStream()
// //    record_trace("start process = " + dot)
//       record_trace("dot = " + text)
//       text.write(out)
//       out.flush
//       out.close
// //      val bytes = stream2Bytes(in) 2009-03-18
// //      record_trace("bytes = " + bytes.length)
// //      new BinaryContent(bytes, context)
//       name match {
//         case Some(s) => ChunkBag.createInputStream(in, context, s, MimeType.image_png)
//         case None => ChunkBag.createInputStream(in, context, null, MimeType.image_png)
//       }
//     } catch {
//       case e: IOException => {
//         // Cannot run program "dot": error=2, No such file or directory
//         throw new IOException("graphvizのdotコマンドが動作しませんでした。\ngraphvizについてはhttp://www.graphviz.org/を参照してください。\n[詳細エラー: %s]".format(e.getMessage))
//       }
//     } finally {
//       if (in != null) in.close
// //      err.close
// //      record_trace("finish process = " + dot)
//     }
//   }
  protected final def make_diagram_png(text: String, name: Option[String] = None): ChunkBag = {
    val layout = "dot"
    val cmd = s"dot -Tpng -K${layout} -q"
    context.shellRunAsChunkBag(cmd, text, name)
  }

  protected final def make_diagram_svg(text: String, name: Option[String] = None): ChunkBag = {
    val layout = "dot"
    val cmd = s"dot -Tsvg -K${layout} -q"
    context.shellRunAsChunkBag(cmd, text, name)
  }

  protected final def make_diagram_webp(text: String, name: Option[String] = None): ChunkBag = {
    val layout = "dot"
    val cmd = s"dot -Twebp -K${layout} -q"
    context.shellRunAsChunkBag(cmd, text, name)
  }
}
