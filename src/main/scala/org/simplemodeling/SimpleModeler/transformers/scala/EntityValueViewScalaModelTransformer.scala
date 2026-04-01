package org.simplemodeling.SimpleModeler.transformers.scala

/*
 * @since   Sep. 20, 2025
 * @version Apr.  2, 2026
 * @author  ASAMI, Tomoharu
 */
class EntityValueViewScalaModelTransformer()
  extends EntityValueProjectionScalaModelTransformer(None)

class EntityValueSummaryScalaModelTransformer()
  extends EntityValueProjectionScalaModelTransformer(Some("summary"))

class EntityValueDetailScalaModelTransformer()
  extends EntityValueProjectionScalaModelTransformer(Some("detail"))
