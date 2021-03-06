package org.simplemodeling.SimpleModeler.transformer.maker

import scala.collection.mutable.ArrayBuffer
// import org.simplemodeling.SimpleModeler.entity._
import com.asamioffice.goldenport.text.UString.notNull
// import org.simplemodeling.model._

/*
 * Scala Class Attribute Definition
 * 
 * @since   Aug. 19, 2011
 *  version Feb. 20, 2012
 *  version May.  6, 2012
 *  version Nov. 22, 2012
 *  version Feb. 23, 2013
 * @version May. 16, 2020
 * @author  ASAMI, Tomoharu
 */
class ScalaClassAttributeDefinition(
  pContext: PContext,
  model: PModel,
  aspects: Seq[ScalaAspect],
  attr: PAttribute,
  owner: ScalaClassDefinition,
  smaker: ScalaMaker) extends GenericClassAttributeDefinition(pContext, model, aspects, attr, owner) with ScalaMakerHolder {
  sm_open(smaker, aspects)

  override protected def head_imports_Extension {
//    sm_import("org.json.*")
  }

  override def constant_property {
    // TODO move to companion object
//    sm_public_static_final_String_literal(propertyConstantName, propertyLiteral)
  }

  override protected def variable_plain_Inject_Annotation {
    sm_pln("@Inject")
  }

  override protected def variable_plain_Attribute_Instance_Variable(typename: String, varname: String) {
    sm_private_instance_variable(attr, typename, varname);
  }

  override protected def variable_plain_Transient_Instance_Variable(typename: String, varname: String) {
    sm_private_transient_instance_variable(attr, typename, varname);
  }

  override protected def method_bean_single_plain() {
    single_value_attribute_method
  }

  protected final def mapping_single_value_attribute_method(getter: String, setter: String) {
    sm_public_get_or_null_method(javaType, attrName, getter, attrName)
    if (attr.attributeType == PBooleanType) {
      sm_public_is_method(attrName, varName);
    }
    if (is_settable()) {
      sm_public_set_or_null_method(attrName, javaType, paramName, setter)
      sm_public_with_or_null_method(owner.name, attrName, javaType, paramName, setter)
    }
  }

  protected final def single_value_attribute_method() {
    sm_public_get_method(javaType, attrName, varName)
    if (attr.attributeType == PBooleanType) {
      sm_public_is_method(attrName, varName)
    }
    if (is_settable()) {
      sm_public_set_method(attrName, javaType, paramName, varName)
    }
  }

  override protected def method_bean_single_byte() {
    mapping_multi_value_attribute_method("%s.byteValue()", "%s.shortValue()") // XXX app engine ?
  }

  override protected def method_bean_single_integer() {
    mapping_multi_value_attribute_method("new BigInteger(%s)", "%s.toString()")
  }    

  override protected def method_bean_single_decimal() {
    mapping_multi_value_attribute_method("new BigDecimal(%s)", "%s.toString()")
  }

  override def method_bean_single_entity_Simple(e: PEntityType) {
    single_value_attribute_method
  }

  override def method_bean_single_entity_Composition_Reference_Property(e: PEntityType) {
    sys.error("not supported yet")
  }

  override def method_bean_single_entity_Aggregation_Reference_Property(e: PEntityType) {
    sys.error("not supported yet")
  }

  override def method_bean_single_entity_Association_Reference_Property(e: PEntityType) {
    sys.error("not supported yet")
  }

  override def method_bean_single_entity_Composition_Id_Property(e: PEntityType) {
    sys.error("not supported yet")
  }

  override def method_bean_single_entity_Aggregation_Id_Property(e: PEntityType) {
    sys.error("not supported yet")
  }

  override def method_bean_single_entity_Association_Id_Property(e: PEntityType) {
    sys.error("not supported yet")
  }

  override def method_bean_single_entity_Composition_Id_Reference_Property(e: PEntityType) {
    sys.error("not supported yet")
  }

  override def method_bean_single_entity_Aggregation_Id_Reference_Property(e: PEntityType) {
    sys.error("not supported yet")
  }

  override def method_bean_single_entity_Association_Id_Reference_Property(e: PEntityType) {
    sys.error("not supported yet")
  }

  override def method_bean_single_entity_Query_Property(e: PEntityType) {
    sys.error("not supported yet")
  }

//     sm_public_void_method("set%s(%s %s)", attrName.capitalize, javaType, paramName) {

  // Part
  override protected def method_bean_single_part(p: PEntityPartType) {
    sm_public_get_method(javaType, attrName, erPartVarName)
    if (attr.attributeType == PBooleanType) {
      sm_public_is_method(attrName, varName)
    }
    if (is_settable()) {
      sm_public_set_method(attrName, javaType, paramName, erPartVarName)
    }
  }

  override protected def method_bean_single_powertype(e: PPowertypeType) {
    sm_public_get_method(javaType, attrName, varName); // erPowerVarName); 
    if (attr.attributeType == PBooleanType) {
      sm_public_is_method(attrName, varName)
    }
    if (is_settable()) {
      sm_public_set_method(attrName, javaType, paramName, varName); // erPowerVarName);
    }
  }

  override protected def method_bean_multi_plain() {
    multi_value_attribute_method
  }

  override protected def method_bean_multi_byte() {
    mapping_single_value_attribute_method("%s.byteValue()", "%s.shortValue()")
  }

  override protected def method_bean_multi_integer() {
    mapping_single_value_attribute_method("new BigInteger(%s)", "%s.toString()")
  }    

  override protected def method_bean_multi_decimal() {
    mapping_single_value_attribute_method("new BigDecimal(%s)", "%s.toString()")
  }

  override def method_bean_multi_entity_Simple(e: PEntityType) {
    multi_value_attribute_method
  }

  override def method_bean_multi_entity_Composition_Reference_Property(e: PEntityType) {
    sys.error("not supported yet")
  }

  override def method_bean_multi_entity_Aggregation_Reference_Property(e: PEntityType) {
    sys.error("not supported yet")
  }

  override def method_bean_multi_entity_Association_Reference_Property(e: PEntityType) {
    sys.error("not supported yet")
  }

  override def method_bean_multi_entity_Composition_Id_Property(e: PEntityType) {
    sys.error("not supported yet")
  }

  override def method_bean_multi_entity_Aggregation_Id_Property(e: PEntityType) {
    sys.error("not supported yet")
  }

  override def method_bean_multi_entity_Association_Id_Property(e: PEntityType) {
    sys.error("not supported yet")
  }

  override def method_bean_multi_entity_Composition_Id_Reference_Property(e: PEntityType) {
    sm_pln("fill_%s();".format(attrName))
    sm_if_else(erAssocVarName + " != null") {
      sm_pln("return Collections.unmodifiableList(%s);", erAssocVarName)
    }
    sm_else {
      sm_pln("return Collections.emptyList();")
    }
  }

  override def method_bean_multi_entity_Aggregation_Id_Reference_Property(e: PEntityType) {
  }

  override def method_bean_multi_entity_Association_Id_Reference_Property(e: PEntityType) {
  }

  override def method_bean_multi_entity_Query_Property(e: PEntityType) {
  }

  // part
  override protected def method_bean_multi_part(p: PEntityPartType) {
/*
    sm_public_method("public " + javaType + " get" + attrName.capitalize + " ()") {
      sm_pln("fill_%s();", attrName)
      sm_if_else(erPartVarName + " != null") {
        sm_pln("return Collections.unmodifiableList(%s);", erPartVarName)
      }
      sm_else {
        sm_pln("return Collections.emptyList();")
      }
    }
    if (is_settable) {
      sm_public_void_method("set" + attrName.capitalize + "(" + javaType + " " + paramName + ")") {
        sm_assign_this(erPartVarName, "new ArrayList<%s>(%s)", javaElementType, paramName)  
      }
      sm_public_void_method("public void add" + attrName.capitalize + "(" + javaElementType + " " + paramName + ")") {
        sm_if_null("this." + erPartVarName) {
          sm_assign_this(erPartVarName, "new ArrayList<%s>()", javaElementType) 
        }
        sm_pln("this.%s.add(%s);", erPartVarName, paramName) 
      }
    }
*/
  }

  override protected def method_bean_multi_powertype(e: PPowertypeType) {
/*
    sm_public_get_list_method_prologue(javaType, attrName, erPartVarName) {
      sm_pln("fill_%s()", attrName)
    }
    if (is_settable()) {
      sm_public_set_list_method(attrName, javaElementType, paramName, erPartVarName)
      sm_public_add_list_element_method(attrName, javaElementType, paramName, erPartVarName)
    }
*/
  }

  def multi_value_attribute_method {
    sm_public_get_list_method(javaType, attrName, erPartVarName)
    if (is_settable()) {
      sm_public_set_list_method(attrName, javaElementType, paramName, erPartVarName)
      sm_public_add_list_element_method(attrName, javaElementType, paramName, erPartVarName)
    }
  }

  def mapping_multi_value_attribute_method(getter: String, setter: String) {
/*
    sm_public_method("%s get%s()", javaType, attrName.capitalize) {
      sm_if_else_not_null(attrName) {
        sm_var_List_new_ArrayList("result", attr.elementTypeName); 
        sm_for(persistent_element_type, "elem", attrName) {
          sm_pln("result.add(%s);", getter.format("elem"))
        }
        sm_return("result")
      }
      sm_else {
        sm_return("Collections.emptyList()");
      }
    }
    if (is_settable()) {
      sm_public_set_list_method(attrName, javaElementType, paramName, erPartVarName)
      sm_public_add_list_element_method(attrName, javaElementType, paramName, erPartVarName)
    }
*/
  }

  /*
   * method_as
   */
  override def method_as_string_multi_entity(e: PEntityType) {
    
  }

  override def method_as_string_multi_part(e: PEntityPartType) {
    
  }

  override def method_as_string_multi_powertype(e: PPowertypeType) {
    
  }

  override def method_as_string_multi_byte {
    
  }

  override def method_as_string_multi_integer {
    
  }

  override def method_as_string_multi_decimal {
    
  }

  override def method_as_string_multi_plain {
    
  }

  override def method_as_string_single_entity(e: PEntityType) {
    
  }

  override def method_as_string_single_part(e: PEntityPartType) {
    
  }

  override def method_as_string_single_powertype(e: PPowertypeType) {
    
  }

  override def method_as_string_single_byte {
    
  }

  override def method_as_string_single_integer {
    
  }

  override def method_as_string_single_decimal {
    
  }

  override def method_as_string_single_plain {
    
  }

  // as_xml
  override def method_as_xml_multi_entity(e: PEntityType) {
    
  }

  override def method_as_xml_multi_part(e: PEntityPartType) {
    
  }

  override def method_as_xml_multi_powertype(e: PPowertypeType) {
    
  }

  override def method_as_xml_multi_byte {
    
  }

  override def method_as_xml_multi_integer {
    
  }

  override def method_as_xml_multi_decimal {
    
  }

  override def method_as_xml_multi_plain {
    
  }

  override def method_as_xml_single_entity(e: PEntityType) {
    
  }

  override def method_as_xml_single_part(e: PEntityPartType) {
    
  }

  override def method_as_xml_single_powertype(e: PPowertypeType) {
    
  }

  override def method_as_xml_single_byte {
    
  }

  override def method_as_xml_single_integer {
    
  }

  override def method_as_xml_single_decimal {
    
  }

  override def method_as_xml_single_plain {
    
  }

  // as_json
  override def method_as_json_multi_entity(e: PEntityType) {
    
  }

  override def method_as_json_multi_part(e: PEntityPartType) {
    
  }

  override def method_as_json_multi_powertype(e: PPowertypeType) {
    
  }

  override def method_as_json_multi_byte {
    
  }

  override def method_as_json_multi_integer {
    
  }

  override def method_as_json_multi_decimal {
    
  }

  override def method_as_json_multi_plain {
    
  }

  override def method_as_json_single_entity(e: PEntityType) {
    
  }

  override def method_as_json_single_part(e: PEntityPartType) {
    
  }

  override def method_as_json_single_powertype(e: PPowertypeType) {
    
  }

  override def method_as_json_single_byte {
    
  }

  override def method_as_json_single_integer {
    
  }

  override def method_as_json_single_decimal {
    
  }

  override def method_as_json_single_plain {
    
  }

  // as_csv
  override def method_as_csv_multi_entity(e: PEntityType) {
    
  }

  override def method_as_csv_multi_part(e: PEntityPartType) {
    
  }

  override def method_as_csv_multi_powertype(e: PPowertypeType) {
    
  }

  override def method_as_csv_multi_byte {
    
  }

  override def method_as_csv_multi_integer {
    
  }

  override def method_as_csv_multi_decimal {
    
  }

  override def method_as_csv_multi_plain {
    
  }

  override def method_as_csv_single_entity(e: PEntityType) {
    
  }

  override def method_as_csv_single_part(e: PEntityPartType) {
    
  }

  override def method_as_csv_single_powertype(e: PPowertypeType) {
    
  }

  override def method_as_csv_single_byte {
    
  }

  override def method_as_csv_single_integer {
    
  }

  override def method_as_csv_single_decimal {
    
  }

  override def method_as_csv_single_plain {
    
  }

  // as_urlencode
  override def method_as_urlencode_multi_entity(e: PEntityType) {
    
  }

  override def method_as_urlencode_multi_part(e: PEntityPartType) {
    
  }

  override def method_as_urlencode_multi_powertype(e: PPowertypeType) {
    
  }

  override def method_as_urlencode_multi_byte {
    
  }

  override def method_as_urlencode_multi_integer {
    
  }

  override def method_as_urlencode_multi_decimal {
    
  }

  override def method_as_urlencode_multi_plain {
    
  }

  override def method_as_urlencode_single_entity(e: PEntityType) {
    
  }

  override def method_as_urlencode_single_part(e: PEntityPartType) {
    
  }

  override def method_as_urlencode_single_powertype(e: PPowertypeType) {
    
  }

  override def method_as_urlencode_single_byte {
    
  }

  override def method_as_urlencode_single_integer {
    
  }

  override def method_as_urlencode_single_decimal {
    
  }

  override def method_as_urlencode_single_plain {
    
  }

  /*
   * method_by
   */
  override def method_by_string_multi_entity(e: PEntityType) {
    
  }

  override def method_by_string_multi_part(e: PEntityPartType) {
    
  }

  override def method_by_string_multi_powertype(e: PPowertypeType) {
    
  }

  override def method_by_string_multi_byte {
    
  }

  override def method_by_string_multi_integer {
    
  }

  override def method_by_string_multi_decimal {
    
  }

  override def method_by_string_multi_plain {
    
  }

  override def method_by_string_single_entity(e: PEntityType) {
    
  }

  override def method_by_string_single_part(e: PEntityPartType) {
    
  }

  override def method_by_string_single_powertype(e: PPowertypeType) {
    
  }

  override def method_by_string_single_byte {
    
  }

  override def method_by_string_single_integer {
    
  }

  override def method_by_string_single_decimal {
    
  }

  override def method_by_string_single_plain {
    
  }

  // by_xml
  override def method_by_xml_multi_entity(e: PEntityType) {
    
  }

  override def method_by_xml_multi_part(e: PEntityPartType) {
    
  }

  override def method_by_xml_multi_powertype(e: PPowertypeType) {
    
  }

  override def method_by_xml_multi_byte {
    
  }

  override def method_by_xml_multi_integer {
    
  }

  override def method_by_xml_multi_decimal {
    
  }

  override def method_by_xml_multi_plain {
    
  }

  override def method_by_xml_single_entity(e: PEntityType) {
    
  }

  override def method_by_xml_single_part(e: PEntityPartType) {
    
  }

  override def method_by_xml_single_powertype(e: PPowertypeType) {
    
  }

  override def method_by_xml_single_byte {
    
  }

  override def method_by_xml_single_integer {
    
  }

  override def method_by_xml_single_decimal {
    
  }

  override def method_by_xml_single_plain {
    
  }

  // by_json
  override def method_by_json_multi_entity(e: PEntityType) {
    
  }

  override def method_by_json_multi_part(e: PEntityPartType) {
    
  }

  override def method_by_json_multi_powertype(e: PPowertypeType) {
    
  }

  override def method_by_json_multi_byte {
    
  }

  override def method_by_json_multi_integer {
    
  }

  override def method_by_json_multi_decimal {
    
  }

  override def method_by_json_multi_plain {
    
  }

  override def method_by_json_single_entity(e: PEntityType) {
//    sm_method("public %s with%s_json(JSONObject json) throws JSONException", owner.name, attrName.capitalize) {
//      sm_assign_this(attrName, "%s.getFactory().create%s(json)", owner.factoryName, e.entity.documentName)
//      sm_return_this
//    } 
  }

  override def method_by_json_single_part(e: PEntityPartType) {
    
  }

  override def method_by_json_single_powertype(e: PPowertypeType) {
    
  }

  override def method_by_json_single_byte {
    
  }

  override def method_by_json_single_integer {
    
  }

  override def method_by_json_single_decimal {
    
  }

  override def method_by_json_single_plain {
  }

  // by_csv
  override def method_by_csv_multi_entity(e: PEntityType) {
    
  }

  override def method_by_csv_multi_part(e: PEntityPartType) {
    
  }

  override def method_by_csv_multi_powertype(e: PPowertypeType) {
    
  }

  override def method_by_csv_multi_byte {
    
  }

  override def method_by_csv_multi_integer {
    
  }

  override def method_by_csv_multi_decimal {
    
  }

  override def method_by_csv_multi_plain {
    
  }

  override def method_by_csv_single_entity(e: PEntityType) {
    
  }

  override def method_by_csv_single_part(e: PEntityPartType) {
    
  }

  override def method_by_csv_single_powertype(e: PPowertypeType) {
    
  }

  override def method_by_csv_single_byte {
    
  }

  override def method_by_csv_single_integer {
    
  }

  override def method_by_csv_single_decimal {
    
  }

  override def method_by_csv_single_plain {
    
  }

  // by_urlencode
  override def method_by_urlencode_multi_entity(e: PEntityType) {
    
  }

  override def method_by_urlencode_multi_part(e: PEntityPartType) {
    
  }

  override def method_by_urlencode_multi_powertype(e: PPowertypeType) {
    
  }

  override def method_by_urlencode_multi_byte {
    
  }

  override def method_by_urlencode_multi_integer {
    
  }

  override def method_by_urlencode_multi_decimal {
    
  }

  override def method_by_urlencode_multi_plain {
    
  }

  override def method_by_urlencode_single_entity(e: PEntityType) {
    
  }

  override def method_by_urlencode_single_part(e: PEntityPartType) {
    
  }

  override def method_by_urlencode_single_powertype(e: PPowertypeType) {
    
  }

  override def method_by_urlencode_single_byte {
    
  }

  override def method_by_urlencode_single_integer {
    
  }

  override def method_by_urlencode_single_decimal {
    
  }

  override def method_by_urlencode_single_plain {
    
  }
  
  /*
   * method_with
   */
  override def method_with_plain_multi_entity(e: PEntityType) {
/*
    sm_public_method("%s with%s(%s %s)", owner.name, attrName.capitalize, javaType, paramName) {
      sm_if(varName + " != null") {
        sm_assign_new_ArrayList(varName, javaElementType);
      }
      sm_pln("%s.addAll(%s);", varName, paramName)
      sm_return_this
    }
    sm_public_method("%s with%s(%s %s)", owner.name, attrName.capitalize, javaElementType, paramName) {
      sm_if(varName + " != null") {
        sm_assign_new_ArrayList(varName, javaElementType);
      }
      sm_pln("%s.add(%s);", varName, paramName)
      sm_return_this
    }
*/
  }

  override def method_with_plain_multi_part(e: PEntityPartType) {
    
  }

  override def method_with_plain_multi_powertype(e: PPowertypeType) {
    
  }

  override def method_with_plain_multi_byte {
    
  }

  override def method_with_plain_multi_integer {
    
  }

  override def method_with_plain_multi_decimal {
    
  }

  override def method_with_plain_multi_plain {
/*
    sm_public_method("%s with%s(%s %s)", owner.name, attrName.capitalize, javaType, paramName) {
      sm_if(varName + " != null") {
        sm_assign_new_ArrayList(varName, javaElementType);
      }
      sm_pln("%s.addAll(%s);")
      sm_return_this
    }
    sm_public_method("%s with%s(%s %s)", owner.name, attrName.capitalize, javaElementType, paramName) {
      sm_if(varName + " != null") {
        sm_assign_new_ArrayList(varName, javaElementType);
      }
      sm_pln("%s.add(%s);")
      sm_return_this
    }
*/
  }

  override def method_with_plain_single_entity(e: PEntityType) {
    sm_public_with_method(owner.name, attrName, e.entity.name, paramName, varName)
  }

  override def method_with_plain_single_part(e: PEntityPartType) {
    
  }

  override def method_with_plain_single_powertype(e: PPowertypeType) {
    
  }

  override def method_with_plain_single_byte {
    
  }

  override def method_with_plain_single_integer {
    
  }

  override def method_with_plain_single_decimal {
    
  }

  override def method_with_plain_single_plain {
    sm_public_with_method(owner.name, attrName, javaType, paramName, varName)
  }

  override def method_with_string {
    if (attr.isHasMany) {
      attr.attributeType match {
        case e: PEntityType     => method_with_string_multi_entity(e)
        case p: PEntityPartType => method_with_string_multi_part(p)
        case p: PPowertypeType  => method_with_string_multi_powertype(p)
        case v: PByteType       => method_with_string_multi_byte
        case v: PIntegerType    => method_with_string_multi_integer
        case v: PDecimalType    => method_with_string_multi_decimal
        case _                  => method_with_string_multi_plain
      }
    } else {
      attr.attributeType match {
        case e: PEntityType     => method_with_string_single_entity(e)
        case p: PEntityPartType => method_with_string_single_part(p)
        case p: PPowertypeType  => method_with_string_single_powertype(p)
        case v: PByteType       => method_with_string_single_byte
        case v: PIntegerType    => method_with_string_single_integer
        case v: PDecimalType    => method_with_string_single_decimal
        case _                  => method_with_string_single_plain
      }
    }
    
  }

  override def method_with_string_multi_entity(e: PEntityType) {
    
  }

  override def method_with_string_multi_part(e: PEntityPartType) {
    
  }

  override def method_with_string_multi_powertype(e: PPowertypeType) {
    
  }

  override def method_with_string_multi_byte {
    
  }

  override def method_with_string_multi_integer {
    
  }

  override def method_with_string_multi_decimal {
    
  }

  override def method_with_string_multi_plain {
    
  }

  override def method_with_string_single_entity(e: PEntityType) {
    
  }

  override def method_with_string_single_part(e: PEntityPartType) {
    
  }

  override def method_with_string_single_powertype(e: PPowertypeType) {
    
  }

  override def method_with_string_single_byte {
    
  }

  override def method_with_string_single_integer {
    
  }

  override def method_with_string_single_decimal {
    
  }

  override def method_with_string_single_plain {
    
  }

  // with_xml
  override def method_with_xml_multi_entity(e: PEntityType) {
    
  }

  override def method_with_xml_multi_part(e: PEntityPartType) {
    
  }

  override def method_with_xml_multi_powertype(e: PPowertypeType) {
    
  }

  override def method_with_xml_multi_byte {
    
  }

  override def method_with_xml_multi_integer {
    
  }

  override def method_with_xml_multi_decimal {
    
  }

  override def method_with_xml_multi_plain {
    
  }

  override def method_with_xml_single_entity(e: PEntityType) {
    
  }

  override def method_with_xml_single_part(e: PEntityPartType) {
    
  }

  override def method_with_xml_single_powertype(e: PPowertypeType) {
    
  }

  override def method_with_xml_single_byte {
    
  }

  override def method_with_xml_single_integer {
    
  }

  override def method_with_xml_single_decimal {
    
  }

  override def method_with_xml_single_plain {
    
  }

  // with_json
  override def method_with_json_multi_entity(e: PEntityType) {
    
  }

  override def method_with_json_multi_part(e: PEntityPartType) {
    
  }

  override def method_with_json_multi_powertype(e: PPowertypeType) {
    
  }

  override def method_with_json_multi_byte {
    
  }

  override def method_with_json_multi_integer {
    
  }

  override def method_with_json_multi_decimal {
    
  }

  override def method_with_json_multi_plain {
    
  }

  override def method_with_json_single_entity(e: PEntityType) {
    
  }

  override def method_with_json_single_part(e: PEntityPartType) {
    
  }

  override def method_with_json_single_powertype(e: PPowertypeType) {
    
  }

  override def method_with_json_single_byte {
    
  }

  override def method_with_json_single_integer {
    
  }

  override def method_with_json_single_decimal {
    
  }

  override def method_with_json_single_plain {
    
  }

  // with_csv
  override def method_with_csv_multi_entity(e: PEntityType) {
    
  }

  override def method_with_csv_multi_part(e: PEntityPartType) {
    
  }

  override def method_with_csv_multi_powertype(e: PPowertypeType) {
    
  }

  override def method_with_csv_multi_byte {
    
  }

  override def method_with_csv_multi_integer {
    
  }

  override def method_with_csv_multi_decimal {
    
  }

  override def method_with_csv_multi_plain {
    
  }

  override def method_with_csv_single_entity(e: PEntityType) {
    
  }

  override def method_with_csv_single_part(e: PEntityPartType) {
    
  }

  override def method_with_csv_single_powertype(e: PPowertypeType) {
    
  }

  override def method_with_csv_single_byte {
    
  }

  override def method_with_csv_single_integer {
    
  }

  override def method_with_csv_single_decimal {
    
  }

  override def method_with_csv_single_plain {
    
  }

  // with_urlencode
  override def method_with_urlencode_multi_entity(e: PEntityType) {
    
  }

  override def method_with_urlencode_multi_part(e: PEntityPartType) {
    
  }

  override def method_with_urlencode_multi_powertype(e: PPowertypeType) {
    
  }

  override def method_with_urlencode_multi_byte {
    
  }

  override def method_with_urlencode_multi_integer {
    
  }

  override def method_with_urlencode_multi_decimal {
    
  }

  override def method_with_urlencode_multi_plain {
    
  }

  override def method_with_urlencode_single_entity(e: PEntityType) {
    
  }

  override def method_with_urlencode_single_part(e: PEntityPartType) {
    
  }

  override def method_with_urlencode_single_powertype(e: PPowertypeType) {
    
  }

  override def method_with_urlencode_single_byte {
    
  }

  override def method_with_urlencode_single_integer {
    
  }

  override def method_with_urlencode_single_decimal {
    
  }

  override def method_with_urlencode_single_plain {
    
  }
  
  /*
   * macro_to
   */
  override def method_to_string_multi_entity(e: PEntityType) {
    
  }

  override def method_to_string_multi_part(e: PEntityPartType) {
    
  }

  override def method_to_string_multi_powertype(e: PPowertypeType) {
    
  }

  override def method_to_string_multi_byte {
    
  }

  override def method_to_string_multi_integer {
    
  }

  override def method_to_string_multi_decimal {
    
  }

  override def method_to_string_multi_plain {
    
  }

  override def method_to_string_single_entity(e: PEntityType) {
    
  }

  override def method_to_string_single_part(e: PEntityPartType) {
    
  }

  override def method_to_string_single_powertype(e: PPowertypeType) {
    
  }

  override def method_to_string_single_byte {
    
  }

  override def method_to_string_single_integer {
    
  }

  override def method_to_string_single_decimal {
    
  }

  override def method_to_string_single_plain {
    
  }

  // to_xml
  override def method_to_xml_multi_entity(e: PEntityType) {
    
  }

  override def method_to_xml_multi_part(e: PEntityPartType) {
    
  }

  override def method_to_xml_multi_powertype(e: PPowertypeType) {
    
  }

  override def method_to_xml_multi_byte {
    
  }

  override def method_to_xml_multi_integer {
    
  }

  override def method_to_xml_multi_decimal {
    
  }

  override def method_to_xml_multi_plain {
    
  }

  override def method_to_xml_single_entity(e: PEntityType) {
    
  }

  override def method_to_xml_single_part(e: PEntityPartType) {
    
  }

  override def method_to_xml_single_powertype(e: PPowertypeType) {
    
  }

  override def method_to_xml_single_byte {
    
  }

  override def method_to_xml_single_integer {
    
  }

  override def method_to_xml_single_decimal {
    
  }

  override def method_to_xml_single_plain {
    
  }

  // to_json
  override def method_to_json_multi_entity(e: PEntityType) {
    
  }

  override def method_to_json_multi_part(e: PEntityPartType) {
    
  }

  override def method_to_json_multi_powertype(e: PPowertypeType) {
    
  }

  override def method_to_json_multi_byte {
    
  }

  override def method_to_json_multi_integer {
    
  }

  override def method_to_json_multi_decimal {
    
  }

  override def method_to_json_multi_plain {
    
  }

  override def method_to_json_single_entity(e: PEntityType) {
    
  }

  override def method_to_json_single_part(e: PEntityPartType) {
    
  }

  override def method_to_json_single_powertype(e: PPowertypeType) {
    
  }

  override def method_to_json_single_byte {
    
  }

  override def method_to_json_single_integer {
    
  }

  override def method_to_json_single_decimal {
    
  }

  override def method_to_json_single_plain {
    
  }

  // to_csv
  override def method_to_csv_multi_entity(e: PEntityType) {
    
  }

  override def method_to_csv_multi_part(e: PEntityPartType) {
    
  }

  override def method_to_csv_multi_powertype(e: PPowertypeType) {
    
  }

  override def method_to_csv_multi_byte {
    
  }

  override def method_to_csv_multi_integer {
    
  }

  override def method_to_csv_multi_decimal {
    
  }

  override def method_to_csv_multi_plain {
    
  }

  override def method_to_csv_single_entity(e: PEntityType) {
    
  }

  override def method_to_csv_single_part(e: PEntityPartType) {
    
  }

  override def method_to_csv_single_powertype(e: PPowertypeType) {
    
  }

  override def method_to_csv_single_byte {
    
  }

  override def method_to_csv_single_integer {
    
  }

  override def method_to_csv_single_decimal {
    
  }

  override def method_to_csv_single_plain {
    
  }

  // to_urlencode
  override def method_to_urlencode_multi_entity(e: PEntityType) {
    
  }

  override def method_to_urlencode_multi_part(e: PEntityPartType) {
    
  }

  override def method_to_urlencode_multi_powertype(e: PPowertypeType) {
    
  }

  override def method_to_urlencode_multi_byte {
    
  }

  override def method_to_urlencode_multi_integer {
    
  }

  override def method_to_urlencode_multi_decimal {
    
  }

  override def method_to_urlencode_multi_plain {
    
  }

  override def method_to_urlencode_single_entity(e: PEntityType) {
    
  }

  override def method_to_urlencode_single_part(e: PEntityPartType) {
    
  }

  override def method_to_urlencode_single_powertype(e: PPowertypeType) {
    
  }

  override def method_to_urlencode_single_byte {
    
  }

  override def method_to_urlencode_single_integer {
    
  }

  override def method_to_urlencode_single_decimal {
    
  }

  override def method_to_urlencode_single_plain {
    
  }

  /*
   * macro_from
   */
  override def method_from_string_multi_entity(e: PEntityType) {
    
  }

  override def method_from_string_multi_part(e: PEntityPartType) {
    
  }

  override def method_from_string_multi_powertype(e: PPowertypeType) {
    
  }

  override def method_from_string_multi_byte {
    
  }

  override def method_from_string_multi_integer {
    
  }

  override def method_from_string_multi_decimal {
    
  }

  override def method_from_string_multi_plain {
    
  }

  override def method_from_string_single_entity(e: PEntityType) {
    
  }

  override def method_from_string_single_part(e: PEntityPartType) {
    
  }

  override def method_from_string_single_powertype(e: PPowertypeType) {
    
  }

  override def method_from_string_single_byte {
    
  }

  override def method_from_string_single_integer {
    
  }

  override def method_from_string_single_decimal {
    
  }

  override def method_from_string_single_plain {
    
  }

  // from_xml
  override def method_from_xml_multi_entity(e: PEntityType) {
    
  }

  override def method_from_xml_multi_part(e: PEntityPartType) {
    
  }

  override def method_from_xml_multi_powertype(e: PPowertypeType) {
    
  }

  override def method_from_xml_multi_byte {
    
  }

  override def method_from_xml_multi_integer {
    
  }

  override def method_from_xml_multi_decimal {
    
  }

  override def method_from_xml_multi_plain {
    
  }

  override def method_from_xml_single_entity(e: PEntityType) {
    
  }

  override def method_from_xml_single_part(e: PEntityPartType) {
    
  }

  override def method_from_xml_single_powertype(e: PPowertypeType) {
    
  }

  override def method_from_xml_single_byte {
    
  }

  override def method_from_xml_single_integer {
    
  }

  override def method_from_xml_single_decimal {
    
  }

  override def method_from_xml_single_plain {
    
  }

  // from_json
  override def method_from_json_multi_entity(e: PEntityType) {
    
  }

  override def method_from_json_multi_part(e: PEntityPartType) {
    
  }

  override def method_from_json_multi_powertype(e: PPowertypeType) {
    
  }

  override def method_from_json_multi_byte {
    
  }

  override def method_from_json_multi_integer {
    
  }

  override def method_from_json_multi_decimal {
    
  }

  override def method_from_json_multi_plain {
    
  }

  override def method_from_json_single_entity(e: PEntityType) {
    
  }

  override def method_from_json_single_part(e: PEntityPartType) {
    
  }

  override def method_from_json_single_powertype(e: PPowertypeType) {
    
  }

  override def method_from_json_single_byte {
    
  }

  override def method_from_json_single_integer {
    
  }

  override def method_from_json_single_decimal {
    
  }

  override def method_from_json_single_plain {
    
  }

  // from_csv
  override def method_from_csv_multi_entity(e: PEntityType) {
    
  }

  override def method_from_csv_multi_part(e: PEntityPartType) {
    
  }

  override def method_from_csv_multi_powertype(e: PPowertypeType) {
    
  }

  override def method_from_csv_multi_byte {
    
  }

  override def method_from_csv_multi_integer {
    
  }

  override def method_from_csv_multi_decimal {
    
  }

  override def method_from_csv_multi_plain {
    
  }

  override def method_from_csv_single_entity(e: PEntityType) {
    
  }

  override def method_from_csv_single_part(e: PEntityPartType) {
    
  }

  override def method_from_csv_single_powertype(e: PPowertypeType) {
    
  }

  override def method_from_csv_single_byte {
    
  }

  override def method_from_csv_single_integer {
    
  }

  override def method_from_csv_single_decimal {
    
  }

  override def method_from_csv_single_plain {
    
  }

  // from_urlencode
  override def method_from_urlencode_multi_entity(e: PEntityType) {
    
  }

  override def method_from_urlencode_multi_part(e: PEntityPartType) {
    
  }

  override def method_from_urlencode_multi_powertype(e: PPowertypeType) {
    
  }

  override def method_from_urlencode_multi_byte {
    
  }

  override def method_from_urlencode_multi_integer {
    
  }

  override def method_from_urlencode_multi_decimal {
    
  }

  override def method_from_urlencode_multi_plain {
    
  }

  override def method_from_urlencode_single_entity(e: PEntityType) {
    
  }

  override def method_from_urlencode_single_part(e: PEntityPartType) {
    
  }

  override def method_from_urlencode_single_powertype(e: PPowertypeType) {
    
  }

  override def method_from_urlencode_single_byte {
    
  }

  override def method_from_urlencode_single_integer {
    
  }

  override def method_from_urlencode_single_decimal {
    
  }

  override def method_from_urlencode_single_plain {
    
  }

  /*
   * Voucher
   */
  override def method_voucher_plain() {
    method_voucher_common(true)
  }

  override def method_voucher_shallow() {
    method_voucher_common(false)
  }

  def method_voucher_common(isDeepCopy: Boolean) {
    val attrName = attr.name;
    val varName = var_name
    val persistentVarName = entity_ref_persistent_var_name
    val refVarName = entity_ref_assoc_var_name
    val partVarName = entity_ref_part_var_name
    val powerVarName = entity_ref_powertype_var_name
    val fromName = "this." + attrName
    val toName = "doc." + attrName

    def one {
      def update(converter: String) {
        sm_if_else_null(fromName) {
          sm_assign_null(toName)
        }
        sm_else {
          sm_assign(toName, converter.format(fromName)) 
        }
      }

      attr.attributeType match {
        case t: PDateTimeType => {
          sm_assign("doc." + varName, "this." + varName)
//          sm_assign("doc.%s_date".format(varName), "Util.makeDate(this.%s)", varName)
//          sm_assign("doc.%s_time".format(varName), "Util.makeTime(this.%s)", varName)
//          sm_assign("doc.%s_now".format(varName), "false")
        }
        case e: PEntityType => {
          if (isDeepCopy) {
            // XXX
          } else {
            // XXX
          }
          entityPersistentKind {
            new EntityAttributeKindFunction[Unit] {
              def apply_Simple() = method_voucher_make_single_entity_simple(e)
              def apply_Composition_Reference() = method_voucher_make_single_entity_composition_reference_property(e)
              def apply_Aggregation_Reference() = method_voucher_make_single_entity_aggregation_reference_property(e)
              def apply_Association_Reference() = method_voucher_make_single_entity_association_reference_property(e)
              def apply_Composition_Id() = method_voucher_make_single_entity_composition_id_property(e)
              def apply_Aggregation_Id() = method_voucher_make_single_entity_aggregation_id_property(e)
              def apply_Association_Id() = method_voucher_make_single_entity_association_id_property(e)
              def apply_Composition_Id_Reference() = method_voucher_make_single_entity_composition_id_reference_property(e)
              def apply_Aggregation_Id_Reference() = method_voucher_make_single_entity_aggregation_id_reference_property(e)
              def apply_Association_Id_Reference() = method_voucher_make_single_entity_association_id_reference_property(e)
              def apply_Query() = method_voucher_make_single_entity_query_property(e)
            }
          }
        }
        case p: PEntityPartType => {
          sm_pln("doc.%s = this.%s.make_voucher();", varName, refVarName)
        }
        case p: PPowertypeType => {
          sm_if_not_null("this." + varName) {
            sm_pln("doc.%s = this.%s.getKey()", varName, powerVarName)
          }
        }
        case v: PByteType    => update("%s.byteValue()")
        case v: PIntegerType => update("new BigInteger(%s)")
        case v: PDecimalType => update("new BigDecimal(%s)")
        case _ => sm_pln("doc.%s = this.%s;", varName, varName)
      }
    } // one

    def many {
      def update(converter: String) {
        sm_if_not_null(fromName) {
          sm_for(persistent_element_type(), "elem", fromName) {
            sm_pln("%s.add(%s);", toName, converter.format("elem")) 
          }
        }
      }

      attr.attributeType match {
        case t: PDateTimeType => { // XXX
          sm_pln("doc.%s = %s;", varName, varName)
//          sm_pln("doc.%s_date = Util.makeDate(%s);", varName, varName)
//          sm_pln("doc.%s_time = Util.makeTime(%s);", varName, varName)
//          sm_pln("doc.%s_now = false;")
        }
        case e: PEntityType => {
          if (isDeepCopy) {
            // XXX
          } else {
            // XXX
          }
          entityPersistentKind {
            new EntityAttributeKindFunction[Unit] {
              def apply_Simple() = method_voucher_make_multi_entity_simple(e)
              def apply_Composition_Reference() = method_voucher_make_multi_entity_composition_reference_property(e)
              def apply_Aggregation_Reference() = method_voucher_make_multi_entity_aggregation_reference_property(e)
              def apply_Association_Reference() = method_voucher_make_multi_entity_association_reference_property(e)
              def apply_Composition_Id() = method_voucher_make_multi_entity_composition_id_property(e)
              def apply_Aggregation_Id() = method_voucher_make_multi_entity_aggregation_id_property(e)
              def apply_Association_Id() = method_voucher_make_multi_entity_association_id_property(e)
              def apply_Composition_Id_Reference() = method_voucher_make_multi_entity_composition_id_reference_property(e)
              def apply_Aggregation_Id_Reference() = method_voucher_make_multi_entity_aggregation_id_reference_property(e)
              def apply_Association_Id_Reference() = method_voucher_make_multi_entity_association_id_reference_property(e)
              def apply_Query() = method_voucher_make_multi_entity_query_property(e)
            }
          }
        }
        case p: PEntityPartType => {
          sm_pln("fill_%s();", attrName)
          sm_if_not_null("this." + partVarName) {
            sm_for(java_element_type() + " element: " + partVarName) {
              sm_pln("doc.%s.add(element.make_voucher());", attrName)
            }
          }
        }
        case v: PByteType    => update("%s.byteValue()")
        case v: PIntegerType => update("new BigInteger(%s)")
        case v: PDecimalType => update("new BigDecimal(%s)")
        case _ => {
          sm_if_not_null("this." + varName) {
            sm_pln("doc.%s..addAll(this.%s);", varName, varName)  
          }
        }
      }
    }

    if (is_settable) {
      if (attr.isHasMany) {
        many
      } else {
        one
      }
    }
  }
/*
          
          def id {
            sm_assign("doc." + pContext.variableName4RefId(attr), "this." + persistentVarName) 
          }

          def get_id {
            sm_assign("doc." + pContext.variableName4RefId(attr), "this.%s.getId()", refVarName)
          }

          def deep {
            sm_pln("fill_%s();".format(attrName))
            sm_if_not_null(refVarName) {
              sm_assign("doc.%s".format(varName), "%s.make_voucher()", refVarName)
            }
          }

          def shallow {
            sm_pln("fill_%s();".format(attrName))
            sm_if_not_null(refVarName) {
              sm_assign("doc.%s".format(varName), "%s.make_voucher_shallow()", refVarName)
            }
          }

          if (is_owned_property()) {
            sm_pln("fill_%s();".format(attrName))
            sm_if_not_null(refVarName) {
              get_id
              sm_pln("doc.%s = %s.make_voucher();".format(varName, refVarName))
            }
          } else if (is_query_property()) {
            if (attr.isComposition) {
              deep
            } else if (attr.isAggregation) {
              if (isDeepCopy) {
                shallow
              }
            }
          } else {
            if (attr.isComposition) {
              sm_pln("fill_%s();".format(attrName))
              sm_if_not_null(refVarName) {
                get_id
                sm_pln("doc.%s = %s.make_voucher();".format(varName, refVarName))
              }
            } else if (attr.isAggregation) {
              id
              if (isDeepCopy) {
                shallow
              }
            } else {
              id
            }
          }
        }
 */
  def method_voucher_make_single_entity_simple(e: PEntityType) {
    sm_pln("doc.%s = this.%s.make_voucher();", varName, varName)
  }

  def method_voucher_make_single_entity_composition_reference_property(e: PEntityType) {
    sys.error("not supported yet")
  }

  def method_voucher_make_single_entity_aggregation_reference_property(e: PEntityType) {
    sys.error("not supported yet")
  }

  def method_voucher_make_single_entity_association_reference_property(e: PEntityType) {
    sys.error("not supported yet")
  }

  def method_voucher_make_single_entity_composition_id_property(e: PEntityType) {
    sys.error("not supported yet")
  }

  def method_voucher_make_single_entity_aggregation_id_property(e: PEntityType) {
    sys.error("not supported yet")
  }

  def method_voucher_make_single_entity_association_id_property(e: PEntityType) {
    sys.error("not supported yet")
  }

  def method_voucher_make_single_entity_composition_id_reference_property(entityType: PEntityType) {
    sm_for(persistent_element_type + " element: this." + erAssocVarName) {
      sm_pln("this.%s.add(element.get());", erPersistentVarName, entityType.entity.idName.capitalize) 
    } 
  }

  def method_voucher_make_single_entity_aggregation_id_reference_property(e: PEntityType) {
    sys.error("not supported yet")
  }

  def method_voucher_make_single_entity_association_id_reference_property(e: PEntityType) {
    sys.error("not supported yet")
  }

  def method_voucher_make_single_entity_query_property(e: PEntityType) {
    sys.error("not supported yet")
  }
/*
          def id {
            sm_if_not_null("this." + persistentVarName) {
              sm_pln("doc.%s.addAll(this.%s);", pContext.variableName4RefId(attr), persistentVarName)
            }
          }

          def get_id {
            sm_if_not_null("this." + persistentVarName) {
              sm_pln("doc.%s.addAll(this.%s);", pContext.variableName4RefId(attr), persistentVarName) 
            }
          }

          def deep {
            sm_pln("fill_%s();".format(attrName))
            sm_for("%s entity: %s".format(e.entity.name, refVarName)) {
              sm_pln("doc.%s.add(entity.make_voucher());".format(varName))
            }
          }

          def shallow {
            sm_pln("fill_%s();".format(attrName))
            sm_for("%s entity: %s".format(e.entity.name, refVarName)) {
              sm_pln("doc.%s.add(entity.make_voucher_shallow());".format(varName))
            }
          }

          if (is_owned_property()) {
            sm_for("%s entity: get%s()".format(e.entity.name, attrName.capitalize)) {
              sm_pln("doc.%s.add(entity.make_voucher());".format(varName))
              sm_pln("doc.%s.add(entity.getId());".format(pContext.variableName4RefId(attr)))
            }
          } else if (is_query_property()) {
            if (attr.isComposition) {
              deep
            } else if (attr.isAggregation) {
              if (isDeepCopy) {
                shallow
              }
            }
          } else {
            if (attr.isComposition) {
              id
              deep
            } else if (attr.isAggregation) {
              id
              if (isDeepCopy) {
                shallow
              }
            } else {
              id
            }
          }
        }
 */
  def method_voucher_make_multi_entity_simple(e: PEntityType) {
  }

  def method_voucher_make_multi_entity_composition_reference_property(e: PEntityType) {
  }

  def method_voucher_make_multi_entity_aggregation_reference_property(e: PEntityType) {
  }

  def method_voucher_make_multi_entity_association_reference_property(e: PEntityType) {
  }

  def method_voucher_make_multi_entity_composition_id_property(e: PEntityType) {
  }

  def method_voucher_make_multi_entity_aggregation_id_property(e: PEntityType) {
  }

  def method_voucher_make_multi_entity_association_id_property(e: PEntityType) {
  }

  def method_voucher_make_multi_entity_composition_id_reference_property(entityType: PEntityType) {
    sm_for(persistent_element_type + " element: this." + erAssocVarName) {
      sm_pln("this.%s.add(element.get());", erPersistentVarName, entityType.entity.idName.capitalize) 
    } 
  }

  def method_voucher_make_multi_entity_aggregation_id_reference_property(e: PEntityType) {
  }

  def method_voucher_make_multi_entity_association_id_reference_property(e: PEntityType) {
  }

  def method_voucher_make_multi_entity_query_property(e: PEntityType) {
  }

  // update
  override def voucher_methods_update_attribute() {
    val fromName = "doc." + varName
    val toName = "this." + varName

    def update_attribute_one {
      def update(converter: String) {
        sm_if_else_null(fromName) {
          sm_assign_null(toName)
        }
        sm_else {
          sm_assign(toName, converter.format(fromName)) 
        }
      }

      attr.attributeType match { // sync PDomainServiceEntity
        case t: PDateTimeType => {
          sm_assign_this(varName, "doc." + varName)
//          sm_if("doc.%s != null || doc.%s_date != null || doc.%s_time != null || doc.%s_now", varName, varName, varName, varName) {
//            sm_assign_this(varName, "Util.makeDateTime(doc.%s, doc.%s_date, doc.%s_time, doc.%s_now)", varName, varName, varName, varName)
//          }
        }
        case e: PEntityType => {
          entityPersistentKind {
            new EntityAttributeKindFunction[Unit] {
              def apply_Simple() = method_voucher_update_multi_entity_simple(e)
              def apply_Composition_Reference() = method_voucher_update_multi_entity_composition_reference_property(e)
              def apply_Aggregation_Reference() = method_voucher_update_multi_entity_aggregation_reference_property(e)
              def apply_Association_Reference() = method_voucher_update_multi_entity_association_reference_property(e)
              def apply_Composition_Id() = method_voucher_update_multi_entity_composition_id_property(e)
              def apply_Aggregation_Id() = method_voucher_update_multi_entity_aggregation_id_property(e)
              def apply_Association_Id() = method_voucher_update_multi_entity_association_id_property(e)
              def apply_Composition_Id_Reference() = method_voucher_update_multi_entity_composition_id_reference_property(e)
              def apply_Aggregation_Id_Reference() = method_voucher_update_multi_entity_aggregation_id_reference_property(e)
              def apply_Association_Id_Reference() = method_voucher_update_multi_entity_association_id_reference_property(e)
              def apply_Query() = method_voucher_update_multi_entity_query_property(e)
            }
          }
/*          
          sm_if_not_null("doc." + pContext.variableName4RefId(attr)) {
            sm_assign_this(persistentVarName, "doc." + pContext.variableName4RefId(attr))
          }
*/
        }
        case p: PEntityPartType => {
        }
        case p: PPowertypeType => {
        }
        case v: PByteType    => update("%s.shortValue()")
        case v: PIntegerType => update("%s.toString()")
        case v: PDecimalType => update("%s.toString()")
        case _ => sm_assign_this(varName, "doc." + varName)
      }
    }

    def update_attribute_many {
      def update(converter: String) {
        sm_assign_new_ArrayList(toName, persistent_element_type())
        sm_for(attr.elementTypeName, "elem", fromName) {
          sm_pln("%s.add(%s);", toName, converter.format("elem")) 
        }
      }

      attr.attributeType match { // sync PDomainServiceEntity
        case t: PDateTimeType => { // XXX
          sm_if("doc.%s != null || doc.%s_date != null || doc.%s_time != null || doc.%s_now", varName, varName, varName, varName) {
            sm_assign_this(varName, "Util.makeDateTime(doc.%s, doc.%s_date, doc.%s_time, doc.%s_now)", varName, varName, varName, varName)
          }
        }
        case e: PEntityType => {
          entityPersistentKind {
            new EntityAttributeKindFunction[Unit] {
              def apply_Simple() = method_voucher_update_single_entity_simple(e)
              def apply_Composition_Reference() = method_voucher_update_single_entity_composition_reference_property(e)
              def apply_Aggregation_Reference() = method_voucher_update_single_entity_aggregation_reference_property(e)
              def apply_Association_Reference() = method_voucher_update_single_entity_association_reference_property(e)
              def apply_Composition_Id() = method_voucher_update_single_entity_composition_id_property(e)
              def apply_Aggregation_Id() = method_voucher_update_single_entity_aggregation_id_property(e)
              def apply_Association_Id() = method_voucher_update_single_entity_association_id_property(e)
              def apply_Composition_Id_Reference() = method_voucher_update_single_entity_composition_id_reference_property(e)
              def apply_Aggregation_Id_Reference() = method_voucher_update_single_entity_aggregation_id_reference_property(e)
              def apply_Association_Id_Reference() = method_voucher_update_single_entity_association_id_reference_property(e)
              def apply_Query() = method_voucher_update_single_entity_query_property(e)
            }
          }
/*
          // make_reference_key_unencoded_string
          if (is_owned_property()) {
            sm_pln("// TODO owned")
          } else if (is_query_property()) {
            sm_pln("// TODO query")
          } else {
            sm_pln("// plain")
            val idVarName = "this." + persistentVarName
            sm_assign_new_ArrayList(idVarName, e.entity.idAttr.elementTypeName, "doc." + pContext.variableName4RefId(attr))
          }
*/
        }
        case p: PEntityPartType => {
          sm_if_not_null("doc." + varName) {
            sm_assign_this_new_ArrayList(erPartVarName, java_element_type)
            sm_for(java_doc_element_type + " element: doc." + attrName) {
              sm_pln("this.%s.add(new %s(element, pm));", erPartVarName, java_element_type) 
            }
          }
        }
        case p: PPowertypeType => sys.error("not supported yet")
        case v: PByteType      => update("%s.shortValue()")
        case v: PIntegerType   => update("%s.toString()")
        case v: PDecimalType   => update("%s.toString()")
        case _ => sm_assign_this_new_ArrayList(varName, persistent_element_type(), "doc." + varName)
      }
    }

    if (is_settable) {
      if (attr.isHasMany) {
        update_attribute_many
      } else {
        update_attribute_one
      }
    }
/*
    def update_owned_property_many {
      sm_pln("// TODO owned many")
    }

    def update_owned_property_one {
      sm_pln("// TODO owned one")
    }

    def update_query_property_many {
      sm_pln("// TODO owned many")
    }

    def update_query_property_one {
      sm_pln("// TODO owned one")
    }

    if (is_settable) {
      if (is_owned_property) {
        if (attr.isHasMany) {
          update_owned_property_many
        } else {
          update_owned_property_one
        }
      } else if (is_query_property) {
        if (attr.isHasMany) {
          update_query_property_many
        } else {
          update_query_property_one
        }
      } else {
        if (attr.isHasMany) {
          update_attribute_many
        } else {
          update_attribute_one
        }
      }
    }
*/
  } // update_attribute

  def method_voucher_update_single_entity_simple(e: PEntityType) {
  }

  def method_voucher_update_single_entity_composition_reference_property(e: PEntityType) {
  }

  def method_voucher_update_single_entity_aggregation_reference_property(e: PEntityType) {
  }

  def method_voucher_update_single_entity_association_reference_property(e: PEntityType) {
  }

  def method_voucher_update_single_entity_composition_id_property(e: PEntityType) {
  }

  def method_voucher_update_single_entity_aggregation_id_property(e: PEntityType) {
  }

  def method_voucher_update_single_entity_association_id_property(e: PEntityType) {
  }

  def method_voucher_update_single_entity_composition_id_reference_property(entityType: PEntityType) {
    sm_for(persistent_element_type + " element: this." + erAssocVarName) {
      sm_pln("this.%s.add(element.get());", erPersistentVarName, entityType.entity.idName.capitalize) 
    } 
  }

  def method_voucher_update_single_entity_aggregation_id_reference_property(e: PEntityType) {
  }

  def method_voucher_update_single_entity_association_id_reference_property(e: PEntityType) {
  }

  def method_voucher_update_single_entity_query_property(e: PEntityType) {
  }
  
  def method_voucher_update_multi_entity_simple(e: PEntityType) {
  }

  def method_voucher_update_multi_entity_composition_reference_property(e: PEntityType) {
  }

  def method_voucher_update_multi_entity_aggregation_reference_property(e: PEntityType) {
  }

  def method_voucher_update_multi_entity_association_reference_property(e: PEntityType) {
  }

  def method_voucher_update_multi_entity_composition_id_property(e: PEntityType) {
  }

  def method_voucher_update_multi_entity_aggregation_id_property(e: PEntityType) {
  }

  def method_voucher_update_multi_entity_association_id_property(e: PEntityType) {
  }

  def method_voucher_update_multi_entity_composition_id_reference_property(entityType: PEntityType) {
    sm_for(persistent_element_type + " element: this." + erAssocVarName) {
      sm_pln("this.%s.add(element.get());", erPersistentVarName, entityType.entity.idName.capitalize) 
    } 
  }

  def method_voucher_update_multi_entity_aggregation_id_reference_property(e: PEntityType) {
  }

  def method_voucher_update_multi_entity_association_id_reference_property(e: PEntityType) {
  }

  def method_voucher_update_multi_entity_query_property(e: PEntityType) {
  }
}
