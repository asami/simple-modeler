package org.simplemodeling.SimpleModeler.transformer.maker

import scalaz._, Scalaz._
import com.asamioffice.goldenport.text.UJavaString

/*
 * @since   Aug. 19, 2011
 *  version Aug. 21, 2011
 *  version May.  3, 2012
 *  version Nov. 23, 2012
 *  version Feb. 23, 2013
 *  version Aug. 11, 2014
 *  version Oct. 17, 2015
 * @version May. 16, 2020
 * @author  ASAMI, Tomoharu
 */
trait ScalaMakerHolder {
  private var _maker: ScalaMaker = null

  protected def sm_to_text: String = {
    _maker.toString
  }

  protected def sm_open(m: ScalaMaker) {
    _maker = m;
  }

  protected def sm_open(aspects: Seq[ScalaAspect]) {
    sm_open(new ScalaMaker, aspects)
  }

  protected def sm_open(m: ScalaMaker, aspects: Seq[ScalaAspect]) {
    require (m != null)
    _maker = m;
    aspects.foreach(_.open(_maker))
  }

  protected def sm_maker = {
    assert (_maker != null)
    _maker
  }

  //
  protected def sm_p(s: String, params: Any*): String = {
    _maker.print(s, params.map(_.asInstanceOf[AnyRef]): _*)
    s
  }

  protected def sm_pln(s: String, params: Any*) {
    _maker.println(s, params.map(_.asInstanceOf[AnyRef]): _*)
  }

  protected def sm_pln() {
    _maker.println()
  }

  protected def sm_block(o: String, ops: Any*)(c: String, cps: Any*)(b: => Unit) {
    _maker.println(o, ops.map(_.asInstanceOf[AnyRef]): _*)
    sm_indent_up
    b
    sm_indent_down
    _maker.println(c, cps.map(_.asInstanceOf[AnyRef]): _*)
  }

  protected def sm_blockp(o: String, ops: Any*)(b: => Unit) {
    _maker.println(o + " {", ops.map(_.asInstanceOf[AnyRef]): _*)
    sm_indent_up
    b
    sm_indent_down
    _maker.println("}")
  }

  protected def sm_blockps(o: String, ops: Any*)(b: => Unit) {
    _maker.println(o + "(", ops.map(_.asInstanceOf[AnyRef]): _*)
    sm_indent_up
    b
    sm_indent_down
    _maker.println(")")
  }

  protected final def sm_string_literal(s: Any) {
    sm_p(to_string_literal(s))
  }

  protected final def to_string_literal(s: Any) = {
    UJavaString.stringLiteral(s.toString)
  }

  protected final def sm_vector_string_literal(values: Seq[Any]) {
    sm_pln("Vector(")
    sm_indent_up
    values.headOption.foreach { x =>
      sm_string_literal(x)
      values.tail.foreach { y =>
        sm_p(",")
        sm_pln
        sm_string_literal(y)
      }
      sm_pln
    }
    sm_indent_down
    sm_pln(")")
  }

  protected final def sm_vector_value(values: Seq[Any]) {
    sm_pln("Vector(")
    sm_indent_up
    values.headOption.foreach { x =>
      sm_p(x.toString)
      values.tail.foreach { y =>
        sm_p(",")
        sm_pln
        sm_p(y.toString)
      }
      sm_pln
    }
    sm_indent_down
    sm_pln(")")
  }

  // 2012-11-20
  /**
   * Used by ScalaClassDefinition.
   */
  protected final def sm_param_list(params: Seq[String]) {
    params.toList match {
      case Nil => sm_p("()")
      case x :: Nil => {
        sm_p("(")
        sm_p(x)
        sm_p(")")
      }
      case List(xs) if xs.length < 3 => {
        sm_p("(")
        sm_p(xs.mkString(", "))
        sm_p(")")
      }
      case x :: xs => {
        sm_pln("(")
        sm_indent_up
        sm_p(x)
        for (a <- xs) {
          sm_pln(",")
          sm_p(a)
        }
        sm_pln
        sm_indent_down
        sm_p(")")
      }
    }
  }

  protected final def sm_list(params: Seq[String]) {
    params.toList match {
      case Nil => Unit
      case x :: Nil =>
        sm_p(x)
      case List(xs) if xs.length < 3 =>
        sm_p(xs.mkString(", "))
      case x :: xs =>
        sm_p(x)
        for (a <- xs) {
          sm_pln(",")
          sm_p(a)
        }
        sm_pln
    }
  }

  protected final def sm_param_list_or_empty(params: Seq[String]) {
    if (params.nonEmpty) sm_param_list(params)
  }

  protected final def sm_param_tree(params: Tree[String]) {
    import Stream.Empty
    params.subForest match {
      case Empty => sm_p("()")
      case x #:: Stream.Empty => {
        sm_p("(")
        sm_param_tree_inner(x)
        sm_p(")")
      }
      case x #:: xs => {
        sm_pln("(")
        sm_indent_up
        sm_param_tree_inner(x)
        for (a <- xs) {
          sm_pln(",")
          sm_param_tree_inner(a)
        }
        sm_pln
        sm_indent_down
        sm_pln(")")
      }
    }
  }

  protected final def sm_param_tree_inner(param: Tree[String]) {
    import Stream.Empty
    param.subForest match {
      case Empty => sm_p(param.rootLabel)
      case x #:: Stream.Empty => {
        sm_p(param.rootLabel)
        sm_p("(")
        sm_param_tree_inner(x)
        sm_p(")")
      }
      case x #:: xs => {
        sm_p(param.rootLabel)
        sm_pln("(")
        sm_indent_up
        sm_param_tree_inner(x)
        for (a <- xs) {
          sm_pln(",")
          sm_param_tree_inner(a)
        }
        sm_pln
        sm_indent_down
        sm_p(")")
      }
    }
  }

  // 2012-11-20
  /**
   * Used by custom configured class definition.
   */
  protected final def sm_object(name: String, parent: String = null, params: Seq[String] = Nil, mixins: Seq[String] = Nil)(body: => Unit) {
    _maker.print("object ")
    _maker.print(name)
    if (parent != null) {
      _maker.print(" extends ")
      _maker.print(parent)
      sm_param_list_or_empty(params)
      if (mixins.nonEmpty) {
        _maker.print(" with ")
        _maker.print(mixins.mkString(" with "))
      }
      _maker.println(" {")
    } else if (mixins.nonEmpty) {
      _maker.print(" extends ")
      _maker.print(mixins.mkString(" with "))
      _maker.println(" {")
    } else {
      _maker.println(" {")
    }
    sm_indent_up
    body
    sm_indent_down
    _maker.println("}")
  }

  protected final def sm_case_object(
    name: String,
    parent: String = null,
    params: Seq[String] = Nil,
    mixins: Seq[String] = Nil
  )(body: => Unit) {
    _maker.print("case ")
    sm_object(name, parent, params, mixins)(body)
  }

  // 2012-11-20
  /**
   * Used by custom configured class definition.
   */
  protected final def sm_object_without_body(name: String, parent: String = null, params: Seq[String] = Nil, mixins: Seq[String] = Nil) {
    _maker.print("object ")
    _maker.print(name)
    if (parent != null) {
      _maker.print(" extends ")
      _maker.print(parent)
      sm_param_list_or_empty(params)
      if (mixins.isEmpty) {
        _maker.println
      } else {
        _maker.print(" with ")
        _maker.println(mixins.mkString(" with "))
      }
    } else if (mixins.nonEmpty) {
      _maker.print(" extends ")
      _maker.println(mixins.mkString(" with "))
    } else {
      _maker.println
    }
  }

  protected final def sm_object_tree_params_without_body(name: String, parent: String, params: Tree[String], mixins: Seq[String] = Nil) {
    require (parent != null, "parent must not be null.")
    _maker.print("object ")
    _maker.print(name)
    _maker.print(" extends ")
    _maker.print(parent)
    sm_param_tree(params)
    if (mixins.isEmpty) {
      _maker.println
    } else {
      _maker.print(" with ")
      _maker.println(mixins.mkString(" with "))
    }
  }

  protected final def sm_val_case_class_tree_params(name: String, caseclass: String, params: Tree[String]) {
    _maker.print("val ")
    _maker.print(name)
    _maker.print(" = ")
    _maker.print(caseclass)
    sm_param_tree(params)
  }

  //
  protected final def sm_package(name: String) {
    _maker.declarePackage(name)
  }

  protected final def sm_end_package_section() {
    _maker.endPackageSection()
  }

  protected final def sm_import(aName: String) {
    _maker.declareImport(aName)
  }

  protected final def sm_end_import_section() {
    _maker.endImportSection()
  }

  /*
   * variable settings.
   */
  protected final def sm_val_string(varname: String, value: String) {
    _maker.p("val ")
    _maker.p(varname)
    _maker.p(" = ")
    _maker.pln(to_string_literal(value))
  }

  protected final def sm_val_vector_string_literal(varname: String, values: Seq[Any]) {
    sm_p("val ")
    sm_p(varname)
    sm_p(" = ")
    sm_vector_string_literal(values)
  }

  protected final def sm_val_vector_value(varname: String, values: Seq[Any]) {
    sm_p("val ")
    sm_p(varname)
    sm_p(" = ")
    sm_vector_value(values)
  }

  /*
   * old java style variable settings.
   */
  // XXX
  protected final def sm_private_instance_variable(attr: PAttribute, typename: String = null, varname: String = null) {
    val tname = if (typename == null) attr.typeName else typename;
    val vname = if (varname == null) attr.name else varname;
    if (attr.isHasMany) {
      sm_private_instance_variable_list(tname, vname);
    } else {
      sm_private_instance_variable_single(tname, vname);
    }
  }

  // XXX
  protected final def sm_private_instance_variable_single(typename: String, varname: String) {
    _maker.privateInstanceVariableSingle(typename, varname);
  }

  // XXX
  protected final def sm_private_instance_variable_list(typename: String, varname: String) {
    _maker.privateInstanceVariableList(typename, varname);
  }

  // XXX
  protected final def sm_public_final_instance_variable(attr: PAttribute, typename: String = null, varname: String = null) {
    val tname = if (typename == null) attr.typeName else typename;
    val vname = if (varname == null) attr.name else varname;
    if (attr.isHasMany) {
      sm_public_final_instance_variable_list(tname, vname);
    } else {
      sm_public_final_instance_variable_single(tname, vname);
    }
  }

  // XXX
  protected final def sm_public_final_instance_variable_single(typename: String, varname: String) {
    _maker.publicFinalInstanceVariableSingle(typename, varname);
  }

  // XXX
  protected final def sm_public_final_instance_variable_list(typename: String, varname: String) {
    _maker.publicFinalInstanceVariableList(typename, varname);
  }

  // XXX
  protected final def sm_public_instance_variable(attr: PAttribute, typename: String = null, varname: String = null) {
    val tname = if (typename == null) attr.typeName else typename;
    val vname = if (varname == null) attr.name else varname;
    if (attr.isHasMany) {
      sm_public_instance_variable_list(tname, vname);
    } else {
      sm_public_instance_variable_single(tname, vname);
    }
  }

  // XXX
  protected final def sm_public_instance_variable_single(typename: String, varname: String) {
    _maker.publicInstanceVariableSingle(typename, varname);
  }

  // XXX
  protected final def sm_public_instance_variable_list(typename: String, varname: String) {
    _maker.publicInstanceVariableList(typename, varname);
  }

  // XXX
  protected final def sm_private_transient_instance_variable(attr: PAttribute, typename: String = null, varname: String = null) {
    val tname = if (typename == null) attr.typeName else typename;
    val vname = if (varname == null) attr.name else varname;
    if (attr.isHasMany) {
      sm_private_transient_instance_variable_list(tname, vname);
    } else {
      sm_private_transient_instance_variable_single(tname, vname);
    }
  }

  // XXX
  protected final def sm_private_transient_instance_variable_single(typename: String, varname: String) {
    _maker.privateTransientInstanceVariableSingle(typename, varname);
  }

  // XXX
  protected final def sm_private_transient_instance_variable_list(typename: String, varname: String) {
    _maker.privateTransientInstanceVariableList(typename, varname);
  }

  // XXX
  protected final def sm_public_static_final_String_literal(varname: String, form: String, params: AnyRef*) {
    _maker.pln("""public static final String %s = "%s";""", varname, _format(form, params))
  }

  // XXX
  protected final def sm_public_static_final_int(varname: String, form: String, params: AnyRef*) {
    _maker.pln("""public static final int %s = %s;""", varname, _format(form, params))
  }

  // XXX
  protected final def sm_public_static_final(typename: String, varname: String, form: String, params: AnyRef*) {
    _maker.pln("""public static final %s %s = %s;""", typename, varname, _format(form, params))
  }

  // XXX
  protected final def sm_private_static_final_String_literal(varname: String, form: String, params: AnyRef*) {
    _maker.pln("""private static final String %s = "%s";""", varname, _format(form, params))
  }

  // XXX
  protected final def sm_private_static_final_int(varname: String, value: Int) {
    _maker.pln("""private static final int %s = %s;""", varname, value.toString)
  }

  // XXX
  protected final def sm_private_static_final_int(varname: String, form: String, params: AnyRef*) {
    _maker.pln("""private static final int %s = %s;""", varname, _format(form, params))
  }

  // XXX
  protected final def sm_private_static_final(typename: String, varname: String, form: String, params: AnyRef*) {
    _maker.pln("""private static final %s %s = %s;""", typename, varname, _format(form, params))
  }

  // method
  // protected final def sm_method(signature: String, params: AnyRef*)(body: => Unit) {
  //   _maker.method(signature, params: _*)(body)
  // }

  protected final def sm_def(signature: String, params: AnyRef*)(body: => Unit) {
    _maker.publicMethod(signature, params: _*)(body)
  }

  protected final def sm_override_def(signature: String, params: AnyRef*)(body: => Unit) {
    _maker.overridePublicMethod(signature, params: _*)(body)
  }

  protected final def sm_def_string(varname: String, value: Any) {
    _maker.p("def ")
    _maker.p(varname)
    _maker.p(" = ")
    _maker.pln(to_string_literal(value.toString))
  }

  protected final def sm_override_def_string(varname: String, value: Any) {
    _maker.p("override ")
    sm_def_string(varname, value)
  }

  protected final def sm_def_value(varname: String, value: Any) {
    _maker.p("def ")
    _maker.p(varname)
    _maker.p(" = ")
    _maker.pln(value.toString)
  }

  protected final def sm_override_def_value(varname: String, value: Any) {
    _maker.p("override ")
    sm_def_value(varname, value)
  }

  // XXX
  protected final def sm_public_method(signature: String, params: AnyRef*)(body: => Unit) {
    _maker.publicMethod(signature, params: _*)(body)
  }

  // XXX
  protected final def sm_override_public_method(signature: String, params: AnyRef*)(body: => Unit) {
    _maker.pln
    _maker.p("@Override") // XXX
    _maker.publicMethod(signature, params: _*)(body)
  }

  // XXX
  protected final def sm_public_void_method(signature: String, params: AnyRef*)(body: => Unit) {
    _maker.publicVoidMethod(signature, params: _*)(body)
  }

  // XXX
  protected final def sm_public_get_method(typeName: String, attrName: String, expr: AnyRef*) {
    _maker.publicGetMethod(typeName, attrName, expr: _*)
  }

  // XXX
  protected final def sm_public_get_or_null_method(typeName: String, attrName: String, varName: String, expr: String, params: AnyRef*) {
    _maker.publicGetOrNullMethod(typeName, attrName, varName, expr, params: _*)
  }

  // XXX
  protected final def sm_public_is_method(attrName: String, expr: AnyRef*) {
    _maker.publicIsMethod(attrName, expr: _*)
  }

  // XXX
  protected final def sm_public_set_method(attrName: String, typeName: String,
      paramName: String = null, varName: String = null, expr: Seq[AnyRef] = Nil) {
    _maker.publicSetMethod(attrName, typeName, paramName, varName, expr)
  }

  // XXX
  protected final def sm_public_set_or_null_method(attrName: String, typeName: String,
      paramName: String = null, varName: String = null, expr: Seq[AnyRef] = Nil) {
    _maker.publicSetOrNullMethod(attrName, typeName, paramName, varName, expr)
  }

  // XXX
  protected final def sm_public_with_method(className: String, attrName: String, typeName: String,
      paramName: String = null, varName: String = null, expr: Seq[AnyRef] = Nil) {
    _maker.publicWithMethod(className, attrName, typeName, paramName, varName, expr)
  }

  // XXX
  protected final def sm_public_with_or_null_method(className: String, attrName: String, typeName: String,
      paramName: String = null, varName: String = null, expr: Seq[AnyRef] = Nil) {
    _maker.publicWithOrNullMethod(className, attrName, typeName, paramName, varName, expr)
  }

  // XXX
  protected final def sm_override_protected_method(signature: String, params: AnyRef*)(body: => Unit) {
    _maker.pln
    _maker.p("@Override") // XXX
    _maker.protectedMethod(signature, params: _*)(body)
  }

  // XXX
  protected final def sm_protected_final_void_method(signature: String, params: AnyRef*)(body: => Unit) {
    _maker.protectedFinalVoidMethod(signature, params: _*)(body)
  }

  // XXX
  protected final def sm_private_method(signature: String, params: AnyRef*)(body: => Unit) {
    _maker.privateMethod(signature, params: _*)(body)
  }

  // XXX
  protected final def sm_public_static_method(signature: String, params: AnyRef*)(body: => Unit) {
    _maker.publicStaticMethod(signature, params: _*)(body)
  }

  // XXX
  protected final def sm_public_set_list_method(attrName: String, elemTypeName: String, paramName: String, varName: String) {
    val pname = if (paramName != null) paramName else attrName
    val vname = if (varName != null) varName else attrName
    sm_public_void_method("set%s(List<%s> %s)", attrName.capitalize, elemTypeName, pname) {
      sm_assign_this(varName, "new ArrayList<%s>(%s)", elemTypeName, pname)
    }
  }

  // XXX
  protected final def sm_public_add_list_element_method(attrName: String, elemTypeName: String, paramName: String, varName: String) {
    val pname = if (paramName != null) paramName else attrName
    val vname = if (varName != null) varName else attrName
    sm_public_void_method("add%s(%s %s)", attrName, elemTypeName, pname) {
      sm_if("this." + vname + " == null") {
        sm_assign_this(vname, "new ArrayList<%s>()", elemTypeName) 
      }
      sm_pln("this.%s.add(%s);", vname, pname);
    }
  }

  // XXX
  protected final def sm_public_get_list_method(elemTypeName: String, attrName: String, varName: String) {
    val vname = if (varName != null) varName else attrName;  
    sm_public_method("%s get%s()", elemTypeName, attrName.capitalize) {
      sm_if_else(vname + " != null") {
        sm_pln("return Collections.unmodifiableList(%s);", vname)
      }
      sm_else {
        sm_pln("return Collections.emptyList();")
      }
    }
  }

  // XXX
  protected final def sm_public_get_list_method_prologue(elemTypeName: String, attrName: String, varName: String)(body: => Unit) {
    val vname = if (varName != null) varName else attrName;  
    sm_public_method("%s get%s()", elemTypeName, attrName.capitalize) {
      body
      sm_if_else(vname + " != null") {
        sm_pln("return Collections.unmodifiableList(%s);", vname)
      }
      sm_else {
        sm_pln("return Collections.emptyList();")
      }
    }
  }

/*
  // constructor
  protected final def sm_public_constructor(signature: String, params: AnyRef*)(body: => Unit) {
    _maker.publicConstructor(signature, params: _*)(body)
  }
*/  

  // DI
  protected final def sm_inject {
    _maker.makeAnnotation("@Inject")
  }

  // static
  protected final def sm_static(body: => Unit) {
    _maker.staticBlock(body)
  }

  // if
  protected final def sm_if(condition: String, params: AnyRef*)(body: => Unit) {
    _maker.makeIf(condition, params: _*)(body)
  }

  protected final def sm_if_else(condition: String, params: AnyRef*)(body: => Unit) {
    _maker.makeIfElse(condition, params: _*)(body)
  }

  protected final def sm_else_if(condition: String, params: AnyRef*)(body: => Unit) {
    _maker.makeElseIf(condition, params: _*)(body)
  }

  protected final def sm_else_if_else(condition: String, params: AnyRef*)(body: => Unit) {
    _maker.makeElseIfElse(condition, params: _*)(body)
  }

  protected final def sm_else(body: => Unit) {
    _maker.makeElse(body)
  }

  // if variants
  protected final def sm_if_null(expr: String, params: AnyRef*)(body: => Unit) {
    _maker.makeIfNull(expr, params: _*)(body)
  }

  protected final def sm_if_not_null(expr: String, params: AnyRef*)(body: => Unit) {
    _maker.makeIfNotNull(expr, params: _*)(body)
  }

  protected final def sm_if_return(condition: String, cparams: AnyRef*) {
    _maker.makeIfReturn(condition, cparams: _*)
  }

  protected final def sm_if_return_expr(condition: String, cparams: AnyRef*)(expr: String, eparams: AnyRef*) {
    _maker.makeIfReturnExpr(condition, cparams: _*)(expr, eparams: _*)
  }

  protected final def sm_if_null_return(condition: String, cparams: AnyRef*) {
    _maker.makeIfNullReturn(condition, cparams: _*)
  }

  protected final def sm_if_null_return_expr(condition: String, cparams: AnyRef*)(expr: String, eparams: AnyRef*) {
    _maker.makeIfNullReturnExpr(condition, cparams: _*)(expr, eparams: _*)
  }

  protected final def sm_if_not_null_return(condition: String, cparams: AnyRef*) {
    _maker.makeIfNotNullReturn(condition, cparams: _*)
  }

  protected final def sm_if_not_null_return_expr(condition: String, cparams: AnyRef*)(expr: String, eparams: AnyRef*) {
    _maker.makeIfNotNullReturnExpr(condition, cparams: _*)(expr, eparams: _*)
  }

  // XXX
  protected final def sm_if_not_equals_expr(lhs: String, lparams: AnyRef*)(rhs: String, rparams: AnyRef*)(body: => Unit) {
    _maker.makeIf("!%s.equals(%s)", _format(lhs, lparams), _format(rhs, rparams)) {
      body
    }
  }

  // if else variants
  protected final def sm_if_else_null(expr: String, params: AnyRef*)(body: => Unit) {
    _maker.makeIfElseNull(expr, params: _*)(body)
  }

  protected final def sm_if_else_not_null(expr: String, params: AnyRef*)(body: => Unit) {
    _maker.makeIfElseNotNull(expr, params: _*)(body)
  }

  protected final def sm_if_else_return(condition: String, cparams: AnyRef*) {
    _maker.makeIfElseReturn(condition, cparams: _*)
  }

  protected final def sm_if_else_return_expr(condition: String, cparams: AnyRef*)(expr: String, eparams: AnyRef*) {
    _maker.makeIfElseReturnExpr(condition, cparams: _*)(expr, eparams: _*)
  }

  protected final def sm_if_else_null_return(condition: String, cparams: AnyRef*) {
    _maker.makeIfElseNullReturn(condition, cparams: _*)
  }

  protected final def sm_if_else_null_return_expr(condition: String, cparams: AnyRef*)(expr: String, eparams: AnyRef*) {
    _maker.makeIfElseNullReturnExpr(condition, cparams: _*)(expr, eparams: _*)
  }

  protected final def sm_if_else_not_null_return(condition: String, cparams: AnyRef*) {
    _maker.makeIfElseNotNullReturn(condition, cparams: _*)
  }

  protected final def sm_if_else_not_null_return_expr(condition: String, cparams: AnyRef*)(expr: String, eparams: AnyRef*) {
    _maker.makeIfElseNotNullReturnExpr(condition, cparams: _*)(expr, eparams: _*)
  }
  
  // else if variants
  protected final def sm_else_if_null(expr: String, params: AnyRef*)(body: => Unit) {
    _maker.makeElseIfNull(expr, params: _*)(body)
  }

  protected final def sm_else_if_not_null(expr: String, params: AnyRef*)(body: => Unit) {
    _maker.makeElseIfNotNull(expr, params: _*)(body)
  }

  protected final def sm_else_if_return(condition: String, cparams: AnyRef*) {
    _maker.makeElseIfReturn(condition, cparams: _*)
  }

  protected final def sm_else_if_return_expr(condition: String, cparams: AnyRef*)(expr: String, eparams: AnyRef*) {
    _maker.makeElseIfReturnExpr(condition, cparams: _*)(expr, eparams: _*)
  }

  protected final def sm_else_if_null_return(condition: String, cparams: AnyRef*) {
    _maker.makeElseIfNullReturn(condition, cparams: _*)
  }

  protected final def sm_else_if_null_return_expr(condition: String, cparams: AnyRef*)(expr: String, eparams: AnyRef*) {
    _maker.makeElseIfNullReturnExpr(condition, cparams: _*)(expr, eparams: _*)
  }

  protected final def sm_else_if_not_null_return(condition: String, cparams: AnyRef*) {
    _maker.makeElseIfNotNullReturn(condition, cparams: _*)
  }

  protected final def sm_else_if_not_null_return_expr(condition: String, cparams: AnyRef*)(expr: String, eparams: AnyRef*) {
    _maker.makeElseIfNotNullReturnExpr(condition, cparams: _*)(expr, eparams: _*)
  }

  // else if else variants
  protected final def sm_else_if_else_null(expr: String, params: AnyRef*)(body: => Unit) {
    _maker.makeElseIfElseNull(expr, params: _*)(body)
  }

  protected final def sm_else_if_else_not_null(expr: String, params: AnyRef*)(body: => Unit) {
    _maker.makeElseIfElseNotNull(expr, params: _*)(body)
  }

  protected final def sm_else_if_else_return(condition: String, cparams: AnyRef*) {
    _maker.makeElseIfElseReturn(condition, cparams: _*)
  }

  protected final def sm_else_if_else_return_expr(condition: String, cparams: AnyRef*)(expr: String, eparams: AnyRef*) {
    _maker.makeElseIfElseReturnExpr(condition, cparams: _*)(expr, eparams: _*)
  }

  protected final def sm_else_if_else_null_return(condition: String, cparams: AnyRef*) {
    _maker.makeElseIfElseNullReturn(condition, cparams: _*)
  }

  protected final def sm_else_if_else_null_return_expr(condition: String, cparams: AnyRef*)(expr: String, eparams: AnyRef*) {
    _maker.makeElseIfElseNullReturnExpr(condition, cparams: _*)(expr, eparams: _*)
  }

  protected final def sm_else_if_else_not_null_return(condition: String, cparams: AnyRef*) {
    _maker.makeElseIfElseNotNullReturn(condition, cparams: _*)
  }

  protected final def sm_else_if_else_not_null_return_expr(condition: String, cparams: AnyRef*)(expr: String, eparams: AnyRef*) {
    _maker.makeElseIfElseNotNullReturnExpr(condition, cparams: _*)(expr, eparams: _*)
  }

  // for
  protected final def sm_for(typeName: String, varName: String, generator: String)(body: => Unit) {
    _maker.makeFor(typeName, varName, generator)(body)
  }

  protected final def sm_for(expr: String)(body: => Unit) {
    _maker.makeFor(expr)(body)
  }

  protected final def sm_while(expr: String, params: AnyRef*)(body: => Unit) {
    _maker.makeWhile(expr, params: _*)(body)
  }

  protected final def sm_switch(cond: String, params: AnyRef*)(body: => Unit) {
    _maker.makeSwitch(cond, params: _*)(body)
  }

  protected final def sm_case(pattern: String)(body: => Unit) {
    _maker.makeCase(pattern)(body)
  }

  protected final def sm_case_return(pattern: String, form: String, params: AnyRef*) {
    _maker.makeCaseReturn(pattern, form, params: _*)
  }

  protected final def sm_case_default(body: => Unit) {
    _maker.makeCaseDefault(body)
  }

  // try
  protected final def sm_try(body: => Unit) {
    _maker.makeTry(body)
  }

  protected final def sm_catch(condition: String)(body: => Unit) {
    _maker.makeCatch(condition)(body)
  }

  protected final def sm_catch_end(condition: String)(body: => Unit) {
    _maker.makeCatchEnd(condition)(body)
  }

  protected final def sm_catch_nop(condition: String) {
    _maker.makeCatchNop(condition)
  }

  protected final def sm_finally(body: => Unit) {
    _maker.makeFinally(body)
  }

  protected final def sm_return(expr: String, params: AnyRef*) {
    _maker.makeReturn(_format(expr, params))
  }

  protected final def sm_return() {
    _maker.makeReturn()
  }

  protected final def sm_return_null() {
    _maker.makeReturnNull()
  }

  protected final def sm_return_true() {
    _maker.makeReturnTrue()
  }

  protected final def sm_return_false() {
    _maker.makeReturnFalse()
  }

  protected final def sm_return_this() {
    _maker.makeReturnThis()
  }

  // assign
  protected final def sm_assign(varname: String, expr: String) {
    _maker.assign(varname, expr)
  }

  protected final def sm_assign(varname: String, expr: String, args: AnyRef*) {
    _maker.assign(varname, expr, args: _*)
  }

  protected final def sm_assign_null(varname: String) {
    _maker.assignNull(varname)
  }

  protected final def sm_assign_true(varname: String) {
    _maker.assignTrue(varname)
  }

  protected final def sm_assign_new(varname: String, classname: String) {
    _maker.assignNew(varname, classname)
  }

  protected final def sm_assign_new(varname: String, classname: String, signature: String, params: AnyRef*) {
    _maker.assignNew(varname, classname, _format(signature, params))
  }

  protected final def sm_assign_new_Container(varname: String, classname: String, containee: String) {
    _maker.assignNewContainer(varname, classname, containee)
  }

  protected final def sm_assign_new_Container(varname: String, classname: String, containee: String, signature: String, params: AnyRef*) {
    _maker.assignNewContainer(varname, classname, containee, signature, params)
  }

  protected final def sm_assign_new_ArrayList(varname: String, classname: String) {
    _maker.assignNewArrayList(varname, classname)
  }

  protected final def sm_assign_new_ArrayList(varname: String, classname: String, signature: String, params: AnyRef*) {
    _maker.assignNewArrayList(varname, classname, signature, params: _*)
  }

  // assign this
  protected final def sm_assign_this(varname: String, expr: String, args: AnyRef*) {
    _maker.assignThis(varname, expr, args: _*)
  }

  protected final def sm_assign_this_null(varname: String) {
    _maker.assignThisNull(varname)
  }

  protected final def sm_assign_this_true(varname: String) {
    _maker.assignThisTrue(varname)
  }

  protected final def sm_assign_this_new(varname: String, classname: String) {
    _maker.assignThisNew(varname, classname)
  }

  protected final def sm_assign_this_new(varname: String, classname: String, signature: String, params: AnyRef*) {
    _maker.assignThisNew(varname, classname, _format(signature, params))
  }

  protected final def sm_assign_this_new_Container(varname: String, classname: String, containee: String) {
    _maker.assignThisNewContainer(varname, classname, containee)
  }

  protected final def sm_assign_this_new_Container(varname: String, classname: String, containee: String, signature: String, params: AnyRef*) {
    _maker.assignThisNewContainer(varname, classname, containee, signature, params)
  }

  protected final def sm_assign_this_new_ArrayList(varname: String, classname: String) {
    _maker.assignThisNewArrayList(varname, classname)
  }

  protected final def sm_assign_this_new_ArrayList(varname: String, classname: String, signature: String, params: AnyRef*) {
    _maker.assignThisNewArrayList(varname, classname, signature, params)
  }

  protected final def sm_assign_this_null_or_object(attrname: String, nullvalue: String = "null")
                                     (rawtype: String, rawform: String, rawparams: AnyRef*)
                                     (newform: String) {
    val varname = attrname + "value"
    sm_var(rawtype, varname, rawform, rawparams: _*)
    sm_if_else("%s == %s", varname, nullvalue) {
      sm_assign_this_null(attrname)
    }
    sm_else {
      sm_assign_this(attrname, newform, varname)
    }
  }

  // var
  protected final def sm_var(typename: String, varname: String, expr: String, params: AnyRef*) {
    _maker.makeVar(typename, varname, expr, params: _*)
  }

  protected final def sm_var_String(varname: String, expr: String, params: AnyRef*) {
    _maker.makeVarString(varname, expr, params: _*)
  }

  protected final def sm_var_null(varname: String, typename: String) {
    _maker.makeVarNull(typename, varname)
  }

  protected final def sm_var_new(typename: String, varname: String) {
    _maker.makeVar(typename, varname)
  }

  protected final def sm_var_List_new_ArrayList(classname: String, varname: String) {
    _maker.varListNewArrayList(classname, varname)
  }

  protected final def sm_var_new_StringBuilder() {
    _maker.makeStringBuilderVar()
  }

  protected final def sm_return_StringBuilder() {
    _maker.makeStringBuilderReturn()
  }

  protected final def sm_append_String(s: String, params: AnyRef*) {
    _maker.makeAppendString(s, params: _*)
  }

  protected final def sm_append_var_String(varname: String, s: String, params: AnyRef*) {
    _maker.makeAppendVarString(varname, s, params: _*)
  }

  protected final def sm_append_expr(s: String, params: AnyRef*) {
    _maker.makeAppendExpr(s, params: _*)
  }

  protected final def sm_var_append_var_expr(varname: String, s: String, params: AnyRef*) {
    _maker.makeAppendVarExpr(varname, s, params: _*)
  }

  protected final def sm_UnsupportedOperationException() {
    _maker.pln("throw new UnsupportedOperationException();")
  }

  protected final def sm_code(template: CharSequence, replaces: Map[String, String] = Map.empty) {
    _maker.code(template, replaces)
  }

  //
  protected final def sm_indent_up() {
    _maker.indentUp
  }

  protected final def sm_indent_down() {
    _maker.indentDown
  }

  private def _format(format: String, params: Seq[AnyRef]) = {
    if (params.isEmpty) format
    else format.format(params: _*)
  }
}
