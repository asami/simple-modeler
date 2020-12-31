package org.simplemodeling.model

/*
 * Migrated from SAttributeKind.
 *
 * @since   Sep. 12, 2008
 *  version Oct. 24, 2009
 *  version Feb.  6, 2013
 * @version Jan.  4, 2020
 * @author  ASAMI, Tomoharu
 */
class MAttributeKind {
}

class NullAttributeKind extends MAttributeKind
object NullAttributeKind extends NullAttributeKind
class IdAttributeKind extends MAttributeKind
object IdAttributeKind extends IdAttributeKind
class NameAttributeKind extends MAttributeKind
object NameAttributeKind extends NameAttributeKind
class LogicalDeleteAttributeKind extends MAttributeKind
object LogicalDeleteAttributeKind extends LogicalDeleteAttributeKind

/**
 * Googleアカウントと連結。(OpenIdは?)
 */
class UserAttributeKind extends MAttributeKind
object UserAttributeKind extends UserAttributeKind

/**
 * atom:feed/atom:title
 * atom:entry/atom:title
 * app:workspace/atom:title
 * app:collection/atom:title
 */
class TitleAttributeKind extends MAttributeKind
object TitleAttributeKind extends TitleAttributeKind

/**
 * atom:feed/atom:subtitle
 */
class SubTitleAttributeKind extends MAttributeKind
object SubTitleAttributeKind extends SubTitleAttributeKind

/**
 * atom:entry/atom:summary
 */
class SummaryAttributeKind extends MAttributeKind
object SummaryAttributeKind extends SummaryAttributeKind

/**
 * atom:entry/atom:author
 * atom:feed/atom:author
 *
 * Googleアカウントと連結。(OpenIdは?)
 */
class AuthorAttributeKind extends MAttributeKind
object AuthorAttributeKind extends AuthorAttributeKind

/**
 * atom:entry/atom:category*
 * atom:feed/atom:category*
 */
class CategoryAttributeKind extends MAttributeKind
object CategoryAttributeKind extends CategoryAttributeKind

/**
 * app:collection/app:categories*
 * schemeごとにcategoryの集合を定義
 * 
 */
class CategoriesAttributeKind extends MAttributeKind
object CategoriesAttributeKind extends CategoriesAttributeKind

/**
 * atom:feed/atom:icon
 * アイコン画像の管理ロジックを自動生成。
 */
class IconAttributeKind extends MAttributeKind
object IconAttributeKind extends IconAttributeKind

/**
 * atom:feed/atom:logo
 * ロゴ画像の管理ロジックを自動生成。
 */
class LogoAttributeKind extends MAttributeKind
object LogoAttributeKind extends LogoAttributeKind

/**
 * atom:entry/atom:link
 * atom:feed/atom:link
 */
class LinkAttributeKind extends MAttributeKind
object LinkAttributeKind extends LinkAttributeKind

/**
 * atom:entry/atom:content
 */
class ContentAttributeKind extends MAttributeKind
object ContentAttributeKind extends ContentAttributeKind

/**
 * atom:entry/atom:published
 */
class CreatedAttributeKind extends MAttributeKind
object CreatedAttributeKind extends CreatedAttributeKind

/**
 * atom:entry/atom:updated
 * atom:feed/atom:updated
 */
class UpdatedAttributeKind extends MAttributeKind
object UpdatedAttributeKind extends UpdatedAttributeKind

/**
 * Used for logical delete.
 */
class DeletedAttributeKind extends MAttributeKind
object DeletedAttributeKind extends UpdatedAttributeKind
