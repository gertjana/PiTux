package net.addictivesoftware.model

import net.liftweb.mapper._
import net.liftweb.sitemap.Loc.{LocGroup, If}
import net.liftweb.http._


class Leaders extends LongKeyedMapper[Leaders]
              with IdPK {
  def getSingleton = Leaders

  object culprits extends MappedString(this, 200)
  object successCount extends MappedLong(this)
  object failCount extends MappedLong(this)


}

object Leaders extends Leaders
               with LongKeyedMetaMapper[Leaders]
               with CRUDify[Long, Leaders] {

  override def editMenuLocParams = If(User.loggedIn_? _, () => RedirectResponse("/")) ::User.testSuperUser ::  super.editMenuLocParams
  override def viewMenuLocParams = If(User.loggedIn_? _, () => RedirectResponse("/")) :: User.testSuperUser :: super.viewMenuLocParams
  override def createMenuLocParams = If(User.loggedIn_? _, () => RedirectResponse("/")) :: User.testSuperUser :: LocGroup("crud", "crud") :: super.createMenuLocParams
  override def showAllMenuLocParams = If(User.loggedIn_? _, () => RedirectResponse("/")) :: User.testSuperUser :: LocGroup("crud", "crud") :: super.showAllMenuLocParams



}