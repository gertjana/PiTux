package net.addictivesoftware.model

import net.liftweb.mapper._
import net.liftweb.sitemap.Loc.{LocGroup, If}
import net.liftweb.http.RedirectResponse


class BuildStatus extends LongKeyedMapper[BuildStatus]
                  with IdPK  {

  def getSingleton = BuildStatus

  object job extends MappedString(this, 100)
  object buildId extends MappedString(this,100)
  object number extends MappedLong(this)
  object result extends MappedString(this, 100)
  object timestamp extends MappedDateTime(this)
  object culprits extends MappedString(this, 200)
}

object BuildStatus extends BuildStatus
                   with LongKeyedMetaMapper[BuildStatus]
                   with CRUDify[Long, BuildStatus] {

  override def editMenuLocParams = If(User.loggedIn_? _, () => RedirectResponse("/")) ::User.testSuperUser ::  super.editMenuLocParams
  override def viewMenuLocParams = If(User.loggedIn_? _, () => RedirectResponse("/")) :: User.testSuperUser :: super.viewMenuLocParams
  override def createMenuLocParams = If(User.loggedIn_? _, () => RedirectResponse("/")) :: User.testSuperUser :: LocGroup("crud", "crud") :: super.createMenuLocParams
  override def showAllMenuLocParams = If(User.loggedIn_? _, () => RedirectResponse("/")) :: User.testSuperUser :: LocGroup("crud", "crud") :: super.showAllMenuLocParams
}



