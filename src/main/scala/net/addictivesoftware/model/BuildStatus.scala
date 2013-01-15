package net.addictivesoftware.model

import net.liftweb.mapper._


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

}



