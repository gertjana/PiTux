package net.addictivesoftware.model

import net.liftweb.mapper._


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


}