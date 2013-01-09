package net.addictivesoftware.model

import net.liftweb.mapper._
import net.addictivesoftware.actors.BuildStatusScheduler


class Job extends LongKeyedMapper[Job] with IdPK  {
  def getSingleton = Job

  object jobid extends MappedString(this, 16)
  object name extends MappedString(this, 100)
  object interval extends MappedInt(this)

}

object Job extends Job with LongKeyedMetaMapper[Job] with CRUDify[Long, Job] {


  override def afterCreate = scheduleJobAfterCreate _ :: super.afterCreate

  private def scheduleJobAfterCreate(job: Job) {
    BuildStatusScheduler ! BuildStatusScheduler.AddJob(job.name.is, job.jobid.is, job.interval.is)
    job
  }

  override def afterDelete = resetJobsAfterDelete _ :: super.afterDelete

  private def resetJobsAfterDelete(job: Job) {
    BuildStatusScheduler ! BuildStatusScheduler.ResetJobs
  }

}
