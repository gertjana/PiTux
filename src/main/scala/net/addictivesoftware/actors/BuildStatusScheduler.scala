package net.addictivesoftware.actors

import net.liftweb.actor.LiftActor
import net.addictivesoftware.model.Job
import collection.mutable.HashMap
import net.liftweb.common.Loggable

object BuildStatusScheduler extends LiftActor with Loggable {
  case class ScheduleJobs()
  case class Stop()
  case class ResetJobs()
  case class AddJob(jobName:String, jobId:String, interval:Int)

  def messageHandler = {
    case ScheduleJobs => {
      logger.info("scheduling jobs")
      Job.findAll().map(job => {
          BuildStatusRetrievalActor ! BuildStatusRetrievalActor.RetrieveStatus(job.name.is, job.jobid.is, job.interval.is)
      })
    }

    case AddJob(jobName:String, jobId:String, interval:Int) => {
      logger.info("adding job :" + jobName)
      if (jobName.length > 0 && interval > 0)
        BuildStatusRetrievalActor ! BuildStatusRetrievalActor.RetrieveStatus(jobName, jobId, interval)
    }

    case ResetJobs => {
      logger.info("resetting jobs ...")
      this ! this.Stop
      this ! this.ScheduleJobs
    }

    //not working currently
    case Stop => {
      logger.info("stop scheduling jobs")

      BuildStatusRetrievalActor ! BuildStatusRetrievalActor.Stop
    }
  }

}
