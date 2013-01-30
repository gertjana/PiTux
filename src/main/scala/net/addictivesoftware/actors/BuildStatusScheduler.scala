package net.addictivesoftware.actors

import net.liftweb.actor.LiftActor
import net.addictivesoftware.model.Job
import collection.mutable.LinkedList
import net.liftweb.common.Loggable

object BuildStatusScheduler extends LiftActor with Loggable {
  case class ScheduleJobs()
  case class Stop()
  case class ResetJobs()
  case class AddJob(jobName:String, jobId:String, interval:Int)

  var jobActors:List[LiftActor] = Nil

  def messageHandler = {
    case ScheduleJobs => {
      logger.info("scheduling jobs")
      jobActors  = Job.findAll().map(job => {
        val buildStatusRetrievalActor = BuildStatusRetrievalActor
        buildStatusRetrievalActor ! BuildStatusRetrievalActor.RetrieveStatus(job.name.is, job.jobid.is, job.interval.is)
        buildStatusRetrievalActor
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
      logger.debug("stop scheduling BuildStatusRetrieval jobs")
      for (jobActor <- jobActors) {
        jobActor ! BuildStatusRetrievalActor.Stop
      }
    }
  }

}
