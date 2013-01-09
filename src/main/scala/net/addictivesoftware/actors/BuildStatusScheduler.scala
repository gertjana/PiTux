package net.addictivesoftware.actors

import net.liftweb.actor.LiftActor
import net.addictivesoftware.model.Job
import collection.mutable.HashMap

object BuildStatusScheduler extends LiftActor {
  case class ScheduleJobs()
  case class Stop()
  case class ResetJobs()
  case class AddJob(jobName:String, jobId:String, interval:Int)

  def messageHandler = {
    case ScheduleJobs => {
      println("scheduling jobs")
      Job.findAll().map(job => {
          BuildStatusRetrievalActor ! BuildStatusRetrievalActor.RetrieveStatus(job.name.is, job.jobid.is, job.interval.is)
      })
    }

    case AddJob(jobName:String, jobId:String, interval:Int) => {
      println("adding job :" + jobName)
      if (jobName.length > 0 && interval > 0)
        BuildStatusRetrievalActor ! BuildStatusRetrievalActor.RetrieveStatus(jobName, jobId, interval)
    }

    case ResetJobs => {
      println("resetting jobs ...")
      this ! this.Stop
      this ! this.ScheduleJobs
    }

    case Stop => {
      println("stop scheduling jobs");
      BuildStatusRetrievalActor ! BuildStatusRetrievalActor.Stop
    }
  }

}
