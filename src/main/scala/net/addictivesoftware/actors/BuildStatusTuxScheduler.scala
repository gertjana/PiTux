package net.addictivesoftware.actors

import net.liftweb.actor.LiftActor
import net.addictivesoftware.model.{Job, BuildStatus}
import collection.mutable
import net.liftweb.mapper.{Descending, OrderBy}
import net.liftweb.util.Schedule
import net.liftweb.util.Helpers._

object BuildStatusTuxScheduler extends LiftActor {
  case class ScheduleTux()
  case class Stop()

  private var stopped = false

  protected def messageHandler = {
    case ScheduleTux => {
      println("Schedule Tux")
      val rotationJobs:mutable.HashMap[String,(String,String)] = new mutable.HashMap[String,(String, String)]()
      val bs = BuildStatus.findAll(OrderBy(BuildStatus.timestamp, Descending))
      val jobs:List[String] = Job.findAll().map(_.name.is);
      for (status <- bs) {
        if (!rotationJobs.contains(status.job.is) && jobs.contains(status.job.is)) {
          rotationJobs.put(status.job.is, (status.result.is, status.culprits.is))
        }
      }
      rotationJobs.foreach(status => {
        displayStatus(status._1, status._2._1, status._2._2)
        Thread.sleep(10000)
      })
      if (!stopped) {
        Schedule.schedule(this, ScheduleTux, 5 seconds);
      }
    }
    case Stop =>
      this.stopped = true
    case (_) =>
      println("unknown message recieved")
  }

  private def displayStatus(job:String, result:String, culprits:String) {
    println(result)
    println("/========================================/")
    println("/ " + job)
    println("/ " + culprits)
    println("/========================================/")
  }

}
