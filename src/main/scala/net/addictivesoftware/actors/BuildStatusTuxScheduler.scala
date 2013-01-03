package net.addictivesoftware.actors

import net.liftweb.actor.LiftActor
import net.addictivesoftware.model.{Job, BuildStatus}
import collection.mutable
import net.liftweb.mapper.{Descending, OrderBy}
import net.liftweb.util.{Props, Schedule}
import net.liftweb.util.Helpers._
import net.addictivesoftware.rpitux.Tux

object BuildStatusTuxScheduler extends LiftActor {
  case class ScheduleTux()
  case class Stop()

  private var stopped = false

  protected def messageHandler = {
    case ScheduleTux => {
      Tux.i2cBus = Int.unbox(Props.get("i2c.bus"))
      Tux.i2cAddress = Byte.unbox(Props.get("i2c.address"))

      println("I2C Bus : " + Tux.i2cBus)
      println("I2C Address : " + Tux.i2cAddress)
      println("Schedule Tux")
      val rotationJobs:mutable.HashMap[String,(String,String)] = new mutable.HashMap[String,(String, String)]()
      val bs = BuildStatus.findAll(OrderBy(BuildStatus.timestamp, Descending))
      val jobs:List[String] = Job.findAll().map(_.name.is)
      for (status <- bs) {
        if (!rotationJobs.contains(status.job.is) && jobs.contains(status.job.is)) {
          rotationJobs.put(status.job.is, (status.result.is, status.culprits.is))
        }
      }
      if (rotationJobs.size == 0) {
        Tux.setStatus("OFF", "No jobs found", "please add some")
      }

      rotationJobs.foreach(status => {
        Tux.setStatus(status._2._1, status._1, status._2._2)
        Thread.sleep(10000)
      })
      if (!stopped) {
        Schedule.schedule(this, ScheduleTux, 5 seconds)
      }
    }
    case Stop =>
      this.stopped = true
    case (_) =>
      println("unknown message recieved")
  }

  private def displayStatus(job:String, result:String, culprits:String) {



  }

}
