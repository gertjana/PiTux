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
    case ScheduleTux =>
      try {
        val bus = Props.get("i2c.bus").open_!
        val address = Props.get("i2c.address").open_!

        val tux = new Tux();
        tux.i2cBus = bus.toInt
        tux.i2cAddress = address.toByte

        println("Schedule Tux...")
        val rotationJobs: mutable.HashMap[String, (String, String)] = new mutable.HashMap[String, (String, String)]()
        val bs = BuildStatus.findAll(OrderBy(BuildStatus.timestamp, Descending))
        val jobs: List[String] = Job.findAll().map(_.jobid.is)
        val aggregated: mutable.HashMap[String, Int] = new mutable.HashMap[String, Int]()
        aggregated.+=(("SUCCESS", 0))
        aggregated.+=(("UNSTABLE", 0))
        aggregated.+=(("FAILURE", 0))

        for (status <- bs) {
          if (!rotationJobs.contains(status.job.is) && jobs.contains(status.job.is)) {
            rotationJobs.put(status.job.is, (status.result.is, status.culprits.is))
            aggregated.update(status.result.is, aggregated.get(status.result.is).get+1)
          }
        }

        if (rotationJobs.size == 0) {
          tux.setStatus("OFF", "No jobs found", "please add some")
        } else {
          tux.setStatus("OFF", "Looping through", String.format("%s builds", ""+rotationJobs.size))
          tux.setStatus("OFF", "SUCC/UNST/FAIL",
            String.format("%s%%/%s%%/%s%%",
              ""+(aggregated.get("SUCCESS").get*100/rotationJobs.size),
              ""+(aggregated.get("UNSTABLE").get*100/rotationJobs.size),
              ""+(aggregated.get("FAILURE").get*100/rotationJobs.size)
            ))
          Thread.sleep(5000)
        }

        rotationJobs.foreach(status => {
          println("setting status for " + status._1)
          tux.setStatus(status._2._1, status._1, status._2._2)
          Thread.sleep(5000)
        })
      } catch {
        case e: Exception => e.printStackTrace(System.out)
      } finally {
        if (!stopped) {
          Schedule.schedule(this, ScheduleTux, 10 seconds)
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
