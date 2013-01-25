package net.addictivesoftware.actors

import net.liftweb.actor.LiftActor
import net.addictivesoftware.model.Leaders
import net.liftweb.common.Loggable

object BuildStatusTuxScheduler extends LiftActor with Loggable {
  import net.addictivesoftware.model.{Job, BuildStatus}
  import collection.mutable
  import net.liftweb.mapper.{Descending, OrderBy}
  import net.liftweb.util.{Props, Schedule}
  import net.liftweb.util.Helpers._
  import net.addictivesoftware.rpitux.Tux


  case class ScheduleTux()
  case class Stop()

  private var stopped = false

  protected def messageHandler = {
    case ScheduleTux =>
      try {
        val bus = Props.get("i2c.bus").open_!
        val address = Props.get("i2c.address").open_!

        val tux = new Tux(bus.toInt, address.toByte)

        logger.info("Schedule Tux...")
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

        // show stats
        if (rotationJobs.size == 0) {
          tux.setStatus("OFF", "No jobs found", "please add some")
          Thread.sleep(5 seconds)
        } else {
          tux.setStatus("OFF", "Looping through", String.format("%s builds", ""+rotationJobs.size))
          Thread.sleep(2 seconds)
          tux.setStatus("OFF", "SUCC UNST FAIL",
            String.format("%s%% %s%% %s%%",
              fillOutText((aggregated.get("SUCCESS").get*100/rotationJobs.size), 3, true),
              fillOutText((aggregated.get("UNSTABLE").get*100/rotationJobs.size), 3, true),
              fillOutText((aggregated.get("FAILURE").get*100/rotationJobs.size), 3, true)
            ))
          Thread.sleep(5 seconds)
          val leaders:List[(String, Long)] = Leaders.findAll().map(leader => new Tuple2(leader.culprits.is, leader.successCount.is-leader.failCount.is))
            .sortBy(_._2).reverse.toStream.take(5).toList
          if (leaders.length >= 1) {
              tux.setStatus("OFF",
                            "Leaderboard:",
                            String.format("1:%s (%s)", leaders.head._1, ""+leaders.head._2))
              Thread.sleep(5 seconds)
          }
          if (leaders.length == 2) {
            tux.setStatus("OFF",
                          String.format("2:%s (%s)", leaders.apply(1)._1, ""+leaders.apply(1)._2),
                          "")
            Thread.sleep(5 seconds)
          } else if (leaders.length >= 3) {
            tux.setStatus("OFF",
                          String.format("2:%s (%s)", leaders.apply(1)._1, ""+leaders.apply(1)._2),
                          String.format("3:%s (%s)", leaders.apply(2)._1, ""+leaders.apply(2)._2))
            Thread.sleep(5 seconds)
          }
          if (leaders.length == 4) {
            tux.setStatus("OFF",
                          String.format("4:%s (%s)", leaders.apply(3)._1, ""+leaders.apply(3)._2),
                          "")
            Thread.sleep(5 seconds)
          } else if (leaders.length >= 5) {
            tux.setStatus("OFF",
                          String.format("4:%s (%s)", leaders.apply(3)._1, ""+leaders.apply(3)._2),
                          String.format("5:%s (%s)", leaders.apply(4)._1, ""+leaders.apply(4)._2))
            Thread.sleep(5 seconds)
          }
        }

        //show jobs
        rotationJobs.foreach(status => {
          logger.info("setting status for " + status._1)
          tux.setStatus(status._2._1, status._1, status._2._2)
          Thread.sleep(5 seconds)
        })
      } catch {
        case e: Exception => e.printStackTrace(System.out)
      } finally {
        if (!stopped) {
          Schedule.schedule(this, ScheduleTux, 1 seconds)
        }
      }

    case Stop =>
      this.stopped = true
    case (_) =>
      logger.info("unknown message recieved")
  }




  private def fillOutText(any:Any, len:Int, alignLeft:Boolean):String = {
    val text = any.toString
    if (text.length >= len) {
      text.substring(0,len)
    } else {
      if (!alignLeft) {
        text + " " * (len-text.length())
      } else {
        " " * (len-text.length()) + text
      }
    }
  }

}
