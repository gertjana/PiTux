package net.addictivesoftware.actors

import net.liftweb.actor.LiftActor
import net.liftweb.util.{Props, Schedule, HttpHelpers}
import net.liftweb.util.Helpers._
import net.liftweb.json._
import net.addictivesoftware.model.{Leaders, BuildStatus}
import dispatch._
import java.util.Date
import net.liftweb.mapper.By
import net.liftweb.common.{Box, Full}

object BuildStatusRetrievalActor extends LiftActor {
  case class RetrieveStatus(jobName:String, jobId:String, interval:Int)
  case class Stop()

  private var stopped = false

  implicit val formats = net.liftweb.json.DefaultFormats

  private val jenkinsUrl:Box[String] = Props.get("jenkins.url")
  private val successCount:Int = Props.get("success.count").open_!.toInt
  private val unstableCount:Int = Props.get("unstable.count").open_!.toInt
  private val failCount:Int = Props.get("fail.count").open_!.toInt



  def messageHandler = {
    case RetrieveStatus(jobName, jobId, interval) =>
      if (!stopped)
        Schedule.schedule(this, RetrieveStatus(jobName, jobId, interval), interval minutes)
      try {
        println("Running job " + jobName)
        jenkinsUrl match {
          case Full(u) => {
            val request = url(u.format(myUrlEncode(jobName)))
            val response:String = Http(request as_str)

            val bs = parse(response)

            val isBuilding = (bs \ "building").extract[Boolean]

            if (!isBuilding) {

              val date = new Date((bs \ "timestamp").extract[Long])

              var culprits = ""
              val culpritsList:List[String] = List()
              val culpritsListJson = (bs \ "culprits" \ "fullName")

              if (culpritsListJson.isInstanceOf[JArray])
                culpritsList :: culpritsListJson.extract[List[String]]
              if (culpritsList.size > 0) {
                culprits = culpritsList.reduceLeft(_ + ", " + _)
              }
              else if (culpritsListJson.isInstanceOf[JString])
                culprits = culpritsListJson.extract[String]
              else
                culprits = "..."

              val result = (bs \ "result").extract[String]

              val buildStatus = new BuildStatus()
                .job(jobId)
                .buildId((bs \ "id").extract[String])
                .number((bs \ "number").extract[Long])
                .result(result)
                .timestamp(date)
                .culprits(culprits)

              //only save a build status once
              BuildStatus.find(
                By(BuildStatus.buildId, buildStatus.buildId),
                By(BuildStatus.job, jobId)) match {
                case Full(bs) => {
                  println("already known build")
                }
                case (_) => {
                  val savedBS = buildStatus.saveMe();
                  println("saving " + savedBS)
                  updateLeaders(culprits, result)
                }
              }
            } else {
              println("skipping not finished build")
            }
          }
          case (_) => {
            println("Url in properties file set?")
          }
        }
    } catch {
      case e:Exception => {println(e)}
    }

    case Stop =>
      stopped = true
    case (_) =>
      println("unknown message recieved")
  }

  private def updateLeaders(culprits:String, result:String) {


    if (result.equals("...")) return
    Leaders.find(By(Leaders.culprits, culprits)) match {
      case (Full(leader)) => {
        //existing Leader, updating
        if (result.equals("SUCCESS")) {
           leader.successCount(leader.successCount.is+successCount)
        }
        if (result.equals("UNSTABLE")) {
          leader.failCount(leader.failCount.is+unstableCount)
        }
        if (result.equals("FAILURE")) {
          leader.failCount(leader.failCount.is+failCount)
        }

        leader.save()
      }
      case (_) => {
        //new Leader, creating
        val leader = new Leaders()
        leader.culprits(culprits)
        if (result.equals("SUCCESS")) {
          leader.successCount(successCount)
          leader.failCount(0)
        }
        if (result.equals("UNSTABLE")) {
          leader.successCount(0)
          leader.failCount(unstableCount)
        }
        if (result.equals("FAILURE")) {
          leader.successCount(0)
          leader.failCount(failCount)
        }
        leader.save()
      }
    }
  }

  private def myUrlEncode(in: String) = {
    urlEncode(in).replace("+", "%20")
  }
}


