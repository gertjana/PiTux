package net.addictivesoftware.snippet

import xml.{Text, NodeSeq}
import net.liftweb.util.Helpers._
import net.addictivesoftware.model.{Leaders, Job, BuildStatus}
import net.liftweb.mapper._
import net.liftweb.mapper.MaxRows
import collection.mutable
import collection.mutable.ListBuffer


class Overview {

  def statuses(in: NodeSeq): NodeSeq =  {
    val statuses = BuildStatus.findAll(MaxRows(25), OrderBy(BuildStatus.timestamp, Descending))
    statuses.flatMap {
      case (buildStatus) => bind("status", in,
                "job" -> buildStatus.job,
                "number" -> buildStatus.number,
                "result" -> buildStatus.result,
                "timestamp" -> buildStatus.timestamp,
                "culprits" -> buildStatus.culprits)
    }
  }

  def jobs(in:NodeSeq): NodeSeq = {
    val jobs = Job.findAll(OrderBy(Job.name, Ascending))
    jobs.flatMap {
      case (job) => bind("job", in,
                "name" -> job.name,
                "id" -> job.jobid,
                "interval" -> job.interval)
    }
  }


  def schedule(in: NodeSeq): NodeSeq = {
    val rotationJobs:mutable.HashMap[String,(String,String)] = new mutable.HashMap[String,(String, String)]()
    val bs = BuildStatus.findAll(OrderBy(BuildStatus.timestamp, Descending))
    val jobs:List[String] = Job.findAll().map(_.jobid.is);
    for (status <- bs) {
      if (!rotationJobs.contains(status.job.is) && jobs.contains(status.job.is)) {
        rotationJobs.put(status.job.is, (status.result.is, status.culprits.is))
      }
    }

    rotationJobs.flatMap {
      case (rotationJob) => bind("schedule", in,
        "job" -> rotationJob._1,
        AttrBindParam("result", rotationJob._2._1, "class"),
        "culprits" -> rotationJob._2._2)
    }.toSeq
  }

  def leaders(in:NodeSeq) : NodeSeq = {
    val leaders:List[Leaders] = Leaders.findAll()
    var sortedLeaders = leaders
      .map(leader => new Tuple3("", leader.culprits.is, leader.successCount.is-leader.failCount.is))
      .sortBy(_._3).reverse
    var cnt = 0
    var previous:Long = 0
    val board = new ListBuffer[Tuple3[String, String, Long]]()
    for (leader <- sortedLeaders) {
      if (leader._3 != previous) {
        previous = leader._3
        cnt = cnt + 1
        board.append(new Tuple3(cnt.toString, leader._2, leader._3))
      } else {
        board.append(new Tuple3("", leader._2, leader._3))
      }

    }
    board.toList.flatMap {
      case (pos, name, count) => bind("leader", in,
        "position" -> pos,
        "culprit" -> name,
        "count" -> count)
    }.toSeq

  }

}