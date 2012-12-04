package net.addictivesoftware.snippet

import xml.{Text, NodeSeq}
import net.liftweb.util.Helpers._
import net.addictivesoftware.model.{Job, BuildStatus}
import net.liftweb.mapper._
import net.liftweb.mapper.MaxRows
import collection.mutable
import net.liftweb.common.Full


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
                "interval" -> job.interval)
    }
  }


  def schedule(in: NodeSeq): NodeSeq = {
    val rotationJobs:mutable.HashMap[String,(String,String)] = new mutable.HashMap[String,(String, String)]()
    val bs = BuildStatus.findAll(OrderBy(BuildStatus.timestamp, Descending))
    val jobs:List[String] = Job.findAll().map(_.name.is);
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
}