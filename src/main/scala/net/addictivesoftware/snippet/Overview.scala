package net.addictivesoftware.snippet

import xml.{Text, NodeSeq}
import net.liftweb.util.Helpers._
import net.addictivesoftware.model.{Job, BuildStatus}
import net.liftweb.mapper.{Ascending, OrderBy, Descending}


class Overview {

  def statuses(in: NodeSeq): NodeSeq =  {
    val statuses = BuildStatus.findAll(OrderBy(BuildStatus.id, Descending))
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
    val jobs = Job.findAll(OrderBy(Job.name, Ascending));
    jobs.flatMap {
      case (job) => bind("job", in,
                "name" -> job.name,
                "interval" -> job.interval)
    }
  }

}