package bootstrap.liftweb

import _root_.net.liftweb.util._
import _root_.net.liftweb.common._
import _root_.net.liftweb.http._
import _root_.net.liftweb.http.provider._
import _root_.net.liftweb.sitemap._
import net.liftweb.mapper._
import _root_.java.sql.{Connection, DriverManager}
import _root_.net.addictivesoftware.model._
import _root_.net.addictivesoftware.actors._
import net.liftweb.sitemap.Loc.LocGroup
import net.liftweb.sitemap.Loc.LocGroup
import net.liftweb.common.Full


/**
 * A class that's instantiated early and run.  It allows the application
 * to modify lift's environment
 */
class Boot {
  def boot {
    if (!DB.jndiJdbcConnAvailable_?) {
      val vendor = 
	new StandardDBVendor(Props.get("db.driver") openOr "org.h2.Driver",
			     Props.get("db.url") openOr 
			     "jdbc:h2:lift_proto.db;AUTO_SERVER=TRUE",
			     Props.get("db.user"), Props.get("db.password"))

      LiftRules.unloadHooks.append(vendor.closeAllConnections_! _)

      DB.defineConnectionManager(DefaultConnectionIdentifier, vendor)
    }

    // where to search snippet
    LiftRules.addToPackages("net.addictivesoftware")
    Schemifier.schemify(true, Schemifier.infoF _, BuildStatus, Job, Leaders, User)

    val home = Menu(Loc("Home", "index" :: Nil, "Home", LocGroup("static", "static")))
    val lb = Menu(Loc("Leaderboard", "leaderboard" :: Nil, "Leaderboard", LocGroup("static", "static")))
    val jobs = Menu(Loc("Jobs", "jobs" :: Nil, "Jobs", LocGroup("static", "static")))
    val bs = Menu(Loc("BuildStatuses", "buildstatuses" :: Nil, "BuildStatuses", LocGroup("static", "static")))

    // Build SiteMap
    def sitemap() = SiteMap(
      home :: jobs :: bs :: lb ::
      BuildStatus.menus :::
      Job.menus :::
      Leaders.menus :::
      User.menus
        :_*)
    LiftRules.setSiteMapFunc(sitemap)

    /*
     * Show the spinny image when an Ajax call starts
     */
    LiftRules.ajaxStart =
      Full(() => LiftRules.jsArtifacts.show("ajax-loader").cmd)

    /*
     * Make the spinny image go away when it ends
     */
    LiftRules.ajaxEnd =
      Full(() => LiftRules.jsArtifacts.hide("ajax-loader").cmd)

    LiftRules.early.append(makeUtf8)

    //LiftRules.loggedInTest = Full(() => User.loggedIn_?)

    S.addAround(DB.buildLoanWrapper)

    //create admin user if needed
    makeAdminUser

    // Actor to drive Penquin
    BuildStatusTuxScheduler ! BuildStatusTuxScheduler.ScheduleTux
    // Setup Actor to retrieve Build Statuses
    BuildStatusScheduler ! BuildStatusScheduler.ScheduleJobs

    LiftRules.unloadHooks.append( () => BuildStatusScheduler ! BuildStatusScheduler.Stop )
    LiftRules.unloadHooks.append( () => BuildStatusTuxScheduler ! BuildStatusTuxScheduler.Stop )

  }


  /**
   * Force the request to be UTF-8
   */
  private def makeUtf8(req: HTTPRequest) {
    req.setCharacterEncoding("UTF-8")
  }

  private def makeAdminUser = {
    val adminUser = Props.get("admin.user").openTheBox.trim()
    val adminPassword = Props.get("admin.password").openTheBox.trim()

    User.find(By(User.email, adminUser)) match {
      case Full(user) => {
        println("Admin user already exists. skipping generation")
      }
      case (_) => {
        new User()
          .firstName(adminUser)
          .lastName(adminUser)
          .password(adminPassword)
          .email(adminUser)
          .superUser(true)
          .validated(true)
          .save()
        println("Admin user created")
      }
    }
  }

}
