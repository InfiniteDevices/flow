package com.eon.opcuapubsubclient

import scala.concurrent.{Await, ExecutionContext, Future}
import scala.concurrent.duration.Duration
import scala.util.{Failure, Success}
import akka.actor.{ActorRef, ActorSystem}
import akka.http.scaladsl.Http
import akka.http.scaladsl.server.Route
import akka.stream.ActorMaterializer
import com.eon.opcuapubsubclient.config.AppConfig

//#main-class
object Bootstrap extends App  {

  // 1. configuration
  val appCfg = AppConfig.load()

  // 2. server-bootstrapping
  implicit val system: ActorSystem = ActorSystem(appCfg.appName)
  implicit val materializer: ActorMaterializer = ActorMaterializer()
  implicit val executionContext: ExecutionContext = system.dispatcher

  // 3. routes configurations
  //val userRegistryActor: ActorRef = system.actorOf(UserRegistryActor.props, "userRegistryActor")
  //lazy val routes: Route = userRoutes

  // 4. http-server
  val serverBinding: Future[Http.ServerBinding] =
    Http().bindAndHandle(
      null,
      appCfg.appHost,
      appCfg.appPort)

  serverBinding.onComplete {
    case Success(bound) =>
      println(s"Server online at http://${bound.localAddress.getHostString}:${bound.localAddress.getPort}/")
    case Failure(e) =>
      Console.err.println(s"Server could not start!")
      e.printStackTrace()
      // terminate actor system
      system.terminate()
      System.exit(-1)
  }

  Await.result(system.whenTerminated, Duration.Inf)
}
