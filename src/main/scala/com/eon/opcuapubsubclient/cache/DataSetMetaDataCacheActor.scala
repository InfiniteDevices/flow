package com.eon.opcuapubsubclient.cache

import akka.actor._
import akka.pattern.ask
import akka.util.Timeout

import scala.concurrent.duration._
import com.eon.opcuapubsubclient.cache.CacheActor.{Add, Evict, Get, PublisherID}
import com.eon.opcuapubsubclient.domain.PayloadTypes.DataSetMetaData

import scala.concurrent.Future

/**
  * The idea behind this actor is that we can dump messages (stored as a HashMap)
  * into it and retrieve it based on the key
  */
trait DataSetMetaDataCache {
  def evict(key: PublisherID): Unit
  def add(key: PublisherID, value: DataSetMetaData): Unit
  def get(key: PublisherID): Future[DataSetMetaData]
}

/**
  * Simple object cache
  * TODO: Implement this cache
  */
class SimpleDataSetMetaDataCache extends DataSetMetaDataCache {
  override def evict(key: PublisherID): Unit = ???

  override def add(key: PublisherID, value: DataSetMetaData): Unit = ???

  override def get(key: PublisherID): Future[DataSetMetaData] = ???
}

class DataSetMetaDataCacheActor(actorRef: ActorRef) extends DataSetMetaDataCache {

  implicit val timeout: Timeout = 2.seconds // TODO: This could be injected from outside!

  override def evict(key: PublisherID): Unit = actorRef ! Evict(key)

  override def add(key: PublisherID, value: DataSetMetaData): Unit = actorRef ! Add(key, value)

  override def get(key: PublisherID): Future[DataSetMetaData] = actorRef.ask(Get(key)).mapTo[DataSetMetaData]
}
class CacheActor extends Actor {
  override def receive: Receive = {
    case _ => run(Map.empty[PublisherID, DataSetMetaData])
  }

  def run(cache: Map[PublisherID, DataSetMetaData]): Receive = {
    // Mutation messages
    case Evict(key) =>
      context.become(run(cache - key))
    case Add(key, value) =>
      context.become(run(cache + (key -> value)))

    // Information retrieval messages
    case Get(name) =>
      cache.get(name)
  }
}
object CacheActor {

  type PublisherID = String

  sealed trait CacheMessage
  case class Evict(key: String) extends CacheMessage
  case class Add(key: String, value: DataSetMetaData) extends CacheMessage
  case class Get(key: String) extends CacheMessage

  def props(): Props =
    Props(new CacheActor)
}
