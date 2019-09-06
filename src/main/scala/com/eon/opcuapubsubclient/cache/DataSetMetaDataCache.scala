package com.eon.opcuapubsubclient.cache

import akka.actor.{Actor, Props}
import com.eon.opcuapubsubclient.cache.DataSetMetaDataCache._
import com.eon.opcuapubsubclient.domain.PayloadTypes.DataSetMetaData

/**
  * The idea behind this actor is that we can dump messages (stored as a HashMap)
  * into it and retrieve it based on the key
  */
class DataSetMetaDataCache extends Actor {

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
object DataSetMetaDataCache {

  type PublisherID = String

  sealed trait CacheMessage
  case class Evict(key: String) extends CacheMessage
  case class Add(key: String, value: DataSetMetaData) extends CacheMessage
  case class Get(key: String) extends CacheMessage

  def props(): Props =
    Props(new DataSetMetaDataCache)
}
