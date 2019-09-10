package com.eon.opcuapubsubclient.cache

import com.eon.opcuapubsubclient.domain.PayloadTypes.DataSetMetaData

/**
 * The idea behind this actor is that we can dump messages (stored as a HashMap)
 * into it and retrieve it based on the key
 */
trait DataSetMetaDataCache {
  type PublisherID = String
  def evict(key: PublisherID): Unit
  def add(key: PublisherID, value: DataSetMetaData): Unit
  def get(key: PublisherID): DataSetMetaData
}

/**
 * Simple object cache
 * TODO: Implement this cache
 */
class SimpleDataSetMetaDataCache extends DataSetMetaDataCache {
  override def evict(key: PublisherID): Unit = ???

  override def add(key: PublisherID, value: DataSetMetaData): Unit = ???

  override def get(key: PublisherID): DataSetMetaData = ???
}

