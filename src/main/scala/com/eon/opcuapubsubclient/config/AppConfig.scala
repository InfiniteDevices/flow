package com.eon.opcuapubsubclient.config

import com.typesafe.config.Config

/**
  * Type-safe configuration used throughout the application.
  */
final case class AppConfig(
  environment: String,
  appName: String,
  appHost: String,
  appPort: Int,
  opcCfg: OpcUAPubSubConfig
)
final case class OpcUAPubSubConfig(
  serverEndPointUrl: String,
  publisherIds: Seq[String]
)
final case class MqttConfig(
  url: String,
  topic: String,
  clientId: String = "default-client-id",
  persistenceDir: Option[String] = None
)
final case class KafkaConfig(
  servers: String,
  topics: String,
  groupId: String)
object AppConfig {

  def load(): AppConfig =
    load(ConfigUtil.loadFromEnv())

  def load(config: Config): AppConfig = {
    AppConfig(
      environment = config.getString("environment"),
      appName = config.getString("appName"),
      appHost = Option(config.getString("appHost")).getOrElse("localhost"),
      appPort = Option(config.getInt("appPort")).getOrElse(8080),
      opcCfg = OpcUAPubSubConfig(
        serverEndPointUrl = Option(config.getString("opc.server.url")).getOrElse("localhost:8080"),
        publisherIds = Option(config.getString("opc.publisherId")).toSeq
      )
    )
  }
}