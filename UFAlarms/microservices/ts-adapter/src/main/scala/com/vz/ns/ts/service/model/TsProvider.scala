package com.vz.ns.ts.service.model

case class TsProvider(id: Option[String],
                      devicekind: Option[String],
                      kind: Option[String],
                      version: Option[String],
                      name: Option[String],
                      description: Option[String],
                      inventory: Option[TsProviderRoute],
                      sink: Option[TsProviderRoute],
                      source: Option[TsProviderRoute])

case class TsProviderRoute(alias: Option[String],
                           basePath: Option[String],
                           relativePath: Option[String],
                           hostAndPort: Option[String])
