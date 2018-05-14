package com.verizon.netsense.services.datamigration.migration

import com.verizon.netsense.services.datamigration.utils.Logging
import com.verizon.netsense.services.datamigration.utils.MigrationStatus.{isMigrated, updateMigration}


trait Migration extends Logging {

  def migrate()

  def applyMigration(name: String, version: Int): Unit = {
    if (!isMigrated(name, version)) {
      try {
        migrate()
        updateMigration(name, version, true)
      } catch {
        case ex: Exception =>
          updateMigration(name, version, false)
          log.error(s"Exception while running $name job: ${ex.getMessage}")
      }
    } else {
      log.error(s"Skipping ${name}:${version} migration")
    }
  }
}
