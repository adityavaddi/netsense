-define(LEGACY, 'legacy').

%% where to store files like firmware
-define(STORAGE_DIR, "/tmp/legacy").

%% sensorsamplereq sensor gg which is non-existent
-define(BOGUS_SENSOR_PAYLOAD, <<122, 4, 10, 2, 103, 103>>).

%% OTA Default Config
-define(OTA_DEFAULT_S3_BUCKET, "sensity-firmware").
-define(OTA_DEFAULT_HOST, "127.0.0.1").
-define(OTA_DEFAULT_WORKERS, 10).

%% OTA Status (in order)
-define(OTA_JOB_RECEIVED, "JOB_RECEIVED").
-define(OTA_FIRMWARE_NOT_FOUND, "FIRMWARE_NOT_FOUND").
-define(OTA_FAIL_JOB_REGISTRATION, "FAIL_JOB_REGISTRATION").
-define(OTA_FAIL_REGISTRATION, "FAIL_REGISTRATION").
-define(OTA_ALREADY_UPDATING, "ALREADY_UPDATING").
-define(OTA_NODE_OFFLINE, "NODE_OFFLINE").
-define(OTA_COMMAND_SENT, "COMMAND_SENT").
-define(OTA_NODE_REBOOTING, "NODE_REBOOTING").
-define(OTA_START_DOWNLOAD, "START_DOWNLOAD").
-define(OTA_STOP_DOWNLOAD, "STOP_DOWNLOAD").
-define(OTA_UPDATE_SUCCESSFUL, "UPDATE_SUCCESSFUL").
-define(OTA_UPDATE_FAILED, "UPDATE_FAILED").
-define(OTA_JOB_DONE, "JOB_DONE").
