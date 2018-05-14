package com.verizon.netsense.model

import com.fasterxml.jackson.annotation.JsonProperty

// Temporarily adding lookup for backward compatibility - to keep field names as in 3.0.5 and earlier (datadealer).
object TrafficMetadataMapper {
  val metadataMap : Map[String,String] =   Map[String, String](
    "o" -> "detected_objects",
    "c" -> "count",
    "o.c" -> "class",
    "uuid" -> "trafficdetectioneventid", // "detectedobjectid",
    "o.uuid" -> "detectedobjectid",
    // o
    "wv" -> "world_velocity",
    "wvp" -> "world_velocity",
    "iv" -> "image_velocity",
    "ivp" -> "image_velocity",
    "wp" -> "world_bounding_box",
    "p.wp" -> "world_bounding_box",
    "world" -> "world_bounding_box",
    "img" -> "image_bounding_box",
    "imgp" -> "image_bounding_box",
    "wh" -> "height",
    "o.wp" -> "position_precision",
    // header
    "n" -> "name",
    "ch" -> "channel",
    "uuid" -> "uuid",
    "e" -> "active",
    "t" -> "time",
    "u" -> "user_initials",
    "des" -> "description",
    "tag" -> "tag",
    // other
    "u" -> "x",
    "v" -> "y",
    "lat" -> "latitude",
    "lon" -> "longitude",
    "dt" -> "dwell_time"
  )

}
