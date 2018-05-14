Sensor Service
===============
 This is a scala based micro-service works on Akka streams.
 - Key Features:
    1. Ingesting time series device sensor samples events to Cassandra database.
    2. Catering Sensor History Query APIs such as
        - `/customers/{orgid}/sites/{siteid}/nodes/{nodeid}/sensors/{sensorid}/date/{date}/limit/{limit}`
        - `/customers/{orgid}/sites/{siteid}/sensors/{sensorid}/from/{date1}/to/{date2}/limit/{limit}/period/{period}`
        - `/customers/{orgid}/sites/{siteid}/nodes/{nodeid}/sensors/{sensorid}/from/{date1}/to/{date2}/limit/{limit}`
        - `/customers/{orgid}/sites/{siteid}/nodes/{nodeid}/sensors/{sensorid}/from/{date1}/to/{date2}/limit/{limit}/period/{period}`
    3. Sending device sensor command to request for latest event from sensor on each API call.
    4. Generating Over/Under Power alarms based on the `main_power` and `driver_level`.


Infrastructure dependencies.
----------------------------
 1. It uses Kafka as bus to communicate with Bridge, Datdealer, Interface-Service and Alert-service.
 2. It uses Cassandra for performing get and store operations on sensor historical data from **device_sensor_samples** table.
 3. It performs read operations from Neo4j DB:
    - to find the node model for routing the command to core or video node
    - to get the time_zone of site for posix to timestamp conversion for sensor query replies.
    - to read fixture data to calculate Over/Under Power alarms(which will be sent to alert-service via Kafka).


Dependencies needed for running App or Simulator
------------------------------------------------
1. sbt
2. Kafka 0.10.x or higher
3. Cassandra
4. Neo4j (Sensor service for every sensor historical call makes a neo4j query to find the type of device by `model`)


For running the application,

    cd $WORKSPACE/Farallones/microservices
    sbt sensor-service/run

For running sbt tests,

    cd $WORKSPACE/Farallones/microservices
    sbt sensor-service/test



Steps to run simulators
-----------------------
Currently Sensor service support two simulators.

 1. Sensor sample event ingestion simulator.

  - For running the simulator.
      ```
      cd $WORKSPACE/Farallones/microservices
      sbt sensor-service/"test:run-main com.verizon.netsense.simulator.SensorSampleEventSimulator"
      ```

  - Below are the allowed env variables.
       1. `ss_sim_event_max_limit`       - to generate the maximum UNSOL sensor sample events - default `100`
       2. `ss_sim_event_throttle_level`  - throttle at which the device events should be generated - default `100`

 2. Sensor historical query simulator.
  - For running the simulator.
    ```
    cd $WORKSPACE/Farallones/microservices
    sbt sensor-service/"test:run-main com.verizon.netsense.simulator.SensorSampleQuerySimulator"
    ```
  - Below are the allowed env variables.
      1. `ss_sim_query_task`            - allowed values **`simulate`** (simulate queries) **`setup`**
                                          (setup neo4j data) **`cleanup`** (clean neo4j testdata) - default `simulate`
      2. `ss_sim_query_max_limit`       - total number of events(queries) the simulator will generate - default `100`
      3. `ss_sim_query_throttle_level`  - throttle at which events(queries) simulated - default `100`
      4. `ss_sim_query_userid`          - userid used in query - default `uberuser`
      5. `ss_sim_query_siteid`          - siteid used in query - default `ubersite`
      6. `ss_sim_query_orgid`           - orgid used in query - default `uberorg`

   **Note:** Node ids and sensor types are picked from `nodesensors.csv`

