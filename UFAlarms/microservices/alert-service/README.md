Wiki link :-
______________

https://xeranet.atlassian.net/wiki/spaces/NSN/pages/123695068/Alert+Service


Interface Service CRUD Flow Simulator Steps on local box :-
____________________________________________________________

1. Install and start cassandra version
2. Run simulator.LoadOrgTable to load farallones.orghierarchy_by_nodeid for lookup data
3. Install and start kafka zookeeper and broker node with default configuration (port 9092)

4. From microservices directory execute :-
   sbt alert-service/"test:runMain simulator.InterfaceCrudSimulator"

5. start kafka consumer for isreq, isresp to verify messages
6. verify alerts table in db