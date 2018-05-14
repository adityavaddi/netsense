FROM ts-test.gr.svcs.verizon.com/dakota-cassandra:3.9-1.0

CMD mkdir -p /opt/cassandra-init
ADD cassandra-init/ /opt/cassandra-init/
WORKDIR /opt/cassandra-init

ENTRYPOINT ["./dbinit.sh"]