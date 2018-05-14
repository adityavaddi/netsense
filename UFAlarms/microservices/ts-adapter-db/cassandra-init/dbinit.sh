#!/usr/bin/env bash
set -e

export CQLSH_HOST=${CASSANDRA_HOST-localhost}
export CQLSH_PORT=${CASSANDRA_PORT-9042}
export CASSANDRA_HOME=${CASSANDRA_HOME-/opt/cassandra}
export PATH=$PATH:$CASSANDRA_HOME/bin

echo "Creating schemas and tables..."
cqlsh ${CQLSH_HOST} ${CQLSH_PORT} -f init_db.cql
echo "Completed initializing the database..."