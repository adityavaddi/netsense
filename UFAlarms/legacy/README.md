# Legacy

## Design

![Design](doc/design.png)

## Usage

### Release

`_build/default/rel/legacy/bin/legacy start` starts Legacy is the background

`_build/default/rel/legacy/bin/legacy foreground` starts Legacy is the foreground

`_build/default/rel/legacy/bin/legacy console` starts Legacy is the foreground with tty

### Docker

`docker-compose up` This will create a cluster of 2 Legacy instances (+ rabbitmq, haproxy & graphite)

### Commands

* `bin/legacy eval "lager:set_loglevel(lager_console_backend, debug)."` Set console log level

### Health Checks

You can query these urls to get info:

* `http://IP:8080/status` General info about legacy
* `http://IP:8080/status/nodes` Connected nodes to Legacy cluster
* `http://IP:8080/status/nodes/{NODE_ID}` Check if node is connected to Legacy

These helthchecks will also be performed by Docker to producte healthy/unhealthy status.

## Building

** ERLANG OTP 19.3 REQUIRED**

Legacy uses `rebar3` for compiling, testing and releasing.

Only the binary is required and can be found @ https://s3.amazonaws.com/rebar3/rebar3

`rebar3 compile && rebar3 release`

## Config

All the config files are located in `legacy/config`.

* `sys.config` Legacy's main configuration file
* `vm.args` Arguments passed to the Erlang VM (also needed by Legacy)
* `ct.config` is only used for testing
* `haprxy.cfg` HAProxy configuration for `docker-compose`

### vm.args

```
## Name of the node
-sname legacy@127.0.0.1

## Cookie for distributed erlang
-setcookie random

```
`-sname` :

> A node is an executing Erlang runtime system that has been given a name, using the command-line flag -name (long names) or -sname (short names).

> The format of the node name is an atom name@host. name is the name given by the user. host is the full host name if long names are used, or the first part of the host name if short names are used. node() returns the name of the node.

`-setcookie` :

> Sets the magic cookie of the node to Cookie;

> Authentication determines which nodes are allowed to communicate with each other. In a network of different Erlang nodes, it is built into the system at the lowest possible level. Each node has its own magic cookie, which is an Erlang atom.
When a node tries to connect to another node, the magic cookies are compared. If they do not match, the connected node rejects the connection.

More info @ http://erlang.org/doc/man/erl.html http://erlang.org/doc/reference_manual/distributed.html

### sys.config

The `sys.config` file must respect [Erlang syntax](http://learnyousomeerlang.com/starting-out-for-real) otherwise Legacy will not start.

Comments can be added with `%`.

```
[
    {ssl_options, [{reuse_sessions, false}]},
    {legacy, [
        {keepalive, 180000}, % Interval (in ms) at which Legacy will send a bogus message to keep WS connection alive
        {rabbitmq, [  % RabbitMQ Configuration
            {host, "rabbitmq"}, %% rabbitmq_service
            {username, "farallones"},
            {password, "sensity1"},
            {timeout, 5000},  % Connection timeout
            {exchanges, [ % List of exchange declared by Legacy
                [{name, "node.commands"}, {type, "topic"}],
                [{name, "node.events"}, {type, "topic"}]
            ]},
            {publisher, [
                {size, 10} % Number of RabbitMQ publishers
            ]},
            {subscriber, [
                {'exchange', "node.commands"},
                {'key', "#"},
                {'queue', "legacy"},
                {'msg_handler', 'legacy_cmd_handler'}
            ]}
        ]},
        {s3, [ % AWS S3 Options
            {key, "AKIAJHXVVJ2KABYSEUTA"},
            {secret, "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"},
            {host, "s3-us-west-2.amazonaws.com"}
        ]},
        {ota, [ % OTA releated options
            {host, "127.0.0.1"}, %% legacy_ota Host should match Legacy's host and be reachable with port 8080 and 10443
            {bucket, "sensity-firmware"}, % S3 Bucket where firmwares are stored
            {workers, 10} % Default OTA speed (10 nodes at a time)
        ]},
        {metrics, [
            {interval, 5000},  % Reporting interval
            {graphite, [ % graphite connection info
                {prefix, "legacy"}, %% graphite_prefix
                {host, "127.0.0.1"}, %% graphite_service
                {port, 2003}, %% graphite_port
                {api_key, ""}
            ]}
        ]}
    ]},
    {gproc,[ % Used to cluster multiple Legacys
        {gproc_dist, ['legacy@127.0.0.1']} % Should be a list of node ex ['legacy1@10.0.0.1', 'legacy2@10.0.0.2']
    ]},
    {lager, [ % Logging library see https://github.com/erlang-lager/lager
        {log_root, "/var/log/legacy"},
        {crash_log, "crash.log"},
        {handlers, [
            {lager_console_backend, [
                {level, info}, %% stdout_level
                {formatter_config,
                    [
                        "[", date, " ", time, "] ", pid, " [", severity,"]",  {nodeid, [" [", nodeid, "]"], ""}, " [",
                        {module, ""}, {function, [":", function], ""}, {line, [":", line], ""}, "] ", message, "\n"
                    ]
                }
            ]},
            {lager_stderr_backend, [
                {level, error}, %% stderr_level
                {formatter_config,
                    [
                        "[", date, " ", time, "] ", pid, " [", severity,"]",  {nodeid, [" [", nodeid, "]"], ""}, " [",
                        {module, ""}, {function, [":", function], ""}, {line, [":", line], ""}, "] ", message, "\n"
                    ]
                }
            ]}
        ]}
    ]}
].


```

## Testing

* `rebar3 test`

## RabbitMQ

Please refer to https://www.rabbitmq.com/tutorials/amqp-concepts.html

### Exchanges

| Name | Type | Description |
| --- | --- | --- |
| `node.events` | Topic | Events received from the nodes by Legacy
| `node.commands` | Topic | Commands received by services to be sent to the node

### Queue

Legacy will create one queue for itself named `legacy`. The queue will bound the the exchange `node.commands` with `#`.

Internally and clustered, legacy will use a work queue (see https://www.rabbitmq.com/tutorials/tutorial-two-javascript.html), so that they will only be one legacy queue with legacy instances being a worker for it.

### Topics

*This is subject to change to use RestPackMQ*

Topics will always contain `nodeid`.

| Topic | Description | Example
| --- | --- | --- |
| `NODEID.login.req` | Login req message | `N007.login.req`
| `NODEID.login.disconnected` | Node disconnected message | `N007.login.disconnected`
| `NODEID.sensor.SENSOR` | Sensor sample message | `N007.sensor.lt`
| `NODEID.EVENT` | Any other message (alarm ...) | `N007.alarm`

#### Refs

- https://www.rabbitmq.com/tutorials/tutorial-five-javascript.html
- https://www.rabbitmq.com/tutorials/tutorial-four-javascript.html
