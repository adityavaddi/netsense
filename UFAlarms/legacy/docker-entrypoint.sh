#!/bin/bash -eu

replacesysconfig() {
    setting="${1}"
    value="${2}"
    file="/opt/legacy/releases/1.0.0/sys.config"

    if [ -n "${value}" ]; then
        if grep -q -F "${setting}" "${file}"; then
            sed --in-place "s|${setting}|${value}|" "${file}"
        else
            echo "FAILED to find ${setting} in ${file}"
        fi
    fi
}

replacevmargs() {
    setting="${1}"
    value="${2}"
    file="/opt/legacy/releases/1.0.0/vm.args"

    if [ -n "${value}" ]; then
        if grep -q -F "${setting}" "${file}"; then
            sed --in-place "s|${setting}|${value}|" "${file}"
        else
            echo "FAILED to find ${setting} in ${file}"
        fi
    fi
}

# LOGS
replacesysconfig "{level, info}, %% stdout_level" "{level, ${stdout_level:-info}}, %% stdout_level"
replacesysconfig "{level, error}, %% stderr_level" "{level, ${stderr_level:-error}}, %% stderr_level"

# Linked Services
# RabbitMQ
replacesysconfig "{host, \"rabbitmq\"}, %% rabbitmq_service" "{host, \"${rabbitmq_service:-rabbitmq}\"}, %% rabbitmq_service"
# Graphite
replacesysconfig "{host, \"127.0.0.1\"}, %% graphite_service" "{host, \"${graphite_service:-127.0.0.1}\"}, %% graphite_service"
replacesysconfig "{port, 2003}, %% graphite_port" "{port, ${graphite_port:-2003}}, %% graphite_port"
replacesysconfig "{prefix, \"legacy\"}, %% graphite_prefix" "{prefix, \"${graphite_prefix:-legacy}\"}, %% graphite_prefix"

# Clustering Options
if [ "${sname:-false}" = "true" ]; then
  sed --in-place "s|-name|-sname|" /opt/legacy/releases/1.0.0/vm.args
fi

if [[ "${clustered:-false}" == "true" ]]; then
    replacesysconfig "'legacy@127.0.0.1'" "'legacy@"`echo ${legacy_hosts:-legacy} | sed "s/,/','legacy@/g"`"'"
else
    replacesysconfig "'legacy@127.0.0.1'" "'legacy@"$(echo ${legacy_service:-127.0.0.1})"'"
fi

# Extra
replacesysconfig "{host, \"127.0.0.1\"}, %% legacy_ota" "{host, \"${legacy_ota:-127.0.0.1}\"}, %% legacy_ota"
replacevmargs "legacy@127.0.0.1" "legacy@${legacy_service:-127.0.0.1}"

exec "$@"
