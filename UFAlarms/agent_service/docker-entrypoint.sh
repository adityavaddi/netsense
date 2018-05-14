#!/bin/bash -eu

replace() {
    setting="${1}"
    value="${2}"
    file="/deploy/_rel/device_service_release/releases/1/sys.config"

    if [ -n "${value}" ]; then
        if grep -q -F "${setting}" "${file}"; then
            sed --in-place "s|${setting}|${value}|" "${file}"
        else
            echo "FAILED to find ${setting} in ${file}"
            # exit 1
        fi
    fi
}

setting() {
    setting="${1}"
    value="${2}"
    file="/deploy/_rel/device_service_release/releases/1/sys.config"

    if [ -n "${value}" ]; then
        if grep -q -F "${setting}" "${file}"; then
            sed --in-place "s|\(.*\)${setting}\(.*\)|\1${value}\2|" "${file}"
        else
            echo "FAILED to find ${setting} in ${file}"
            # exit 1
        fi
    fi
}

setting "{{dcc.local-ip}}" "${dcc_service:-127.0.0.1}"
setting "{{datadealer.local-ip}}" "${datadealer_service:-127.0.0.1}"
setting "{{interface.local-ip}}" "${datadealer_service:-127.0.0.1}"
setting "{{mqtt-fqdn}}" "${mqtt_service:-devmqtt.sensity.com}"
setting "{{graphite.local-ip}}" "${graphite_service:-localhost}"

replace ",{port, 2883}" ",{port, ${mqtt_port:-8883}}"
if [ "${use_ssl:-false}" = "false" ]; then
    replace "{certfile, \"/var/lib/ssl/sensity.com.crt\"}" "% {certfile, \"/var/lib/ssl/sensity.com.crt\"}"
    replace ",{keyfile,  \"/var/lib/ssl/sensity.com.key\"}" "% ,{keyfile,  \"/var/lib/ssl/sensity.com.key\"}"
    replace ",{cacertfile, \"/var/lib/ssl/sensity.ca.crt\"}" "% ,{cacertfile, \"/var/lib/ssl/sensity.ca.crt\"}"
fi

exec "$@"
