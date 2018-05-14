#!/bin/bash -eu

replace() {
    setting="${1}"
    value="${2}"
    file="/deploy/_rel/releases/1/sys.config"

    if [ -n "${value}" ]; then
        if grep -q -F "${setting}" "${file}"; then
            sed --in-place "s|${setting}|${value}|" "${file}"
        else
            echo "FAILED to find ${setting} in ${file}"
            exit 1
        fi
    fi
}

setting() {
    setting="${1}"
    value="${2}"
    file="/deploy/_rel/releases/1/sys.config"

    if [ -n "${value}" ]; then
        if grep -q -F "${setting}" "${file}"; then
            sed --in-place "s|\(.*\)${setting}\(.*\)|\1${value}\2|" "${file}"
        else
            echo "FAILED to find ${setting} in ${file}"
            exit 1
        fi
    fi
}

setting "{{dcc.local-ip}}" "${legacy_service:-127.0.0.1}"
setting "{{datadealer.local-ip}}" "${datadealer_service:-127.0.0.1}"
setting "{{graphite.local-ip}}" "${graphite_service:-localhost}"
setting "{{legacy-fqdn}}" "${legacy_service:-127.0.0.1}"

replace ",{haproxy, \"{{interface.local-ip}}\"}" ",{haproxy, \"${datadealer_service:-127.0.0.1}\"}"
replace ",{moscahost,\"{{interface.local-ip}}\"}" ",{moscahost,\"${mosquitto_service:-localhost}\"}"
replace ",{moscaport, 3002}" ",{moscaport, ${mosquitto_port:-3002}}"

exec "$@"
