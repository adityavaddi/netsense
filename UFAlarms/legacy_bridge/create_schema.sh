echo Setting up Mnesia database at /tmp/legacy_mnesia..
mkdir -p /tmp/legacy_mnesia
mkdir -p $HOME/sensity_firmware
chown -R $USER $HOME/sensity_firmware
chown -R $USER /tmp/legacy_mnesia
file="/tmp/legacy_mnesia/schema.DAT"
if [ -e "$file" ]; then
	echo "legacy_mnesia schema already exists, not recreating it."
else
	echo "createing legacy_mnesia schema"
    erl -mnesia dir '"/tmp/legacy_mnesia"' -noshell -eval 'mnesia:create_schema([node()])' -eval 'init:stop()'
fi

