echo Setting up Mnesia database at /tmp/dcc_mnesia..
mkdir -p /tmp/dcc_mnesia
chown -R $USER /tmp/dcc_mnesia
file="/tmp/dcc_mnesia/schema.DAT"
if [ -e "$file" ]; then
	echo "dcc_mnesia schema already exists, not recreating it."
else
	echo "createing dcc_mnesia schema"
    erl -mnesia dir '"/tmp/dcc_mnesia"' -noshell -eval 'mnesia:create_schema([node()])' -eval 'init:stop()'
fi

