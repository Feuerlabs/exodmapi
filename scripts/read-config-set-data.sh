#!/bin/sh

. $HOME/.exodmrc

if [ $# != 2 ]
then
    echo "Usage: $0 config-set area"
    exit 255
fi

curl -u $USER_AUTH -k -X POST $URL -d @- <<EOF
{
    "json-rpc": "2.0",
    "method": "exodm:read-config-set-data",
    "id": "1",
    "params": {
	"name": "$1",
        "area": "$2"
    }
}
EOF
