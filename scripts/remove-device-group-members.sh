#!/bin/sh

. $HOME/.exodmrc

if [ $# -lt 2 ]
then
    echo "Usage: $0 device-group devid1 devid2 ..."
    exit 255
fi

GID=$1
shift
FIRST_ENTRY=true
while [ "$#" -gt "0" ]
do
    if [ "$FIRST_ENTRY" = "true" ]
    then
        VAL="\"$1\""
        FIRST_ENTRY=false
    else
        VAL="$VAL, \"$1\""
    fi
    shift
done

echo "GID = $GID"
echo "VAL = $VAL"

curl -u $USER_AUTH -k -X POST $URL -d @- << EOF
{
    "jsonrpc": "2.0",
    "method": "exodm:remove-device-group-members",
    "id": "1",
    "params":
    {
        "device-groups": [ "$GID" ],
        "dev-id": [ $VAL ]
    }
}
EOF
