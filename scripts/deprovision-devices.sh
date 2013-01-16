#!/bin/sh

. $HOME/.exodmrc

if [ $# -lt 1 ]
then
    echo "Usage: $0 dev-id1 [dev-id2 [...]]"
    exit 255
fi

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

echo "VAL = $VAL"

curl -u $USER_AUTH -k -X POST $URL -d @- << EOF
{
    "jsonrpc": "2.0",
    "method": "exodm:deprovision-devices",
    "id": "1",
    "params":
    {
        "dev-id": [ $VAL ]
    }
}
EOF
