#!/bin/sh
# attributes must be given as " \"attr1\",\"attr2\" "
. $HOME/.exodmrc

if [ $# != 3 ]
then
    echo "Usage: $0 batch-size previous-device attribute-list (\"\" start from beginning)"
    exit 255
fi

curl -u $USER_AUTH -k -X POST $URL -d @- <<EOF
{
    "json-rpc": "2.0",
    "method": "exodm:list-devices-attributes",
    "id": "1",
    "params": {
	"n": $1,
	"previous": "$2",
        "attributes": [$3],
        "pattern": "",
        "direction": "ascending"
    }
}
EOF
