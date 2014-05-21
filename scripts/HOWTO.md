HOWTO run exodmapi scripts
======================

# 1- create the an account

This script must run on the same machine as the dm server is located.

    ./create-account.sh <name> <password>

The user "admin" is automatically create and may be access through the
<name>/admin pair.

# 2 - create a device type

    ./create-device-type.sh <device-type-name> <protocol>

Where protocol maybe: ga_ck3 | exodm\_bert etc.

# 3a - upload yang spec 

    ./create-yang-module <yang-spec-file-name>

# 3 - create config set

    ./create-config-set.sh <config-set-name> <yang-spec-name> <notification-url>

# example bootstrap of dm server

    ./create-account.sh abc abc123
	
    ./create-device-type.sh abc exodm_bert
	
    ./create-yang-module.sh spec.yang
	
    ./create-config-set.sh conf1 spec.yang http://localhost:8380

	./add-config-set-members.sh conf1 1001 1002 1003 1004
	
    ./update-config-set.sh conf1 http://localhost:8380 x 1 y 2 z 3

    ./push-config-set.sh conf1
	
