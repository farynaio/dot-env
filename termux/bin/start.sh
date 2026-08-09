#!/bin/bash

set -e

ADB_IP=192.168.1.38
ADB_PORT=42171

adb connect $ADB_IP:$ADB_PORT

adb shell appops set com.android.systemui READ_CLIPBOARD deny
