#!/bin/bash

exec proot-distro login pi --isolated --bind ~/.pi:/root/.pi -- pi --mode rpc --no-session