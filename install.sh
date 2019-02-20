#!/bin/sh

set -xeu

cp -f thermischd         /usr/local/bin/thermischd
cp -f thermischd.service /etc/systemd/system/thermischd.service
systemctl enable --now thermischd
