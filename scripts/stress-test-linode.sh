#!/bin/bash

# Log all stdout output to stackscript.log
exec &> >(tee -i /var/log/stackscript.log)

# Uncomment next line to enable debugging features
# set -xeo pipefail

cd $HOME

# Download stress test binary
binary="$HOME/simplex-stress-test"
curl -L -o $binary https://github.com/simplex-chat/simplex-chat/releases/download/v1.2.1/simplex-stress-test
chmod +x $binary

# / Create systemd service
cat <<EOT >> /etc/systemd/system/simplex-stress-test.service
[Unit]
Description=SMP server stress test

[Service]
Type=simple
ExecStart=/bin/sh -c 'exec $binary >> $HOME/test.log 2>&1'
Restart=always
RestartSec=3

[Install]
WantedBy=multi-user.target

EOT
# Create systemd service /

# Start systemd service
chmod 644 /etc/systemd/system/simplex-stress-test.service
sudo systemctl enable simplex-stress-test
sudo systemctl start simplex-stress-test
