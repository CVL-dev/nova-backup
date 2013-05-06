#!/bin/bash

echo "nova-backup/runcron.sh"

date

cd /home/carlo/CharacterisationVL_EC2_credentials;
source openrc.sh
source ec2rc.sh
cd - &> /dev/null # Why does this print /home/carlo?

/home/carlo/.cabal/bin/nova-backup-util --run-backups


