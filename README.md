nova-backup
===========

Utility for running backups of Nectar VMs using the nova command line client.

# Installation

    cabal install

    export PATH=$PATH:$HOME/.cabal/bin

# Usage

Run with no arguments for usage instructions:

    nova-backup-util

# Python stuff:

Edit `NOVA_*` credentials in `credentials.py`, then:

    celery -A novabackup worker --loglevel=debug --no-color -n novabackup

    python run_backups.py

