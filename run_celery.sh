#!/bin/bash

cd /home/carlo/nova-backup && celery -A novabackup worker --loglevel=debug --no-color -n cvl-novabackup -Q novabackup --concurrency 100
