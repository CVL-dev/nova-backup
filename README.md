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

Create the file `credentials.py` and define Nova credentials (see the variables `NOVA_*` in `novabackup.py`).

Start the celery worker:

    celery -A novabackup worker --loglevel=debug --no-color -n novabackup

Run the main backup script:

    python run_backups.py

This script prints a list of job IDs and VM names, e.g.

    c60341f5-f97a-4c12-9a2e-f7b993e2b6da carlo_thrash_01
    9252d10f-1a96-4860-ae8f-ddf3b12a72a8 carlo_um_002
    ccdda9d4-061f-418e-8558-cb9d39181d58 carlo_cvl_test_003

Check the active jobs:

    ./print_active_tasks.py | grep novabackup

For example:

    $ ./print_active_tasks.py | grep novabackup
    c60341f5-f97a-4c12-9a2e-f7b993e2b6da novabackup.BackupVM
    9252d10f-1a96-4860-ae8f-ddf3b12a72a8 novabackup.BackupVM
    ccdda9d4-061f-418e-8558-cb9d39181d58 novabackup.BackupVM

Inspect a backup job using its ID:

    $ ./print_task_info.py c60341f5-f97a-4c12-9a2e-f7b993e2b6da
    id: c60341f5-f97a-4c12-9a2e-f7b993e2b6da
    result: {'info': u'waiting for image 7a30f13e-cd36-4d90-9f3d-0846154000c2 which is 25% complete', 'params': (u'carlo_thrash_01',)}
    status: PROGRESS
    traceback: None

    $ ./print_task_info.py c60341f5-f97a-4c12-9a2e-f7b993e2b6da
    id: c60341f5-f97a-4c12-9a2e-f7b993e2b6da
    result: (u'carlo_thrash_01',)
    status: SUCCESS
    traceback: None

If a job fails, the `traceback` will have useful debug info.

# In Use

## Start backups and save job IDs:

    now=`date +%Y-%m-%d-%H%M`
    python run_backups.py > run_backups_${now}.log

## Show status of each job:

    cat run_backups_${now}.log | cut -f 1 -d ' ' | xargs -n 1 ./print_task_info.py
