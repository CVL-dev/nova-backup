# nova-backup

Utility for running backups of Nectar VMs using the nova command line client.

# Installation

Install extra packages from epel:

    sudo yum -y install http://dl.fedoraproject.org/pub/epel/6/i386/epel-release-6-8.noarch.rpm
    sudo yum -y install python-novaclient python-iso8601

Install redis:

    wget http://redis.googlecode.com/files/redis-2.6.12.tar.gz
    sudo tar xzf redis-2.6.12.tar.gz -C /opt/
    cd /opt/redis-2.6.12
    sudo make

Install fabric:

    sudo yum -y install gcc python-devel
    sudo easy_install pip
    sudo pip install fabric

Install celery:

    sudo pip install -U Celery
    sudo pip install -U celery-with-redis

# Configuration

Create the file `credentials.py` and define Nova credentials (see the variables `NOVA_*` in `novabackup.py`).

# Running

Start redis:

    /opt/redis-2.6.12/src/redis-server

Start the celery worker:

    celery -A novabackup worker --loglevel=debug --no-color -n novabackup --concurrency 100

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

# Notes for actual use

Start backups and save job IDs:

    now=`date +%Y-%m-%d-%H%M`
    python run_backups.py &> run_backups_${now}.log

Show status of each job:

    cat run_backups_${now}.log | cut -f 1 -d ' ' | xargs -n 1 ./print_task_info.py
