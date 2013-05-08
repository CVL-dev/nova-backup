#!/usr/bin/env python

from credentials import *

from novaclient.v1_1 import client
from time import sleep

from celery import Celery
from celery.task import Task

import logging

celery = Celery('novabackup', backend='redis://localhost', broker='redis://localhost')

def nova_client():
    """
    Returns a Python object for the nova client.

    >>> nc = nova_client()
    >>> print nc.flavors.list()
    [<Flavor: m1.small>, <Flavor: m1.medium>, <Flavor: m1.xlarge>, <Flavor: m1.xxlarge>, <Flavor: m1.large>]
    """

    return client.Client(NOVA_EMAIL_ADDRESS,
                         NOVA_PASSWORD,
                         NOVA_TENANCY,
                         NOVA_URL,
                         service_type="compute")

def utc_to_local(utc_datestring):
    from dateutil import parser, tz
    return parser.parse(utc_datestring).astimezone(tz.tzlocal())

def images_in_progress(nc, vm_id, backup_name):
    return [i for i in nc.images.list() if hasattr(i, 'server') and i.server['id'] == vm_id and i.name == backup_name and i.progress < 100]

def run_backup(nc, vm_name):
    # Make sure that the VM isn't busy doing another backup.
    vm = nc.servers.find(name=vm_name)
    state = getattr(vm, 'OS-EXT-STS:task_state')
    if state is not None:
        raise ValueError, 'VM %s "%s" is busy: %s' % (vm.id, vm.name, state,)

    # Run the backup.
    backup_name = 'CVL_BACKUP_' + vm_name
    vm = nc.servers.find(name=vm_name)
    vm.backup(backup_name, 'weekly', 4)

    # Wait for the backup job to start.
    while getattr(nc.servers.find(name=vm_name), 'OS-EXT-STS:task_state') != 'image_backup':
        logging.debug('Waiting for backup job to start on VM %s' % (vm.id,))
        sleep(3)

    # Wait for the image to run to 100%.
    while True:
        images = images_in_progress(nc, vm.id, backup_name)

        if images == []: break

        for i in images:
            logging.debug('Waiting for image %s which is %d%% complete.' % (i.id, i.progress,))

        sleep(3)

    # Everything should be finished.
    assert getattr(nc.servers.find(name=vm_name), 'OS-EXT-STS:task_state') != 'image_backup'
    assert images_in_progress(nc, vm, backup_name) == []

    logging.debug('Backup for VM %s "%s" completed.' % (vm.id, vm.name,))

def most_recent_backups(nc):
    for vm in nc.servers.findall():
        backup_name = 'CVL_BACKUP_' + vm.name
        images = sorted([(utc_to_local(i.updated), i.id) for i in nc.images.list() if hasattr(i, 'server') and i.server['id'] == vm.id and i.name == backup_name])

        if images == []:
            print 'NONE', vm.name
        else:
            print images[-1][0].strftime('%Y-%m-%d %H:%M:%S'), images[-1][1], vm.name

    return images

class BackupVM(Task):
    def run(self, vm_name):
        logging.debug('backing up VM "%s"' % (vm_name,))
        run_backup(nova_client(), vm_name)
