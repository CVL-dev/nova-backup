#!/usr/bin/env python

from credentials import *
from novabackup import nova_client, utc_to_local

if __name__ == '__main__':
    nc = nova_client()

    for vm in nc.servers.findall():
        backup_name = 'CVL_BACKUP_' + vm.name
        # print backup_name
        images = sorted([(utc_to_local(i.updated), i.id) for i in nc.images.list() if i.name == backup_name and hasattr(i, 'server') and i.server['id'] == vm.id])

        if images == []:
            print 'NONE', vm.name
        else:
            print images[-1][0].strftime('%Y-%m-%d %H:%M:%S'), images[-1][1], vm.name
