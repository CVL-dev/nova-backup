from novabackup import BackupVM
from novabackup import nova_client
import sys
import time

DAY_START = 4
DAY_END   = 10

if time.localtime().tm_hour in range(DAY_START, DAY_END + 1):
    print 'Error: not starting backups, current time is between %.2d and %.2d hours' % (DAY_START, DAY_END,)
    sys.exit(1)

for vm in nova_client().servers.list():
    job = BackupVM.apply_async(args=[vm.name], kwargs={}, queue='novabackup')
    print job, vm.name
