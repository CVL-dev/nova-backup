from novabackup import BackupVM
from novabackup import nova_client

for vm in nova_client().servers.list():
    job = BackupVM.apply_async(args=[vm.name], kwargs={}, queue='novabackup')
    print vm.name, job
