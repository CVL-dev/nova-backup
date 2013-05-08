from novabackup import BackupVM
from novabackup import nova_client

for vm in nova_client().servers.list():
    job = BackupVM.delay(vm.name)
    print vm.name, job


