from novabackup import BackupVM
from novabackup import nova_client

for vm in nova_client().servers.list():
    if vm.name not in ['carlo_thrash_01', 'carlo_um_002', 'carlo_cvl_test_003',]: continue

    job = BackupVM.apply_async(args=[vm.name], kwargs={}, queue='novabackup')
    print job, vm.name
