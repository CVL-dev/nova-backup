from novabackup import SleepTest
from novabackup import nova_client

for x in range(10):
    job = SleepTest.apply_async(args=[x], kwargs={}, queue='novabackup')
    print job
