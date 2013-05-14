#!/usr/bin/env python

from celery import Celery
from celery.result import AsyncResult
from celery.task.control import revoke

celery = Celery('novabackup', backend='redis://localhost', broker='redis://localhost')

from celery.task.control import inspect

i = inspect()

for d in [i.scheduled(), i.active()]:
    for host, workers in d.iteritems():
        for w in workers:
            res = AsyncResult(w['id'])

            print res.id
            print res.result
            print res.status
            print revoke(res.id, terminate=True)
