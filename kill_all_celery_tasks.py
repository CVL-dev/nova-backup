#!/usr/bin/env python

from celery import Celery
from celery.result import AsyncResult
from celery.task.control import revoke
from celery.task.control import inspect

celery = Celery('novabackup', backend='redis://localhost', broker='redis://localhost')

for d in [inspect().scheduled(), inspect().active()]:
    for host, workers in d.iteritems():
        for w in workers:
            res = AsyncResult(w['id'])

            print res.id
            print res.result
            print res.status
            print revoke(res.id, terminate=True)
