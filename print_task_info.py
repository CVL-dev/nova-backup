#!/usr/bin/env python

from celery import Celery
from celery.task import Task
from celery.result import AsyncResult
import sys

celery = Celery('novabackup', backend='redis://localhost', broker='redis://localhost')


if __name__ == '__main__':
    id = sys.argv[1]

    res = AsyncResult(id)

    print 'id:',        res
    print 'result:',    res.result
    print 'status:',    res.status
    print 'traceback:', res.traceback
