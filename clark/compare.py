#!/usr/bin/env python -tt

from pymarc import MARCReader

with open("~/data/CUL-marc/Columbia-extract-20190831-001.mrc", "rb") as fh:
    reader = MARCReader(fh, utf8_handling='replace')
    for index, record in enumerate(reader):
        lead = record.leader
        print("{}\t{}".format(index, lead))
