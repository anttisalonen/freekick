#!/usr/bin/env python3.0

import os
import re
import sys

if __name__ == "__main__":
    pid = 2000
    dir_prefix = "./teams/"
    with open('codes.txt', 'r') as c:
        codes = c.read()

    with open('teams.txt', 'r') as f:
        for line in f:
            spl = line.split(':')
            if len(spl) < 3 or len(spl[1]) < 2:
                continue

            in_file_name = dir_prefix + spl[0]
            country_name = spl[1]
            country_adj  = spl[2].strip('\n')

            m = re.search('%s \((.+?)\)' % country_name, codes)
            if m == None:
                print("Country {0} not found in codes!".format(country_name))
                sys.exit(1)
            outfile_suffix = m.group(1).capitalize()

            club_out = dir_prefix + "Clubs_" + outfile_suffix + ".xml"
            pl_out = dir_prefix + "Players_%04d.xml" % (pid / 1000)
            country_out = dir_prefix + "Country_" + outfile_suffix + ".xml"
            os.system('./main %d %s "%s" "%s" %s %s %s' % (pid, in_file_name, country_name, country_adj, club_out, pl_out, country_out))
            pid += 2000
