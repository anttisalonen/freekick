#!/usr/bin/python

import sys

import Database

def print_club_info(clubname, db):
    print db.clubs[clubname]

def main():
    if len(sys.argv) != 2:
        usage(sys.argv[0])
        sys.exit(1)
    clubname = sys.argv[1]
    database_path = "../../../share/DB/"
    db = Database.get_db(database_path)
    print_club_info(clubname, db)
    sys.exit(0)

def usage(name):
    print "Usage: %s clubname" % name

if __name__ == "__main__":
    main()
