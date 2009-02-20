#!/usr/bin/python

import sys
import time

import libfreekick_client_py as freekick

def main():
    conn = freekick.IP_Connection()
    conn.ip_address = "127.0.0.1"
    conn.port = "32105"
    ai = False
    network = freekick.Network(conn, ai)
    network.run(True)
    time.sleep(0.1)
    if not network.is_connected():
        print "Could not connect"
        sys.exit(1)

    print "waiting for input..."
    network.read_connection(True)
    print "read"

    m = freekick.InitialDataRequest()
    network.sendMessage(m)
    
    counter = 10
    while counter > 0:
        print "waiting for input..."
        network.read_connection(True)
        print "read"
        counter -= 1

    ms = network.getMatchStatus()
    print "pitch width: %d" % ms.getPitchWidth()
    time.sleep(1)
    sys.exit(0)

if __name__ == "__main__":
    main()
