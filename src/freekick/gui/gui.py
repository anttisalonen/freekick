#!/usr/bin/env python

import sys
import subprocess
import os
import signal
import time

from PyQt4 import QtGui, QtCore

import Database

class Lineups(QtGui.QWidget):
    def __init__(self, clubnames, parent=None):
        QtGui.QWidget.__init__(self, parent)
        self.setWindowTitle('Lineups')
        self.back = QtGui.QPushButton("Go Back", self)
        self.forward = QtGui.QPushButton("Play", self)

        clubs = []
        clubs.append(db.clubs[str(clubnames[0].text())])
        clubs.append(db.clubs[str(clubnames[1].text())])
        self.lineups = []
        for i in range(2):
            self.lineups.append(QtGui.QListWidget())
            self.lineups[i].setSelectionMode(QtGui.QAbstractItemView.MultiSelection)
            for plnum in clubs[i].contracts:
                pl = db.players[plnum]
                clubs[i].players = pl
                self.lineups[i].addItem("%s" % pl)

        hbox1 = QtGui.QHBoxLayout()
        hbox1.addWidget(self.lineups[0])
        hbox1.addWidget(self.lineups[1])

        hbox2 = QtGui.QHBoxLayout()
        hbox2.addWidget(self.forward)
        hbox2.addWidget(self.back)

        vbox = QtGui.QVBoxLayout()
        vbox.addLayout(hbox1)
        vbox.addLayout(hbox2)

        self.setLayout(vbox)
        self.resize(300, 150)

        self.connect(self.forward, QtCore.SIGNAL('clicked()'), self.start)
        self.connect(self.back, QtCore.SIGNAL('clicked()'), self, QtCore.SLOT('close()'))

    def start(self):
        srv = subprocess.Popen("./fkserver")
        time.sleep(1)
        time.sleep(0.1)
        cli = subprocess.Popen("./launch_client.sh")
        aic = subprocess.Popen("./aiclient")
        cli.communicate()
        os.kill(aic.pid, signal.SIGTERM)
        os.kill(srv.pid, signal.SIGTERM)

    def doLineups(self):
        ln = Lineups(self.list.selectedItems()[:2])
        ln.show()
        self.wins = [ln]

class Friendly(QtGui.QWidget):
    def __init__(self, parent=None):
        QtGui.QWidget.__init__(self, parent)

        self.setWindowTitle('Friendly match')

        self.lineups = QtGui.QPushButton("Go to lineups", self)
        self.lineups.setEnabled(False)
        back = QtGui.QPushButton("Back", self)

        self.list = QtGui.QListWidget()
        for clubname in db.clubs.keys():
            self.list.addItem(clubname)
        self.list.setSelectionMode(QtGui.QAbstractItemView.MultiSelection)
        self.list.sortItems()

        vbox1 = QtGui.QVBoxLayout()
        vbox1.addWidget(self.list)

        vbox2 = QtGui.QVBoxLayout()
        vbox2.addWidget(self.lineups)
        vbox2.addWidget(back)

        hbox = QtGui.QHBoxLayout()
        hbox.addLayout(vbox1)
        hbox.addLayout(vbox2)

        self.setLayout(hbox)
        self.resize(300, 150)

        self.connect(self.lineups, QtCore.SIGNAL('clicked()'), self.doLineups)
        self.connect(back, QtCore.SIGNAL('clicked()'), self, QtCore.SLOT('close()'))
        self.connect(self.list, QtCore.SIGNAL('itemSelectionChanged()'), self.newSelection)

    def newSelection(self):
        if len(self.list.selectedItems()) == 2:
            self.lineups.setEnabled(True)
        else:
            self.lineups.setEnabled(False)

    def doLineups(self):
        ln = Lineups(self.list.selectedItems()[:2])
        ln.show()
        self.wins = [ln]

class MainMenu(QtGui.QWidget):
    def __init__(self, parent=None):
        QtGui.QWidget.__init__(self, parent)

        self.setWindowTitle('Freekick menu')

        clubeditor = QtGui.QPushButton("Club Editor")
        playereditor = QtGui.QPushButton("Player Editor")
        orgeditor = QtGui.QPushButton("Organization Editor")
        pitcheditor = QtGui.QPushButton("Pitch Editor")
        options = QtGui.QPushButton("Options")
        quit = QtGui.QPushButton("Quit", self)
        friendly = QtGui.QPushButton("Friendly")
        diy = QtGui.QPushButton("DIY Tournament")
        preset = QtGui.QPushButton("Preset Tournament")
        season = QtGui.QPushButton("Preset Season")
        career = QtGui.QPushButton("Career")
        loadgame = QtGui.QPushButton("Load Game")

        vbox1 = QtGui.QVBoxLayout()
        vbox1.addWidget(clubeditor)
        vbox1.addWidget(playereditor)
        vbox1.addWidget(orgeditor)
        vbox1.addWidget(pitcheditor)
        vbox1.addWidget(options)
        vbox1.addWidget(quit)

        vbox2 = QtGui.QVBoxLayout()
        vbox2.addWidget(friendly)
        vbox2.addWidget(diy)
        vbox2.addWidget(preset)
        vbox2.addWidget(season)
        vbox2.addWidget(career)
        vbox2.addWidget(loadgame)

        hbox = QtGui.QHBoxLayout()
        hbox.addLayout(vbox1)
        hbox.addLayout(vbox2)

        self.setLayout(hbox)
        self.resize(300, 150)

        self.connect(friendly, QtCore.SIGNAL('clicked()'), self.doFriendly)
        self.connect(quit, QtCore.SIGNAL('clicked()'), QtGui.qApp, QtCore.SLOT('quit()'))

    def doFriendly(self):
        fr = Friendly()
        fr.show()
        self.wins = [fr]

database_path = "../share/DB/"
db = Database.get_db(database_path)

if __name__ == '__main__':
    app = QtGui.QApplication(sys.argv)
    mm = MainMenu()
    mm.show()
    sys.exit(app.exec_())
