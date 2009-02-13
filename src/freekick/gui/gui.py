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
        self.resize(800, 600)

        self.connect(self.forward, QtCore.SIGNAL('clicked()'), self.start)
        self.connect(self.back, QtCore.SIGNAL('clicked()'), self, QtCore.SLOT('close()'))

    def start(self):
        cwd = os.getcwd()
        srv = subprocess.Popen(os.path.join(cwd, "fkserver"))
        time.sleep(1)
        cli = subprocess.Popen(os.path.join(cwd, "launch_client.sh"))
        time.sleep(0.1)
        aic = subprocess.Popen(os.path.join(cwd, "aiclient"))
        cli.communicate()
        time.sleep(0.5)
        os.kill(aic.pid, signal.SIGTERM)
        time.sleep(0.5)
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
        self.resize(800, 600)

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

class DIY(QtGui.QWidget):
    def __init__(self, parent=None):
        QtGui.QWidget.__init__(self, parent)

        self.max_clubs = 128
        self.max_num_stages = 12

        self.setWindowTitle('DIY')

        self.forward = QtGui.QPushButton("Choose clubs", self)
        self.forward.setEnabled(False)
        self.back = QtGui.QPushButton("Back", self)
        self.bottom_hbox = QtGui.QHBoxLayout()
        self.bottom_hbox.addWidget(self.back)
        self.bottom_hbox.addWidget(self.forward)

        self.stageboxes = []
        self.additional_clubs_boxes = []
        self.num_clubs_boxes = []
        self.stage_type_boxes = []
        for stage in range(self.max_num_stages):
            self.stageboxes.append(QtGui.QHBoxLayout())
            self.num_clubs_boxes.append(QtGui.QSpinBox())
            self.stage_type_boxes.append(QtGui.QComboBox())
            self.additional_clubs_boxes.append(QtGui.QSpinBox())
            self.stageboxes[stage].addWidget(self.additional_clubs_boxes[stage])
            self.stageboxes[stage].addWidget(self.num_clubs_boxes[stage])
            self.stageboxes[stage].addWidget(self.stage_type_boxes[stage])

        # defaults
        self.num_clubs_boxes[0].setValue(32)
        self.num_clubs_boxes[1].setValue(16)
        self.num_clubs_boxes[2].setValue(8)
        self.num_clubs_boxes[3].setValue(4)
        self.num_clubs_boxes[4].setValue(2)

        for stage in range(5, len(self.num_clubs_boxes)):
            self.num_clubs_boxes[stage].setValue(1)

        self.num_clubs_boxes[0].setRange(self.num_clubs_boxes[1].value() + 1, self.max_clubs)

        vbox = QtGui.QVBoxLayout()
        for n in range(self.max_num_stages):
            vbox.addLayout(self.stageboxes[n])
        vbox.addLayout(self.bottom_hbox)

        self.setLayout(vbox)
        self.resize(800, 600)

        for stage in range(self.max_num_stages):
            self.connect(self.num_clubs_boxes[stage], QtCore.SIGNAL('valueChanged(int)'), self.update_stages)
            self.connect(self.additional_clubs_boxes[stage], QtCore.SIGNAL('valueChanged(int)'), self.update_stages)
        self.connect(self.back, QtCore.SIGNAL('clicked()'), self, QtCore.SLOT('close()'))

        self.update_stages()

    def update_stages(self):
        for stage in range(0, self.max_num_stages):
            if stage == 0:
                max_num = self.max_clubs
            else:
                max_num = self.num_clubs_boxes[stage - 1].value() + self.additional_clubs_boxes[stage - 1].value() - 1
            if stage == (self.max_num_stages - 1):
                min_num = 1
            else:
                min_num = self.num_clubs_boxes[stage + 1].value() + 1 - self.additional_clubs_boxes[stage].value()
                if min_num == 2:
                    min_num = 1
            if max_num - min_num < 0:
                self.num_clubs_boxes[stage].setRange(1, 1)
                self.num_clubs_boxes[stage].setEnabled(False)
            else:
                self.num_clubs_boxes[stage].setRange(min_num, max_num)
                self.num_clubs_boxes[stage].setEnabled(True)

            self.additional_clubs_boxes[stage].setRange(0, self.max_clubs - self.num_clubs_boxes[stage].value())

            total_clubs_here = self.num_clubs_boxes[stage].value() + self.additional_clubs_boxes[stage].value()
            if stage == (self.max_num_stages - 1):
                self.set_correct_stage_types(self.stage_type_boxes[stage], total_clubs_here, 1)
            else:
                self.set_correct_stage_types(self.stage_type_boxes[stage], total_clubs_here, self.num_clubs_boxes[stage + 1].value())
            everything_ok = all([b.count() > 0 or (not b.isEnabled()) for b in self.stage_type_boxes])
            self.forward.setEnabled(everything_ok)

    def set_correct_stage_types(self, combo, num_clubs, num_clubs_next):
        combo.clear()
        if num_clubs <= 1:
            combo.setEnabled(False)
            return
        else:
            combo.setEnabled(True)
        if num_clubs_next * 2 == num_clubs:
            combo.addItem("Knockout")
        for n in range(24, 2, -1):
            if num_clubs % n == 0:
                num_clubs_per_group = n
                num_groups = num_clubs // n
                if num_groups > 16:
                    continue
                if num_groups > 1:
                    combo.addItem("%d groups of %d" % (num_groups, num_clubs_per_group))
                else:
                    combo.addItem("%d group of %d" % (num_groups, num_clubs_per_group))

class MainMenu(QtGui.QWidget):
    def __init__(self, parent=None):
        QtGui.QWidget.__init__(self, parent)

        self.wins = []
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
        self.resize(800, 600)

        self.connect(friendly, QtCore.SIGNAL('clicked()'), self.doFriendly)
        self.connect(diy, QtCore.SIGNAL('clicked()'), self.doDIY)
        self.connect(quit, QtCore.SIGNAL('clicked()'), QtGui.qApp, QtCore.SLOT('quit()'))

    def doFriendly(self):
        fr = Friendly()
        fr.show()
        self.wins += [fr]

    def doDIY(self):
        wn = DIY()
        wn.show()
        self.wins += [wn]

database_path = "../share/DB/"
db = Database.get_db(database_path)

if __name__ == '__main__':
    app = QtGui.QApplication(sys.argv)
    mm = MainMenu()
    mm.show()
    sys.exit(app.exec_())
