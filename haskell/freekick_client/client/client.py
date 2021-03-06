#!/usr/bin/env python


#Import Modules
import os, pygame
from pygame.locals import *
import socket
import time

if not pygame.font: print 'Warning, fonts disabled'
if not pygame.mixer: print 'Warning, sound disabled'

HOST = '127.0.0.1'
PORT = 32105

#functions to create our resources
def load_image(name, colorkey=None):
	fullname = os.path.join('data', name)
	try:
		image = pygame.image.load(fullname)
	except pygame.error, message:
		print 'Cannot load image:', fullname
		raise SystemExit, message
	image = image.convert()
	if colorkey is not None:
		if colorkey is -1:
			colorkey = image.get_at((0,0))
		image.set_colorkey(colorkey, RLEACCEL)
	return image, image.get_rect()

def load_sound(name):
	class NoneSound:
		def play(self): pass
	if not pygame.mixer or not pygame.mixer.get_init():
		return NoneSound()
	fullname = os.path.join('data', name)
	try:
		sound = pygame.mixer.Sound(fullname)
	except pygame.error, message:
		print 'Cannot load sound:', fullname
		raise SystemExit, message
	return sound

class Vector():
	def __init__(self):
		self.x = 0
		self.y = 0
		self.z = 0

	def add(self, vector):
		self.x += vector.x
		self.y += vector.y
		self.z += vector.z
		return self

	def add3(self, a, b, c):
		self.x += a
		self.y += b
		self.z += c
		return self

	def multiply(self, s):
		self.x *= s
		self.y *= s
		self.z *= s
		return self

	def getxy(self):
		return ((self.x, self.y))

class Fieldobject(pygame.sprite.Sprite):
	def __init__(self):
		pygame.sprite.Sprite.__init__(self)
		self.location = Vector()
		self.acceleration = Vector()
		self.velocity = Vector()
		self.timedelta = 0

	def update(self):
		self.velocity.add (self.acceleration.multiply(self.timedelta))
		self.location.add (self.velocity.multiply(self.timedelta))
		self.rect = self.location.getxy()
		
	def accelerate(self, a, b, c):
		self.acceleration.add3(a, b, c)

	def settimedelta(self, time):
		self.timedelta = time

class Ball(Fieldobject):
	def __init__(self):
		Fieldobject.__init__(self)
		self.image, self.rect = load_image('ball.png', -1)

	def update(self):
		Fieldobject.update(self)

class Player(Fieldobject):
	def __init__(self):
		Fieldobject.__init__(self)
		self.image, self.rect = load_image('player.png', -1)
		self.num = -1
		self.team = -1

	def update(self):
		Fieldobject.update(self)

def main():
	"""this function is called when the program starts.
	   it initializes everything it needs, then runs in
	   a loop until the function returns."""
#Initialize Everything
	pygame.init()
	screen = pygame.display.set_mode((640, 480))
	pygame.display.set_caption('Free Kick client')
	pygame.mouse.set_visible(1)

#Create The Backgound
	background = pygame.Surface(screen.get_size())
	background = background.convert()
	background.fill((0, 150, 0))


#Put Text On The Background, Centered
	"""if pygame.font:
		font = pygame.font.Font(None, 36)
		text = font.render("Pummel The Chimp, And Win $$$", 1, (10, 10, 10))
		textpos = text.get_rect(centerx=background.get_width()/2)
		background.blit(text, textpos) """


#Display The Background
	screen.blit(background, (0, 0))
	pygame.display.flip()

#Prepare Game Objects
	clock = pygame.time.Clock()
#	whiff_sound = load_sound('whiff.wav')
#	punch_sound = load_sound('punch.wav')
	player = Player()
	ball = Ball()
	player.settimedelta(1/60)
	ball.settimedelta(1/60)
	allsprites = pygame.sprite.RenderPlain((player, ball))

#Wait for server
	connected = False
	while connected == False:
		s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
		try:
			s.connect((HOST, PORT))
			connected = True
		except socket.error:
			print "Could not connect to server; retry in 1 second"
			time.sleep(1)
			connected = True

#Main Loop
	while 1:
		clock.tick(60)
#		data = s.recv(1024)
		player.accelerate(100, 100, 0)

	#Handle Input Events
		for event in pygame.event.get():
			if event.type == QUIT:
				return
			elif event.type == KEYDOWN and event.key == K_ESCAPE:
				return
			elif event.type == MOUSEBUTTONDOWN:
				pass
			elif event.type is MOUSEBUTTONUP:
				pass

		allsprites.update()

	#Draw Everything
		screen.blit(background, (0, 0))
		allsprites.draw(screen)
		pygame.display.flip()

#Game Over
	s.close()


#this calls the 'main' function when this script is executed
if __name__ == '__main__': main()

