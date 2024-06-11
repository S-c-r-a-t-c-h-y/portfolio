import ctypes
from ctypes import Structure, c_long, byref

user32 = ctypes.windll.user32

LEFTDOWN = 0x00000002
LEFTUP = 0x00000004
MIDDLEDOWN = 0x00000020
MIDDLEUP = 0x00000040
MOVE = 0x00000001
ABSOLUTE = 0x00008000
RIGHTDOWN = 0x00000008
RIGHTUP = 0x00000010
WHEEL = 0x0800
HWHEEL = 0x01000

BASE_SCROLL_SPEED = 20


class POINT(Structure):
    _fields_ = [("x", c_long), ("y", c_long)]


def cursor_position():
    pt = POINT()
    user32.GetCursorPos(byref(pt))
    return [pt.x, pt.y]


def move(x, y):
    user32.SetCursorPos(int(x), int(y))


def leftclick():
    user32.mouse_event(LEFTDOWN, 0, 0, 0, 0)
    user32.mouse_event(LEFTUP, 0, 0, 0, 0)


def lefthold():
    user32.mouse_event(LEFTDOWN, 0, 0, 0, 0)


def leftrelease():
    user32.mouse_event(LEFTUP, 0, 0, 0, 0)


def rightclick():
    user32.mouse_event(RIGHTDOWN, 0, 0, 0, 0)
    user32.mouse_event(RIGHTUP, 0, 0, 0, 0)


def righthold():
    user32.mouse_event(RIGHTDOWN, 0, 0, 0, 0)


def rightrelease():
    user32.mouse_event(RIGHTUP, 0, 0, 0, 0)


def middleclick():
    user32.mouse_event(MIDDLEDOWN, 0, 0, 0, 0)
    user32.mouse_event(MIDDLEUP, 0, 0, 0, 0)


def middlehold():
    user32.mouse_event(MIDDLEDOWN, 0, 0, 0, 0)


def middlerelease():
    user32.mouse_event(MIDDLEUP, 0, 0, 0, 0)


def scroll_up(speed=BASE_SCROLL_SPEED):
    user32.mouse_event(WHEEL, 0, 0, speed, 0)


def scroll_down(speed=BASE_SCROLL_SPEED):
    user32.mouse_event(WHEEL, 0, 0, -speed, 0)


def scroll_right(speed=BASE_SCROLL_SPEED):
    user32.mouse_event(HWHEEL, 0, 0, speed, 0)


def scroll_left(speed=BASE_SCROLL_SPEED):
    user32.mouse_event(HWHEEL, 0, 0, -speed, 0)
