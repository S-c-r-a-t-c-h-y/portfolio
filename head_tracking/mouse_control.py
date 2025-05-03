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


def cursor_position() -> list[int, int]:
    """
    Retrieves the current position of the cursor.
    Returns:
        List[int, int]: A tuple containing the x and y coordinates of the cursor.
    """
    pt = POINT()
    user32.GetCursorPos(byref(pt))
    return [pt.x, pt.y]


def move(x, y):
    """
    Moves the cursor to the specified position.
    Args:
        x (int): The x-coordinate of the new cursor position.
        y (int): The y-coordinate of the new cursor position
    """
    pass
    # user32.SetCursorPos(int(x), int(y))


def leftclick():
    """
    Simulates a left mouse button click.
    """
    user32.mouse_event(LEFTDOWN, 0, 0, 0, 0)
    user32.mouse_event(LEFTUP, 0, 0, 0, 0)


def lefthold():
    """
    Simulates holding the left mouse button.
    """
    user32.mouse_event(LEFTDOWN, 0, 0, 0, 0)


def leftrelease():
    """
    Simulates releasing the left mouse button.
    """
    user32.mouse_event(LEFTUP, 0, 0, 0, 0)


def rightclick():
    """
    Simulates a right mouse button click.
    """
    user32.mouse_event(RIGHTDOWN, 0, 0, 0, 0)
    user32.mouse_event(RIGHTUP, 0, 0, 0, 0)


def righthold():
    """
    Simulates holding the right mouse button.
    """
    user32.mouse_event(RIGHTDOWN, 0, 0, 0, 0)


def rightrelease():
    """
    Simulates releasing the right mouse button.
    """
    user32.mouse_event(RIGHTUP, 0, 0, 0, 0)


def middleclick():
    """
    Simulates a middle mouse button click.
    """
    user32.mouse_event(MIDDLEDOWN, 0, 0, 0, 0)
    user32.mouse_event(MIDDLEUP, 0, 0, 0, 0)


def middlehold():
    """
    Simulates holding the middle mouse button.
    """
    user32.mouse_event(MIDDLEDOWN, 0, 0, 0, 0)


def middlerelease():
    """
    Simulates releasing the middle mouse button.
    """
    user32.mouse_event(MIDDLEUP, 0, 0, 0, 0)


def scroll_up(speed=BASE_SCROLL_SPEED):
    """
    Simulates scrolling up.
    Args:
        speed (int, optional): The speed of the scroll. Defaults to BASE_SCROLL_SPEED.
    """
    user32.mouse_event(WHEEL, 0, 0, speed, 0)


def scroll_down(speed=BASE_SCROLL_SPEED):
    """
    Simulates scrolling down.
    Args:
        speed (int, optional): The speed of the scroll. Defaults to BASE_SCROLL_SPEED.
    """
    user32.mouse_event(WHEEL, 0, 0, -speed, 0)


def scroll_right(speed=BASE_SCROLL_SPEED):
    """
    Simulates scrolling right.
    Args:
        speed (int, optional): The speed of the scroll. Defaults to BASE_SCROLL_SPEED.
    """
    user32.mouse_event(HWHEEL, 0, 0, speed, 0)


def scroll_left(speed=BASE_SCROLL_SPEED):
    """
    Simulates scrolling left.
    Args:
        speed (int, optional): The speed of the scroll. Defaults to BASE_SCROLL_SPEED.
    """
    user32.mouse_event(HWHEEL, 0, 0, -speed, 0)
