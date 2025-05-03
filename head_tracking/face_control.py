import face_analyser
from math import pi, sqrt
from mouse_control import *

"""
This module provides a class `FaceControl` that allows controlling the mouse cursor and mouse buttons using face movements and eye blinks.
Classes:
    FaceControl
"""


def clamp(num, min_value, max_value):
    """
    Clamps a number between a minimum and maximum value.
    Args:
        num (float): The number to clamp.
        min_value (float): The minimum value.
        max_value (float): The maximum value.
    Returns:
        float: The clamped number.
    """
    return max(min(num, max_value), min_value)


class FaceControl:
    """
    A class to control the mouse cursor and mouse buttons using face movements and eye blinks.

    Attributes:
    -----------
        screen_width (int): The width of the screen.
        screen_height (int): The height of the screen.
        frames_blinking_before_eyes_closed (int): The number of frames before considering the eyes closed.
        movement_sensitivity (int): The sensitivity of the cursor movement.
        detection_sensitivity (int): The sensitivity of the face movement detection.
        debug (bool): Flag to enable or disable debug mode.
        calibrated (bool): Flag to indicate if the system is calibrated.
        neutral_yaw (float): The neutral yaw angle.
        neutral_pitch (float): The neutral pitch angle.
        neutral_roll (float): The neutral roll angle.
        left_blink_counter (int): Counter for consecutive frames the left eye was closed.
        right_blink_counter (int): Counter for consecutive frames the right eye was closed.
        both_blink_counter (int): Counter for consecutive frames both eyes were closed.
        holding_left (bool): Flag to indicate if the left mouse button is held.
        holding_right (bool): Flag to indicate if the right mouse button is held.
        tracking_enabled (bool): Flag to indicate if the mouse should follow face movements.
        analyser (FaceAnalyser): An instance of the FaceAnalyser class.
        cursor_pos (list): The current cursor position.
    """

    def __init__(
        self,
        width,
        height,
        *,
        blinking_closure_treshold,
        frames_blinking_before_eyes_closed,
        eye_darkness_treshold,
        movement_sensitivity=30,
        detection_sentitivity=12,
        debug=False
    ):
        """
        Initializes the FaceControl class with the given parameters.
        Args:
            width (int): The width of the screen.
            height (int): The height of the screen.
            blinking_closure_treshold (float): The threshold for detecting eye closure.
            frames_blinking_before_eyes_closed (int): The number of frames before considering the eyes closed.
            movement_sensitivity (int, optional): The sensitivity of the cursor movement. Defaults to 30.
            detection_sentitivity (int, optional): The sensitivity of the face movement detection. Defaults to 12.
            debug (bool, optional): Flag to enable or disable debug mode. Defaults to False.
        """
        self.screen_width = width
        self.screen_height = height
        self.frames_blinking_before_eyes_closed = frames_blinking_before_eyes_closed

        # number of pixel moved per frame at normal speed
        self.movement_sensitivity = movement_sensitivity

        self.detection_sensitivity = detection_sentitivity

        self.debug: bool = debug

        self.calibrated = False
        self.neutral_yaw = None
        self.neutral_pitch = None
        self.neutral_roll = None

        # counts the number of consecutive frames the eye was closed
        self.left_blink_counter: int = 0
        self.right_blink_counter: int = 0
        self.both_blink_counter: int = 0

        # flags whether the mouse button is held or not
        self.holding_left: bool = False
        self.holding_right: bool = False

        # flags whether the mouse should follow face movements or not
        self.tracking_enabled: bool = False

        self.analyser = face_analyser.FaceAnalyser(
            blinking_closure_treshold=blinking_closure_treshold, eye_darkness_treshold=eye_darkness_treshold, debug=debug
        )

        self.cursor_pos = cursor_position()

    def calibrate(self):
        """
        Calibrates the neutral yaw, pitch, and roll angles based on the current face position.
        """
        self.neutral_yaw, self.neutral_pitch = self.analyser.yaw_and_pitch()
        self.neutral_roll = self.analyser.roll()

        self.calibrated = True

    def update(self, face, image):
        """
        Updates the face control system with the given face data.
        Args:
            face: The face data to update the system with.
        """
        self.analyser.update(face, image)
        self.update_closure()

        if not self.calibrated:
            self.calibrate()

        if self.tracking_enabled:
            yaw_angle, pitch_angle = self.analyser.yaw_and_pitch()
            roll_angle = self.analyser.roll()

            yaw_angle -= self.neutral_yaw
            pitch_angle -= self.neutral_pitch
            roll_angle -= self.neutral_roll
            pitch_angle = -pitch_angle

            if self.debug:
                print(yaw_angle * 180 / pi, pitch_angle * 180 / pi, roll_angle * 180 / pi)

            mouvement_intensity = sqrt(yaw_angle**2 + pitch_angle**2) * 100

            if mouvement_intensity >= self.detection_sensitivity:
                self.cursor_pos[0] = clamp(
                    self.cursor_pos[0] + yaw_angle * self.movement_sensitivity,
                    0,
                    self.screen_width,
                )
                self.cursor_pos[1] = clamp(
                    self.cursor_pos[1] + pitch_angle * self.movement_sensitivity,
                    0,
                    self.screen_height,
                )
                move(self.cursor_pos[0], self.cursor_pos[1])
            else:
                left_eye_pos, right_eye_pos = self.analyser.eye_positions()
                if left_eye_pos == face_analyser.EyePosition.LEFT or right_eye_pos == face_analyser.EyePosition.LEFT:
                    self.cursor_pos[0] = clamp(
                        self.cursor_pos[0] - 10,
                        0,
                        self.screen_width,
                    )
                    move(self.cursor_pos[0], self.cursor_pos[1])
                elif left_eye_pos == face_analyser.EyePosition.RIGHT or right_eye_pos == face_analyser.EyePosition.RIGHT:
                    self.cursor_pos[0] = clamp(
                        self.cursor_pos[0] + 10,
                        0,
                        self.screen_width,
                    )
                    move(self.cursor_pos[0], self.cursor_pos[1])

        left_blinking = self.left_eye_closed()
        right_blinking = self.right_eye_closed()
        both_blinking = self.both_eye_closed()

        if left_blinking and not self.holding_left:
            self.holding_left = True
            lefthold()
        if right_blinking and not self.holding_right:
            self.holding_right = True
            righthold()

        if self.holding_left and not left_blinking:
            self.holding_left = False
            leftrelease()
        if self.holding_right and not right_blinking:
            self.holding_right = False
            rightrelease()

        if both_blinking:
            self.tracking_enabled = not self.tracking_enabled
            if self.tracking_enabled:
                self.cursor_pos = cursor_position()

        ###* scrolling movement control ###

        # if roll_angle * 180 / pi >= UP_SCROLL_ROLL:
        #     scroll_up()
        # elif roll_angle * 180 / pi <= DOWN_SCROLL_ROLL:
        #     scroll_down()

    def update_closure(self):
        """
        Updates the closure status of the eyes based on the face analyser data.
        """

        left_closed, right_closed = self.analyser.closure()
        both_closed = left_closed and right_closed

        # determines the number of consecutive frames that a single eye was closed
        if both_closed or not left_closed:
            self.left_blink_counter = 0
        elif left_closed:
            self.left_blink_counter += 1

        if both_closed or not right_closed:
            self.right_blink_counter = 0
        elif right_closed:
            self.right_blink_counter += 1

        if both_closed:
            self.both_blink_counter += 1
        else:
            self.both_blink_counter = 0

    def left_eye_closed(self) -> bool:
        """
        Checks if the left eye is closed.
        Returns:
            bool: True if the left eye is closed, False otherwise.
        """
        return self.left_blink_counter >= self.frames_blinking_before_eyes_closed

    def right_eye_closed(self) -> bool:
        """
        Checks if the right eye is closed.
        Returns:
            bool: True if the right eye is closed, False otherwise.
        """
        return self.right_blink_counter >= self.frames_blinking_before_eyes_closed

    def both_eye_closed(self) -> bool:
        """
        Checks if both eyes are closed.
        Returns:
            bool: True if both eyes are closed, False otherwise.
        """
        return self.both_blink_counter >= self.frames_blinking_before_eyes_closed
