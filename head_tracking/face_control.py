import face_analyser
from math import pi, sqrt
from mouse_control import *


def clamp(num, min_value, max_value):
    return max(min(num, max_value), min_value)


class FaceControl:

    def __init__(
        self,
        width,
        height,
        *,
        blinking_closure_treshold,
        frames_blinking_before_eyes_closed,
        movement_sensitivity=30,
        detection_sentitivity=12,
        debug=False
    ):
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
            blinking_closure_treshold=blinking_closure_treshold, debug=debug
        )

        self.cursor_pos = cursor_position()

    def calibrate(self):
        self.neutral_yaw, self.neutral_pitch = self.analyser.yaw_and_pitch()
        self.neutral_roll = self.analyser.roll()

        self.calibrated = True

    def update(self, face):
        self.analyser.update_face(face)
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
                print(
                    yaw_angle * 180 / pi, pitch_angle * 180 / pi, roll_angle * 180 / pi
                )

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
        return self.left_blink_counter >= self.frames_blinking_before_eyes_closed

    def right_eye_closed(self) -> bool:
        return self.right_blink_counter >= self.frames_blinking_before_eyes_closed

    def both_eye_closed(self) -> bool:
        return self.both_blink_counter >= self.frames_blinking_before_eyes_closed
