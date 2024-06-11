from math import *
import numpy as np


class FaceAnalyser:
    def __init__(self, *, blinking_closure_treshold, debug=False):
        self.blinking_closure_treshold = blinking_closure_treshold

        self.debug: bool = debug

        self.face = None
        # counts the number of consecutive frames the eye was closed
        self.left_blink_counter: int = 0
        self.right_blink_counter: int = 0
        self.both_blink_counter: int = 0

    def update_face(self, face):
        self.face = face

    def closure(self):
        ###* determining whether eyes are closed or not ###
        upper_right_eye_coordinates = self.face.landmark[159]
        lower_right_eye_coordinates = self.face.landmark[145]

        upper_left_eye_coordinates = self.face.landmark[386]
        lower_left_eye_coordinates = self.face.landmark[373]

        left_eye_closure = abs(
            upper_left_eye_coordinates.y - lower_left_eye_coordinates.y
        )
        right_eye_closure = abs(
            upper_right_eye_coordinates.y - lower_right_eye_coordinates.y
        )

        left_blinking: bool = left_eye_closure < self.blinking_closure_treshold
        right_blinking: bool = right_eye_closure < self.blinking_closure_treshold

        return left_blinking, right_blinking

    def yaw(self) -> float:
        outer_right_eye_coordinates = self.face.landmark[33]
        outer_left_eye_coordinates = self.face.landmark[263]

        # calculates the spatial displacment of the lips and the distance between them
        dx = outer_left_eye_coordinates.x - outer_right_eye_coordinates.x
        dz = outer_left_eye_coordinates.z - outer_right_eye_coordinates.z
        horizontal_dist_eyes = sqrt(dx**2 + dz**2)

        # determines the yaw of the head using the relative positions of the lips
        cos_yaw = dx / horizontal_dist_eyes
        sin_yaw = dz / horizontal_dist_eyes
        yaw_angle = atan2(sin_yaw, cos_yaw)

        return yaw_angle

    def pitch(self) -> float:
        nose_coordinates = self.face.landmark[1]
        chin_coordinates = self.face.landmark[199]

        # calculates the spatial displacment of the chin compared to the nose and the distance between them
        dy = nose_coordinates.y - chin_coordinates.y
        dz = nose_coordinates.z - chin_coordinates.z
        dist_nose_chin = sqrt(dy**2 + dz**2)

        # determines the pitch of the head using the relative positions of the nose and the chin
        cos_pitch = dy / dist_nose_chin
        sin_pitch = dz / dist_nose_chin
        pitch_angle = atan2(sin_pitch, cos_pitch)

        return pitch_angle

    def yaw_and_pitch(self):
        nose_coordinates = self.face.landmark[1]
        outer_right_eye_coordinates = self.face.landmark[33]
        outer_left_eye_coordinates = self.face.landmark[263]

        normal = np.cross(
            [
                outer_right_eye_coordinates.x - nose_coordinates.x,
                outer_right_eye_coordinates.y - nose_coordinates.y,
                outer_right_eye_coordinates.z - nose_coordinates.z,
            ],
            [
                outer_left_eye_coordinates.x - nose_coordinates.x,
                outer_left_eye_coordinates.y - nose_coordinates.y,
                outer_left_eye_coordinates.z - nose_coordinates.z,
            ],
        )

        norm = sqrt(normal[0] ** 2 + normal[1] ** 2 + normal[2] ** 2)
        flat_norm = sqrt(normal[0] ** 2 + normal[2] ** 2)

        yaw_angle = atan2(normal[0] / flat_norm, normal[2] / flat_norm)
        pitch_angle = atan2(normal[1] / norm, flat_norm / norm)

        return yaw_angle, pitch_angle

    def roll(self) -> float:
        outer_right_eye_coordinates = self.face.landmark[33]
        outer_left_eye_coordinates = self.face.landmark[263]

        dx = outer_left_eye_coordinates.x - outer_right_eye_coordinates.x
        dy = outer_left_eye_coordinates.y - outer_right_eye_coordinates.y
        horizontal_dist_eyes = sqrt(dx**2 + dy**2)

        # determines the roll of the head using the relative positions of the lips
        cos_roll = dx / horizontal_dist_eyes
        sin_roll = dy / horizontal_dist_eyes
        roll_angle = atan2(sin_roll, cos_roll)

        return roll_angle
