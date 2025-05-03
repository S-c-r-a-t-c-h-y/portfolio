from math import *
import numpy as np
import cv2 as cv
import enum

"""
This module provides a class `FaceAnalyser` that allows analysing face landmarks and determining face movements and eye blinks.
Classes:
    FaceAnalyser
"""


RIGHT_EYE = [33, 7, 163, 144, 145, 153, 154, 155, 133, 173, 157, 158, 159, 160, 161, 246]
LEFT_EYE = [362, 382, 381, 380, 374, 373, 390, 249, 263, 466, 388, 387, 386, 385, 384, 398]


class EyePosition(enum.Enum):
    """
    An enumeration to represent the position of the eye.
    """

    RIGHT = "RIGHT"
    CENTER = "CENTER"
    LEFT = "LEFT"
    CLOSED = "CLOSED"


class FaceAnalyser:
    """
    A class to analyse face landmarks and determine face movements and eye blinks.
    """

    def __init__(self, *, blinking_closure_treshold, eye_darkness_treshold, debug=False):
        """
        Initializes the FaceAnalyser class.
        Args:
            blinking_closure_treshold (float): The threshold for detecting eye closure.
            debug (bool, optional): Flag to enable or disable debug mode. Defaults to False.
        """
        self.blinking_closure_treshold = blinking_closure_treshold

        self.debug: bool = debug

        self.face = None
        self.image = None
        # counts the number of consecutive frames the eye was closed
        self.left_blink_counter: int = 0
        self.right_blink_counter: int = 0
        self.both_blink_counter: int = 0

        self.eye_darkness_treshold = eye_darkness_treshold

    def update(self, face, image):
        """
        Updates the face landmark data.
        Args:
            face (Face): The face data to be analysed.
            image (np.array): The image data.
        """
        self.face = face
        self.image = image

    def closure(self):
        """
        Determines whether the eyes are closed or not.
        Returns:
            Tuple[bool, bool]: A tuple containing the left and right eye closure status.
        """
        upper_right_eye_coordinates = self.face.landmark[159]
        lower_right_eye_coordinates = self.face.landmark[145]

        upper_left_eye_coordinates = self.face.landmark[386]
        lower_left_eye_coordinates = self.face.landmark[373]

        left_eye_closure = abs(upper_left_eye_coordinates.y - lower_left_eye_coordinates.y)
        right_eye_closure = abs(upper_right_eye_coordinates.y - lower_right_eye_coordinates.y)

        left_blinking: bool = left_eye_closure < self.blinking_closure_treshold
        right_blinking: bool = right_eye_closure < self.blinking_closure_treshold

        return left_blinking, right_blinking

    def yaw(self) -> float:
        """
        Determines the yaw of the head.
        Returns:
            float: The yaw angle of the head.
        """
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
        """
        Determines the pitch of the head
        Returns:
            float: The pitch angle of the head.
        """
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
        """
        Determines the yaw and pitch of the head.
        Returns:
            Tuple[float, float]: A tuple containing the yaw and pitch angles of the head.
        """
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
        """
        Determines the roll of the head.
        Returns:
            float: The roll angle of the head.
        """
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

    def __eyes_extractor(img, right_eye_coords, left_eye_coords):
        # converting color image to  scale image
        gray = cv.cvtColor(img, cv.COLOR_RGB2GRAY)

        # getting the dimension of image
        dim = gray.shape

        # creating mask from gray scale dim
        mask = np.zeros(dim, dtype=np.uint8)

        # drawing Eyes Shape on mask with white color
        cv.fillPoly(mask, [np.array(right_eye_coords, dtype=np.int32)], 255)
        cv.fillPoly(mask, [np.array(left_eye_coords, dtype=np.int32)], 255)

        # showing the mask
        # cv.imshow('mask', mask)

        # draw eyes image on mask, where white shape is
        eyes = cv.bitwise_and(gray, gray, mask=mask)
        # change black color to gray other than eys
        # cv.imshow('eyes draw', eyes)
        eyes[mask == 0] = 155

        # getting minium and maximum x and y  for right and left eyes
        # For Right Eye
        r_max_x = (max(right_eye_coords, key=lambda item: item[0]))[0]
        r_min_x = (min(right_eye_coords, key=lambda item: item[0]))[0]
        r_max_y = (max(right_eye_coords, key=lambda item: item[1]))[1]
        r_min_y = (min(right_eye_coords, key=lambda item: item[1]))[1]

        # For LEFT Eye
        l_max_x = (max(left_eye_coords, key=lambda item: item[0]))[0]
        l_min_x = (min(left_eye_coords, key=lambda item: item[0]))[0]
        l_max_y = (max(left_eye_coords, key=lambda item: item[1]))[1]
        l_min_y = (min(left_eye_coords, key=lambda item: item[1]))[1]

        # croping the eyes from mask
        cropped_right = eyes[r_min_y:r_max_y, r_min_x:r_max_x]
        cropped_left = eyes[l_min_y:l_max_y, l_min_x:l_max_x]

        # returning the cropped eyes
        return cropped_right, cropped_left

    def __estimate_position(self, cropped_eye):
        # getting height and width of eye
        h, w = cropped_eye.shape

        # remove the noise from images
        gaussain_blur = cv.GaussianBlur(cropped_eye, (9, 9), 0)
        median_blur = cv.medianBlur(gaussain_blur, 3)

        # applying thrsholding to convert binary_image
        ret, threshed_eye = cv.threshold(median_blur, self.eye_darkness_treshold, 255, cv.THRESH_BINARY)

        # cv.imshow("threshed_eye", threshed_eye)

        # create fixd part for eye with
        piece = int(w / 3)

        # slicing the eyes into three parts
        right_piece = threshed_eye[0:h, 0:piece]
        center_piece = threshed_eye[0:h, piece : piece + piece]
        left_piece = threshed_eye[0:h, piece + piece : w]

        # calling pixel counter function
        eye_position = FaceAnalyser.__count_pixel(right_piece, center_piece, left_piece)

        return eye_position

    # creating pixel counter function
    def __count_pixel(first_piece, second_piece, third_piece):
        # counting black pixel in each part
        right_part = np.sum(first_piece == 0)
        center_part = np.sum(second_piece == 0)
        left_part = np.sum(third_piece == 0)
        # creating list of these values
        eye_parts = [right_part, center_part, left_part]

        # getting the index of max values in the list
        max_index = eye_parts.index(max(eye_parts))
        pos_eye = ""
        if max_index == 0:
            pos_eye = EyePosition.RIGHT
        elif max_index == 1:
            pos_eye = EyePosition.CENTER
        elif max_index == 2:
            pos_eye = EyePosition.LEFT
        else:
            pos_eye = EyePosition.CLOSED
        return pos_eye

    # def __count_pixel(first_piece, second_piece, third_piece, fourth_piece, fifth_piece):
    #     # counting black pixel in each part
    #     right_part = np.sum(first_piece == 0)
    #     center_part = np.sum(second_piece == 0)
    #     left_part = np.sum(third_piece == 0)
    #     up_part = np.sum(fourth_piece == 0)
    #     down_part = np.sum(fifth_piece == 0)
    #     # creating list of these values
    #     eye_parts = [right_part, center_part, left_part, up_part, down_part]

    #     # getting the index of max values in the list
    #     max_index = eye_parts.index(max(eye_parts))
    #     pos_eye = ""
    #     if max_index == 0:
    #         pos_eye = "RIGHT"

    #     elif max_index == 1:
    #         pos_eye = "CENTER"

    #     elif max_index == 2:
    #         pos_eye = "LEFT"

    #     elif max_index == 3:
    #         pos_eye = "UP"

    #     elif max_index == 4:
    #         pos_eye = "DOWN"

    #     else:
    #         pos_eye = "CLOSED"

    #     return pos_eye

    # def __estimate_position(cropped_eye):
    #     # getting height and width of eye
    #     h, w = cropped_eye.shape

    #     # remove the noise from images
    #     gaussain_blur = cv.GaussianBlur(cropped_eye, (9, 9), 0)
    #     median_blur = cv.medianBlur(gaussain_blur, 3)

    #     # applying thrsholding to convert binary_image
    #     ret, threshed_eye = cv.threshold(median_blur, 130, 255, cv.THRESH_BINARY)

    #     # create fixd part for eye with
    #     piece = int(w / 3)
    #     place = int(h / 3)
    #     down = int(h / 2)
    #     # slicing the eyes into three parts
    #     right_piece = threshed_eye[0:h, 0:piece]
    #     center_piece = threshed_eye[0:h, piece : piece + piece]
    #     left_piece = threshed_eye[0:h, piece + piece : w]
    #     up_piece = threshed_eye[0:down, 0:w]
    #     down_piece = threshed_eye[down:h, 0:w]

    #     # calling pixel counter function
    #     eye_position = FaceAnalyser.__count_pixel(right_piece, center_piece, left_piece, up_piece, down_piece)

    #     return eye_position

    def eye_positions(self):
        img_height, img_width = self.image.shape[:2]

        right_coords = [(int((point := self.face.landmark[p]).x * img_width), int(point.y * img_height)) for p in RIGHT_EYE]
        left_coords = [(int((point := self.face.landmark[p]).x * img_width), int(point.y * img_height)) for p in LEFT_EYE]
        crop_right, crop_left = FaceAnalyser.__eyes_extractor(self.image, right_coords, left_coords)

        # cv.imshow("right", crop_right)
        # cv.imshow("left", crop_left)

        eye_position_right = self.__estimate_position(crop_right)
        eye_position_left = self.__estimate_position(crop_left)

        return eye_position_left, eye_position_right
