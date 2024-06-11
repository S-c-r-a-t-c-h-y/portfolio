import cv2
import mediapipe as mp
from math import *
import screeninfo
from mouse_control import *
from face_control import *


DEBUG = True


def get_screen_metrics():
    screens = screeninfo.get_monitors()
    screen = screens[0]

    return screen.width, screen.height


SCREEN_WIDTH, SCREEN_HEIGHT = get_screen_metrics()

mp_drawing = mp.solutions.drawing_utils
mp_drawing_styles = mp.solutions.drawing_styles
mp_face_mesh = mp.solutions.face_mesh


DOWN_SCROLL_ROLL = -30
UP_SCROLL_ROLL = 30


### * control over the blinking of the eyes ###
# closure of the eye needed to count as closed
BLINKING_TRESHOLD: float = 0.012

# number of frames needed to register it as a click
BLINKING_COUNT_TRESHOLD: int = 4


controller = FaceControl(
    SCREEN_WIDTH,
    SCREEN_HEIGHT,
    blinking_closure_treshold=BLINKING_TRESHOLD,
    frames_blinking_before_eyes_closed=BLINKING_COUNT_TRESHOLD,
    debug=DEBUG,
)


drawing_spec = mp_drawing.DrawingSpec(thickness=1, circle_radius=1)
cap = cv2.VideoCapture(0)
with mp_face_mesh.FaceMesh(
    max_num_faces=1,
    refine_landmarks=True,
    min_detection_confidence=0.5,
    min_tracking_confidence=0.5,
) as face_mesh:
    while cap.isOpened():
        success, image = cap.read()
        if not success:
            print("Ignoring empty camera frame.")
            # If loading a video, use 'break' instead of 'continue'.
            continue

        # To improve performance, optionally mark the image as not writeable to
        # pass by reference.
        image.flags.writeable = False
        image = cv2.cvtColor(image, cv2.COLOR_BGR2RGB)
        results = face_mesh.process(image)

        if not results.multi_face_landmarks:
            continue

        face = results.multi_face_landmarks[0]
        # The coordinates of the nth landmark are stored in face.landmark[n]

        controller.update(face)

        ###* Draw the face mesh annotations on the image. ###

        if DEBUG:
            image.flags.writeable = True
            image = cv2.cvtColor(image, cv2.COLOR_RGB2BGR)

            mp_drawing.draw_landmarks(
                image=image,
                landmark_list=face,
                connections=mp_face_mesh.FACEMESH_TESSELATION,
                landmark_drawing_spec=None,
                connection_drawing_spec=mp_drawing_styles.get_default_face_mesh_tesselation_style(),
            )
            mp_drawing.draw_landmarks(
                image=image,
                landmark_list=face,
                connections=mp_face_mesh.FACEMESH_CONTOURS,
                landmark_drawing_spec=None,
                connection_drawing_spec=mp_drawing_styles.get_default_face_mesh_contours_style(),
            )
            mp_drawing.draw_landmarks(
                image=image,
                landmark_list=face,
                connections=mp_face_mesh.FACEMESH_IRISES,
                landmark_drawing_spec=None,
                connection_drawing_spec=mp_drawing_styles.get_default_face_mesh_iris_connections_style(),
            )
            ####################################################

            # Flip the image horizontally for a selfie-view display.
            cv2.imshow("MediaPipe Face Mesh", cv2.flip(image, 1))

            key = cv2.waitKey(5)
            if key & 0xFF == ord("s"):
                break
            elif key & 0xFF == ord("u"):
                controller.calibrate()

cap.release()
print("stopped")
