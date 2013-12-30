
#ifndef __camera_camera_hpp_

#define __camera_camera_hpp_

#include <GL/glut.h>
#include <glm/glm.hpp>

class camera {
   private:
      glm::vec3 viewDir,
                rightVector,
                upVector,
                position;

      float rotatedX,
            rotatedY,
            rotatedZ;

   public:
      camera(void);

      // positions the camera by transforming the scene
      void place(void);

      void move(const glm::vec3 &dir);

      // x rotation
      void pitch(const float angle);
      // y rotation
      void yaw(const float angle);
      // z rotation
      void roll(const float angle);

      // z movement
      void advance(const float distance);
      // y movement
      void ascend(const float distance);
      // x movement
      void strafe(const float distance);
};

#endif //__camera_camera_hpp_
