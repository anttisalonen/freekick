/************************************************************************
  This file is part of Freekick.

  Freekick is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  Freekick is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with Freekick.  If not, see <http://www.gnu.org/licenses/>.

  Copyright Antti Salonen, 2008
  This file was generated on So Okt 26 2008 at 12:09:20
**************************************************************************/


#ifndef ENTITY_H
#define ENTITY_H

#include <iostream>
#include <string>
#include <exception>
#include <boost/date_time/posix_time/posix_time.hpp>

#include "Vector3.h"
#include "Color.h"

/**
 * class Entity
 */

namespace addutil
{
    class Entity
    {
    public:
        virtual ~Entity() { }
        /**
         * @return Vector3*
         */
        virtual const Vector3& getPosition ( ) const;
        /**
         * @return Color*
         */
        virtual const Color& getColor ( ) const;
        /**
         * @return string*
         */
        virtual const std::string& getModel ( ) const;
        /**
         * @return Vector3*
         */
        virtual const Vector3& getDirection ( ) const;
        virtual const Vector3& getVelocity() const;
        virtual const Vector3& getAcceleration() const;
        void setMass(float m);
        void setPosition(float x, float y, float z);
        void setVelocity(float x, float y, float z);
        void setAcceleration(float x, float y, float z);
        void setDirection(float x, float y, float z);
        void setModel(std::string m);
        void setColor(float r, float g, float b);
        void setColor(const Color& col);
        void setAutomaticOrientation(bool a);
        void update(float interval);
        void update(int v, float x, float y, float z);
        void interpolate(boost::posix_time::ptime pt);
        virtual const int getID() = 0;

    protected:
        Entity(float _mass = 0.0f, std::string _model = "");

    private:
        Vector3 position;
        Vector3 old_position;
        Vector3 velocity;
        Vector3 acceleration;
        Vector3 direction;
        std::string model;
        float mass;
        Color color;
        bool autoorientation;
        boost::posix_time::ptime last_move;
    };
}

#endif // ENTITY_H
