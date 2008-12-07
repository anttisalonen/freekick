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
**************************************************************************/

#include "Entity.h"

namespace addutil
{
    Entity::Entity(float _mass, std::string _model)
        : model(_model),
          mass(_mass)
    {
    }

    const Vector3& Entity::getPosition ( ) const
    {
        return position;
    }

    const Color& Entity::getColor ( ) const
    {
        return color;
    }

    const std::string& Entity::getModel ( ) const
    {
        return model;
    }

    /*
    const Vector3 Entity::getDirection ( ) const
    {
        return orientation.toVector();
    }
    */

    const Quaternion& Entity::getOrientation() const
    {
        return orientation;
    }

    void Entity::setMass(float m)
    {
        mass = m;
    }

    void Entity::setPosition(float x, float y, float z, boost::posix_time::ptime pt)
    {
        float xdiff = x - old_position.x;
        float ydiff = y - old_position.y;
        float zdiff = z - old_position.z;

        if(pt != boost::posix_time::not_a_date_time)
        {
            boost::posix_time::time_period diff_time(last_move, pt);
            boost::posix_time::time_duration diff_dur = diff_time.length();
            long us_diff = diff_dur.total_microseconds();

            float scale = us_diff / 1000000.0f;
            setVelocity(xdiff / scale, ydiff / scale, zdiff / scale);

            last_move = pt;
        }

        if(autoorientation)
        {
            // TODO
            /*
            Vector3 v(xdiff, ydiff, zdiff);
            v.normalize();
            float yaw   = atan2f(v.y, v.x);
            float pitch = asinf(v.z);
            if(xdiff)
                direction.x = xdiff;
            if(ydiff)
                direction.y = ydiff;
            if(zdiff)
                direction.z = zdiff;
            if(xdiff || ydiff || zdiff)
                direction.normalize();
            */
        }

        position.x = x;
        position.y = y;
        position.z = z;
        old_position = position;
    }

    /*
    void Entity::setDirection(float x, float y, float z)
    {
        orientation.fromVector(x, y, z);
    }
    */

    void Entity::setOrientation(float w, float x, float y, float z)
    {
        orientation.set(w, x, y, z);
    }

    void Entity::setModel(std::string m)
    {
        model = m;
    }

    void Entity::setColor(float r, float g, float b)
    {
        if(r < 0.0f || g < 0.0f || b < 0.0f ||
           r > 1.0f || g > 1.0f || b > 1.0f)
            throw "Entity::setColor: invalid parameters";
        color.red = r;
        color.green = g;
        color.blue = b;
    }

    void Entity::setColor(const Color& col)
    {
        setColor(col.red, col.green, col.blue);
    }

    void Entity::setAutomaticOrientation(bool a)
    {
        autoorientation = a;
    }

    const Vector3& Entity::getVelocity() const
    {
        return velocity;
    }

    const Vector3& Entity::getAcceleration() const
    {
        return acceleration;
    }

    void Entity::setVelocity(float x, float y, float z)
    {
        velocity.x = x;
        velocity.y = y;
        velocity.z = z;
    }

    void Entity::setAcceleration(float x, float y, float z)
    {
        acceleration.x = x;
        acceleration.y = y;
        acceleration.z = z;
    }

    void Entity::update(float interval)
    {
        velocity += (acceleration * interval);
        position += (velocity * interval);
    }

    void Entity::interpolate(boost::posix_time::ptime pt)
    {
        boost::posix_time::time_period diff_time(last_move, pt);
        boost::posix_time::time_duration diff_dur = diff_time.length();
        long us_diff = diff_dur.total_microseconds();
        position = old_position + (velocity * (us_diff / 1000000.0f));
    }

    void Entity::update(int v, float x, float y, float z)
    {
        switch(v)
        {
            case 0:
                setPosition(x, y, z);
                break;
            case 1:
                setVelocity(x, y, z);
                break;
            case 2:
                setAcceleration(x, y, z);
                break;
            default:
                break;
        }
    }
}
