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

#include "Drawable.h"

const Vector3& Drawable::getPosition ( ) const
{
    return position;
}

const Color& Drawable::getColor ( ) const
{
    return color;
}

const std::string& Drawable::getModel ( ) const
{
    return model;
}

const Vector3& Drawable::getDirection ( ) const
{
    return direction;
}

void Drawable::setPosition(float x, float y, float z)
{
    boost::posix_time::ptime this_move = boost::posix_time::microsec_clock::universal_time();
    boost::posix_time::time_period diff_time(last_move, this_move);
    boost::posix_time::time_duration diff_dur = diff_time.length();
    long us_diff = diff_dur.total_microseconds();

    float xdiff = x - old_position.x;
    float ydiff = y - old_position.y;
    float zdiff = z - old_position.z;
    float scale = us_diff / 1000000.0f;
    setVelocity(xdiff / scale, ydiff / scale, zdiff / scale);

    last_move = this_move;
    float vel_length = velocity.length();
    if(zdiff)
        std::cout << this_move << " us_diff: " << us_diff << " vel.z: " << velocity.z << std::endl;

    if(autoorientation)
    {
        // float xdiff = x - position.x;
        // float ydiff = y - position.y;
        // float zdiff = z - position.z;
        if(xdiff)
            direction.x = xdiff;
        if(ydiff)
            direction.y = ydiff;
        if(zdiff)
            direction.z = zdiff;
        if(xdiff || ydiff || zdiff)
            direction.normalize();
    }
    position.x = x;
    position.y = y;
    position.z = z;
    old_position = position;
}

void Drawable::setDirection(float x, float y, float z)
{
    direction.x = x;
    direction.y = y;
    direction.z = z;
    direction.normalize();
}

void Drawable::setModel(std::string m)
{
    model = m;
}

void Drawable::setColor(float r, float g, float b)
{
    if(r < 0.0f || g < 0.0f || b < 0.0f ||
       r > 1.0f || g > 1.0f || b > 1.0f)
        throw "Drawable::setColor: invalid parameters";
    color.red = r;
    color.green = g;
    color.blue = b;
}

void Drawable::setColor(const Color& col)
{
    setColor(col.red, col.green, col.blue);
}

void Drawable::setAutomaticOrientation(bool a)
{
    autoorientation = a;
}

const Vector3& Drawable::getVelocity() const
{
    return velocity;
}

const Vector3& Drawable::getAcceleration() const
{
    return acceleration;
}

void Drawable::setVelocity(float x, float y, float z)
{
    velocity.x = x;
    velocity.y = y;
    velocity.z = z;
}

void Drawable::setAcceleration(float x, float y, float z)
{
    acceleration.x = x;
    acceleration.y = y;
    acceleration.z = z;
}

void Drawable::update(float interval)
{
    velocity += (acceleration * interval);
    position += (velocity * interval);
}

void Drawable::interpolate(boost::posix_time::ptime pt)
{
    boost::posix_time::time_period diff_time(last_move, pt);
    boost::posix_time::time_duration diff_dur = diff_time.length();
    long us_diff = diff_dur.total_microseconds();
    position = old_position + (velocity * (us_diff / 1000000.0f));
}
