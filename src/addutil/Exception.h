/************************************************************************
  This file is part of Freekick.

  Freekick is free software: you can redistribute it and/or modify
  it under the terms of the GNU Affero General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  Freekick is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU Affero General Public License for more details.

  You should have received a copy of the GNU Affero General Public License
  along with Freekick.  If not, see <http://www.gnu.org/licenses/>.

  Copyright Antti Salonen, 2008
**************************************************************************/


#ifndef ADDUTIL_EXCEPTION_H
#define ADDUTIL_EXCEPTION_H

#include <string>
#include <iostream>
#include <exception>

#include <boost/exception.hpp>
#include <boost/version.hpp>

/**
 * class Exception
 */

namespace addutil
{
    class Exception : public std::exception
    {
    public:
        Exception(const std::string& msg);
        virtual ~Exception() throw() { }
        virtual const char* what() const throw();
    private:
        std::string m_msg;
    };
    
    void output_boost_exception(boost::exception& e, const std::string& msg = "A boost::exception has occurred");
}

#endif // ADDUTIL_EXCEPTION_H
