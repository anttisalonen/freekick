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

#include "Exception.h"

namespace addutil
{
    Exception::Exception(const std::string& msg)
        : m_msg(msg)
    {
    }

    const char* Exception::what() const throw ()
    {
        return m_msg.c_str();
    }

    void output_boost_exception(boost::exception& e, const std::string& msg)
    {
#if defined(BOOST_VERSION) && (BOOST_VERSION < 103700)
        std::cerr << msg << ": " << e.diagnostic_information() << std::endl;
#elif BOOST_VERSION < 104000 
        std::cerr << msg << ": " << boost::exception_detail::get_diagnostic_information(e) << std::endl;
#else 
        std::cerr << msg << ": " << boost::exception_detail::get_diagnostic_information(e, __func__) << std::endl;
#endif
    }
}
