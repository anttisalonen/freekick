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

#include "Exception.h"

namespace addutil
{
    void output_boost_exception(boost::exception& e, const std::string& msg)
    {
#if defined(BOOST_VERSION) && (BOOST_VERSION < 103700)
        std::cerr << msg << ": " << e.diagnostic_information() << std::endl;
#else 
        std::cerr << msg << ": " << boost::exception_detail::get_diagnostic_information(e) << std::endl;
#endif
    }
}