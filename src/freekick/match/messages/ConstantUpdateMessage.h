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


#ifndef FREEKICK_MATCH_MESSAGES_CONSTANTUPDATEMESSAGE_H
#define FREEKICK_MATCH_MESSAGES_CONSTANTUPDATEMESSAGE_H

#include <string>
#include <cstdlib>

#include <boost/regex.hpp>
#include <boost/array.hpp>

#include "addutil/Vector3.h"
#include "addutil/Quaternion.h"

#include "CausedStatus.h"
#include "ControlledStatus.h"

#include "StandardMessage.h"

namespace freekick
{
    namespace match
    {
        namespace messages
        {
            class ConstantUpdateMessage : public StandardMessage
            {
            public:
                ConstantUpdateMessage(PlayerID plid, int der, 
                                      addutil::Vector3 vec, addutil::Quaternion quat, 
                                      CausedStatus causs = CausedStatus(), ControlledStatus contrs = ControlledStatus())
                    : m_plid(plid)
                    , m_der(der)
                    , m_vec(vec)
                    , m_quat(quat)
                    , m_causs(causs)
                    , m_contrs(contrs)
                {
                }
                ConstantUpdateMessage(std::string& msg)
                {
                    using namespace boost;
                    regex expr(".*?\\(([-0-9]+) +([012]) +([[:print:]]+?) +([[:print:]]+?) +([[:print:]]+?) +([[:print:]]+?) +([[:print:]]+?) +([[:print:]]+?) +([[:print:]]+?) +([0-9]+) +([0-9]+)\\).*");
                    cmatch what;
                    if(regex_match(msg.c_str(), what, expr))
                    {
                        boost::array<std::string, 11> ss;
                        for(int i = 0; i < 11; i++)
                        {
                            ss[i].assign(what[i + 1].first, what[i + 1].second);
                        }
                        m_plid   = atoi(ss[0].c_str());
                        m_der    = atoi(ss[1].c_str());
                        m_vec.x  = atof(ss[2].c_str());
                        m_vec.y  = atof(ss[3].c_str());
                        m_vec.z  = atof(ss[4].c_str());
                        m_quat.w = atof(ss[5].c_str());
                        m_quat.x = atof(ss[6].c_str());
                        m_quat.y = atof(ss[7].c_str());
                        m_quat.z = atof(ss[8].c_str());
                        m_causs  = atoi(ss[9].c_str());
                        m_contrs = atoi(ss[10].c_str());
                    }
                    else
                        throw "Constant Update Message: failed parse";                    
                }
                virtual ~ConstantUpdateMessage() { }
                const std::string toString() const
                {
                    std::ostringstream oss(std::ostringstream::out);
                    oss << m_plid << " " << m_der << " " 
                        << m_vec.x << " " << m_vec.y << " " << m_vec.z << " " 
                        << m_quat.w << " " << m_quat.x << " " << m_quat.y << " " << m_quat.z << " "
                        << m_causs << " " << m_contrs;
                    return stdString(oss.str());
                }
                void getVector(addutil::Vector3& t) const { t = m_vec; }
                void getQuaternion(addutil::Quaternion& t) const { t = m_quat; }
                PlayerID getPlayerID() const { return m_plid; }
                int getDerivative() const { return m_der; }
                void getCausedStatus(CausedStatus& t) const { t = m_causs; }
                void getControlledStatus(ControlledStatus& t) const { t = m_contrs; }

            private:
                PlayerID m_plid;
                int m_der;
                addutil::Vector3 m_vec;
                addutil::Quaternion m_quat;
                CausedStatus m_causs;
                ControlledStatus m_contrs;
            };
        }
    }
}

#endif
