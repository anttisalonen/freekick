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


#ifndef ADDUTIL_XML_H
#define ADDUTIL_XML_H

#include <string>
#include <sstream>

#include <libxml/parser.h>
#include <libxml/tree.h>

#include "Exception.h"
#include "General.h"

namespace addutil
{
    namespace xml
    {
        xmlNodePtr add_child(xmlNodePtr parent, const char* name);
        void add_attribute(xmlNodePtr n, const char* name, const char* value);
        void add_attribute(xmlNodePtr n, const char* name, int value);
        void add_name_attribute(xmlNodePtr n, const char* value, const char* first_name_attr_name, const char* last_name_attr_name);
        bool node_has_name(const xmlNodePtr node, const char* name);
        bool node_is_node(const xmlNodePtr node, const char* name);
        void color_to_xml(xmlNodePtr parent, char color, const char* node_name);
        void get_attribute(const xmlNodePtr node, const char* attrname, std::string& attrvalue);
        void get_attribute(const xmlNodePtr node, const char* attrname, int& attrvalue);
        void get_attribute(const xmlNodePtr node, const char* attrname, float& attrvalue);
        void node_to_stringstream(const xmlNodePtr node, std::ostringstream& oss, bool formatted);
    }
}

#endif
